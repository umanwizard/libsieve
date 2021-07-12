use crate::sema;
use anyhow::bail;
use bmail::{Message, SmtpEnvelope};
use sema::Ast;
use sema::{AddressPart, AddressTest, EnvelopeTest, ExistsTest, HeaderTest, SizeTest, TestCommand};
use std::collections::{hash_map::Entry::Occupied, hash_map::Entry::Vacant, HashMap, HashSet};

#[derive(Debug)]
pub enum Action<'a> {
    Fileinto(&'a str),
    Redirect(&'a str),
    Keep,
}

#[derive(PartialEq)]
enum KeepStyle {
    Explicit,
    Implicit,
    Cancelled,
}

struct Context<'a> {
    cap_fileinto: bool,
    cap_envelope: bool,
    keep: KeepStyle,
    fileintos: HashSet<&'a str>,
    redirects: HashSet<&'a str>,
    stopped: bool,
}

impl<'a> Default for Context<'a> {
    fn default() -> Self {
        Self {
            cap_fileinto: false,
            cap_envelope: false,
            keep: KeepStyle::Implicit,
            fileintos: HashSet::default(),
            redirects: HashSet::default(),
            stopped: false,
        }
    }
}

fn extract_ap(part: AddressPart, address: &str) -> anyhow::Result<&str> {
    let txt = match part {
        sema::AddressPart::Localpart => address.split('@').next().unwrap(),
        sema::AddressPart::Domain => address.splitn(2, '@').nth(1).unwrap_or(address),
        sema::AddressPart::All => address,
    };
    Ok(txt)
}

fn check_test<'a, 'm>(
    test: &'a sema::TestCommand,
    msg: &'m Message<'m>,
    header_idx: &'m HashMap<Vec<u8>, Vec<usize>>,
    ctx: &mut Context<'a>,
    env: &'m SmtpEnvelope,
) -> anyhow::Result<bool> {
    match test {
        TestCommand::Address(AddressTest {
            address_part,
            header_list,
            key_list,
        }) => {
            for h in header_list {
                for &idx in header_idx.get(h.as_bytes()).unwrap_or(&vec![]) {
                    let txt = extract_ap(
                        *address_part,
                        std::str::from_utf8(&msg.header()[idx].unfolded_value().0)?,
                    )?;
                    for k in key_list {
                        if k.is_match(txt) {
                            return Ok(true);
                        }
                    }
                }
            }
            Ok(false)
        }
        TestCommand::Allof(inner) => {
            for inner in inner {
                let res = check_test(inner, msg, header_idx, ctx, env)?;
                if !res {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        TestCommand::Anyof(inner) => {
            for inner in inner {
                if check_test(inner, msg, header_idx, ctx, env)? {
                    return Ok(true);
                }
            }
            Ok(false)
        }
        TestCommand::Envelope(EnvelopeTest {
            address_part,
            envelope_part,
            key_list,
        }) => {
            for ep in envelope_part {
                let ep = match ep.to_ascii_lowercase().as_str() {
                    "from" => env.from.as_ref(),
                    "to" => env.to.as_ref(),
                    _ => bail!("Unrecognized envelope part: {}", ep),
                };
                if let Some(ep) = ep {
                    let txt = extract_ap(*address_part, ep)?;
                    for k in key_list {
                        if k.is_match(txt) {
                            return Ok(true);
                        }
                    }
                }
            }
            Ok(false)
        }
        TestCommand::Exists(ExistsTest { header_names }) => Ok(header_names
            .iter()
            .all(|hn| header_idx.get(hn.as_bytes()).is_some())),
        TestCommand::False => Ok(false),
        TestCommand::Header(HeaderTest {
            header_names,
            key_list,
        }) => {
            for h in header_names {
                for field in header_idx
                    .get(h.as_bytes())
                    .unwrap_or(&vec![])
                    .iter()
                    .map(|&idx| &msg.header()[idx])
                {
                    for k in key_list {
                        if k.is_match(std::str::from_utf8(&field.unfolded_value().0)?) {
                            return Ok(true);
                        }
                    }
                }
            }
            Ok(false)
        }
        TestCommand::Not(inner) => Ok(!check_test(&**inner, msg, header_idx, ctx, env)?),
        TestCommand::Size(SizeTest { over, limit }) => Ok(if *over {
            (msg.size() as u64) > *limit
        } else {
            (msg.size() as u64) < *limit
        }),
        TestCommand::True => Ok(true),
    }
}

fn execute_command<'a, 'm>(
    cmd: &'a sema::TopLevelCommand,
    msg: &'m Message<'m>,
    header_idx: &'m HashMap<Vec<u8>, Vec<usize>>,
    ctx: &mut Context<'a>,
    env: &'m SmtpEnvelope,
) -> anyhow::Result<()> {
    use sema::*;
    use TopLevelCommand::*;
    match cmd {
        If(IfControl {
            branches,
            else_branch,
        }) => {
            let mut hit = false;
            for (test, block) in branches {
                if check_test(test, msg, header_idx, ctx, env)? {
                    execute_block(&block.0, msg, header_idx, ctx, env)?;
                    hit = true;
                    break;
                }
            }
            if !hit {
                if let Some(Block(cmds)) = else_branch {
                    execute_block(&cmds, msg, header_idx, ctx, env)?;
                }
            }
        }
        Require(RequireControl { capabilities }) => {
            for cap in capabilities {
                match cap.as_str() {
                    "fileinto" => ctx.cap_fileinto = true,
                    "envelope" => ctx.cap_envelope = true,
                    _ => bail!("Capability {} not implemented.", cap),
                }
            }
        }
        Stop => {
            ctx.stopped = true;
            return Ok(());
        }
        Fileinto(s) => {
            if ctx.keep == KeepStyle::Implicit {
                ctx.keep = KeepStyle::Cancelled;
            }
            ctx.fileintos.insert(s);
        }
        Redirect(s) => {
            if ctx.keep == KeepStyle::Implicit {
                ctx.keep = KeepStyle::Cancelled;
            }
            ctx.redirects.insert(s);
        }
        Keep => {
            ctx.keep = KeepStyle::Explicit;
        }
        Discard => {
            if ctx.keep == KeepStyle::Implicit {
                ctx.keep = KeepStyle::Cancelled;
            }
        }
    }
    Ok(())
}

fn execute_block<'a, 'm>(
    cmds: &'a [sema::TopLevelCommand],
    msg: &'m Message<'m>,
    header_idx: &'m HashMap<Vec<u8>, Vec<usize>>,
    ctx: &mut Context<'a>,
    env: &'m SmtpEnvelope,
) -> anyhow::Result<()> {
    for cmd in cmds {
        execute_command(cmd, msg, header_idx, ctx, env)?;
        if ctx.stopped {
            break;
        }
    }
    Ok(())
}

pub fn execute<'a, 'm>(
    ast: &'a Ast,
    msg: &'m Message<'m>,
    env: &'m SmtpEnvelope,
) -> anyhow::Result<Vec<Action<'a>>> {
    let mut header_idx: HashMap<Vec<u8>, Vec<usize>> = HashMap::new();
    for (idx, h) in msg.header().iter().enumerate() {
        let mut name = h.name().0.to_vec();
        for ch in name.iter_mut() {
            *ch = ch.to_ascii_uppercase();
        }
        match header_idx.entry(name) {
            Occupied(oe) => oe.into_mut().push(idx),
            Vacant(ve) => {
                ve.insert(vec![idx]);
            }
        }
    }
    let mut ctx = Context::default();
    execute_block(&ast.commands, msg, &header_idx, &mut ctx, env)?;
    let mut result = vec![];
    result.extend(ctx.redirects.iter().map(|&addr| Action::Redirect(addr)));
    result.extend(ctx.fileintos.iter().map(|&file| Action::Fileinto(file)));
    let keep = match ctx.keep {
        KeepStyle::Cancelled => false,
        KeepStyle::Implicit | KeepStyle::Explicit => true,
    };
    if keep {
        result.push(Action::Keep);
    }
    Ok(result)
}
