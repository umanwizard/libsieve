use crate::{email, sema};
use email::ParsedMessage;
use sema::Ast;
use std::collections::HashSet;

pub enum Action<'a> {
    Fileinto(&'a str),
    Redirect(&'a str),
    Keep,
}

#[derive(PartialEq)]
enum KeepStyle {
    Explicit,
    Implicit,
    Canceled,
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

fn check_test<'a, 'm>(
    _test: &'a sema::TestCommand,
    _msg: &'m ParsedMessage<'m>,
    _ctx: &mut Context<'a>,
) -> Result<bool, String> {
    unimplemented!()
}

fn execute_command<'a, 'm>(
    cmd: &'a sema::TopLevelCommand,
    msg: &'m ParsedMessage<'m>,
    ctx: &mut Context<'a>,
) -> Result<(), String> {
    use sema::*;
    use TopLevelCommand::*;
    match cmd {
        If(IfControl {
            branches,
            else_branch,
        }) => {
            let mut hit = false;
            for (test, block) in branches {
                if check_test(test, msg, ctx)? {
                    execute_block(&block.0, msg, ctx)?;
                    hit = true;
                    break;
                }
            }
            if !hit {
                if let Some(Block(cmds)) = else_branch {
                    execute_block(&cmds, msg, ctx)?;
                }
            }
        }
        Require(RequireControl { capabilities }) => {
            for cap in capabilities {
                match cap.as_str() {
                    "fileinto" => ctx.cap_fileinto = true,
                    "envelope" => ctx.cap_envelope = true,
                    _ => return Err(format!("Capability {} not implemented.", cap)),
                }
            }
        }
        Stop => {
            ctx.stopped = true;
            return Ok(());
        }
        Fileinto(s) => {
            ctx.fileintos.insert(s);
        }
        Redirect(s) => {
            ctx.redirects.insert(s);
        }
        Keep => {
            ctx.keep = KeepStyle::Explicit;
        }
        Discard => {
            if ctx.keep == KeepStyle::Implicit {
                ctx.keep = KeepStyle::Canceled;
            }
        }
    }
    Ok(())
}

fn execute_block<'a, 'm>(
    cmds: &'a [sema::TopLevelCommand],
    msg: &'m ParsedMessage<'m>,
    ctx: &mut Context<'a>,
) -> Result<(), String> {
    for cmd in cmds {
        execute_command(cmd, msg, ctx)?;
        if ctx.stopped {
            break;
        }
    }
    Ok(())
}

pub fn execute<'a, 'm>(
    ast: &'a Ast,
    msg: &'m ParsedMessage<'m>,
) -> Result<Vec<Action<'a>>, String> {
    let mut ctx = Context::default();
    execute_block(&ast.commands, msg, &mut ctx)?;
    unimplemented!()
}
