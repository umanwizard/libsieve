use crate::parse;
use parse::Argument;

use parse::Command;
use parse::Document;
use parse::StringIsh;
use parse::Test;

use itertools::Either;
use std::iter;
use memmem::{TwoWaySearcher, Searcher};

type Result<Node> = std::result::Result<Node, String>;

#[derive(Debug)]
pub struct Ast {
    pub commands: Vec<TopLevelCommand>,
}
#[derive(Debug)]
pub enum TopLevelCommand {
    If(IfControl),
    Require(RequireControl),
    Stop,
    Fileinto(String),
    Redirect(String),
    Keep,
    Discard,
}
#[derive(Debug)]
pub struct IfControl {
    pub branches: Vec<(TestCommand, Block)>,
    pub else_branch: Option<Block>,
}
#[derive(Debug)]
pub struct Block(pub Vec<TopLevelCommand>);

#[derive(Debug)]
pub struct RequireControl {
    pub capabilities: Vec<String>,
}
#[derive(Debug)]
pub enum TestCommand {
    Address(AddressTest),
    Allof(Vec<TestCommand>),
    Anyof(Vec<TestCommand>),
    Envelope(EnvelopeTest),
    Exists(ExistsTest),
    False,
    Header(HeaderTest),
    Not(Box<TestCommand>),
    Size(SizeTest),
    True,
}

#[derive(Debug)]
pub enum Matcher {
    Is(String),
    Contains(String),
    Regex(regex::Regex),
}

#[derive(Debug)]
pub struct MatchKey {
    pub ascii_casemap: bool,
    pub matcher: Matcher,
}

impl MatchKey {
    // yes, the RFC allows you to assume haystacks can be converted to unicode!
    pub fn is_match(&self, haystack: &str) -> bool {
        // string_holder exists to make sure a newly allocated string, if necessary, isn't deleted
        // until the end of the frame
        let mut string_holder = None;
        let haystack = if self.ascii_casemap {
            string_holder = Some(haystack.to_ascii_uppercase());
            string_holder.as_ref().unwrap()
        } else {
            haystack
        };

        match &self.matcher {
            Matcher::Is(s) => s == haystack,
            // TODO: We shouldn't need to construct this every time,
            // but lifetimes make that hard to do, since there isn't
            // an owning version of TwoWaySearcher.
            Matcher::Contains(s) => TwoWaySearcher::new(s.as_bytes()).search_in(haystack.as_bytes()).is_some(),
            Matcher::Regex(r) => r.is_match(haystack),
        }
    }
    fn new(cmp: Comparator, typ: MatchType, mut s: String) -> Result<MatchKey> {
        let ascii_casemap = match cmp {
            AsciiCasemap => true,
            Octet => false,
        };
        if ascii_casemap {
            s.make_ascii_uppercase();
        }
        let matcher = match typ {
            MatchType::Is => Ok(Matcher::Is(s)),
            MatchType::Contains => Ok(Matcher::Contains(s)),
            MatchType::Matches => {
                let mut is_escaping = false;
                let s: String = iter::once('^')
                    .chain(s.chars().flat_map(move |ch| match ch {
                        '*' if !is_escaping => Either::Left(iter::once('.').chain(iter::once('*'))),
                        '?' if !is_escaping => Either::Right(Either::Left(iter::once('.'))),
                        '\\' if !is_escaping => {
                            is_escaping = true;
                            Either::Right(Either::Right(iter::empty()))
                        }
                        _ => {
                            is_escaping = false;
                            if regex_syntax::is_meta_character(ch) {
                                Either::Left(iter::once('\\').chain(iter::once(ch)))
                            } else {
                                Either::Right(Either::Left(iter::once(ch)))
                            }
                        }
                    }))
                    .chain(iter::once('$'))
                    .collect();
                if is_escaping {
                    Err(String::from("Unterminated escape"))
                } else {
                    Ok(Matcher::Regex(regex::Regex::new(&s).unwrap()))
                }
            }
        };
        matcher.map(|matcher| MatchKey {
            ascii_casemap,
            matcher,
        })
    }
}

#[derive(Debug)]
pub struct AddressTest {
    pub address_part: AddressPart,
    pub header_list: Vec<String>,
    pub key_list: Vec<MatchKey>,
}
#[derive(Debug)]
pub struct EnvelopeTest {
    pub address_part: AddressPart,
    pub envelope_part: Vec<String>,
    pub key_list: Vec<MatchKey>,
}
#[derive(Debug)]
pub struct ExistsTest {
    pub header_names: Vec<String>,
}
#[derive(Debug)]
pub struct HeaderTest {
    pub header_names: Vec<String>,
    pub key_list: Vec<MatchKey>,
}
#[derive(Debug)]
pub struct SizeTest {
    pub over: bool,
    pub limit: u64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Comparator {
    Octet,
    AsciiCasemap,
}
impl Default for Comparator {
    fn default() -> Self {
        Comparator::AsciiCasemap
    }
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AddressPart {
    Localpart,
    Domain,
    All,
}
impl Default for AddressPart {
    fn default() -> Self {
        AddressPart::All
    }
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MatchType {
    Is,
    Contains,
    Matches,
}
impl Default for MatchType {
    fn default() -> Self {
        MatchType::Is
    }
}

pub fn analyze<'doc>(doc: &'doc Document) -> Result<Ast> {
    let commands = commands(&doc.commands)?;
    Ok(Ast { commands })
}

pub fn commands<'doc>(cmds: &'doc [Command]) -> Result<Vec<TopLevelCommand>> {
    let mut ret = Vec::new();
    let mut to_skip = 0;
    for i in 0..cmds.len() {
        if to_skip > 0 {
            to_skip -= 1;
            continue;
        }
        let cmd = &cmds[i];
        let next = if cmd.id == "if" {
            let ctrl = if_control(&cmds[i..])?;
            to_skip = ctrl.branches.len();
            if ctrl.else_branch.is_none() {
                to_skip -= 1;
            }
            TopLevelCommand::If(ctrl)
        } else {
            non_if_command(&cmd)?
        };
        ret.push(next);
    }
    Ok(ret)
}

pub fn if_control<'doc>(cmds: &'doc [Command]) -> Result<IfControl> {
    let mut branches = vec![];
    let mut else_branch = None;
    for i in 0..cmds.len() {
        let cmd = &cmds[i];
        if i == 0 {
            assert_eq!("if", cmd.id);
            branches.push(if_branch(cmd)?);
        } else {
            match cmd.id.to_lowercase().as_str() {
                "elsif" => branches.push(if_branch(cmd)?),
                "else" => {
                    if i != cmds.len() - 1 {
                        return Err("Else cannot be followed by elsif or if".to_owned());
                    }
                    else_branch = Some(e_branch(cmd)?);
                }
                _ => break,
            }
        }
    }
    Ok(IfControl {
        branches,
        else_branch,
    })
}

pub fn if_branch<'doc>(cmd: &'doc Command) -> Result<(TestCommand, Block)> {
    if cmd.args.inner.len() > 0 {
        Err("if/elsif cannot have non-test arguments.".to_owned())
    } else if cmd.args.tests.len() != 1 {
        Err("if/elsif must have exactly one test.".to_owned())
    } else {
        Ok((test_command(&cmd.args.tests[0])?, block(&cmd.block)?))
    }
}

pub fn e_branch<'doc>(cmd: &'doc Command) -> Result<Block> {
    assert_eq!("else", cmd.id.to_lowercase().as_str());
    if cmd.args.inner.len() > 0 || cmd.args.tests.len() > 0 {
        Err("else cannot have any arguments.".to_owned())
    } else {
        Ok(block(&cmd.block)?)
    }
}

pub fn test_command<'doc>(cmd: &'doc Test) -> Result<TestCommand> {
    let result = match cmd.id.to_lowercase().as_str() {
        "address" => address(&cmd),
        "allof" => allof(&cmd),
        "anyof" => anyof(&cmd),
        "envelope" => envelope(&cmd),
        "exists" => exists(&cmd),
        "false" => false_(&cmd),
        "header" => header(&cmd),
        "not" => not(&cmd),
        "size" => size(&cmd),
        "true" => true_(&cmd),
        _ => Err(format!("Unrecognized test command: {}", cmd.id)),
    };
    result
}

#[derive(PartialEq)]
enum NonTaggedArg<'doc> {
    Strings(&'doc [StringIsh<'doc>]),
    Number(u64),
}
#[derive(Default, PartialEq)]
struct Args<'doc> {
    comparator: Option<Comparator>,
    address_part: Option<AddressPart>,
    match_type: Option<MatchType>,
    over: Option<bool>,
    positional: Vec<NonTaggedArg<'doc>>,
}

fn analyze_args<'doc>(args: &'doc [Argument]) -> Result<Args<'doc>> {
    let mut ret: Args<'doc> = Default::default();
    let mut it = args.iter();
    enum ArgKind<'doc> {
        Comparator(Comparator),
        AddressPart(AddressPart),
        MatchType(MatchType),
        Over(bool),
        Positional(NonTaggedArg<'doc>),
    }
    loop {
        let arg = it.next();
        let arg = match arg {
            None => break,
            Some(Argument::Tag(s)) => {
                if ret.positional.len() > 0 {
                    return Err(format!("Tag {} after positional argument", s));
                }
                match s.to_lowercase().as_ref() {
                    "comparator" => {
                        if let Some(Argument::Strings(ss)) = it.next() {
                            if ss.len() != 1 {
                                return Err(format!(
                                    "Comparator {} has more than one argument.",
                                    s
                                ));
                            }
                            match ss[0].to_string().as_ref() {
                                "i;octet" => ArgKind::Comparator(Comparator::Octet),
                                "i;ascii-casemap" => ArgKind::Comparator(Comparator::AsciiCasemap),
                                _ => return Err(format!("Unrecognized comparator: {}", s)),
                            }
                        } else {
                            return Err(format!("Comparator {} has no argument.", s));
                        }
                    }
                    "contains" => ArgKind::MatchType(MatchType::Contains),
                    "is" => ArgKind::MatchType(MatchType::Is),
                    "matches" => ArgKind::MatchType(MatchType::Matches),
                    "localpart" => ArgKind::AddressPart(AddressPart::Localpart),
                    "domain" => ArgKind::AddressPart(AddressPart::Domain),
                    "all" => ArgKind::AddressPart(AddressPart::All),
                    "over" => ArgKind::Over(true),
                    "under" => ArgKind::Over(false),
                    _ => return Err(format!("Unrecognized tag: {}", s)),
                }
            }
            Some(Argument::Strings(ss)) => ArgKind::Positional(NonTaggedArg::Strings(&ss)),
            Some(Argument::Number(n)) => ArgKind::Positional(NonTaggedArg::Number(*n)),
        };
        match arg {
            ArgKind::Comparator(c) => {
                if ret.comparator.is_some() {
                    return Err(format!("Comparator specified twice."));
                }
                ret.comparator = Some(c);
            }
            ArgKind::AddressPart(ap) => {
                if ret.address_part.is_some() {
                    return Err(format!("Address part specified twice."));
                }
                ret.address_part = Some(ap);
            }
            ArgKind::MatchType(mt) => {
                if ret.match_type.is_some() {
                    return Err(format!("Match type specified twice."));
                }
                ret.match_type = Some(mt);
            }
            ArgKind::Over(over) => {
                if ret.over.is_some() {
                    return Err(format!("At most one of :over or :under is allowed."));
                }
                ret.over = Some(over);
            }
            ArgKind::Positional(nta) => {
                ret.positional.push(nta);
            }
        };
    }
    Ok(ret)
}

pub fn address<'doc>(cmd: &'doc Test) -> Result<TestCommand> {
    if !cmd.args.tests.is_empty() {
        return Err(format!("Address cannot have test arguments."));
    }
    let args: Args<'doc> = analyze_args(&cmd.args.inner)?;
    if args.positional.len() != 2 {
        return Err(format!(
            "Address test takes exactly two positional arguments."
        ));
    }
    if let NonTaggedArg::Strings(hl) = args.positional[0] {
        if let NonTaggedArg::Strings(kl) = args.positional[1] {
            let comparator = args.comparator.unwrap_or_else(Default::default);
            let match_type = args.match_type.unwrap_or_else(Default::default);
            let key_list: Result<Vec<_>> = kl
                .iter()
                .map(StringIsh::to_string)
                .map(|s| MatchKey::new(comparator, match_type, s))
                .collect();
            return Ok(TestCommand::Address(AddressTest {
                address_part: args.address_part.unwrap_or_else(Default::default),
                header_list: hl.iter().map(StringIsh::to_string).collect(),
                key_list: key_list?,
            }));
        }
    }
    return Err(format!(
        "Address headers and keys must be strings or string lists."
    ));
}

pub fn allof<'doc>(cmd: &'doc Test) -> Result<TestCommand> {
    if !cmd.args.inner.is_empty() {
        Err(format!("allof only takes other tests as arguments."))
    } else {
        let res: Result<Vec<_>> = cmd.args.tests.iter().map(|t| test_command(t)).collect();
        Ok(TestCommand::Allof(res?))
    }
}

pub fn anyof<'doc>(cmd: &'doc Test) -> Result<TestCommand> {
    if !cmd.args.inner.is_empty() {
        Err(format!("anyof only takes other tests as arguments."))
    } else {
        let res: Result<Vec<_>> = cmd.args.tests.iter().map(|t| test_command(t)).collect();
        Ok(TestCommand::Anyof(res?))
    }
}

pub fn envelope<'doc>(cmd: &'doc Test) -> Result<TestCommand> {
    if !cmd.args.tests.is_empty() {
        return Err(format!("Envelope cannot have test arguments."));
    }
    let args: Args<'doc> = analyze_args(&cmd.args.inner)?;
    if args.positional.len() != 2 {
        return Err(format!("Envelope takes exactly two positional arguments."));
    }
    if let NonTaggedArg::Strings(ep) = args.positional[0] {
        if let NonTaggedArg::Strings(kl) = args.positional[1] {
            let comparator = args.comparator.unwrap_or_else(Default::default);
            let match_type = args.match_type.unwrap_or_else(Default::default);
            let key_list: Result<Vec<_>> = kl
                .iter()
                .map(StringIsh::to_string)
                .map(|s| MatchKey::new(comparator, match_type, s))
                .collect();
            return Ok(TestCommand::Envelope(EnvelopeTest {
                address_part: args.address_part.unwrap_or_else(Default::default),
                envelope_part: ep.iter().map(StringIsh::to_string).collect(),
                key_list: key_list?,
            }));
        }
    }
    return Err(format!(
        "Envelope parts and keys must be strings or string lists."
    ));
}

pub fn exists<'doc>(cmd: &'doc Test) -> Result<TestCommand> {
    if !cmd.args.tests.is_empty() {
        return Err(format!("Exists cannot have test arguments."));
    }
    let args: Args<'doc> = analyze_args(&cmd.args.inner)?;
    if args.positional.len() != 1 {
        return Err(format!("Exists takes exactly one positional argument."));
    }
    if let NonTaggedArg::Strings(hn) = args.positional[0] {
        return Ok(TestCommand::Exists(ExistsTest {
            header_names: hn.iter().map(StringIsh::to_string).collect(),
        }));
    }
    return Err(format!(
        "Exists header names must be a string or string list."
    ));
}

pub fn false_<'doc>(_cmd: &'doc Test) -> Result<TestCommand> {
    Ok(TestCommand::False)
}

pub fn header<'doc>(cmd: &'doc Test) -> Result<TestCommand> {
    if !cmd.args.tests.is_empty() {
        return Err(format!("Header cannot have test arguments."));
    }
    let args: Args<'doc> = analyze_args(&cmd.args.inner)?;
    if args.positional.len() != 2 {
        return Err(format!("Header takes exactly two positional arguments."));
    }
    if args.address_part.is_some() {
        return Err(format!("Header does not take an address part."));
    }
    if let NonTaggedArg::Strings(hn) = args.positional[0] {
        if let NonTaggedArg::Strings(kl) = args.positional[1] {
            let comparator = args.comparator.unwrap_or_else(Default::default);
            let match_type = args.match_type.unwrap_or_else(Default::default);
            let key_list: Result<Vec<_>> = kl
                .iter()
                .map(StringIsh::to_string)
                .map(|s| MatchKey::new(comparator, match_type, s))
                .collect();
            return Ok(TestCommand::Header(HeaderTest {
                header_names: hn.iter().map(StringIsh::to_string).collect(),
                key_list: key_list?,
            }));
        }
    }
    return Err(format!(
        "Header names and keys must be strings or string lists."
    ));
}

pub fn not<'doc>(cmd: &'doc Test) -> Result<TestCommand> {
    if !cmd.args.tests.len() == 1 {
        return Err(format!("Not takes exactly one test argument."));
    }
    let args: Args<'doc> = analyze_args(&cmd.args.inner)?;
    if args != Default::default() {
        return Err(format!("Not takes no positional or tagged arguments."));
    }
    Ok(TestCommand::Not(Box::new(test_command(
        &cmd.args.tests[0],
    )?)))
}

pub fn size<'doc>(cmd: &'doc Test) -> Result<TestCommand> {
    if !cmd.args.tests.is_empty() {
        return Err(format!("Size cannot have test arguments."));
    }
    let args = analyze_args(&cmd.args.inner)?;
    if args.address_part.is_some() {
        return Err(format!("Size does not take an address part."));
    }
    if args.match_type.is_some() {
        return Err(format!("Size does not take an match type."));
    }
    if args.comparator.is_some() {
        return Err(format!("Size does not take a comparator."));
    }
    let over = match args.over {
        None => return Err(format!("Size takes exactly one of :over or :under.")),
        Some(over) => over,
    };
    if args.positional.len() != 1 {
        return Err(format!("Size takes exactly one positional argument."));
    }
    if let NonTaggedArg::Number(limit) = args.positional[0] {
        Ok(TestCommand::Size(SizeTest { over, limit }))
    } else {
        Err(format!("Size's positional argument must be a number."))
    }
}

pub fn true_<'doc>(_cmd: &'doc Test) -> Result<TestCommand> {
    Ok(TestCommand::True)
}

pub fn block<'doc>(cmd: &'doc [Command]) -> Result<Block> {
    let x = commands(cmd)?;
    Ok(Block(x))
}

pub fn non_if_command<'doc>(cmd: &'doc Command) -> Result<TopLevelCommand> {
    if !cmd.args.tests.is_empty() {
        return Err(format!("{} does not take test arguments.", cmd.id));
    }
    let mut args = analyze_args(&cmd.args.inner)?;
    let pos = std::mem::replace(&mut args.positional, vec![]);
    if pos.len() > 1 {
        return Err(format!("Too many args for {}.", cmd.id));
    }
    if args != Default::default() {
        return Err(format!("{} does not take tagged arguments.", cmd.id));
    }
    match cmd.id.to_lowercase().as_str() {
        "require" => {
            if pos.len() != 1 {
                Err(format!("Require takes one positional arg."))
            } else if let NonTaggedArg::Strings(ss) = pos[0] {
                Ok(TopLevelCommand::Require(RequireControl {
                    capabilities: ss.iter().map(StringIsh::to_string).collect(),
                }))
            } else {
                Err(format!("Require arg must be a string or string list."))
            }
        }
        "stop" => {
            if !pos.is_empty() {
                Err(format!("stop takes no arguments."))
            } else {
                Ok(TopLevelCommand::Stop)
            }
        }
        "keep" => {
            if !pos.is_empty() {
                Err(format!("keep takes no arguments."))
            } else {
                Ok(TopLevelCommand::Keep)
            }
        }
        "discard" => {
            if !pos.is_empty() {
                Err(format!("discard takes no arguments."))
            } else {
                Ok(TopLevelCommand::Discard)
            }
        }
        "fileinto" => {
            if pos.len() != 1 {
                Err(format!("Fileinto takes one positional arg."))
            } else if let NonTaggedArg::Strings(mb) = pos[0] {
                if mb.len() == 1 {
                    Ok(TopLevelCommand::Fileinto(mb[0].to_string()))
                } else {
                    Err(format!("Fileinto arg must be a string."))
                }
            } else {
                Err(format!("Fileinto arg must be a string."))
            }
        }
        "redirect" => {
            if pos.len() != 1 {
                Err(format!("Redirect takes one positional arg."))
            } else if let NonTaggedArg::Strings(addr) = pos[0] {
                if addr.len() == 1 {
                    Ok(TopLevelCommand::Redirect(addr[0].to_string()))
                } else {
                    Err(format!("Redirect arg must be a string."))
                }
            } else {
                Err(format!("Redirect arg must be a string."))
            }
        }
        _ => Err(format!("Unrecognized command.")),
    }
}
