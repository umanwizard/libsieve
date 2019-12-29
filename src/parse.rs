use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag, take, take_while, take_while1, take_while_m_n},
    character::complete::{crlf, digit1, none_of, not_line_ending, one_of, space0, space1},
    combinator::{all_consuming, iterator, map, map_res, opt, recognize, value, verify},
    error::{ErrorKind, ParseError},
    multi::{fold_many0, many0, many1, many_till, separated_list, separated_nonempty_list},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

fn w<'a, O, P>(p: P) -> impl Fn(&'a str) -> IResult<&'a str, O>
where
    P: Fn(&'a str) -> IResult<&'a str, O>,
{
    preceded(many0(white_space), p)
}

fn is_idalpha(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_idalphanum(c: char) -> bool {
    is_idalpha(c) || c.is_numeric()
}

fn identifier(input: &str) -> IResult<&str, &str> {
    w(recognize(pair(
        take_while1(is_idalpha),
        take_while(is_idalphanum),
    )))(input)
}

fn hash_comment(input: &str) -> IResult<&str, ()> {
    map(
        tuple((
            tag("#"),
            take_while(|c| c != '\r' && c != '\n'),
            tag("\r\n"),
        )),
        |_| (),
    )(input)
}

fn non_newline(input: &str) -> IResult<&str, &str> {
    recognize(none_of(("\n\r")))(input)
}

fn bracket_comment(input: &str) -> IResult<&str, ()> {
    value(
        (),
        tuple((tag("/*"), many_till(alt((non_newline, crlf)), tag("*/")))),
    )(input)
}

fn comment(input: &str) -> IResult<&str, ()> {
    alt((hash_comment, bracket_comment))(input)
}

fn multiline_literal(input: &str) -> IResult<&str, &str> {
    terminated(
        map(
            opt(recognize(pair(
                take_while1(|c| c != '.' && c != '\r' && c != '\n'),
                take_while(|c| c != '\r' && c != '\n'),
            ))),
            |op| op.unwrap_or(""),
        ),
        crlf,
    )(input)
}

fn multiline_dotstart(input: &str) -> IResult<&str, &str> {
    delimited(
        tag("."),
        verify(not_line_ending, |s: &str| s.len() > 0),
        crlf,
    )(input)
}

fn multi_line(input: &str) -> IResult<&str, Vec<&str>> {
    delimited(
        tuple((
            w(tag("text:")),
            space0,
            alt((hash_comment, value((), crlf))),
        )),
        many0(alt((multiline_literal, multiline_dotstart))),
        tag(".\r\n"),
    )(input)
}

#[derive(Debug, PartialEq)]
enum Quantifier {
    U,
    K,
    M,
    G,
}

impl Quantifier {
    fn weight(&self) -> u64 {
        match self {
            Quantifier::U => 1,
            Quantifier::K => 1024,
            Quantifier::M => 1024 * 1024,
            Quantifier::G => 1024 * 1024 * 1024,
        }
    }
}

fn quantifier(input: &str) -> IResult<&str, Quantifier> {
    use Quantifier::*;
    map(opt(one_of("KMG")), |c| match c {
        None => U,
        Some(c) => match c {
            'K' => K,
            'M' => M,
            'G' => G,
            _ => unreachable!(),
        },
    })(input)
}
#[test]
fn parse_quantifier() {
    assert_eq!(quantifier("K"), Ok(("", Quantifier::K)));
    assert_eq!(quantifier(""), Ok(("", Quantifier::U)));
}

fn number(input: &str) -> IResult<&str, u64> {
    w(map_res(pair(digit1, quantifier), |(n, q)| {
        n.parse::<u64>()
            .map_err(|_| (input, ErrorKind::TooLarge))
            .and_then(|n| {
                n.checked_mul(q.weight())
                    .ok_or((input, ErrorKind::TooLarge))
            })
    }))(input)
}
#[test]
fn parse_number() {
    assert_eq!(number("1234K blah"), Ok((" blah", 1234 * 1024)));
    assert_eq!(number("1234 foo"), Ok((" foo", 1234)));
}

// Called "tag" in RFC5228
fn tagged_id(input: &str) -> IResult<&str, &str> {
    preceded(w(tag(":")), identifier)(input)
}

fn white_space(input: &str) -> IResult<&str, ()> {
    alt((value((), comment), value((), crlf), value((), space1)))(input)
    //    alt((comment, value((), many1(alt((crlf, space1))))))(input)
}

fn quoted_string(input: &str) -> IResult<&str, String> {
    let one: usize = 1;
    delimited(
        w(tag("\"")),
        escaped_transform(none_of(r#"\""#), '\\', take(one)),
        tag("\""),
    )(input)
}

// PARSING BEGINS HERE

pub fn document(input: &str) -> IResult<&str, Document> {
    all_consuming(delimited(
        many0(white_space),
        map(many0(command), |commands| Document { commands }),
        many0(white_space),
    ))(input)
}

fn command(input: &str) -> IResult<&str, Command> {
    map(
        tuple((
            identifier,
            argument_group,
            alt((
                value(vec![], w(tag(";"))),
                delimited(w(tag("{")), many0(command), w(tag("}"))),
            )),
        )),
        |(id, args, block)| Command { id, args, block },
    )(input)
}

fn test_list(input: &str) -> IResult<&str, Vec<Test>> {
    map(
        opt(alt((
            map(test, |t| vec![t]),
            delimited(
                w(tag("(")),
                separated_nonempty_list(w(tag(",")), test),
                w(tag(")")),
            ),
        ))),
        |o| o.unwrap_or(vec![]),
    )(input)
}

fn argument_group(input: &str) -> IResult<&str, ArgumentGroup> {
    map(pair(many0(argument), test_list), |(args, tests)| {
        ArgumentGroup { inner: args, tests }
    })(input)
}

fn string_list(input: &str) -> IResult<&str, Vec<StringIsh>> {
    alt((
        map(stringish, |s| vec![s]),
        delimited(
            w(tag("[")),
            separated_nonempty_list(w(tag(",")), stringish),
            w(tag("]")),
        ),
    ))(input)
}

fn stringish(input: &str) -> IResult<&str, StringIsh> {
    alt((
        map(quoted_string, |s| StringIsh::Quoted(s)),
        map(multi_line, |v| StringIsh::MultiLine(v)),
    ))(input)
}

fn argument(input: &str) -> IResult<&str, Argument> {
    alt((
        map(string_list, |sl| Argument::Strings(sl)),
        map(number, |n| Argument::Number(n)),
        map(tagged_id, |id| Argument::Tag(id)),
    ))(input)
}

fn test(input: &str) -> IResult<&str, Test> {
    map(tuple((identifier, argument_group)), |(id, args)| Test {
        id,
        args,
    })(input)
}

#[derive(Debug, Clone)]
pub struct Document<'doc> {
    pub commands: Vec<Command<'doc>>,
}

#[derive(Debug, Clone)]
pub struct Command<'doc> {
    pub id: &'doc str,
    pub args: ArgumentGroup<'doc>,
    pub block: Vec<Command<'doc>>,
}

#[derive(Debug, Clone)]
pub struct ArgumentGroup<'doc> {
    pub inner: Vec<Argument<'doc>>,
    pub tests: Vec<Test<'doc>>,
}

#[derive(Debug, Clone)]
pub enum Argument<'doc> {
    Strings(Vec<StringIsh<'doc>>),
    Number(u64),
    Tag(&'doc str),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringIsh<'doc> {
    Quoted(String),
    MultiLine(Vec<&'doc str>),
}

impl<'doc> StringIsh<'doc> {
    pub fn to_string(&self) -> String {
        match self {
            Self::Quoted(s) => s.clone(),
            Self::MultiLine(ss) => ss.concat(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Test<'doc> {
    pub id: &'doc str,
    pub args: ArgumentGroup<'doc>,
}

#[test]
fn parse_quoted_string() {
    assert_eq!(
        quoted_string(r#""asdf\"jk\\l""#),
        Ok(("", String::from(r#"asdf"jk\l"#)))
    );
}

const HASHES: &'static [&'static str] = &["# This is a #hash comment\r\n"];
const NON_HASHES: &'static [&'static str] = &["This is not\r. \r\n", " # Nor this.\r\n"];
const BRACKETS: &'static [&'static str] = &[
    "/* This is a bracket comment*/",
    "/* And so /* \r\n is this */",
];
const NON_BRACKETS: &'static [&'static str] = &["/* But \n this fails */"];
#[test]
fn parse_id() {
    assert_eq!(identifier("hello_there0"), Ok(("", "hello_there0")));
    assert!(identifier("0hello_there0").is_err());
}
#[test]
fn parse_hash_comment() {
    for s in HASHES {
        assert!(hash_comment(s).is_ok());
    }
    for s in NON_HASHES {
        assert!(hash_comment(s).is_err());
    }
}
#[test]
fn parse_bracket_comment() {
    for s in BRACKETS {
        assert!(bracket_comment(s).is_ok());
    }
    for s in NON_BRACKETS {
        assert!(bracket_comment(s).is_err());
    }
}
#[test]
fn parse_comment() {
    for s in BRACKETS {
        assert!(comment(s).is_ok());
    }
    for s in NON_BRACKETS {
        assert!(comment(s).is_err());
    }
    for s in HASHES {
        assert!(comment(s).is_ok());
    }
    for s in NON_HASHES {
        assert!(comment(s).is_err());
    }
}
#[test]
fn parse_multiline_literal() {
    assert_eq!(
        multiline_literal("Hello, there!\r\n"),
        Ok(("", "Hello, there!"))
    );
    assert!(multiline_literal(".Dots are not allowed\r\n").is_err());
    assert!(multiline_literal("Neither are\ninternal newlines\r\n").is_err());
}
#[test]
fn parse_multiline_dotstart() {
    assert_eq!(
        multiline_dotstart(".Dots are OK here\r\n"),
        Ok(("", "Dots are OK here"))
    );
    assert!(multiline_dotstart(".\r\n").is_err());
    assert!(multiline_dotstart("No dot is bad\r\n").is_err());
}
#[test]
fn parse_multi_line() {
    assert_eq!(multi_line("text: \t #begin text\r\nThis is some multi-line text\r\n.With embedded dots\r\n..\r\n.\r\n"),
        Ok(("", vec!["This is some multi-line text", "With embedded dots", "."])));
}
