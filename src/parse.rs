use nom::{
  IResult,
  bytes::complete::{escaped_transform, take, tag, take_while_m_n, take_while, take_while1},
  character::complete::{crlf, not_line_ending, none_of, one_of, space1, space0, digit1},
  combinator::{verify, recognize, map_res, map, iterator, value, opt},
  sequence::{tuple, pair, delimited, preceded, terminated},
  branch::{alt},
  error::ErrorKind,
  multi::{separated_nonempty_list, many0, many1, fold_many0, separated_list, many_till},
};

fn is_idalpha(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_idalphanum(c: char) -> bool {
    is_idalpha(c) || c.is_numeric()
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(take_while1(is_idalpha), take_while(is_idalphanum)))(input)
}

fn hash_comment(input: &str) -> IResult<&str, ()> {
    map(tuple((tag("#"), take_while(|c| c != '\r' && c != '\n'), tag("\r\n"))),
        |_| ())(input)
}

fn non_newline(input: &str) -> IResult<&str, &str> {
    recognize(none_of(("\n\r")))(input)
}

fn bracket_comment(input: &str) -> IResult<&str, ()> {
   value((), tuple((tag("/*"), many_till(alt((non_newline, crlf)), tag("*/")))))
       (input)
}

fn comment(input: &str) -> IResult<&str, ()> {
    alt((hash_comment, bracket_comment))(input)
}

fn multiline_literal(input: &str) -> IResult<&str, &str> {
    terminated(map(opt(recognize(pair(take_while1(|c| c != '.' && c != '\r' && c != '\n'),
                                      take_while(|c| c != '\r' && c != '\n')))),
               |op| op.unwrap_or("")), crlf)(input)
}

fn multiline_dotstart(input: &str) -> IResult<&str, &str> {
    delimited(tag("."), verify(not_line_ending, |s: &str| s.len() > 0), crlf)(input)
}

fn multi_line(input: &str) -> IResult<&str, Vec<&str>> {
    delimited(tuple((tag("text:"), space0, alt((hash_comment, value((), crlf))))),
        many0(alt((multiline_literal, multiline_dotstart))),
        tag(".\r\n"))(input)
}

#[derive(Debug, PartialEq)]
enum Quantifier {
    U, K, M, G
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
            _ => unreachable!()
        }
    })(input)
}
#[test]
fn parse_quantifier() {
    assert_eq!(quantifier("K"), Ok(("", Quantifier::K)));
    assert_eq!(quantifier(""), Ok(("", Quantifier::U)));
}

fn number(input: &str) -> IResult<&str, u64> {
    map_res(pair(digit1, quantifier),
        |(n, q)| n.parse::<u64>().map_err(|_| (input, ErrorKind::TooLarge)).and_then(
            |n| n.checked_mul(q.weight()).ok_or((input, ErrorKind::TooLarge))
        ))(input)
}
#[test]
fn parse_number() {
    assert_eq!(number("1234K blah"), Ok((" blah", 1234 * 1024)));
    assert_eq!(number("1234 foo"), Ok((" foo", 1234)));
}
        
// Called "tag" in RFC5228
fn tagged_id(input: &str) -> IResult<&str, &str> {
    preceded(tag(":"), identifier)(input)
}

fn white_space(input: &str) -> IResult<&str, ()> {
    alt((comment, value((), many1(alt((crlf, space1))))))(input)
}

fn quoted_string(input: &str) -> IResult<&str, String> {
    let one: usize = 1;
    delimited(
        tag("\""),
        escaped_transform(
            none_of(r#"\""#),
            '\\',
            take(one)),
        tag("\""))(input)
}

// PARSING BEGINS HERE

pub fn document(input: &str) -> IResult<&str, Document> {
    map(many0(command),
        |commands| Document { commands })
        (input)
}

fn command(input: &str) -> IResult<&str, Command> {
    map(tuple((
            identifier,
            argument_group,
            alt((value(vec![], tag(";")), 
                 delimited(tag("{"), many0(command), tag("}"))))
            )),
        |(id, args, block)| Command { id, args, block })(input)
}

fn test_list(input: &str) -> IResult<&str, Vec<Test>> {
    alt((
        map(test, |t| vec![t]),
        delimited(
            tag("("),
            separated_nonempty_list(tag(","), test),
            tag(")"))))
    (input)
}

fn argument_group(input: &str) -> IResult<&str, ArgumentGroup> {
    map(pair(many0(argument), test_list),
        |(args, tests)| ArgumentGroup { inner: args, tests })(input)
}

fn string_list(input: &str) -> IResult<&str, Vec<StringIsh>> {
    alt((
        map(stringish, |s| vec![s]),
        delimited(
            tag("["),
            separated_nonempty_list(tag(","), stringish),
            tag("]"))))
    (input)
}

fn stringish(input: &str) -> IResult<&str, StringIsh> {
    alt((
        map(quoted_string, |s| StringIsh::Quoted(s)),
        map(multi_line, |v| StringIsh::MultiLine(v))))
    (input)
}

fn argument(input: &str) -> IResult<&str, Argument> {
    alt((map(string_list, |sl| Argument::Strings(sl)),
         map(number, |n| Argument::Number(n)),
         map(tagged_id, |id| Argument::Tag(id))))(input)
}

fn test(input: &str) -> IResult<&str, Test> {
    map(
        tuple((identifier, argument_group)),
        |(id, args)| Test { id, args } )
    (input)
}

#[derive(Clone)]
pub struct Document<'doc> {
    commands: Vec<Command<'doc>>
}

#[derive(Clone)]
pub struct Command<'doc> {
    id: &'doc str,
    args: ArgumentGroup<'doc>,
    block: Vec<Command<'doc>>,
}

#[derive(Clone)]
pub struct ArgumentGroup<'doc> {
    inner: Vec<Argument<'doc>>,
    tests: Vec<Test<'doc>>,
}

#[derive(Clone)]
pub enum Argument<'doc> {
    Strings(Vec<StringIsh<'doc>>),
    Number(u64),
    Tag(&'doc str),
}

#[derive(Clone)]
pub enum StringIsh<'doc> {
    Quoted(String),
    MultiLine(Vec<&'doc str>),
}

#[derive(Clone)]
pub struct Test<'doc> {
    id: &'doc str,
    args: ArgumentGroup<'doc>,
}







#[test]
fn parse_quoted_string() {
    assert_eq!(quoted_string(r#""asdf\"jk\\l""#), Ok(("", String::from(r#"asdf"jk\l"#))));
}

const HASHES: &'static [&'static str]
    = &["# This is a #hash comment\r\n"];
const NON_HASHES: &'static [&'static str]
    = &["This is not\r. \r\n",
        " # Nor this.\r\n"];
const BRACKETS: &'static [&'static str]
    = &["/* This is a bracket comment*/",
        "/* And so /* \r\n is this */",];
const NON_BRACKETS: &'static [&'static str]
    = &["/* But \n this fails */",];
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
    assert_eq!(multiline_literal("Hello, there!\r\n"), Ok(("", "Hello, there!")));
    assert!(multiline_literal(".Dots are not allowed\r\n").is_err());
    assert!(multiline_literal("Neither are\ninternal newlines\r\n").is_err());
}
#[test]
fn parse_multiline_dotstart() {
    assert_eq!(multiline_dotstart(".Dots are OK here\r\n"), Ok(("", "Dots are OK here")));
    assert!(multiline_dotstart(".\r\n").is_err());
    assert!(multiline_dotstart("No dot is bad\r\n").is_err());
}
#[test]
fn parse_multi_line() {
    assert_eq!(multi_line("text: \t #begin text\r\nThis is some multi-line text\r\n.With embedded dots\r\n..\r\n.\r\n"),
        Ok(("", vec!["This is some multi-line text", "With embedded dots", "."])));
}
