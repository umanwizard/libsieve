use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::tag_no_case;
use nom::bytes::complete::take_while;
use nom::bytes::complete::take_while1;
use nom::character::complete::crlf;
use nom::combinator::complete;
use nom::combinator::consumed;
use nom::combinator::map;
use nom::combinator::opt;
use nom::combinator::recognize;
use nom::combinator::value;
use nom::error::Error;
use nom::error::ErrorKind;
use nom::error::ParseError;
use nom::multi::fold_many0;
use nom::multi::fold_many1;
use nom::multi::many0;
use nom::multi::many0_count;
use nom::multi::many1;
use nom::multi::many1_count;
use nom::multi::separated_list1;
use nom::sequence::terminated;
use nom::sequence::tuple;
use nom::Err;
use nom::IResult;

use crate::email;
use email::btv_new::{ByteStr, ByteString};
use email::error::EmailError;
use email::headers::{HeaderField, HeaderFieldInner, HeaderFieldKind};

use super::address::{address, mailbox};
use super::cfws;
use super::date_time::date_time;
use super::unstructured;

fn is_ftext(ch: u8) -> bool {
    (33 <= ch && ch <= 57) || (59 <= ch && ch <= 126)
}
fn header_name(input: &[u8]) -> IResult<&[u8], HeaderFieldKind> {
    use HeaderFieldKind::*;
    alt((
        value(OrigDate, tag_no_case("date")),
        value(From, tag_no_case("from")),
        value(Sender, tag_no_case("sender")),
        value(ReplyTo, tag_no_case("reply-to")),
        value(To, tag_no_case("to")),
        value(Cc, tag_no_case("cc")),
        value(Bcc, tag_no_case("bcc")),
        value(Unstructured, take_while1(is_ftext)),
    ))(input)
}

fn header_inner(
    hfk: HeaderFieldKind,
) -> impl Fn(&[u8]) -> IResult<&[u8], HeaderFieldInner, EmailError> {
    use HeaderFieldKind::*;

    move |i| match hfk {
        Unstructured => map(unstructured, |cooked| {
            HeaderFieldInner::Unstructured(cooked)
        })(i)
        .map_err(nom::Err::convert),
        OrigDate => map(date_time, |dt| HeaderFieldInner::OrigDate(dt))(i),
        From => map(separated_list1(tag(b","), mailbox), HeaderFieldInner::From)(i)
            .map_err(nom::Err::convert),
        Sender => map(mailbox, HeaderFieldInner::Sender)(i).map_err(nom::Err::convert),
        ReplyTo => map(
            separated_list1(tag(b","), address),
            HeaderFieldInner::ReplyTo,
        )(i)
        .map_err(nom::Err::convert),
        To => map(separated_list1(tag(b","), address), HeaderFieldInner::To)(i)
            .map_err(nom::Err::convert),
        Cc => map(separated_list1(tag(b","), address), HeaderFieldInner::Cc)(i)
            .map_err(nom::Err::convert),
        Bcc => map(
            opt(alt((
                separated_list1(tag(b","), address),
                value(vec![], cfws),
            ))),
            |maybe_list| HeaderFieldInner::Bcc(maybe_list.unwrap_or(vec![])),
        )(i)
        .map_err(nom::Err::convert),
    }
}

pub fn header_field(input: &[u8]) -> IResult<&[u8], HeaderField, EmailError> {
    let (i, (name, hfk)) =
        terminated(consumed(header_name), tag(b":"))(input).map_err(nom::Err::convert)?;
    let (i, (raw_value, inner)) = terminated(consumed(header_inner(hfk)), crlf)(i)?;

    Ok((
        i,
        HeaderField {
            name: ByteStr::from_slice(name),
            raw_value,
            inner,
        },
    ))
}

#[test]
fn test_from() {
    let test = r#"From: Brennan Vincent <brennan@umanwizard.com>
"#
    .replace('\n', "\r\n");
    let hs = complete(header_field)(test.as_bytes());
    eprintln!("{:?}", hs);
}
