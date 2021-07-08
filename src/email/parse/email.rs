use nom::character::complete::crlf;
use nom::combinator::consumed;
use nom::combinator::map;
use nom::combinator::opt;
use nom::combinator::recognize;
use nom::multi::fold_many_m_n;
use nom::multi::many0;
use nom::multi::separated_list0;
use nom::sequence::terminated;
use nom::sequence::tuple;
use nom::IResult;
use nom::Parser;

use super::header::header_field;
use super::satisfy_byte;

use crate::email;
use email::btv_new::Message;
use email::error::EmailError;

fn is_text(ch: u8) -> bool {
    ch < 128 && ch != b'\r' && ch != b'\n'
}

fn text998(input: &[u8]) -> IResult<&[u8], &[u8]> {
    recognize(fold_many_m_n(
        0,
        998,
        satisfy_byte(is_text),
        (),
        |(), _ch| (),
    ))(input)
}

pub fn body(input: &[u8]) -> IResult<&[u8], Vec<&[u8]>> {
    separated_list0(crlf, text998)
        .map(|mut v| {
            if v.last() == Some(&&b""[..]) {
                v.pop();
            }
            v
        })
        .parse(input)
}

pub fn message(input: &[u8]) -> IResult<&[u8], Message, EmailError> {
    tuple((
        terminated(many0(header_field), crlf),
        nom::Parser::into(consumed(body)),
    ))
    .map(|(header_fields, (body, body_lines))| Message {
        header: header_fields,
        body,
        body_lines,
    })
    .parse(input)
}
