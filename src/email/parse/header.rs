use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::tag_no_case;
use nom::bytes::complete::take_while;
use nom::bytes::complete::take_while1;
use nom::character::complete::crlf;
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
use nom::sequence::terminated;
use nom::sequence::tuple;
use nom::Err;
use nom::IResult;

use crate::email;
use email::error::EmailError;
use email::headers::{HeaderField, HeaderFieldInner, HeaderFieldKind};

use super::date_time::date_time;
use super::unstructured;

fn is_ftext(ch: u8) -> bool {
    (33 <= ch && ch <= 57) || (59 <= ch && ch <= 126)
}
fn header_name(input: &[u8]) -> IResult<&[u8], HeaderFieldKind> {
    use HeaderFieldKind::*;
    alt((
        value(OrigDate, tag_no_case("date")),
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
    }
}

pub fn header_field(input: &[u8]) -> IResult<&[u8], HeaderField, EmailError> {
    let (i, (name, hfk)) =
        terminated(consumed(header_name), tag(b":"))(input).map_err(nom::Err::convert)?;
    let (i, (raw_value, inner)) = terminated(consumed(header_inner(hfk)), crlf)(i)?;

    Ok((
        i,
        HeaderField {
            name,
            raw_value,
            inner,
        },
    ))
}
