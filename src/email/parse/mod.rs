use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_while;
use nom::bytes::complete::take_while1;
use nom::combinator::complete;
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
use nom::sequence::tuple;
use nom::Err;
use nom::IResult;

use crate::email::btv_new::{ByteStr, ByteString};

pub mod address;
pub mod date_time;
pub mod email;
pub mod header;

fn is_wsp(ch: u8) -> bool {
    ch == b' ' || ch == b'\t'
}

/// Recognize folding white space - semantically treated as a space
pub fn fws(input: &[u8]) -> IResult<&[u8], ()> {
    let (i, _o) = tuple((
        opt(tuple((take_while(is_wsp), tag(b"\r\n")))),
        take_while1(is_wsp),
    ))(input)?;

    Ok((i, ()))
}

fn satisfy_byte<F>(cond: F) -> impl Fn(&[u8]) -> IResult<&[u8], u8>
where
    F: Fn(u8) -> bool,
{
    move |input| {
        if input.is_empty() {
            Err(Err::Error(Error::from_error_kind(input, ErrorKind::Eof)))
        } else {
            let ch = input[0];
            if cond(ch) {
                Ok((&input[1..], input[0]))
            } else {
                Err(Err::Error(Error::from_error_kind(
                    input,
                    ErrorKind::Satisfy,
                )))
            }
        }
    }
}

fn is_vchar(ch: u8) -> bool {
    0x21 <= ch && ch <= 0x7e
}

fn is_quotable(ch: u8) -> bool {
    is_vchar(ch) || is_wsp(ch)
}

pub fn quoted_pair(input: &[u8]) -> IResult<&[u8], u8> {
    let (i, (_backslash, ch)) = tuple((tag(b"\\"), satisfy_byte(is_quotable)))(input)?;
    Ok((i, ch))
}

fn is_ctext(ch: u8) -> bool {
    (33 <= ch && ch <= 39) || (42 <= ch && ch <= 91) || (93 <= ch && ch <= 126)
}

fn ccontent(input: &[u8]) -> IResult<&[u8], ()> {
    alt((
        value((), satisfy_byte(is_ctext)),
        value((), quoted_pair),
        comment,
    ))(input)
}

fn comment(input: &[u8]) -> IResult<&[u8], ()> {
    value(
        (),
        tuple((
            tag(b"("),
            many0_count(tuple((opt(fws), ccontent))),
            opt(fws),
            tag(b")"),
        )),
    )(input)
}

fn is_atext(ch: u8) -> bool {
    ch.is_ascii_alphanumeric() || b"!#$%&'*+-/=?^_`{|}~".iter().any(|ch2| *ch2 == ch)
}

pub fn is_special(ch: u8) -> bool {
    b"()<>[]:;@\\,.\"".iter().any(|ch2| *ch2 == ch)
}

pub fn atom(input: &[u8]) -> IResult<&[u8], &ByteStr> {
    map(
        tuple((opt(cfws), take_while1(is_atext), opt(cfws))),
        |(_, the_atom, _)| ByteStr::from_slice(the_atom),
    )(input)
}

fn dot_atom_text(input: &[u8]) -> IResult<&[u8], &ByteStr> {
    // dot-atom-text   =   1*atext *("." 1*atext)
    map(
        recognize(tuple((
            take_while1(is_atext),
            many0_count(tuple((tag(b"."), take_while1(is_atext)))),
        ))),
        ByteStr::from_slice,
    )(input)
}

pub fn dot_atom(input: &[u8]) -> IResult<&[u8], &ByteStr> {
    map(
        tuple((opt(cfws), dot_atom_text, opt(cfws))),
        |(_, the_atom, _)| the_atom,
    )(input)
}

pub fn cfws(input: &[u8]) -> IResult<&[u8], ()> {
    alt((
        value(
            (),
            tuple((many1_count(tuple((opt(fws), comment))), opt(fws))),
        ),
        fws,
    ))(input)
}

fn is_qtext(ch: u8) -> bool {
    ch == 33 || (35 <= ch && ch <= 91) || (93 <= ch && ch <= 126)
}

fn qcontent(input: &[u8]) -> IResult<&[u8], u8> {
    alt((satisfy_byte(is_qtext), quoted_pair))(input)
}

// TODO - Cow here when possible, rather than always allocating?
pub fn quoted_string(input: &[u8]) -> IResult<&[u8], ByteString> {
    map(
        tuple((
            opt(cfws),
            tag(b"\""),
            many0(map(tuple((opt(fws), qcontent)), |(_, ch)| ch)),
            opt(fws),
            tag(b"\""),
            opt(cfws),
        )),
        |(_, _, s, _, _, _)| ByteString(s),
    )(input)
}

// TODO - Cow when possible?
fn word(input: &[u8]) -> IResult<&[u8], ByteString> {
    alt((map(atom, ToOwned::to_owned), quoted_string))(input)
}

// TODO - Cow when possible?
pub fn phrase(input: &[u8]) -> IResult<&[u8], Vec<ByteString>> {
    many1(word)(input)
}

#[test]
pub fn test_multiword_phrase() {
    let test = b"Brennan Vincent";

    let x = complete(phrase)(test).unwrap();
    eprintln!("{:?}", x);
}

// TODO - Cow when possible?
pub fn unstructured(input: &[u8]) -> IResult<&[u8], ByteString> {
    let (i, mut o) = fold_many0(
        tuple((opt(fws), satisfy_byte(is_vchar))),
        vec![],
        |mut s, (maybe_fws, ch)| {
            if let Some(()) = maybe_fws {
                s.push(b' ');
            }
            s.push(ch);
            s
        },
    )(input)?;
    map(
        fold_many0(satisfy_byte(is_wsp), o, |mut s, ch| {
            s.push(ch);
            s
        }),
        ByteString,
    )(i)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_fws() {
        let (i, ()) = super::fws(b"    \r\n   hi!").unwrap();
        assert_eq!(i, b"hi!");
    }
}
