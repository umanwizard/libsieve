use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_while;
use nom::bytes::complete::take_while1;
use nom::combinator::map;
use nom::combinator::opt;
use nom::combinator::recognize;
use nom::combinator::value;
use nom::error::Error;
use nom::error::ErrorKind;
use nom::error::ParseError;
use nom::multi::many0;
use nom::multi::many0_count;
use nom::multi::many1;
use nom::multi::many1_count;
use nom::sequence::tuple;
use nom::Err;
use nom::IResult;

pub mod date_time;

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

pub fn atom(input: &[u8]) -> IResult<&[u8], &[u8]> {
    map(
        tuple((opt(cfws), take_while1(is_atext), opt(cfws))),
        |(_, the_atom, _)| the_atom,
    )(input)
}

fn dot_atom_text(input: &[u8]) -> IResult<&[u8], &[u8]> {
    // dot-atom-text   =   1*atext *("." 1*atext)
    recognize(tuple((
        take_while1(is_atext),
        many0_count(tuple((tag(b"."), take_while1(is_atext)))),
    )))(input)
}

pub fn dot_atom(input: &[u8]) -> IResult<&[u8], &[u8]> {
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
pub fn quoted_string(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(
        tuple((
            opt(cfws),
            tag(b"\""),
            many0(map(tuple((opt(fws), qcontent)), |(_, ch)| ch)),
            opt(fws),
            tag(b"\""),
            opt(cfws),
        )),
        |(_, _, s, _, _, _)| s,
    )(input)
}

// TODO - Cow when possible?
fn word(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    alt((map(atom, <[u8]>::to_vec), quoted_string))(input)
}

// TODO - Cow when possible?
pub fn phrase(input: &[u8]) -> IResult<&[u8], Vec<Vec<u8>>> {
    many1(word)(input)
}

pub fn unstructured(input: &[u8]) {
    //unstructured    =   (*([FWS] VCHAR) *WSP) / obs-unstruct
    todo!()
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_fws() {
        let (i, ()) = super::fws(b"    \r\n   hi!").unwrap();
        assert_eq!(i, b"hi!");
    }
}
