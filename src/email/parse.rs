use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_while;
use nom::bytes::complete::take_while1;
use nom::combinator::opt;
use nom::combinator::value;
use nom::error::Error;
use nom::error::ErrorKind;
use nom::error::ParseError;
use nom::multi::many0_count;
use nom::multi::many1_count;
use nom::sequence::tuple;
use nom::Err;
use nom::IResult;

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

pub fn cfws(input: &[u8]) -> IResult<&[u8], ()> {
    alt((
        value(
            (),
            tuple((many1_count(tuple((opt(fws), comment))), opt(fws))),
        ),
        fws,
    ))(input)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_fws() {
        let (i, ()) = super::fws(b"    \r\n   hi!").unwrap();
        assert_eq!(i, b"hi!");
    }
}
