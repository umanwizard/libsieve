use std::collections::{
    hash_map::Entry::{Occupied, Vacant},
    HashMap,
};
// TODO - use less copies/allocations.
pub struct ParsedMessage {
    pub headers: Vec<(Vec<u8>, Vec<u8>)>,
    pub headers_idx: HashMap<Vec<u8>, Vec<usize>>,
    pub body: Vec<u8>,
    pub size: usize,
}

pub struct SmtpEnvelope {
    pub from: Option<String>,
    pub to: Option<String>,
}

#[derive(Clone, Copy)]
struct ByteLines<'a> {
    remaining: &'a [u8],
}

impl<'a> Iterator for ByteLines<'a> {
    type Item = &'a [u8];
    fn next(&mut self) -> Option<Self::Item> {
        for i in 0..(self.remaining.len() - 1) {
            if self.remaining[i] == b'\r' && self.remaining[i + 1] == b'\n' {
                let ret = &self.remaining[0..i];
                self.remaining = &self.remaining[i + 2..];
                return Some(ret);
            }
        }
        None
    }
}

pub enum MsgParseError {
    ContinuationAtBeginning,
    MalformedHeader(Vec<u8>),
}

pub fn parse<'a>(msg: &'a [u8]) -> Result<ParsedMessage, MsgParseError> {
    let lines = ByteLines { remaining: msg };
    let mut headers_idx: HashMap<Vec<u8>, Vec<usize>> = HashMap::new();
    let mut headers = vec![];
    let mut last_header: Option<(Vec<u8>, Vec<u8>)> = None;
    let mut body = vec![];
    for l in lines {
        let is_continuation = if let Some(ch) = l.first() {
            if ch.is_ascii_whitespace() {
                if let Some((_name, value)) = last_header.as_mut() {
                    value.extend_from_slice(l);
                    true
                } else {
                    return Err(MsgParseError::ContinuationAtBeginning);
                }
            } else {
                false
            }
        } else {
            false
        };
        if !is_continuation {
            if let Some((name, value)) = last_header.take() {
                headers.push((name.clone(), value));
                match headers_idx.entry(name) {
                    Occupied(mut oe) => {
                        oe.get_mut().push(headers.len() - 1);
                    }
                    Vacant(ve) => {
                        ve.insert(vec![headers.len() - 1]);
                    }
                }
            }
            if l.len() == 0 {
                body = lines.remaining.to_vec();
                break;
            }
            let (name, value) = l
                .iter()
                .position(|&ch| ch == b':')
                .map(|pos| l.split_at(pos))
                .ok_or_else(|| MsgParseError::MalformedHeader(l.to_vec()))?;
            let start = value
                .iter()
                .position(|&ch| !ch.is_ascii_whitespace())
                .unwrap_or(value.len());
            let value = value[start..].to_vec();
            last_header = Some((name.to_vec(), value));
        }
    }
    Ok(ParsedMessage {
        headers,
        headers_idx,
        body,
        size: msg.len(),
    })
}
