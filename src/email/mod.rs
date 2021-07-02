use std::fmt::{Debug, Display};
#[derive(Clone)]
pub struct ParsedMessage {
    pub headers: Vec<(Vec<u8>, MessageHeader)>,
    pub body: Vec<u8>,
    pub size: usize,
}

impl Debug for ParsedMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "==HEADER==")?;
        for (name, val) in self.headers.iter() {
            write!(f, "{}\n{:?}", String::from_utf8_lossy(name), val)?;
        }
        writeln!(f, "==BODY==")?;
        write!(f, "{}", String::from_utf8_lossy(&self.body))?;
        Ok(())
    }
}

// TODO - 998 line length?
impl ParsedMessage {
    pub fn to_vec(&self) -> Vec<u8> {
        let mut result = vec![];
        for (h, val) in &self.headers {
            result.extend(h.iter());
            result.extend(b":");
            for raw_line in val.raw.iter() {
                result.extend(raw_line.iter());
                result.extend(b"\r\n");
            }
        }
        result.extend(b"\r\n");
        result.extend(self.body.iter());
        result
    }
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
        for i in 0..self.remaining.len() {
            if self.remaining[i] == b'\r' && self.remaining[i + 1] == b'\n' {
                let ret = &self.remaining[0..i];
                self.remaining = &self.remaining[i + 2..];
                return Some(ret);
            }
        }
        None
    }
}

#[derive(Debug)]
pub enum MsgParseError {
    ContinuationAtBeginning,
    MalformedHeader(Vec<u8>),
}

impl Display for MsgParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MsgParseError::ContinuationAtBeginning => {
                write!(f, "Continuation line at beginning of message")
            }
            MsgParseError::MalformedHeader(line) => write!(
                f,
                "Malformed header line: {}",
                String::from_utf8_lossy(line)
            ),
        }
    }
}

impl std::error::Error for MsgParseError {}

#[derive(Clone)]
pub struct MessageHeader {
    raw: Vec<Vec<u8>>,
    unfolded: Vec<u8>,
}

impl Debug for MessageHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Raw: ")?;
        for ln in self.raw.iter() {
            writeln!(f, "{}", String::from_utf8_lossy(ln))?;
        }
        writeln!(f, "Unfolded: {}", String::from_utf8_lossy(&self.unfolded))?;
        Ok(())
    }
}

impl MessageHeader {
    pub fn push_continuation(&mut self, continuation: &[u8]) {
        assert!(continuation[0].is_ascii_whitespace());
        let trimmed = trim_bytes(continuation);
        self.raw.push(continuation.to_vec());
        self.unfolded.push(b' ');
        self.unfolded.extend_from_slice(trimmed);
    }
    pub fn from_lines<'a, I: IntoIterator<Item = &'a [u8]>>(lines: I) -> Self {
        let mut result = None;
        for (idx, l) in lines.into_iter().enumerate() {
            if idx == 0 {
                result = Some(Self {
                    raw: vec![l.to_vec()],
                    unfolded: l.to_vec(),
                });
            } else {
                result.as_mut().unwrap().push_continuation(l);
            }
        }
        result.expect("A header must be at least one line.")
    }
    pub fn raw(&self) -> &[Vec<u8>] {
        &self.raw
    }
    pub fn unfolded(&self) -> &[u8] {
        &self.unfolded
    }
}

fn trim_bytes<'a>(bytes: &'a [u8]) -> &'a [u8] {
    let first_non_ws = bytes
        .iter()
        .position(|ch| !ch.is_ascii_whitespace())
        .unwrap_or(bytes.len());
    &bytes[first_non_ws..]
}

pub fn parse<'a>(msg: &'a [u8]) -> Result<ParsedMessage, MsgParseError> {
    let lines = ByteLines { remaining: msg };
    let mut headers = vec![];
    let mut last_header: Option<(Vec<u8>, MessageHeader)> = None;
    let mut body = vec![];
    let mut in_body = false;
    for l in lines {
        if in_body {
            body.extend_from_slice(l);
            body.extend(b"\r\n")
        } else {
            let is_continuation = l.get(0).map(u8::is_ascii_whitespace).unwrap_or(false);
            if is_continuation {
                let header = &mut last_header
                    .as_mut()
                    .ok_or(MsgParseError::ContinuationAtBeginning)?
                    .1;
                header.push_continuation(l)
            } else {
                if let Some((name, value)) = last_header.take() {
                    headers.push((name.clone(), value));
                }
                if l.is_empty() {
                    in_body = true;
                    continue;
                }
                let (name, value) = l
                    .iter()
                    .position(|&ch| ch == b':')
                    .map(|pos| (&l[..pos], l[pos + 1..].to_vec()))
                    .ok_or_else(|| MsgParseError::MalformedHeader(l.to_vec()))?;
                let trimmed = trim_bytes(&value).to_vec();
                last_header = Some((
                    name.to_vec(),
                    MessageHeader {
                        raw: vec![value],
                        unfolded: trimmed,
                    },
                ));
            }
        }
    }
    Ok(ParsedMessage {
        headers,
        body,
        size: msg.len(),
    })
}

#[test]
fn test_parse() {
    let test = r#"To: brennan.vincent@gmail.com
From: Brennan Vincent <brennan@umanwizard.com>
Subject: this is a test
Message-ID: <422f4e65-56da-24e2-3467-f60f2cc4d943@umanwizard.com>
Date: Fri, 18 Sep 2020 23:35:23 -0400
User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:68.0) Gecko/20100101
 Thunderbird/68.10.0
MIME-Version: 1.0
Content-Type: text/plain; charset=utf-8; format=flowed
Content-Transfer-Encoding: 7bit
Content-Language: en-US
Bonjour:Tout
  Le
 monde!

testing! 3
"#
    .replace('\n', "\r\n");
    let pm = parse(test.as_bytes()).unwrap();
    assert!(pm.to_vec() == test.into_bytes());
}
