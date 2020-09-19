use std::fmt::Display;
#[derive(Clone)]
pub struct ParsedMessage {
    pub headers: Vec<(Vec<u8>, MessageHeader)>,
    pub body: Vec<u8>,
    pub size: usize,
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

impl MessageHeader {
    pub fn push_continuation(&mut self, continuation: &[u8]) {
        assert!(continuation[0].is_ascii_whitespace());
        let trimmed = trim_bytes(continuation);
        self.raw.push(continuation.to_vec());
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
    eprintln!("Parsing msg: {}", String::from_utf8_lossy(msg));
    let lines = ByteLines { remaining: msg };
    let mut headers = vec![];
    let mut last_header: Option<(Vec<u8>, MessageHeader)> = None;
    let mut body = vec![];
    for l in lines {
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
            if l.len() == 0 {
                body = lines.remaining.to_vec();
                break;
            }
            let (name, value) = l
                .iter()
                .position(|&ch| ch == b':')
                .map(|pos| (&l[..pos], l[pos + 1..].to_vec()))
                .ok_or_else(|| MsgParseError::MalformedHeader(l.to_vec()))?;
            last_header = Some((
                name.to_vec(),
                MessageHeader {
                    raw: vec![value.clone()],
                    unfolded: value,
                },
            ));
        }
    }
    Ok(ParsedMessage {
        headers,
        body,
        size: msg.len(),
    })
}
