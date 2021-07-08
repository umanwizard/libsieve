use std::ops::Deref;

#[derive(Clone)]
pub struct ByteString(pub Vec<u8>);

pub struct ByteStr(pub [u8]);

impl ByteStr {
    pub fn from_slice(slice: &[u8]) -> &Self {
        unsafe { std::mem::transmute(slice) }
    }
}

impl std::fmt::Debug for ByteStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", String::from_utf8_lossy(&self.0))
    }
}

impl std::fmt::Debug for ByteString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.deref())
    }
}

impl Deref for ByteString {
    type Target = ByteStr;

    fn deref(&self) -> &Self::Target {
        ByteStr::from_slice(self.0.as_slice())
    }
}

impl std::borrow::Borrow<ByteStr> for ByteString {
    fn borrow(&self) -> &ByteStr {
        self.deref()
    }
}

impl ToOwned for ByteStr {
    type Owned = ByteString;

    fn to_owned(&self) -> ByteString {
        ByteString(self.0.to_vec())
    }
}

use super::headers::HeaderField;
#[derive(Clone)]
pub struct Message<'a> {
    pub header: Vec<HeaderField<'a>>,
    pub body: &'a [u8],
    pub body_lines: Vec<&'a [u8]>,
}

impl<'a> std::fmt::Debug for Message<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        for HeaderField {
            name,
            raw_value: _,
            inner,
        } in self.header.iter()
        {
            writeln!(f, "{:?}:{:?}", name, inner)?;
        }
        for line in self.body_lines.iter() {
            writeln!(f, "LINE: {}", String::from_utf8_lossy(line))?;
        }
        Ok(())
    }
}
