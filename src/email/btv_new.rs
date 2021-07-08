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
            writeln!(f, "{}:{:?}", String::from_utf8_lossy(name), inner)?;
        }
        for line in self.body_lines.iter() {
            writeln!(f, "LINE: {}", String::from_utf8_lossy(line))?;
        }
        Ok(())
    }
}
