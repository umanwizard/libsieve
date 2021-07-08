use enum_kinds::EnumKind;

pub mod layout;

#[derive(Clone, EnumKind)]
#[enum_kind(HeaderFieldKind)]
pub enum HeaderFieldInner {
    Unstructured(Vec<u8>),
    // "Date:"
    OrigDate(chrono::DateTime<chrono::offset::FixedOffset>),
}

impl std::fmt::Debug for HeaderFieldInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self {
            Self::Unstructured(cooked) => write!(f, "{}", String::from_utf8_lossy(cooked)),
            Self::OrigDate(date) => write!(f, "{:?}", date),
        }
    }
}

#[derive(Clone, Debug)]
pub struct HeaderField<'a> {
    pub name: &'a [u8],
    pub raw_value: &'a [u8],
    pub inner: HeaderFieldInner,
}
