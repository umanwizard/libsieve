use enum_kinds::EnumKind;

pub mod address;
pub mod layout;

use crate::email::btv_new::{ByteStr, ByteString};
use address::{Address, Mailbox};

#[derive(Debug, Clone, EnumKind)]
#[enum_kind(HeaderFieldKind)]
pub enum HeaderFieldInner<'a> {
    Unstructured(ByteString),
    // "Date:"
    OrigDate(chrono::DateTime<chrono::offset::FixedOffset>),
    From(Vec<Mailbox<'a>>),
    Sender(Mailbox<'a>),
    ReplyTo(Vec<Address<'a>>),
    To(Vec<Address<'a>>),
    Cc(Vec<Address<'a>>),
    Bcc(Vec<Address<'a>>),
}

#[derive(Clone, Debug)]
pub struct HeaderField<'a> {
    pub name: &'a ByteStr,
    pub raw_value: &'a [u8],
    pub inner: HeaderFieldInner<'a>,
}
