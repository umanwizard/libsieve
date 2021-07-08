use std::borrow::Cow;

use crate::email::btv_new::{ByteStr, ByteString};

#[derive(Debug, Clone)]
pub enum Domain<'a> {
    Name(&'a ByteStr),
    Literal(ByteString),
}
#[derive(Debug, Clone)]
pub struct AddrSpec<'a> {
    pub local_part: Cow<'a, ByteStr>,
    pub domain: Domain<'a>,
}

#[derive(Debug, Clone)]
pub struct Mailbox<'a> {
    pub display_name: Vec<ByteString>,
    pub addr_spec: AddrSpec<'a>,
}

#[derive(Debug, Clone)]
pub struct Group<'a> {
    pub display_name: Vec<ByteString>,
    pub mailboxes: Vec<Mailbox<'a>>,
}

#[derive(Debug, Clone)]
pub enum Address<'a> {
    Mailbox(Mailbox<'a>),
    Group(Group<'a>),
}
