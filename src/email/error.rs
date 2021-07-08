type NomErr<'a> = nom::error::Error<&'a [u8]>;

#[derive(Debug)]
pub enum EmailError<'a> {
    Parse(NomErr<'a>, Option<Box<EmailError<'a>>>),
    BadDate {
        y: u16,
        m: chrono::Month,
        d: u8,
    },
    BadTZOffset {
        is_east: bool,
        hh: u8,
        mm: u8,
    },
    BadDateTime {
        date: chrono::NaiveDate,
        tz: chrono::offset::FixedOffset,
        h: u8,
        m: u8,
        s: Option<u8>,
    },
    BadWeekday {
        date_time: chrono::DateTime<chrono::offset::FixedOffset>,
        weekday: chrono::Weekday,
    },
}

impl<'a> From<NomErr<'a>> for EmailError<'a> {
    fn from(e: NomErr<'a>) -> Self {
        Self::Parse(e, None)
    }
}

impl<'a> nom::error::ParseError<&'a [u8]> for EmailError<'a> {
    fn from_error_kind(input: &'a [u8], code: nom::error::ErrorKind) -> Self {
        Self::Parse(NomErr { input, code }, None)
    }
    fn append(input: &'a [u8], code: nom::error::ErrorKind, other: Self) -> Self {
        Self::Parse(NomErr { input, code }, Some(Box::new(other)))
    }
}
