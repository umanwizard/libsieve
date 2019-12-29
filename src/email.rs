use std::collections::HashMap;
pub struct ParsedMessage<'m> {
    pub headers: HashMap<&'m [u8], Vec<&'m [u8]>>,
    pub body: &'m [u8],
}
