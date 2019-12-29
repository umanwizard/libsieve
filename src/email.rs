use std::collections::HashMap;
pub struct ParsedMessage<'m> {
    headers: HashMap<&'m [u8], Vec<&'m [u8]>>,
    body: &'m [u8],
}
