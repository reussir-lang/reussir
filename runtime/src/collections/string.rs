use std::string::String as StdString;

use crate::rc::Rc;

#[derive(Clone)]
#[repr(transparent)]
pub struct String(Rc<StdString>);

impl String {
    pub fn new() -> Self {
        Self(Rc::new(StdString::new()))
    }
    pub fn with_capacity(capacity: usize) -> Self {
        Self(Rc::new(StdString::with_capacity(capacity)))
    }
    pub fn make_mut(&mut self) -> &mut StdString {
        self.0.make_mut()
    }
    pub fn push(mut self, ch: char) -> Self {
        self.make_mut().push(ch);
        self
    }
    pub fn to_string(&self) -> String {
        String(self.0.clone())
    }
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        let mut s = String::new();
        s = s.push('a');
        assert_eq!(s.as_str(), "a");
    }
}
