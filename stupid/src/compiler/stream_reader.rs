use std::slice::Iter;
use std::str::Chars;

pub struct StringReader<'a> {
    current: Option<char>,
    position: usize,
    iter: Chars<'a>,
    input: &'a str
}

pub trait StreamReader<T> {
    fn current(&self) -> Option<T>;
    fn check_ahead(&self, offset: usize) -> Option<T>;
    fn advance(&mut self) -> Option<T>;
    fn rewind(&mut self) -> Option<()>;
}

impl<'a> StringReader<'a> {
    pub fn new(input: &'a str) -> StringReader {
        let mut iter = input.chars();
        let current = iter.next();
        StringReader {
            input: input,
            iter: iter,
            position: 0,
            current: current
        }
    }
}

impl<'a> StreamReader<char> for StringReader<'a> {
   fn current(&self) -> Option<char> {
        self.current
    }

   fn check_ahead(&self, offset: usize) -> Option<char> {
        self.iter.clone().nth(offset)
    }

   fn advance(&mut self) -> Option<char> {
        self.position = self.position + 1;
        self.current = self.iter.next();
        self.current()
    }

    #[allow(dead_code)]
   fn rewind(&mut self) -> Option<()> {
        if self.position == 0 { return None }
        self.position = self.position - 1;
        self.iter = self.input.chars();
        self.current = self.iter.nth(self.position);
        Some( () )
    }
}

#[cfg(test)]
mod tests_string_reader {
    use super::*;

    fn sut() -> StringReader<'static> {
        StringReader::new("wtf")
    }

    #[test]
    fn string_reader_with_empty_string_return_always_none() {
        let mut empty = StringReader::new("");
        assert_eq!(empty.current(), None);
        assert_eq!(empty.check_ahead(0), None);
        assert_eq!(empty.rewind(), None);
        assert_eq!(empty.advance(), None);
    }

    #[test]
    fn current_on_new_string_reader_returns_first_char() {
        assert_eq!(sut().current().unwrap(), 'w')
    }

    #[test]
    fn advance_when_string_reader_not_consumed_returns_char() {
        assert_eq!(sut().advance().unwrap(), 't')
    }

    #[test]
    fn check_ahead_when_offset_is_x_returns_same_as_x_minus_one_advance() {
        for x in [0, 1, 234].iter() {
            let mut reader = sut();
            for ad in 0 .. *x + 1 {
                reader.advance();
            }
            assert_eq!(sut().check_ahead(*x), reader.current())
        }
    }

    #[test]
    fn check_ahead_when_offset_is_too_big_for_stream_returns_none() {
        assert_eq!(sut().check_ahead(3), None)
    }

    #[test]
    fn advance_on_consumed_string_reader_returns_none() {
        assert_eq!(consumed_sut().advance(), None)
    }

    #[test]
    fn current_on_consumed_string_reader_returns_none() {
        assert_eq!(consumed_sut().current(), None)
    }

    fn consumed_sut() -> StringReader<'static> {
        let mut reader = sut();
        while let Some(_) = reader.advance() {}
        reader
    }

    #[test]
    fn rewind_when_never_advance_returns_none() {
        assert_eq!(sut().rewind(), None)
    }

    #[test]
    fn rewind_after_advance_returns_first_current() {
        let mut reader = sut();
        reader.advance();
        reader.rewind();
        assert_eq!(reader.current(), sut().current())
    }

    #[test]
    fn rewind_after_advance_returns_to_previous_states() {
        let mut sut = sut();
        sut.advance();
        sut.advance();
        assert_eq!(sut.current().unwrap(), 'f');
        sut.rewind();
        assert_eq!(sut.current().unwrap(), 't');
        sut.rewind();
        assert_eq!(sut.current().unwrap(), 'w');
        sut.advance();
        assert_eq!(sut.current().unwrap(), 't');
    }
}

pub struct GenericReader<'a, T: 'a> {
    current: Option<&'a T>,
    position: usize,
    iter: Iter<'a, T>,
    input: &'a [T]
}

impl<'a, T> GenericReader<'a, T> {
    pub fn new(input: &'a [T]) -> GenericReader<'a, T> {
        let mut iter = input.iter();
        let current = iter.next();
        GenericReader {
            input: input,
            iter: iter,
            position: 0,
            current: current
        }
    }
}

impl<'a, T> StreamReader<&'a T> for GenericReader<'a, T> {
    fn current(&self) -> Option<&'a T> {
        self.current
    }

    fn check_ahead(&self, offset: usize) -> Option<&'a T> {
        self.iter.clone().nth(offset)
    }

    fn advance(&mut self) -> Option<&'a T> {
        self.position = self.position + 1;
        self.current = self.iter.next();
        self.current()
    }

    #[allow(dead_code)]
    fn rewind(&mut self) -> Option<()> {
        if self.position == 0 { return None }
        self.position = self.position - 1;
        self.iter = self.input.iter();
        self.current = self.iter.nth(self.position);
        Some(())
    }
}