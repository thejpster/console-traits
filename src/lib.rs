//! # Console Traits
//!
//! Contains a trait which describes a console. A console is a rectangular monospaced text display, of a certain width and height. You can write Unicode text to it.
//!
//! Currently we assume UNIX LF sematics - that is a sole LF implies a new
//! line *and* carriage return (as distinct to Windows semantics where you
//! would need to send a CRLF pair).
//!
//! Implementors should handle the following Unicode characters specially:
//!
//! * 0x08 (BS)  - Backspaces one character (and erases it)
//! * 0x09 (TAB) - Move to next tab stop, or the end of the line if no tab stops left.
//! * 0x0A (LF)  - Line feed.
//! * 0x0D (CR)  - Carriage return.
//! * 0x7F (DEL) - Ignored.
#![cfg_attr(not(test), no_std)]

#[cfg(test)]
use std as core;

/// Identifies a horizontal row on the screen. Zero is at the top.
#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Copy, Clone)]
pub struct Row(pub u8);

/// Describes a vertical column on the screen. Zero is on the left.
#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Copy, Clone)]
pub struct Col(pub u8);

/// Describes a place on the screen. (0, 0) is the top left.
#[derive(Debug, Copy, Clone)]
pub struct Position {
    /// The horizontal row (zero at the top)
    pub row: Row,
    /// The vertical column (zero on the left)
    pub col: Col,
}

#[derive(Debug, Copy, Clone)]
pub enum EscapeCharMode {
    Waiting,
    Seen,
}

/// How to handle Control Characters
#[derive(Debug, Copy, Clone)]
pub enum ControlCharMode {
    Interpret,
    Display,
}

#[derive(Debug, Copy, Clone)]
/// Special types of character we need to interpret
pub enum SpecialChar {
    Linefeed,
    CarriageReturn,
    Tab,
    Backspace,
    Delete,
    Escape,
}

/// Abstraction for our console. We can move the cursor around and write text
/// to it. You should use either `UnicodeConsole` or `AsciiConsole` depending
/// on whether you want full Unicode support (`&str`, `char`, etc), or just
/// 8-bit characters (`&[u8]` and `u8`).
pub trait BaseConsole {
    type Error;

    /// Gets the last col on the screen.
    fn get_width(&self) -> Col;

    /// Gets the last row on the screen.
    fn get_height(&self) -> Row;

    /// Set the horizontal position for the next text output.
    fn set_col(&mut self, col: Col) -> Result<(), Self::Error>;

    /// Set the vertical position for the next text output.
    fn set_row(&mut self, row: Row) -> Result<(), Self::Error>;

    /// Set the horizontal and vertical position for the next text output.
    fn set_pos(&mut self, pos: Position) -> Result<(), Self::Error>;

    /// Set the horizontal and vertical position for the next text output.
    /// Don't bounds check the value, we've already done it.
    fn set_pos_unbounded(&mut self, pos: Position) {
        let _ = self.set_pos(pos);
    }

    /// Get the current screen position.
    fn get_pos(&self) -> Position;

    /// Set the control char mode
    fn set_control_char_mode(&mut self, mode: ControlCharMode);

    /// Get the current control char mode
    fn get_control_char_mode(&self) -> ControlCharMode;

    /// Set the escape char mode
    fn set_escape_char_mode(&mut self, mode: EscapeCharMode);

    /// Get the current escape char mode
    fn get_escape_char_mode(&self) -> EscapeCharMode;

    /// Called when the screen needs to scroll up one row.
    fn scroll_screen(&mut self) -> Result<(), Self::Error>;

    /// Move the current cursor right one position. Wraps at the end of the
    /// line. Returns Ok(true) if the screen needs to scroll, or Ok(false)
    /// if it does not.
    fn move_cursor_right(&mut self) -> Result<(), Self::Error> {
        let mut pos = self.get_pos();
        if pos.col < self.get_width() {
            // we'll still be on screen
            pos.col.incr();
            // no scroll needed
            self.set_pos_unbounded(pos);
        } else {
            // We're going off the right hand edge
            pos.col = Col::origin();
            if pos.row == self.get_height() {
                // We're at the bottom
                self.set_pos_unbounded(pos);
                self.scroll_screen()?;
            } else {
                // We're not at the bottom (yet)
                pos.row.incr();
                self.set_pos_unbounded(pos);
            }
        }
        Ok(())
    }
}

/// Refinement of `BaseConsole` which supports 8-bit characters. Use this is
/// you are implementing an old-fashioned ASCII console (including extended
/// ASCII, like Code Page 850, or ISO 8859-1).
pub trait AsciiConsole: BaseConsole {
    /// Write a single 8-bit char to the screen at the given position
    /// without updating the current position.
    fn write_char_at(&mut self, ch: u8, pos: Position) -> Result<(), Self::Error>;

    /// Called when they've used an escape character in the string. Currently
    /// you can only escape a single byte. The escape character is `0x1B`.
    /// This function returns 'true' when the escape sequence is complete.
    fn handle_escape(&mut self, escaped_char: u8) -> bool;

    /// Write an 8-bit string to the screen at the given position. Updates the
    /// current position to the end of the string. Strings will wrap across
    /// the end of the screen and scroll the screen if they reach the bottom.
    fn write_string(&mut self, s: &[u8]) -> Result<(), Self::Error> {
        for ch in s.iter() {
            self.write_character(*ch)?;
        }
        Ok(())
    }

    /// Write a single 8-bit char to the screen at the current position.
    fn write_character(&mut self, ch: u8) -> Result<(), Self::Error> {
        match self.get_escape_char_mode() {
            EscapeCharMode::Seen => {
                if self.handle_escape(ch) {
                    self.set_escape_char_mode(EscapeCharMode::Waiting);
                }
            }
            EscapeCharMode::Waiting => {
                let mut pos = self.get_pos();
                match self.is_special(ch) {
                    // Go to start of next row
                    Some(SpecialChar::Linefeed) => {
                        pos.col = Col::origin();
                        if pos.row == self.get_height() {
                            self.set_pos_unbounded(pos);
                            self.scroll_screen()?;
                        } else {
                            pos.row.incr();
                            self.set_pos_unbounded(pos);
                        }
                    }
                    // Go to start of this row
                    Some(SpecialChar::CarriageReturn) => {
                        pos.col = Col::origin();
                        self.set_pos_unbounded(pos);
                    }
                    // Go to next tab stop
                    Some(SpecialChar::Tab) => {
                        let tabs = pos.col.0 / 9;
                        pos.col.0 = (tabs + 1) * 9;
                        pos.col.bound(self.get_width());
                        self.set_pos_unbounded(pos);
                    }
                    // Go back one space (but don't erase anything there)
                    Some(SpecialChar::Backspace) => {
                        if pos.col > Col::origin() {
                            pos.col.decr();
                            self.set_pos_unbounded(pos);
                        }
                    }
                    // Delete is ignored
                    Some(SpecialChar::Delete) => {}
                    // Escape the next char
                    Some(SpecialChar::Escape) => {
                        self.set_escape_char_mode(EscapeCharMode::Seen);
                    }
                    None => {
                        self.write_char_at(ch, pos)?;
                        self.move_cursor_right()?;
                    }
                }
            }
        }
        Ok(())
    }

    /// Write an 8-bit string to the screen at the given position. Updates the
    /// current position to the end of the string. Strings will wrap across
    /// the end of the screen and scroll the screen if they reach the bottom.
    fn write_string_at(&mut self, s: &[u8], pos: Position) -> Result<(), Self::Error> {
        self.set_pos(pos)?;
        self.write_string(s)?;
        Ok(())
    }

    /// Check if an 8-bit char is special
    fn is_special(&self, ch: u8) -> Option<SpecialChar> {
        match self.get_control_char_mode() {
            ControlCharMode::Interpret => match ch {
                b'\n' => Some(SpecialChar::Linefeed),
                b'\r' => Some(SpecialChar::CarriageReturn),
                b'\t' => Some(SpecialChar::Tab),
                0x1b => Some(SpecialChar::Escape),
                0x7f => Some(SpecialChar::Delete),
                0x08 => Some(SpecialChar::Backspace),
                _ => None,
            },
            _ => None,
        }
    }
}

/// Refinement of `BaseConsole` which supports Unicode characters. Use this is
/// you are implementing a modern console with Unicode support.
pub trait UnicodeConsole: BaseConsole {
    /// Write a single Unicode char to the screen at the given position
    /// without updating the current position.
    fn write_char_at(&mut self, ch: char, pos: Position) -> Result<(), Self::Error>;

    /// Called when they've used an escape character in the string. Currently
    /// you can only escape a single byte. The escape character is `0x1B`.
    /// This function returns 'true' when the escape sequence is complete.
    fn handle_escape(&mut self, escaped_char: char) -> bool;

    /// Write a string to the screen at the given position. Updates the
    /// current position to the end of the string. Strings will wrap across
    /// the end of the screen and scroll the screen if they reach the bottom.
    fn write_string(&mut self, s: &str) -> Result<(), Self::Error> {
        for ch in s.chars() {
            self.write_character(ch)?;
        }
        Ok(())
    }

    /// Write a single Unicode char to the screen at the current position.
    fn write_character(&mut self, ch: char) -> Result<(), Self::Error> {
        match self.get_escape_char_mode() {
            EscapeCharMode::Seen => {
                if self.handle_escape(ch) {
                    self.set_escape_char_mode(EscapeCharMode::Waiting);
                }
            }
            EscapeCharMode::Waiting => {
                let mut pos = self.get_pos();
                match self.is_special(ch) {
                    // Go to start of next row
                    Some(SpecialChar::Linefeed) => {
                        pos.col = Col::origin();
                        if pos.row == self.get_height() {
                            self.set_pos_unbounded(pos);
                            self.scroll_screen()?;
                        } else {
                            pos.row.incr();
                            self.set_pos_unbounded(pos);
                        }
                    }
                    // Go to start of this row
                    Some(SpecialChar::CarriageReturn) => {
                        pos.col = Col::origin();
                        self.set_pos_unbounded(pos);
                    }
                    // Go to next tab stop
                    Some(SpecialChar::Tab) => {
                        let tabs = pos.col.0 / 9;
                        pos.col.0 = (tabs + 1) * 9;
                        pos.col.bound(self.get_width());
                        self.set_pos_unbounded(pos);
                    }
                    // Go back one space (but don't erase anything there)
                    Some(SpecialChar::Backspace) => {
                        if pos.col > Col::origin() {
                            pos.col.decr();
                            self.set_pos_unbounded(pos);
                        }
                    }
                    // Delete is ignored
                    Some(SpecialChar::Delete) => {}
                    // Escape the next char
                    Some(SpecialChar::Escape) => {
                        self.set_escape_char_mode(EscapeCharMode::Seen);
                    }
                    None => {
                        self.write_char_at(ch, pos)?;
                        self.move_cursor_right()?;
                    }
                }
            }
        }
        Ok(())
    }

    /// Write a string to the screen at the given position. Updates the
    /// current position to the end of the string. Strings will wrap across
    /// the end of the screen and scroll the screen if they reach the bottom.
    fn write_string_at(&mut self, s: &str, pos: Position) -> Result<(), Self::Error> {
        self.set_pos(pos)?;
        self.write_string(s)?;
        Ok(())
    }

    /// Check if a char is special
    fn is_special(&self, ch: char) -> Option<SpecialChar> {
        match self.get_control_char_mode() {
            ControlCharMode::Interpret => match ch {
                '\n' => Some(SpecialChar::Linefeed),
                '\r' => Some(SpecialChar::CarriageReturn),
                '\t' => Some(SpecialChar::Tab),
                '\u{001b}' => Some(SpecialChar::Escape),
                '\u{007f}' => Some(SpecialChar::Delete),
                '\u{0008}' => Some(SpecialChar::Backspace),
                _ => None,
            },
            _ => None,
        }
    }
}

impl Position {
    /// Create a new position
    pub fn new(row: Row, col: Col) -> Position {
        Position { row, col }
    }

    /// Get the origin (0, 0)
    pub fn origin() -> Position {
        Position {
            row: Row::origin(),
            col: Col::origin(),
        }
    }
}

impl Col {
    /// Get the origin
    pub fn origin() -> Col {
        Col(0)
    }

    pub fn incr(&mut self) -> &mut Self {
        self.0 += 1;
        self
    }

    pub fn decr(&mut self) -> &mut Self {
        self.0 -= 1;
        self
    }

    pub fn bound(&mut self, other: Col) -> &mut Self {
        if self.0 > other.0 {
            self.0 = other.0;
        }
        self
    }
}

impl Row {
    /// Get the origin
    pub fn origin() -> Row {
        Row(0)
    }

    pub fn incr(&mut self) -> &mut Self {
        self.0 += 1;
        self
    }

    pub fn decr(&mut self) -> &mut Self {
        self.0 -= 1;
        self
    }

    pub fn bound(&mut self, other: Row) -> &mut Self {
        if self.0 > other.0 {
            self.0 = other.0;
        }
        self
    }
}

impl core::convert::From<u8> for Row {
    fn from(num: u8) -> Row {
        Row(num)
    }
}

impl core::convert::From<usize> for Row {
    fn from(num: usize) -> Row {
        Row(num as u8)
    }
}

impl core::convert::From<u8> for Col {
    fn from(num: u8) -> Col {
        Col(num)
    }
}

impl core::convert::From<usize> for Col {
    fn from(num: usize) -> Col {
        Col(num as u8)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use core::fmt::Write;

    const WIDTH: u8 = 80;
    const HEIGHT: u8 = 25;

    #[derive(Copy, Clone)]
    struct Line {
        chars: [char; WIDTH as usize],
    }

    struct TestConsole {
        pos: Position,
        lines: [Line; HEIGHT as usize],
        mode: ControlCharMode,
    }

    impl TestConsole {
        fn new() -> TestConsole {
            let line = Line {
                chars: [' '; WIDTH as usize],
            };
            TestConsole {
                lines: [line; HEIGHT as usize],
                pos: Position::origin(),
                mode: ControlCharMode::Interpret,
            }
        }
    }

    impl Console for TestConsole {
        type Error = ();

        /// Gets the last col on the screen.
        fn get_width(&self) -> Col {
            Col(WIDTH - 1)
        }

        /// Gets the last row on the screen.
        fn get_height(&self) -> Row {
            Row(HEIGHT - 1)
        }

        /// Set the horizontal position for the next text output.
        fn set_col(&mut self, col: Col) -> Result<(), Self::Error> {
            self.pos.col = col;
            Ok(())
        }

        /// Set the vertical position for the next text output.
        fn set_row(&mut self, row: Row) -> Result<(), Self::Error> {
            self.pos.row = row;
            Ok(())
        }

        /// Set the horizontal and vertical position for the next text output.
        fn set_pos(&mut self, pos: Position) -> Result<(), Self::Error> {
            self.pos = pos;
            Ok(())
        }

        /// Get the current screen position.
        fn get_pos(&self) -> Position {
            self.pos
        }

        /// Set the control char mode
        fn set_control_char_mode(&mut self, mode: ControlCharMode) {
            self.mode = mode;
        }

        /// Get the current control char mode
        fn get_control_char_mode(&self) -> ControlCharMode {
            self.mode
        }

        /// Called when the screen needs to scroll up one row.
        fn scroll_screen(&mut self) -> Result<(), Self::Error> {
            for row in 0..HEIGHT - 1 {
                self.lines[row as usize] = self.lines[(row as usize) + 1];
                self.lines[(HEIGHT as usize) - 1] = Line {
                    chars: [' '; WIDTH as usize],
                };
            }
            Ok(())
        }

        /// Write a single Unicode char to the screen at the given position
        /// without updating the current position.
        fn write_char_at(&mut self, ch: char, pos: Position) -> Result<(), Self::Error> {
            self.lines[pos.row.0 as usize].chars[pos.col.0 as usize] = ch;
            Ok(())
        }
    }

    impl core::fmt::Write for TestConsole {
        fn write_str(&mut self, s: &str) -> core::fmt::Result {
            self.write_string(s).unwrap();
            Ok(())
        }
    }

    #[test]
    fn test_write() {
        let mut c = TestConsole::new();
        c.write_str("Hello").unwrap();
        assert_eq!(
            &c.lines[0].chars[0..10],
            &"Hello     ".chars().collect::<Vec<char>>()[..]
        );
        assert_eq!(c.pos.row, Row::origin());
        assert_eq!(c.pos.col, Col(5));
    }

    #[test]
    fn test_lf() {
        let mut c = TestConsole::new();
        c.write_str("Hello\n").unwrap();
        assert_eq!(
            &c.lines[0].chars[0..10],
            &"Hello     ".chars().collect::<Vec<char>>()[..]
        );
        assert_eq!(c.pos.row, Row(1));
        assert_eq!(c.pos.col, Col(0));
    }

    #[test]
    fn test_cr() {
        let mut c = TestConsole::new();
        c.write_str("Hello\r123").unwrap();
        assert_eq!(
            &c.lines[0].chars[0..10],
            &"123lo     ".chars().collect::<Vec<char>>()[..]
        );
        assert_eq!(c.pos.row, Row(0));
        assert_eq!(c.pos.col, Col(3));
    }

    #[test]
    fn test_bs() {
        let mut c = TestConsole::new();
        c.write_str("Hello~\u{0008}!").unwrap();
        assert_eq!(
            &c.lines[0].chars[0..10],
            &"Hello!    ".chars().collect::<Vec<char>>()[..]
        );
        assert_eq!(c.pos.row, Row(0));
        assert_eq!(c.pos.col, Col(6));
    }

    #[test]
    fn test_tab() {
        let mut c = TestConsole::new();
        c.write_str("1\t2\tHello\t4").unwrap();
        assert_eq!(
            &c.lines[0].chars[0..28],
            &"1        2        Hello    4"
                .chars()
                .collect::<Vec<char>>()[..]
        );
        assert_eq!(c.pos.row, Row(0));
        assert_eq!(c.pos.col, Col(28));
    }

    #[test]
    fn test_wrap() {
        let mut c = TestConsole::new();
        for line in 0..HEIGHT {
            writeln!(c, "{}", line).unwrap();
        }
        // First line should have a 1 in it, not a 0
        assert_eq!(
            &c.lines[0].chars[0..4],
            &"1   ".chars().collect::<Vec<char>>()[..]
        );
        assert_eq!(c.pos.row, Row(HEIGHT - 1));
        assert_eq!(c.pos.col, Col(0));
    }

}

// End of file
