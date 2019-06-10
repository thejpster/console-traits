//! # Console Traits
//!
//! Contains a trait which describes a console. A console is a rectangular
//! monospaced text display, of a certain width and height. You can write
//! Unicode text to it.
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

/// The states we can be in when parsing ANSI escape sequences
#[derive(Debug, Copy, Clone)]
pub enum EscapeCharMode {
    /// Waiting for 0x1B
    Waiting,
    /// Last byte was 0x1B, waiting for the opening byte (0x40 to 0x5F)
    SeenEscWantOpeningChar,
    /// Opening byte was '[' (Control Sequence Introducer). Next arg is a
    /// paramter, or a final-byte.
    SeenCsiWantArgs,
    /// Processing a CSI. We've seen at one integer paramter. Want more
    /// arguments or a final-byte.
    SeenCsiAndArgWantType(u32),
    /// Processing a CSI. We've seen at two integer paramters. Want more
    /// arguments or a final-byte.
    SeenCsiAndTwoArgsWantType(u32, u32),
    /// Processing a CSI. We've seen something we don't like, so we're looking
    /// for a final-byte.
    SeenCsiBadCode,
}

/// The standard 3-bit ANSI colours.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Colour {
    Black = 0,
    Red = 1,
    Green = 2,
    Yellow = 3,
    Blue = 4,
    Magenta = 5,
    Cyan = 6,
    White = 7,
}

/// The various colour sequences we can decode. A terminal can support zero or
/// more of these.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum CharacterAppearance {
    /// SGR 0
    Reset,
    /// SGR 1
    Bold,
    /// SGR 2
    Faint,
    /// SGR 3
    Italic,
    /// SGR 4
    Underline,
    /// SGR 5
    SlowBlink,
    /// SGR 6
    FastBlink,
    /// SGR 7
    Reverse,
    /// SGR 8
    Conceal,
    /// SGR 9
    CrossedOut,
    /// SGR 10..19 (argument is 0 for default font, otherwise 1..9 for selected font index)
    Font(u8),
    /// SGR 20
    Fraktur,
    /// SGR 21
    DoubleUnderline,
    /// SGR 22
    BoldFaintOff,
    /// SGR 23
    ItalicFrakturOff,
    /// SGR 24
    UnderlineOff,
    /// SGR 25
    BlinkOff,
    /// SGR 27
    InverseOff,
    /// SGR 28
    ConcealOff,
    /// SGR 29
    CrossedOutOff,
    /// SGR 30..37 (argument is from colour table)
    ForegroundColour(Colour),
    /// SGR 39
    DefaultForegroundColour,
    /// SGR 40..47 (argument is from colour table)
    BackgroundColour(Colour),
    /// SGR 49
    DefaultBackgroundColour,
    /// Any other value
    Unknown,
}

impl Into<CharacterAppearance> for u32 {
    fn into(self) -> CharacterAppearance {
        match self {
            0 => CharacterAppearance::Reset,
            1 => CharacterAppearance::Bold,
            2 => CharacterAppearance::Faint,
            3 => CharacterAppearance::Italic,
            4 => CharacterAppearance::Underline,
            5 => CharacterAppearance::SlowBlink,
            6 => CharacterAppearance::FastBlink,
            7 => CharacterAppearance::Reverse,
            8 => CharacterAppearance::Conceal,
            9 => CharacterAppearance::CrossedOut,
            10...19 => CharacterAppearance::Font(self as u8 - 10),
            20 => CharacterAppearance::Fraktur,
            21 => CharacterAppearance::DoubleUnderline,
            22 => CharacterAppearance::BoldFaintOff,
            23 => CharacterAppearance::ItalicFrakturOff,
            24 => CharacterAppearance::UnderlineOff,
            25 => CharacterAppearance::BlinkOff,
            27 => CharacterAppearance::InverseOff,
            28 => CharacterAppearance::ConcealOff,
            29 => CharacterAppearance::CrossedOutOff,
            30 => CharacterAppearance::ForegroundColour(Colour::Black),
            31 => CharacterAppearance::ForegroundColour(Colour::Red),
            32 => CharacterAppearance::ForegroundColour(Colour::Green),
            33 => CharacterAppearance::ForegroundColour(Colour::Yellow),
            34 => CharacterAppearance::ForegroundColour(Colour::Blue),
            35 => CharacterAppearance::ForegroundColour(Colour::Magenta),
            36 => CharacterAppearance::ForegroundColour(Colour::Cyan),
            37 => CharacterAppearance::ForegroundColour(Colour::White),
            39 => CharacterAppearance::DefaultForegroundColour,
            40 => CharacterAppearance::BackgroundColour(Colour::Black),
            41 => CharacterAppearance::BackgroundColour(Colour::Red),
            42 => CharacterAppearance::BackgroundColour(Colour::Green),
            43 => CharacterAppearance::BackgroundColour(Colour::Yellow),
            44 => CharacterAppearance::BackgroundColour(Colour::Blue),
            45 => CharacterAppearance::BackgroundColour(Colour::Magenta),
            46 => CharacterAppearance::BackgroundColour(Colour::Cyan),
            47 => CharacterAppearance::BackgroundColour(Colour::White),
            49 => CharacterAppearance::DefaultBackgroundColour,
            _ => CharacterAppearance::Unknown,
        }
    }
}

/// The various ANSI escape sequences we support. Arguments are generally
/// 1-based,
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum EscapeCode {
    /// Resets the terminal to its initial state
    Reset,
    /// Moves the cursor one cell upwards. If the cursor is at the top of the
    /// screen, this has no effect.
    CursorUp(u32),
    /// Moves the cursor one cell downwards. If the cursor is at the bottom of the
    /// screen, this has no effect.
    CursorDown(u32),
    /// Moves the cursor one cell forwards. If the cursor is at the edge of the
    /// screen, this has no effect.
    CursorForward(u32),
    /// Moves the cursor one cell backwards. If the cursor is at the edge of the
    /// screen, this has no effect.
    CursorBack(u32),
    /// Moves the cursor to the start of the line, `arg` lines down.
    CursorNextLine(u32),
    /// Moves the cursor to the start of the line, `arg` lines up.
    CursorPreviousLine(u32),
    /// Moves the cursor to column `arg`.
    CursorHorizontalAbsolute(u32),
    /// Moves the cursor to row `arg-0`, column `arg-1`.
    CursorPosition(u32, u32),
    /// Clears part of the screen. `arg` can be:
    ///
    /// * 0 - clear from cursor to end of screen
    /// * 1 - clear from cursor to beginning of screen
    /// * 2 - clear entire screen
    /// * 3 - clear entire screen and delete scrollback
    EraseInDisplay(u32),
    /// Erases part of the line. `arg` can be:
    ///
    /// * 0 - clear from cursor to end of line
    /// * 1 - clear from cursor to start of line
    /// * 2 - clear entire line
    EraseInLine(u32),
    /// Scroll whole page up `arg` lines.
    ScrollUp(u32),
    /// Scroll whole page down `arg` lines.
    ScrollDown(u32),
    /// Change character appearance
    SelectGraphicRendition(CharacterAppearance),
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

    /// Handle the ANSI escape sequence which has been detected in the stream
    fn handle_escape(&mut self, code: EscapeCode);

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

    /// ANSI sequences are made entirely of ASCII characters, so we can
    /// implement this one for both AsciiConsole and UnicodeConsole.
    fn ansi_state_machine(&mut self, character: u8) {
        let state = match self.get_escape_char_mode() {
            EscapeCharMode::Waiting => {
                // Er ... this is a bug.
                unreachable!();
            }
            EscapeCharMode::SeenEscWantOpeningChar => match character {
                b'c' => {
                    self.handle_escape(EscapeCode::Reset);
                    EscapeCharMode::Waiting
                }
                b'[' => EscapeCharMode::SeenCsiWantArgs,
                _ => EscapeCharMode::Waiting,
            },
            EscapeCharMode::SeenCsiWantArgs => {
                match character {
                    b'0'...b'9' => {
                        let digit = character - b'0';
                        EscapeCharMode::SeenCsiAndArgWantType(digit as u32)
                    }
                    b';' => {
                        // Missing argument - default to 0
                        EscapeCharMode::SeenCsiAndArgWantType(0)
                    }
                    b'A' => {
                        self.handle_escape(EscapeCode::CursorUp(0));
                        EscapeCharMode::Waiting
                    }
                    b'B' => {
                        self.handle_escape(EscapeCode::CursorDown(0));
                        EscapeCharMode::Waiting
                    }
                    b'C' => {
                        self.handle_escape(EscapeCode::CursorForward(0));
                        EscapeCharMode::Waiting
                    }
                    b'D' => {
                        self.handle_escape(EscapeCode::CursorBack(0));
                        EscapeCharMode::Waiting
                    }
                    b'E' => {
                        self.handle_escape(EscapeCode::CursorNextLine(0));
                        EscapeCharMode::Waiting
                    }
                    b'F' => {
                        self.handle_escape(EscapeCode::CursorPreviousLine(0));
                        EscapeCharMode::Waiting
                    }
                    b'G' => {
                        self.handle_escape(EscapeCode::CursorHorizontalAbsolute(0));
                        EscapeCharMode::Waiting
                    }
                    b'H' => {
                        self.handle_escape(EscapeCode::CursorPosition(0, 0));
                        EscapeCharMode::Waiting
                    }
                    b'J' => {
                        self.handle_escape(EscapeCode::EraseInDisplay(0));
                        EscapeCharMode::Waiting
                    }
                    b'K' => {
                        self.handle_escape(EscapeCode::EraseInLine(0));
                        EscapeCharMode::Waiting
                    }
                    b'S' => {
                        self.handle_escape(EscapeCode::ScrollUp(0));
                        EscapeCharMode::Waiting
                    }
                    b'T' => {
                        self.handle_escape(EscapeCode::ScrollDown(0));
                        EscapeCharMode::Waiting
                    }
                    b'f' => {
                        self.handle_escape(EscapeCode::CursorPosition(0, 0));
                        EscapeCharMode::Waiting
                    }
                    b'm' => {
                        self.handle_escape(EscapeCode::SelectGraphicRendition(
                            CharacterAppearance::Reset,
                        ));
                        EscapeCharMode::Waiting
                    }
                    0x40...0x7E => {
                        // This is a final byte of a type we can't handle
                        EscapeCharMode::Waiting
                    }
                    _ => EscapeCharMode::SeenCsiBadCode,
                }
            }
            EscapeCharMode::SeenCsiAndArgWantType(old_arg) => {
                match character {
                    b'0'...b'9' => {
                        let digit = character - b'0';
                        EscapeCharMode::SeenCsiAndArgWantType((old_arg * 10) + digit as u32)
                    }
                    b';' => EscapeCharMode::SeenCsiAndTwoArgsWantType(old_arg, 0),
                    b'A' => {
                        self.handle_escape(EscapeCode::CursorUp(old_arg));
                        EscapeCharMode::Waiting
                    }
                    b'B' => {
                        self.handle_escape(EscapeCode::CursorDown(old_arg));
                        EscapeCharMode::Waiting
                    }
                    b'C' => {
                        self.handle_escape(EscapeCode::CursorForward(old_arg));
                        EscapeCharMode::Waiting
                    }
                    b'D' => {
                        self.handle_escape(EscapeCode::CursorBack(old_arg));
                        EscapeCharMode::Waiting
                    }
                    b'E' => {
                        self.handle_escape(EscapeCode::CursorNextLine(old_arg));
                        EscapeCharMode::Waiting
                    }
                    b'F' => {
                        self.handle_escape(EscapeCode::CursorPreviousLine(old_arg));
                        EscapeCharMode::Waiting
                    }
                    b'G' => {
                        self.handle_escape(EscapeCode::CursorHorizontalAbsolute(old_arg));
                        EscapeCharMode::Waiting
                    }
                    b'H' => {
                        self.handle_escape(EscapeCode::CursorPosition(old_arg, 0));
                        EscapeCharMode::Waiting
                    }
                    b'J' => {
                        self.handle_escape(EscapeCode::EraseInDisplay(old_arg));
                        EscapeCharMode::Waiting
                    }
                    b'K' => {
                        self.handle_escape(EscapeCode::EraseInLine(old_arg));
                        EscapeCharMode::Waiting
                    }
                    b'S' => {
                        self.handle_escape(EscapeCode::ScrollUp(old_arg));
                        EscapeCharMode::Waiting
                    }
                    b'T' => {
                        self.handle_escape(EscapeCode::ScrollDown(old_arg));
                        EscapeCharMode::Waiting
                    }
                    b'f' => {
                        self.handle_escape(EscapeCode::CursorPosition(old_arg, 0));
                        EscapeCharMode::Waiting
                    }
                    b'm' => {
                        self.handle_escape(EscapeCode::SelectGraphicRendition(old_arg.into()));
                        EscapeCharMode::Waiting
                    }
                    0x40...0x7E => {
                        // This is a final byte of a type we can't handle
                        EscapeCharMode::Waiting
                    }
                    _ => {
                        // Need a final byte
                        EscapeCharMode::SeenCsiBadCode
                    }
                }
            }
            EscapeCharMode::SeenCsiAndTwoArgsWantType(arg0, arg1) => {
                match character {
                    b'0'...b'9' => {
                        let digit = character - b'0';
                        EscapeCharMode::SeenCsiAndTwoArgsWantType(arg0, (arg1 * 10) + digit as u32)
                    }
                    b'H' => {
                        self.handle_escape(EscapeCode::CursorPosition(arg0, arg1));
                        EscapeCharMode::Waiting
                    }
                    b'f' => {
                        self.handle_escape(EscapeCode::CursorPosition(arg0, arg1));
                        EscapeCharMode::Waiting
                    }
                    0x40...0x7E => {
                        // This is a final byte of a type we can't handle
                        EscapeCharMode::Waiting
                    }
                    _ => EscapeCharMode::SeenCsiBadCode,
                }
            }
            EscapeCharMode::SeenCsiBadCode => {
                match character {
                    0x40...0x7E => {
                        // This is a final byte of a type we can't handle
                        EscapeCharMode::Waiting
                    }
                    _ => EscapeCharMode::SeenCsiBadCode,
                }
            }
        };
        // Get the implementor to remember our state for next time
        self.set_escape_char_mode(state);
    }
}

/// Refinement of `BaseConsole` which supports 8-bit characters. Use this is
/// you are implementing an old-fashioned ASCII console (including extended
/// ASCII, like Code Page 850, or ISO 8859-1).
pub trait AsciiConsole: BaseConsole {
    /// Write a single 8-bit char to the screen at the given position
    /// without updating the current position.
    fn write_char_at(&mut self, ch: u8, pos: Position) -> Result<(), Self::Error>;

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
                        self.set_escape_char_mode(EscapeCharMode::SeenEscWantOpeningChar);
                    }
                    None => {
                        self.write_char_at(ch, pos)?;
                        self.move_cursor_right()?;
                    }
                }
            }
            _ => {
                self.ansi_state_machine(ch);
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
                        self.set_escape_char_mode(EscapeCharMode::SeenEscWantOpeningChar);
                    }
                    None => {
                        self.write_char_at(ch, pos)?;
                        self.move_cursor_right()?;
                    }
                }
            }
            _ => {
                // Feed the UTF-8 encoded bytes into the ANSI engine
                let mut buf = [0u8; 6];
                for b in ch.encode_utf8(&mut buf).bytes() {
                    self.ansi_state_machine(b);
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
    pub const fn new(row: Row, col: Col) -> Position {
        Position { row, col }
    }

    /// Get the origin (0, 0)
    pub const fn origin() -> Position {
        Position {
            row: Row::origin(),
            col: Col::origin(),
        }
    }
}

impl Col {
    pub const fn new(value: u8) -> Col {
        Col(value)
    }

    /// Get the origin
    pub const fn origin() -> Col {
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
    pub const fn new(value: u8) -> Row {
        Row(value)
    }

    /// Get the origin
    pub const fn origin() -> Row {
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
        escape_char_mode: EscapeCharMode,
        ansi_codes: Vec<EscapeCode>,
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
                escape_char_mode: EscapeCharMode::Waiting,
                ansi_codes: Vec::new(),
            }
        }
    }

    impl BaseConsole for TestConsole {
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

        /// Set the escape char mode
        fn set_escape_char_mode(&mut self, mode: EscapeCharMode) {
            self.escape_char_mode = mode;
        }

        /// Get the current escape char mode
        fn get_escape_char_mode(&self) -> EscapeCharMode {
            self.escape_char_mode
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

        fn handle_escape(&mut self, code: EscapeCode) {
            self.ansi_codes.push(code);
        }
    }

    impl UnicodeConsole for TestConsole {
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

    #[test]
    fn test_sgr_reset() {
        let mut c = TestConsole::new();
        writeln!(c, "\u{001B}[0m").unwrap();
        assert_eq!(
            &c.ansi_codes,
            &[EscapeCode::SelectGraphicRendition(
                CharacterAppearance::Reset
            )],
        );
        writeln!(c, "\u{001B}[m").unwrap();
        assert_eq!(
            &c.ansi_codes,
            &[
                EscapeCode::SelectGraphicRendition(CharacterAppearance::Reset),
                EscapeCode::SelectGraphicRendition(CharacterAppearance::Reset)
            ],
        );
    }
}

// End of file
