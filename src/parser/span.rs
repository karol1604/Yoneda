use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn line_col(&self, input: &str) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;
        for (i, c) in input.char_indices() {
            if i >= self.start {
                break;
            }
            if c == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        (line, col)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Span({}, {})", self.start, self.end)
    }
}
