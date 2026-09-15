use std::fmt::Display;
use std::iter::Peekable;

use crate::num::Sign;

pub struct Tokenizer<I: Iterator<Item = char>> {
    iter: Peekable<I>,
}

impl<I: Iterator<Item = char>> Tokenizer<I> {
    pub fn new<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = char, IntoIter = I>,
    {
        Self {
            iter: iter.into_iter().peekable(),
        }
    }
}

impl<I: Iterator<Item = char>> Iterator for Tokenizer<I> {
    type Item = crate::Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(Ok(match self.iter.next()? {
            ' ' => return self.next(),
            '(' => Token::ParenOpen,
            ')' => Token::ParenClose,
            '^' => Token::Circ,
            '+' => Token::Sign(Sign::Plus),
            '-' => Token::Sign(Sign::Minus),
            '/' => Token::Slash,
            '\u{2070}' => Token::Exponent('0'),
            '\u{00B9}' => Token::Exponent('1'),
            '\u{00B2}' => Token::Exponent('2'),
            '\u{00B3}' => Token::Exponent('3'),
            '\u{2074}' => Token::Exponent('4'),
            '\u{2075}' => Token::Exponent('5'),
            '\u{2076}' => Token::Exponent('6'),
            '\u{2077}' => Token::Exponent('7'),
            '\u{2078}' => Token::Exponent('8'),
            '\u{2079}' => Token::Exponent('9'),
            '\u{207A}' => Token::ExponentSign(Sign::Plus),
            '\u{207B}' => Token::ExponentSign(Sign::Minus),
            c if c.is_ascii_digit() => {
                let mut num = String::from(c);
                while let Some(c) = self.iter.peek()
                    && c.is_ascii_digit()
                {
                    num.push(*c);
                    self.iter.next();
                }
                Token::Number(num)
            }
            c => {
                let mut unit = String::from(c);
                while let Some(c) = self.iter.peek()
                    && *c != ' '
                    && *c != '('
                    && *c != ')'
                    && *c != '^'
                    && *c != '+'
                    && *c != '-'
                    && *c != '/'
                {
                    unit.push(*c);
                    self.iter.next();
                }
                if unit == "sqrt" {
                    Token::Sqrt
                } else {
                    Token::Unit(unit)
                }
            }
        }))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Unit(String),
    Number(String),
    Sign(Sign),
    Exponent(char),
    ExponentSign(Sign),
    Circ,
    ParenOpen,
    ParenClose,
    Slash,
    Sqrt,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Unit(unit) => write!(f, "{unit}"),
            Token::Number(num) => write!(f, "{num}"),
            Token::Sign(sign) => write!(f, "{sign}"),
            Token::Exponent(exp) => write!(f, "{exp}"),
            Token::ExponentSign(sign) => write!(f, "{sign}"),
            Token::Circ => write!(f, "^"),
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
            Token::Slash => write!(f, "/"),
            Token::Sqrt => write!(f, "sqrt"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let text = "kg m/s^(2/3) sqrt(m)";
        let tokenizer = Tokenizer::new(text.chars());
        let tokens: crate::Result<Vec<Token>> = tokenizer.collect();
        assert_eq!(
            tokens,
            Ok(vec![
                Token::Unit(String::from("kg")),
                Token::Unit(String::from("m")),
                Token::Slash,
                Token::Unit(String::from("s")),
                Token::Circ,
                Token::ParenOpen,
                Token::Number(String::from("2")),
                Token::Slash,
                Token::Number(String::from("3")),
                Token::ParenClose,
                Token::Sqrt,
                Token::ParenOpen,
                Token::Unit(String::from("m")),
                Token::ParenClose,
            ])
        );
    }
}
