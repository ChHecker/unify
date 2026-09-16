use std::fmt::Display;
use std::iter::Peekable;

use crate::Integer;

use super::Sign;

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
            '+' => match self.iter.peek() {
                Some('-') => {
                    self.iter.next();
                    Token::PlusMinus
                }
                _ => Token::Sign(Sign::Plus),
            },
            '-' => Token::Sign(Sign::Minus),
            '±' => Token::PlusMinus,
            '(' => Token::ParenOpen,
            ')' => Token::ParenClose,
            'e' => Token::Exp,
            c if c == ',' || c == '.' => Token::DecSep,
            c if c.is_ascii_digit() => {
                let mut int = String::from(c);
                while let Some(c) = self.iter.peek() {
                    if c.is_ascii_digit() {
                        int.push(*c);
                        self.iter.next();
                    } else if *c == ' ' {
                        self.iter.next();
                    } else {
                        break;
                    }
                }
                Token::Int(int)
            }
            c => return Some(Err(format!("unexpected token '{}'", c))),
        }))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Int(Integer),
    Sign(Sign),
    DecSep,
    Exp,
    PlusMinus,
    ParenOpen,
    ParenClose,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Int(int) => write!(f, "{}", int),
            Token::Sign(sign) => write!(f, "{}", sign),
            Token::DecSep => write!(f, "decsep"),
            Token::Exp => write!(f, "^"),
            Token::PlusMinus => write!(f, "+-"),
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let text = "1 012.231+-20+10-5(32) e 12";
        let tokenizer = Tokenizer::new(text.chars());
        let tokens: crate::Result<Vec<Token>> = tokenizer.collect();
        assert_eq!(
            tokens,
            Ok(vec![
                Token::Int(String::from("1012")),
                Token::DecSep,
                Token::Int(String::from("231")),
                Token::PlusMinus,
                Token::Int(String::from("20")),
                Token::Sign(Sign::Plus),
                Token::Int(String::from("10")),
                Token::Sign(Sign::Minus),
                Token::Int(String::from("5")),
                Token::ParenOpen,
                Token::Int(String::from("32")),
                Token::ParenClose,
                Token::Exp,
                Token::Int(String::from("12")),
            ])
        );
    }
}
