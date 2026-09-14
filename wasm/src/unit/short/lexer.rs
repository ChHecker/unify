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
            c if c.is_numeric() => {
                let mut num = String::from(c);
                while let Some(c) = self.iter.peek()
                    && c.is_numeric()
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
                Token::Unit(unit)
            }
        }))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Unit(String),
    Number(String),
    Sign(Sign),
    Circ,
    ParenOpen,
    ParenClose,
    Slash,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let text = "kg m/s^(2/3)";
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
            ])
        );
    }
}
