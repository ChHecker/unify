use std::fmt::Display;
use std::iter::Peekable;

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
            '/' => Token::Per,
            c if c.is_alphabetic() => {
                let mut unit = String::from(c);
                while let Some(c) = self.iter.peek()
                    && c.is_alphanumeric()
                {
                    unit.push(*c);
                    self.iter.next();
                }

                unit = unit.to_lowercase();

                match unit.as_str() {
                    "per" => Token::Per,
                    _ => Token::Unit(unit),
                }
            }
            c => return Some(Err(format!("unexpected token '{c}'"))),
        }))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Unit(String),
    Per,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Unit(unit) => write!(f, "{unit}"),
            Token::Per => write!(f, "per"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let text = "kilo gram per meter squared";
        let tokenizer = Tokenizer::new(text.chars());
        let tokens: crate::Result<Vec<Token>> = tokenizer.collect();
        assert_eq!(
            tokens,
            Ok(vec![
                Token::Unit(String::from("kilo")),
                Token::Unit(String::from("gram")),
                Token::Per,
                Token::Unit(String::from("meter")),
                Token::Unit(String::from("squared")),
            ])
        );
    }
}
