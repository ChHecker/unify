use std::iter::Peekable;

use crate::Integer;
use crate::num::lexer::Token;
use crate::result::TransposeRef;

use super::Sign;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Num {
    pub float: Option<Float>,
    pub uncert: Option<Uncertainty>,
    pub exp: Option<Exponent>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Float {
    pub sign: Sign,
    pub int: Integer,
    pub dec: Option<Integer>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Uncertainty {
    Shorthand(Integer),                       // e.g. 1.23(4)
    Explicit(Float),                          // e.g. 1.23 ± 0.04
    Asymmetric { plus: Float, minus: Float }, // e.g. +0.02-0.01
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Exponent {
    pub sign: Sign,
    pub int: Integer,
}

impl Default for Exponent {
    fn default() -> Self {
        Self {
            sign: Sign::Plus,
            int: String::from("1"),
        }
    }
}

impl Num {
    pub fn new<T: IntoIterator<Item = crate::Result<Token>>>(iter: T) -> crate::Result<Self> {
        let mut iter = iter.into_iter().peekable();

        let float = match iter.peek().transpose()? {
            Some(Token::Sign(_)) | Some(Token::Int(_)) => Some(Float::new(&mut iter)?),
            _ => None,
        };
        let uncert = Uncertainty::new(&mut iter)?;
        let exp = Exponent::new(&mut iter)?;
        if let Some(next) = iter.next() {
            match next? {
                Token::Int(_) => return Err(String::from("unexpected number after the main number")),
                Token::Sign(_) => return Err(String::from("too many uncertainties")),
                Token::DecSep => return Err(String::from("too many decimal separators")),
                Token::Exp => return Err(String::from("too many exponentials")),
                Token::PlusMinus => return Err(String::from("too many uncertainties")),
                Token::ParenOpen | Token::ParenClose => return Err(String::from("unexpected paranthesis")),
            }
        }

        let num = Self { float, uncert, exp };
        num.validate()?;
        Ok(num)
    }

    fn validate(&self) -> crate::Result<()> {
        if self.float.is_none() && self.exp.is_none() {
            return Err(String::from("empty number"))
        }

        if self.float.is_none() && self.uncert.is_some() {
            return Err(String::from("cannot have uncertainty without value"));
        }

        Ok(())
    }
}

impl Float {
    fn new<T: Iterator<Item = crate::Result<Token>>>(
        iter: &mut Peekable<T>,
    ) -> crate::Result<Self> {
        let sign = match iter.peek().ok_or("invalid number")?.as_ref()? {
            Token::Sign(sign) => {
                let sign = *sign;
                iter.next();
                sign
            }
            _ => Sign::Plus,
        };

        let int = Self::parse_int(iter)?;

        let dec = match iter.peek().transpose()? {
            Some(Token::DecSep) => {
                iter.next();

                Some(Self::parse_int(iter)?)
            }
            _ => None,
        };

        Ok(Self { sign, int, dec })
    }

    fn parse_int<T: Iterator<Item = crate::Result<Token>>>(
        iter: &mut Peekable<T>,
    ) -> crate::Result<String> {
        match iter.next().ok_or("invalid number")?? {
            Token::Int(int) => Ok(int),
            t => Err(format!("unexpected token '{t}'")),
        }
    }
}

impl Uncertainty {
    fn new<T: Iterator<Item = crate::Result<Token>>>(
        iter: &mut Peekable<T>,
    ) -> crate::Result<Option<Self>> {
        let mut uncert = match iter.peek().transpose()? {
            Some(Token::PlusMinus) => {
                iter.next();
                Some(Self::Explicit(Float::new(iter)?))
            }
            Some(Token::ParenOpen) => {
                iter.next();

                let int = match iter.next() {
                    Some(Ok(Token::Int(int))) => int,
                    _ => return Err(String::from("expected integer in uncertainty")),
                };

                if !matches!(iter.next(), Some(Ok(Token::ParenClose))) {
                    return Err(String::from("expected closing parenthesis"));
                }

                Some(Self::Shorthand(int))
            }
            Some(Token::Sign(_)) => {
                let mut float1 = Float::new(iter)?;
                let mut float2 = Float::new(iter)?;

                match (float1.sign, float2.sign) {
                    (Sign::Plus, Sign::Minus) => (),
                    (Sign::Minus, Sign::Plus) => std::mem::swap(&mut float1, &mut float2),
                    _ => return Err(String::from("invalid uncertainties")),
                }

                Some(Self::Asymmetric {
                    plus: float1,
                    minus: float2,
                })
            }
            _ => None,
        };

        if let Some(uncert) = &mut uncert {
            uncert.reduce();
        }

        Ok(uncert)
    }

    fn reduce(&mut self) {
        match self {
            Uncertainty::Asymmetric { plus, minus }
                if plus.int == minus.int && plus.dec == minus.dec =>
            {
                let plus = std::mem::replace(
                    plus,
                    Float {
                        sign: Sign::Plus,
                        int: String::new(),
                        dec: None,
                    },
                );
                *self = Uncertainty::Explicit(plus)
            }
            _ => (),
        }
    }
}

impl Exponent {
    fn new<T: Iterator<Item = crate::Result<Token>>>(
        iter: &mut Peekable<T>,
    ) -> crate::Result<Option<Self>> {
        Ok(match iter.peek().transpose()? {
            Some(Token::Exp) => {
                iter.next();

                let float = Float::new(iter)?;
                if float.dec.is_some() {
                    return Err(String::from("exponent may not be a decimal"));
                }

                Some(Self {
                    sign: float.sign,
                    int: float.int,
                })
            }
            _ => None,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::num::lexer::Tokenizer;

    use super::*;

    #[test]
    fn float() {
        let text = String::from("-1234.567");
        let tokenizer = Tokenizer::new(text.chars());
        let mut iter = tokenizer.peekable();
        let float = Float::new(&mut iter);

        assert_eq!(
            float,
            Ok(Float {
                sign: Sign::Minus,
                int: String::from("1234"),
                dec: Some(String::from("567")),
            })
        )
    }

    #[test]
    fn uncertainty_sym() {
        let text = String::from("+-5");
        let tokenizer = Tokenizer::new(text.chars());
        let mut iter = tokenizer.peekable();
        let uncert = Uncertainty::new(&mut iter);

        assert_eq!(
            uncert,
            Ok(Some(Uncertainty::Explicit(Float {
                sign: Sign::Plus,
                int: String::from("5"),
                dec: None
            })))
        )
    }

    #[test]
    fn uncertainty_asym() {
        let text = String::from("-1+2");
        let tokenizer = Tokenizer::new(text.chars());
        let mut iter = tokenizer.peekable();
        let uncert = Uncertainty::new(&mut iter);

        assert_eq!(
            uncert,
            Ok(Some(Uncertainty::Asymmetric {
                plus: Float {
                    sign: Sign::Plus,
                    int: String::from("2"),
                    dec: None
                },
                minus: Float {
                    sign: Sign::Minus,
                    int: String::from("1"),
                    dec: None
                }
            }))
        )
    }

    #[test]
    fn uncertainty_short() {
        let text = String::from("(24)");
        let tokenizer = Tokenizer::new(text.chars());
        let mut iter = tokenizer.peekable();
        let uncert = Uncertainty::new(&mut iter);

        assert_eq!(uncert, Ok(Some(Uncertainty::Shorthand(String::from("24")))))
    }

    #[test]
    fn num() {
        let text = String::from("-1234.567+23-15e-23");
        let tokenizer = Tokenizer::new(text.chars());
        let iter = tokenizer.peekable();
        let num = Num::new(iter);

        let float = Some(Float {
            sign: Sign::Minus,
            int: String::from("1234"),
            dec: Some(String::from("567")),
        });
        let uncert = Some(Uncertainty::Asymmetric {
            plus: Float {
                sign: Sign::Plus,
                int: String::from("23"),
                dec: None,
            },
            minus: Float {
                sign: Sign::Minus,
                int: String::from("15"),
                dec: None,
            },
        });
        let exp = Some(Exponent {
            sign: Sign::Minus,
            int: String::from("23"),
        });
        assert_eq!(num, Ok(Num { float, uncert, exp }));
    }

    #[test]
    fn num_exp_only() {
        let text = String::from("e-9");
        let tokenizer = Tokenizer::new(text.chars());
        let iter = tokenizer.peekable();
        let num = Num::new(iter);

        let exp = Some(Exponent {
            sign: Sign::Minus,
            int: String::from("9"),
        });
        assert_eq!(
            num,
            Ok(Num {
                float: None,
                uncert: None,
                exp
            })
        );
    }

    #[test]
    fn num_invalid() {
        let text = String::from("+-15e3");
        let tokenizer = Tokenizer::new(text.chars());
        let iter = tokenizer.peekable();
        let num = Num::new(iter);

        assert!(num.is_err())
    }
}
