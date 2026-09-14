use std::iter::Peekable;

use crate::num::Sign;
use crate::result::TransposeRef;
use crate::unit::Units as UnitsLookup;
use crate::unit::parser::*;
use crate::unit::short::lexer::Token;

impl<'a> Units<'a> {
    pub fn short<T: IntoIterator<Item = crate::Result<Token>>>(
        iter: T,
        units_lookup: &'a UnitsLookup,
    ) -> crate::Result<Self> {
        let mut iter = iter.into_iter().peekable();
        let mut units = Vec::new();

        while let Some(token) = iter.peek().transpose()? {
            match token {
                Token::Unit(_) => units.push(Unit::short(&mut iter, units_lookup, false)?),
                Token::Slash => {
                    iter.next();
                    units.push(Unit::short(&mut iter, units_lookup, true)?);
                }
                _ => return Err(String::from("unexpected token")),
            }
        }

        let mut units_num = Vec::new();
        let mut units_denom = Vec::new();
        for unit in units {
            match &unit.exp {
                Some(exp) if exp.sign == Sign::Minus => units_denom.push(unit),
                _ => units_num.push(unit),
            }
        }

        Ok(Self {
            units_num,
            units_denom,
        })
    }
}

impl<'a> Unit<'a> {
    fn short<T: Iterator<Item = crate::Result<Token>>>(
        iter: &mut Peekable<T>,
        units_lookup: &'a UnitsLookup,
        per: bool,
    ) -> crate::Result<Self> {
        let text = match iter.next().transpose()? {
            Some(Token::Unit(unit)) => unit,
            _ => return Err(String::from("invalid token")),
        };

        let (prefix, unit) = if let Some(unit) = units_lookup.get_unit_short(&text) {
            (None, unit)
        } else {
            let mut prefix = None;
            let mut unit = None;

            for i in 0..text.len() {
                let prefix_cur = &text[..i];
                if let Some(prefix_cur) = units_lookup.get_prefix_short(prefix_cur) {
                    prefix = Some(prefix_cur);
                    match units_lookup.get_unit_short(&text[i..]) {
                        Some(unit_cur) => unit = Some(unit_cur),
                        None => return Err(String::from("invalid unit")),
                    }
                }
            }

            match (prefix, unit) {
                (Some(prefix), Some(unit)) => (Some(prefix), unit),
                _ => return Err(String::from("invalid unit")),
            }
        };

        let mut exp = match iter.peek().transpose()? {
            Some(Token::Circ) => {
                iter.next();
                let exp = Exponent::new(iter)?;
                Some(exp)
            }
            _ => None,
        };

        if per {
            match &mut exp {
                Some(exp) => match exp.sign {
                    Sign::Plus => exp.sign = Sign::Minus,
                    Sign::Minus => {
                        return Err(String::from("cannot combine / and negative exponents"));
                    }
                },
                None => {
                    exp = Some(Exponent {
                        sign: Sign::Minus,
                        num: String::from("1"),
                        denom: None,
                    })
                }
            }
        }

        Ok(Self { prefix, unit, exp })
    }
}

impl Exponent {
    fn new<T: Iterator<Item = crate::Result<Token>>>(
        iter: &mut Peekable<T>,
    ) -> crate::Result<Self> {
        Ok(match iter.next().transpose()? {
            Some(Token::Number(num)) => Self {
                sign: Sign::Plus,
                num,
                denom: None,
            },
            Some(Token::Sign(sign)) => {
                let num = match iter.next().transpose()? {
                    Some(Token::Number(num)) => num,
                    Some(_) => return Err(String::from("unexpected token in exponent")),
                    None => return Err(String::from("invalid exponent")),
                };

                Self {
                    sign,
                    num,
                    denom: None,
                }
            }
            Some(Token::ParenOpen) => {
                let sign = match iter.peek().transpose()? {
                    Some(Token::Sign(sign)) => {
                        let sign = *sign;
                        iter.next();
                        sign
                    }
                    _ => Sign::Plus,
                };

                let num = match iter.next().transpose()? {
                    Some(Token::Number(num)) => num,
                    Some(_) => return Err(String::from("unexpected token in exponent")),
                    None => return Err(String::from("invalid exponent")),
                };

                let denom = match iter.peek().transpose()? {
                    Some(Token::Slash) => {
                        iter.next();

                        match iter.next().transpose()? {
                            Some(Token::Number(num)) => Some(num),
                            Some(_) => return Err(String::from("unexpected token in exponent")),
                            None => return Err(String::from("invalid exponent")),
                        }
                    }
                    _ => None,
                };

                if !matches!(iter.next().transpose()?, Some(Token::ParenClose)) {
                    return Err(String::from("unexpected token in exponent"));
                }

                Self { sign, num, denom }
            }
            Some(_) => return Err(String::from("unexpected token in exponent")),
            None => return Err(String::from("invalid exponent")),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::unit::short::lexer::Tokenizer;

    use super::*;

    #[test]
    fn unit() {
        let text = String::from("kg^(2/3)/m^2");
        let tokenizer = Tokenizer::new(text.chars());
        let mut iter = tokenizer.peekable();
        let units_lookup = Default::default();
        let units = Units::short(&mut iter, &units_lookup);

        let kilo = units_lookup.get_prefix_short("k").unwrap();
        let gram = units_lookup.get_unit_short("g").unwrap();
        let meter = units_lookup.get_unit_short("m").unwrap();

        assert_eq!(
            units,
            Ok(Units {
                units_num: vec![Unit {
                    prefix: Some(kilo),
                    unit: gram,
                    exp: Some(Exponent {
                        sign: Sign::Plus,
                        num: String::from("2"),
                        denom: Some(String::from("3"))
                    })
                }],
                units_denom: vec![Unit {
                    prefix: None,
                    unit: meter,
                    exp: Some(Exponent {
                        sign: Sign::Minus,
                        num: String::from("2"),
                        denom: None
                    })
                }]
            })
        )
    }

    #[test]
    fn exponent() {
        let text = String::from("(-2/3)");
        let tokenizer = Tokenizer::new(text.chars());
        let mut iter = tokenizer.peekable();
        let exp = Exponent::new(&mut iter);

        assert_eq!(
            exp,
            Ok(Exponent {
                sign: Sign::Minus,
                num: String::from("2"),
                denom: Some(String::from("3")),
            })
        )
    }
}
