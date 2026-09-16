use std::iter::Peekable;

use crate::num::Sign;
use crate::result::TransposeRef;
use crate::unit::Units as UnitsLookup;
use crate::unit::long::lexer::Token;
use crate::unit::parser::*;

impl<'a> Units<'a> {
    pub fn long<T: IntoIterator<Item = crate::Result<Token>>>(
        iter: T,
        units_lookup: &'a UnitsLookup,
    ) -> crate::Result<Self> {
        let mut iter = iter.into_iter().peekable();
        let mut units = Vec::new();

        while let Some(token) = iter.peek().transpose()? {
            match token {
                Token::Unit(_) => units.push(Unit::long(&mut iter, units_lookup, false)?),
                Token::Per => {
                    iter.next();
                    units.push(Unit::long(&mut iter, units_lookup, true)?);
                }
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

        let units = Self {
            units_num,
            units_denom,
        };
        units.validate()?;

        Ok(units)
    }
}

impl<'a> Unit<'a> {
    fn long<T: Iterator<Item = crate::Result<Token>>>(
        iter: &mut Peekable<T>,
        units_lookup: &'a UnitsLookup,
        per: bool,
    ) -> crate::Result<Self> {
        let text = match iter.next().transpose()? {
            Some(Token::Unit(text)) => text,
            Some(t) => return Err(format!("unexpected token '{t}'")),
            _ => return Err(String::from("invalid token")),
        };

        let prefix = units_lookup.get_prefix(&text);
        let unit = match prefix {
            Some(_) => {
                let text = match iter.next().transpose()? {
                    Some(Token::Unit(text)) => text,
                    Some(t) => return Err(format!("unexpected token '{t}'")),
                    _ => return Err(String::from("invalid token")),
                };
                units_lookup
                    .get_unit(&text)
                    .ok_or(format!("invalid unit '{text}'"))?
            }
            None => units_lookup
                .get_unit(&text)
                .ok_or(format!("invalid unit '{text}'"))?,
        };
        let postfix = match iter.peek().transpose()? {
            Some(Token::Unit(text)) => {
                let postfix = units_lookup.get_postfix(text);
                if postfix.is_some() {
                    iter.next();
                }
                postfix
            }
            _ => None,
        };
        let mut exp = postfix.map(|p| Exponent {
            sign: Sign::Plus,
            num: p.to_owned(),
            denom: None,
        });

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

        Ok(Self {
            prefix,
            unit,
            exp,
            sqrt: false,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::unit::long::lexer::Tokenizer;

    use super::*;

    #[test]
    fn unit() {
        let text = String::from("kilo gram per meter squared");
        let tokenizer = Tokenizer::new(text.chars());
        let mut iter = tokenizer.peekable();
        let units_lookup = Default::default();
        let units = Units::long(&mut iter, &units_lookup);

        let kilo = units_lookup.get_prefix_short("k").unwrap();
        let gram = units_lookup.get_unit_short("g").unwrap();
        let meter = units_lookup.get_unit_short("m").unwrap();

        assert_eq!(
            units,
            Ok(Units {
                units_num: vec![Unit {
                    prefix: Some(kilo),
                    unit: gram,
                    exp: None,
                    sqrt: false
                }],
                units_denom: vec![Unit {
                    prefix: None,
                    unit: meter,
                    exp: Some(Exponent {
                        sign: Sign::Minus,
                        num: String::from("2"),
                        denom: None,
                    }),
                    sqrt: false
                }]
            })
        )
    }
}
