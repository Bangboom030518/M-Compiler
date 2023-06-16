use crate::{gen_rand_vec, prelude::*};
use rand::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Rand)]
pub enum Base {
    Binary,
    Octal,
    Denary,
    Hexadecimal,
}

impl Base {
    const BINARY_DIGITS: &[char] = &['0', '1'];
    const DENARY_DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
    const OCTAL_DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7'];
    const HEXADECIMAL_DIGITS: &[char] = &[
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
    ];
    const ACCEPTED_HEXADECIMAL_DIGITS: &[char] = &[
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B',
        'C', 'D', 'E', 'F',
    ];

    #[must_use]
    pub const fn as_number(self) -> usize {
        match self {
            Self::Binary => 2,
            Self::Octal => 8,
            Self::Denary => 10,
            Self::Hexadecimal => 16,
        }
    }

    #[must_use]
    pub const fn get_accepted_digits(self) -> &'static [char] {
        match self {
            Self::Hexadecimal => Self::ACCEPTED_HEXADECIMAL_DIGITS,
            _ => self.get_digits(),
        }
    }

    #[must_use]
    pub const fn get_digits(self) -> &'static [char] {
        match self {
            Self::Binary => Self::BINARY_DIGITS,
            Self::Denary => Self::DENARY_DIGITS,
            Self::Octal => Self::OCTAL_DIGITS,
            Self::Hexadecimal => Self::HEXADECIMAL_DIGITS,
        }
    }

    #[must_use]
    pub fn parse_digits(self, digits: Vec<usize>) -> Option<usize> {
        digits
            .into_iter()
            .rev()
            .enumerate()
            .fold(Some(0), |previous, (index, digit)| {
                previous.and_then(|previous| -> Option<usize> {
                    index
                        .try_into()
                        .ok()
                        .map(|exponent| previous + (self.as_number().pow(exponent) * digit))
                })
            })
    }

    #[must_use]
    pub const fn fmt_digit(self, digit: usize) -> char {
        self.get_digits()[digit]
    }

    #[must_use]
    pub fn fmt_digits(self, digits: &[usize]) -> String {
        digits.iter().map(|&digit| self.fmt_digit(digit)).collect()
    }

    pub fn rand_digits<G: Rng + ?Sized>(self, rng: &mut G) -> Vec<usize> {
        let digits = self.get_digits();
        gen_rand_vec(rng, |rng| {
            let digit_char = digits.choose(rng).unwrap();
            digits.iter().position(|digit| digit == digit_char).unwrap()
        })
    }
}

impl std::fmt::Display for Base {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Binary => write!(f, "0b"),
            Self::Octal => write!(f, "0o"),
            Self::Denary => write!(f, ""),
            Self::Hexadecimal => write!(f, "0x"),
        }
    }
}

impl NomParse for Base {
    fn parse(input: &str) -> IResult<Self> {
        alt((
            value(Self::Binary, tag("0b")),
            value(Self::Octal, tag("0o")),
            value(Self::Hexadecimal, tag("0x")),
            success(Self::Denary),
        ))(input)
    }
}

impl TryFrom<u8> for Base {
    type Error = u8;

    fn try_from(number: u8) -> Result<Self, u8> {
        match number {
            2 => Ok(Self::Binary),
            8 => Ok(Self::Octal),
            10 => Ok(Self::Denary),
            16 => Ok(Self::Hexadecimal),
            number => Err(number),
        }
    }
}

impl Default for Base {
    fn default() -> Self {
        Self::Denary
    }
}
