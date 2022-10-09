use span::Span;
use span_derive::Span;

#[derive(Debug, Clone, Span)]
pub enum Number {
    Integer(Integer),
    Fractional(Fractional),
}

#[derive(Debug, Clone, Span)]
pub struct Integer {
    pub digits: Vec<usize>,
    pub base: Base,
    pub sign: Sign,
    pub span: Span,
}

#[derive(Debug, Clone, Span)]
pub struct Fractional {
    pub sign: Sign,
    pub whole_digits: Vec<usize>,
    pub fractional_digits: Vec<usize>,
    pub base: Base,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Base {
    Binary = 2,
    Octal = 8,
    Denary = 10,
    Hexadecimal = 16,
}

impl Base {
    const BINARY_DIGITS: &[char] = &['0', '1'];
    const DENARY_DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
    const OCTAL_DIGITS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7'];
    const HEXADECIMAL_DIGITS: &[char] = &[
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
    ];

    #[must_use]
    pub const fn get_digits(&self) -> &[char] {
        match self {
            Self::Binary => Self::BINARY_DIGITS,
            Self::Denary => Self::DENARY_DIGITS,
            Self::Octal => Self::OCTAL_DIGITS,
            Self::Hexadecimal => Self::HEXADECIMAL_DIGITS,
        }
    }

    #[must_use]
    pub fn parse_digits(&self, digits: Vec<usize>) -> Option<usize> {
        digits
            .into_iter()
            .rev()
            .enumerate()
            .fold(Some(0), |previous, (index, digit)| {
                previous.and_then(|previous| -> Option<usize> {
                    index
                        .try_into()
                        .ok()
                        .map(|exponent| previous + ((*self as usize).pow(exponent) * digit))
                })
            })
    }
}

impl std::fmt::Display for Base {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}", format!("{:?}", self).to_lowercase())
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

#[derive(Debug, Clone)]
pub enum Sign {
    Negative,
    Positive,
}
