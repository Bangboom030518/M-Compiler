#[derive(Debug)]
pub enum Number {
    Integer(Integer),
    Fractional(Fractional),
}

#[derive(Debug)]
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

    pub const fn get_digits(&self) -> &[char] {
        match self {
            Self::Binary => Self::BINARY_DIGITS,
            Self::Denary => Self::DENARY_DIGITS,
            Self::Octal => Self::OCTAL_DIGITS,
            Self::Hexadecimal => Self::HEXADECIMAL_DIGITS,
        }
    }

    pub fn parse_digits(&self, digits: Vec<usize>) -> Option<usize> {
        digits
            .into_iter()
            .rev()
            .enumerate()
            .fold(Some(0), |previous, (index, digit)| {
                previous.and_then(|previous| {
                    let exponent: Result<u32, _> = index.try_into();
                    if let Ok(exponent) = exponent {
                        Some(previous + ((*self as usize).pow(exponent) * digit))
                    } else {
                        None
                    }
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

#[derive(Debug)]
pub struct Integer {
    pub digits: Vec<usize>,
    pub base: Base,
    pub sign: Sign,
}

#[derive(Debug)]
pub struct Fractional {
    pub sign: Sign,
    pub whole_digits: Vec<usize>,
    pub fractional_digits: Vec<usize>,
    pub base: Base,
}

#[derive(Debug)]
pub enum Sign {
    Negative,
    Positive,
}
