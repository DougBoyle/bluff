use std::fmt::{Debug, Display};

use enum_map::Enum;

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Enum, Debug)]
pub enum Rank {
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace, // For ordering, Ace is high
}

impl Rank {
    pub fn previous(&self) -> Rank {
        match self {
            Rank::Two => Rank::Ace,
            Rank::Three => Rank::Two,
            Rank::Four => Rank::Three,
            Rank::Five => Rank::Four,
            Rank::Six => Rank::Five,
            Rank::Seven => Rank::Six,
            Rank::Eight => Rank::Seven,
            Rank::Nine => Rank::Eight,
            Rank::Ten => Rank::Nine,
            Rank::Jack => Rank::Ten,
            Rank::Queen => Rank::Jack,
            Rank::King => Rank::Queen,
            Rank::Ace => Rank::King,
        }
    }

    pub fn high_to_low() -> [Rank; 13] {
        [Rank::Ace, Rank::King, Rank::Queen, Rank::Jack, Rank::Ten, Rank::Nine, Rank::Eight, Rank::Seven,
            Rank::Six, Rank::Five, Rank::Four, Rank::Three, Rank::Two]
    }
}

impl Display for Rank {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Rank::Two => "2",
            Rank::Three => "3",
            Rank::Four => "4",
            Rank::Five => "5",
            Rank::Six => "6",
            Rank::Seven => "7",
            Rank::Eight => "8",
            Rank::Nine => "9",
            Rank::Ten => "10",
            Rank::Jack => "J",
            Rank::Queen => "Q",
            Rank::King => "K",
            Rank::Ace => "A",
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Enum, Debug)]
pub enum Suit {
    Clubs,
    Diamonds,
    Hearts,
    Spades,
}

impl Suit {
    fn all() -> [Suit; 4] {
        [Suit::Clubs, Suit::Diamonds, Suit::Hearts, Suit::Spades]
    }
}

impl Display for Suit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Suit::Clubs => "♣",
            Suit::Diamonds => "♢",
            Suit::Hearts => "♡",
            Suit::Spades => "♠",
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Card {
    pub rank: Rank,
    pub suit: Suit,
}

impl Card {
    pub fn all() -> [Card; 52] {
        Suit::all().into_iter()
            .flat_map(|suit| Rank::high_to_low().map(|rank| Card { suit, rank }))
            .collect::<Vec<_>>()
            .try_into().unwrap()
    }
}

impl Display for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}{}]", self.rank, self.suit)
    }
}

impl Debug for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}