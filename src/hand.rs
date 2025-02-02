use std::{cmp::Ordering, iter::zip};

use enum_map::EnumMap;

use crate::card::{Card, Rank, Suit};

/// Ties are broken first by the rank of cards making up the combination below, and then the rank of
/// any other cards included to make up 5 cards.
#[derive(Debug, Eq, PartialEq)]
enum HandRank {
    HighCard(Rank),
    Pair(Rank),
    TwoPair { high_rank: Rank, low_rank: Rank },
    ThreeOfAKind(Rank),
    Straight { high_rank: Rank },
    Flush { ordered_ranks: [Rank; 5] },
    FullHouse { triple_rank: Rank, pair_rank: Rank },
    FourOfAKind(Rank),
    StraightFlush { high_rank: Rank },
}

impl Ord for HandRank {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            // Matching ranks
            (HandRank::StraightFlush { high_rank: first_rank }, HandRank::StraightFlush { high_rank: second_rank })
            | (HandRank::FourOfAKind(first_rank), HandRank::FourOfAKind(second_rank))
            | (HandRank::Straight { high_rank: first_rank }, HandRank::Straight { high_rank: second_rank })
            | (HandRank::ThreeOfAKind(first_rank), HandRank::ThreeOfAKind(second_rank))
            | (HandRank::Pair(first_rank), HandRank::Pair(second_rank))
            | (HandRank::HighCard(first_rank), HandRank::HighCard(second_rank))
                => first_rank.cmp(second_rank),
            
            (HandRank::FullHouse { triple_rank: first_triple, pair_rank: first_pair },
                HandRank::FullHouse { triple_rank: second_triple, pair_rank: second_pair })
                => {
                    let result = first_triple.cmp(second_triple);
                    if result != Ordering::Equal { result } else { first_pair.cmp(second_pair) }
                },
            
            (HandRank::Flush { ordered_ranks: first_ranks }, HandRank::Flush { ordered_ranks: second_ranks })
                => compare_flushes(first_ranks, second_ranks),
            
            (HandRank::TwoPair { high_rank: first_high, low_rank: first_low },
                HandRank::TwoPair { high_rank: second_high, low_rank: second_low })
                => {
                    let result: Ordering = first_high.cmp(second_high);
                    if result != Ordering::Equal { result } else { first_low.cmp(second_low) }
                },

            // Different ranks
            (HandRank::StraightFlush { .. }, _) => Ordering::Greater,
            (_, HandRank::StraightFlush { .. }) => Ordering::Less,

            (HandRank::FourOfAKind(_), _) => Ordering::Greater,
            (_, HandRank::FourOfAKind(_)) => Ordering::Less,
            
            (HandRank::FullHouse { .. }, _) => Ordering::Greater,
            (_, HandRank::FullHouse { .. }) => Ordering::Less,
            
            (HandRank::Flush { .. }, _) => Ordering::Greater,
            (_, HandRank::Flush { .. }) => Ordering::Less,

            (HandRank::Straight { .. }, _) => Ordering::Greater,
            (_, HandRank::Straight { .. }) => Ordering::Less,

            (HandRank::ThreeOfAKind(_), _) => Ordering::Greater,
            (_, HandRank::ThreeOfAKind(_)) => Ordering::Less,

            (HandRank::TwoPair { .. }, _) => Ordering::Greater,
            (_, HandRank::TwoPair { .. }) => Ordering::Less,
            
            (HandRank::Pair(_), _) => Ordering::Greater,
            (_, HandRank::Pair(_)) => Ordering::Less,
        }
    }
}

impl PartialOrd for HandRank {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FiveCardHand {
    cards: [Card; 5],
    rank: HandRank,
    descending_rank_other_cards: Vec<Card>,
}

impl Ord for FiveCardHand {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.rank.cmp(&other.rank) {
            Ordering::Equal => {},
            ord => { return ord; },
        }
        assert_eq!(self.descending_rank_other_cards.len(), other.descending_rank_other_cards.len());
        zip(self.descending_rank_other_cards.iter(), other.descending_rank_other_cards.iter())
            .map(|(first, second)| first.rank.cmp(&second.rank))
            .filter(|&cmp| cmp != Ordering::Equal)
            .next()
            .unwrap_or(Ordering::Equal)
    }
}

impl PartialOrd for FiveCardHand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub fn rank_hand(cards: Vec<Card>) -> FiveCardHand {
    assert!(cards.len() >= 5);

    // TODO: Might be easier just to build per-rank and per-suit structures first, rather than in each check?
    //       e.g. 4 of a kind, 3 of a kind, full house, two pair, pair - all very similar checks
    let mut descending_rank_cards = cards.clone();
    descending_rank_cards.sort_by_key(|card| card.rank);
    descending_rank_cards.reverse();


    if let Some(hand) = try_get_straight_flush(&descending_rank_cards) { return hand };
    if let Some(hand) = try_get_four_of_a_kind(&descending_rank_cards) { return hand };
    if let Some(hand) = try_get_full_house(&descending_rank_cards) { return hand };
    if let Some(hand) = try_get_flush(&descending_rank_cards) { return hand };
    if let Some(hand) = try_get_straight(&descending_rank_cards) { return hand };
    if let Some(hand) = try_get_three_of_a_kind(&descending_rank_cards) { return hand };
    if let Some(hand) = try_get_two_pair(&descending_rank_cards) { return hand };
    if let Some(hand) = try_get_pair(&descending_rank_cards) { return hand };
    get_high_card(&descending_rank_cards)
}

fn try_get_straight_flush(descending_rank_cards: &Vec<Card>) -> Option<FiveCardHand> {
    // Split by suit, then look for a straight in each
    let mut cards_by_suit: EnumMap<Suit, Vec<Card>> = EnumMap::default();
    for card in descending_rank_cards {
        cards_by_suit[card.suit].push(*card);
    }
    cards_by_suit.into_values()
        .filter(|cards| cards.len() >= 5)
        .filter_map(|cards| try_get_straight(&cards))
        // TODO: Split out a simpler method than try_get_straight to avoid matching on HandRank below
        .max_by_key(|hand| {
            match hand {
                FiveCardHand { rank: HandRank::Straight { high_rank }, .. } => *high_rank,
                _ => panic!("try_get_straight returned a hand other than a straight!"),
            }
        })
        .map(|hand| match hand {
            FiveCardHand { cards, rank: HandRank::Straight { high_rank }, .. } =>
                FiveCardHand { cards, rank: HandRank::StraightFlush { high_rank }, descending_rank_other_cards: vec![] },
            _ => panic!("try_get_straight returned a hand other than a straight!")
        })
}

fn try_get_four_of_a_kind(descending_rank_cards: &Vec<Card>) -> Option<FiveCardHand> {
    let cards_by_rank = group_by_rank(descending_rank_cards);
    for quad_rank in Rank::high_to_low() {
        if cards_by_rank[quad_rank].len() < 4 { continue; }

        let mut cards: Vec<Card> = cards_by_rank[quad_rank][..4].iter().copied().collect();
        cards.extend(descending_rank_cards.iter().filter(|card| card.rank != quad_rank).take(1));
        let cards = cards.try_into().unwrap();

        return Some(FiveCardHand { cards, rank: HandRank::FourOfAKind(quad_rank), descending_rank_other_cards: Vec::from(&cards[4..]) });
    }
    None
}

fn try_get_full_house(descending_rank_cards: &Vec<Card>) -> Option<FiveCardHand> {
    let cards_by_rank = group_by_rank(descending_rank_cards);
    for triple_rank in Rank::high_to_low() {
        if cards_by_rank[triple_rank].len() < 3 { continue; }
        for pair_rank in Rank::high_to_low() {
            if pair_rank == triple_rank { continue; }
            if cards_by_rank[pair_rank].len() < 2 { continue; }

            let mut cards: Vec<Card> = cards_by_rank[triple_rank][..3].iter().copied().collect();
            cards.extend_from_slice(&cards_by_rank[pair_rank][..2]);
            let cards = cards.try_into().unwrap();

            return Some(FiveCardHand { cards, rank: HandRank::FullHouse { triple_rank, pair_rank }, descending_rank_other_cards: vec![] });
        }
    }
    None
}

fn try_get_flush(descending_rank_cards: &Vec<Card>) -> Option<FiveCardHand> {
    let mut cards_by_suit: EnumMap<Suit, Vec<Rank>> = EnumMap::default();
    for card in descending_rank_cards {
        cards_by_suit[card.suit].push(card.rank);
    }
    cards_by_suit.into_iter()
        .filter(|(_, ranks)| ranks.len() >= 5)
        .map(|(suit, ranks)| (suit, ranks[..5].try_into().expect("slice did not contain 5 elements")))
        .max_by(|(_, first_ranks), (_, second_ranks)| compare_flushes(first_ranks, second_ranks))
        .map(|(suit, ranks)| {
            let cards = ranks.clone().map(|rank| Card { suit, rank });
            FiveCardHand { cards, rank: HandRank::Flush { ordered_ranks: ranks }, descending_rank_other_cards: vec![] }
        })
}

fn compare_flushes(first_descending_ranks: &[Rank; 5], second_descending_ranks: &[Rank; 5]) -> Ordering {
    (0..5).map(|i| first_descending_ranks[i].cmp(&second_descending_ranks[i]))
        .filter(|&cmp| cmp != Ordering::Equal)
        .next()
        .unwrap_or(Ordering::Equal)
}

fn try_get_straight(descending_rank_cards: &Vec<Card>) -> Option<FiveCardHand> {
    let mut unique_ranks = EnumMap::default();
    for card in descending_rank_cards {
        unique_ranks[card.rank] = Some(card);
    }

    // Ensures we don't consider a straight that would wrap around
    'high: for high_rank in [Rank::Ace, Rank::King, Rank::Queen, Rank::Jack, Rank::Ten, Rank::Nine, Rank::Eight,
        Rank::Seven, Rank::Six, Rank::Five, Rank::Five] {
        let mut cards = vec![];
        let mut current = high_rank;
        for _ in 0..5 {
            match unique_ranks[current] {
                Some(card) => cards.push(*card),
                None => continue 'high, 
            }
            current = current.previous();
        }
        let cards = cards.try_into().unwrap();
        return Some(FiveCardHand { cards, rank: HandRank::Straight { high_rank }, descending_rank_other_cards: vec![] });
    }
    None
}

fn try_get_three_of_a_kind(descending_rank_cards: &Vec<Card>) -> Option<FiveCardHand> {
    let cards_by_rank = group_by_rank(descending_rank_cards);
    for triple_rank in Rank::high_to_low() {
        if cards_by_rank[triple_rank].len() < 3 { continue; }

        let mut cards: Vec<Card> = cards_by_rank[triple_rank][..3].iter().copied().collect();
        cards.extend(descending_rank_cards.iter().filter(|card| card.rank != triple_rank).take(2));
        let cards = cards.try_into().unwrap();

        return Some(FiveCardHand { cards, rank: HandRank::ThreeOfAKind(triple_rank), descending_rank_other_cards: Vec::from(&cards[3..]) });
    }
    None
}

fn try_get_two_pair(descending_rank_cards: &Vec<Card>) -> Option<FiveCardHand> {
    let cards_by_rank = group_by_rank(descending_rank_cards);
    for high_rank in Rank::high_to_low() {
        if cards_by_rank[high_rank].len() < 2 { continue; }
        for low_rank in Rank::high_to_low() {
            if low_rank == high_rank { continue; }
            if cards_by_rank[low_rank].len() < 2 { continue; }

            let other_card = *descending_rank_cards.iter()
                .filter(|card| card.rank != high_rank && card.rank != low_rank)
                .next()
                .expect("Could not find 5th card for two pair");

            let mut cards: Vec<Card> = cards_by_rank[high_rank][..2].iter().copied().collect();
            cards.extend_from_slice(&cards_by_rank[low_rank][..2]);
            cards.push(other_card);
            let cards = cards.try_into().unwrap();

            return Some(FiveCardHand { cards, rank: HandRank::TwoPair { high_rank, low_rank }, descending_rank_other_cards: vec![other_card] });
        }
    }
    None
}

fn try_get_pair(descending_rank_cards: &Vec<Card>) -> Option<FiveCardHand> {
    let cards_by_rank = group_by_rank(descending_rank_cards);
    for pair_rank in Rank::high_to_low() {
        if cards_by_rank[pair_rank].len() < 2 { continue; }

        let mut cards: Vec<Card> = cards_by_rank[pair_rank][..2].iter().copied().collect();
        cards.extend(descending_rank_cards.iter().cloned().filter(|card| card.rank != pair_rank).take(3));
        let cards = cards.try_into().unwrap();

        return Some(FiveCardHand { cards, rank: HandRank::Pair(pair_rank), descending_rank_other_cards: Vec::from(&cards[2..]) });
    }
    None
}

fn get_high_card(descending_rank_cards: &Vec<Card>) -> FiveCardHand {
    let high_card = descending_rank_cards[0];
    let cards = descending_rank_cards[..5].try_into().expect("Found fewer than 5 cards");
    FiveCardHand { cards, rank: HandRank::HighCard(high_card.rank), descending_rank_other_cards: Vec::from(&descending_rank_cards[1..]) }
}

fn group_by_rank(cards: &Vec<Card>) -> EnumMap<Rank, Vec<Card>> {
    let mut cards_by_rank: EnumMap<Rank, Vec<Card>> = EnumMap::default();
    for card in cards {
        cards_by_rank[card.rank].push(*card);
    }
    cards_by_rank
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::*;

    #[test]
    fn test_high_card() {
        let cards = vec![
            Card { rank: Rank::Eight, suit: Suit::Clubs },
            Card { rank: Rank::Two, suit: Suit::Spades },
            Card { rank: Rank::King, suit: Suit::Spades },
            Card { rank: Rank::Queen, suit: Suit::Hearts },
            Card { rank: Rank::Seven, suit: Suit::Diamonds },
            Card { rank: Rank::Four, suit: Suit::Clubs },
        ];

        let hand = rank_hand(cards);

        assert_matches!(hand, FiveCardHand { rank: HandRank::HighCard(Rank::King), .. });
    }

    #[test]
    fn test_full_house() {
        let cards = vec![
            Card { rank: Rank::Eight, suit: Suit::Clubs },
            Card { rank: Rank::Eight, suit: Suit::Spades },
            Card { rank: Rank::Eight, suit: Suit::Spades },
            Card { rank: Rank::Queen, suit: Suit::Hearts },
            Card { rank: Rank::Queen, suit: Suit::Diamonds },
            Card { rank: Rank::Queen, suit: Suit::Clubs },
        ];

        let hand = rank_hand(cards);

        assert_matches!(hand, FiveCardHand { rank: HandRank::FullHouse { triple_rank: Rank::Queen, pair_rank: Rank::Eight }, .. });
    }
}