
#![feature(assert_matches)] // Simpler tests

mod card {
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
}

mod hand {
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
        // TODO: De-dupe some cases / do all equality cases first?
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
        for start in 0..(descending_rank_cards.len() - 3) {
            if descending_rank_cards[start] == descending_rank_cards[start + 3] {
                let other_card = if start == 0 { descending_rank_cards[start + 4] } else { descending_rank_cards[0] };
                
                let mut cards = [Card { rank: Rank::Ace, suit: Suit::Spades}; 5]; // placeholder initial value
                cards[..4].copy_from_slice(&descending_rank_cards[start..start+4]);
                cards[4] = other_card;

                return Some(FiveCardHand {
                    cards, rank: HandRank::FourOfAKind(cards[0].rank), descending_rank_other_cards: vec![other_card]
                });
            }
        }
        None
    }

    fn try_get_full_house(descending_rank_cards: &Vec<Card>) -> Option<FiveCardHand> {
        let mut cards_by_rank: EnumMap<Rank, Vec<Card>> = EnumMap::default();
        for card in descending_rank_cards {
            cards_by_rank[card.rank].push(*card);
        }

        for triple_rank in Rank::high_to_low() {
            if cards_by_rank[triple_rank].len() < 3 { continue; }
            for pair_rank in Rank::high_to_low() {
                if pair_rank == triple_rank { continue; }
                if cards_by_rank[pair_rank].len() < 2 { continue; }

                let mut cards = [Card { rank: Rank::Ace, suit: Suit::Spades}; 5]; // placeholder initial value
                cards[..3].copy_from_slice(&cards_by_rank[triple_rank][..3]);
                cards[3..].copy_from_slice(&cards_by_rank[pair_rank][..2]);

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
        let mut cards = [Card { rank: Rank::Ace, suit: Suit::Spades}; 5]; // placeholder initial value
        'high: for high_rank in [Rank::Ace, Rank::King, Rank::Queen, Rank::Jack, Rank::Ten, Rank::Nine, Rank::Eight,
            Rank::Seven, Rank::Six, Rank::Five, Rank::Five] {
            let mut current = high_rank;
            for i in 0..5 {
                match unique_ranks[current] {
                    Some(card) => cards[i] = *card,
                    None => continue 'high, 
                }
                current = current.previous();
            }
            return Some(FiveCardHand { cards, rank: HandRank::Straight { high_rank }, descending_rank_other_cards: vec![] });
        }
        None
    }

    fn try_get_three_of_a_kind(descending_rank_cards: &Vec<Card>) -> Option<FiveCardHand> {
        let mut cards_by_rank: EnumMap<Rank, Vec<Card>> = EnumMap::default();
        for card in descending_rank_cards {
            cards_by_rank[card.rank].push(*card);
        }

        for triple_rank in Rank::high_to_low() {
            if cards_by_rank[triple_rank].len() < 3 { continue; }

            let mut cards = [Card { rank: Rank::Ace, suit: Suit::Spades}; 5]; // placeholder initial value
            cards[..3].copy_from_slice(&cards_by_rank[triple_rank][..3]);
            cards[3..].copy_from_slice(
                &descending_rank_cards.iter()
                    .cloned()
                    .filter(|card| card.rank != triple_rank)
                    .take(2)
                    .collect::<Vec<_>>()
            );

            return Some(FiveCardHand { cards, rank: HandRank::ThreeOfAKind(triple_rank), descending_rank_other_cards: Vec::from(&cards[3..]) });
        }
        None
    }

    fn try_get_two_pair(descending_rank_cards: &Vec<Card>) -> Option<FiveCardHand> {
        let mut cards_by_rank: EnumMap<Rank, Vec<Card>> = EnumMap::default();
        for card in descending_rank_cards {
            cards_by_rank[card.rank].push(*card);
        }

        for high_rank in Rank::high_to_low() {
            if cards_by_rank[high_rank].len() < 2 { continue; }
            for low_rank in Rank::high_to_low() {
                if low_rank == high_rank { continue; }
                if cards_by_rank[low_rank].len() < 2 { continue; }

                let other_card = *descending_rank_cards.iter()
                    .filter(|card| card.rank != high_rank && card.rank != low_rank)
                    .next()
                    .expect("Could not find 5th card for two pair");

                let mut cards = [Card { rank: Rank::Ace, suit: Suit::Spades}; 5]; // placeholder initial value
                cards[..2].copy_from_slice(&cards_by_rank[high_rank][..2]);
                cards[2..4].copy_from_slice(&cards_by_rank[low_rank][..2]);
                cards[4] = other_card;

                return Some(FiveCardHand { cards, rank: HandRank::TwoPair { high_rank, low_rank }, descending_rank_other_cards: vec![other_card] });
            }
        }
        None
    }

    fn try_get_pair(descending_rank_cards: &Vec<Card>) -> Option<FiveCardHand> {
        let mut cards_by_rank: EnumMap<Rank, Vec<Card>> = EnumMap::default();
        for card in descending_rank_cards {
            cards_by_rank[card.rank].push(*card);
        }

        for pair_rank in Rank::high_to_low() {
            if cards_by_rank[pair_rank].len() < 2 { continue; }

            let mut cards = [Card { rank: Rank::Ace, suit: Suit::Spades}; 5]; // placeholder initial value
            cards[..2].copy_from_slice(&cards_by_rank[pair_rank][..2]);
            cards[2..].copy_from_slice(
                &descending_rank_cards.iter()
                    .cloned()
                    .filter(|card| card.rank != pair_rank)
                    .take(3)
                    .collect::<Vec<_>>()
            );

            return Some(FiveCardHand { cards, rank: HandRank::Pair(pair_rank), descending_rank_other_cards: Vec::from(&cards[2..]) });
        }
        None
    }

    fn get_high_card(descending_rank_cards: &Vec<Card>) -> FiveCardHand {
        let high_card = descending_rank_cards[0];
        let cards = descending_rank_cards[..5].try_into().expect("Found fewer than 5 cards");
        FiveCardHand { cards, rank: HandRank::HighCard(high_card.rank), descending_rank_other_cards: Vec::from(&descending_rank_cards[1..]) }
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
}

pub mod game {
    use std::{cmp::Ordering, fmt::Debug};

    use rand::{rngs::ThreadRng, seq::SliceRandom, Rng};

    use crate::{card::Card, hand::{rank_hand, FiveCardHand}};

    #[derive(Debug)]
    pub struct GameCards {
        player_cards: Vec<[Card; 2]>,
        community_cards: [Card; 5],
    }

    impl GameCards {
        pub fn new_random(players: usize) -> Option<GameCards> {
            Self::new(players, rand::rng())
        }

        pub fn new(players: usize, mut rng: impl Rng) -> Option<GameCards> {
            if players < 2 || players > 10 {
                None // theoretic limit is (52-5)/2 = 23, but betting would be ridiculous.
            } else {
                let mut deck = Card::all();
                deck.shuffle(&mut rng);
                let players = players as usize;
                let player_cards = (0..players)
                    .map(|i| deck[2*i..2*(i+1)].try_into().unwrap())
                    .collect::<Vec<[Card; 2]>>();
                let community_cards = deck[2*players..2*players+5].try_into().unwrap();
                Some(GameCards { player_cards, community_cards })
            }
        }

        pub fn get_hand(&self, player: usize) -> FiveCardHand {
            let mut cards = Vec::from(self.community_cards);
            cards.extend_from_slice(&self.player_cards[player]);
            rank_hand(cards)
        }
    }

    #[derive(Debug)]
    struct Pot {
        current_raise: usize, // amount bet per-player this phase, mainly to tell UI
        current_player_total: usize, // cumulative total across phases, matching chips_by_player
        chips_by_player: Vec<usize>,
        total: usize,
    }

    impl Pot {
        fn new(players: usize, small_blind: usize) -> Pot {
            Pot { current_raise: small_blind, current_player_total: small_blind, chips_by_player: vec![0; players], total: 0 }
        }

        fn inc_bet(&mut self, player: usize, bet: usize) {
            self.chips_by_player[player] += bet;
            self.total += bet;
        }

        fn inc_current_raise(&mut self, inc: usize) {
            self.current_raise += inc;
            self.current_player_total += inc;
        }

        fn reset_current_raise(&mut self) {
            self.current_raise = 0;
        }
    }

    #[derive(Debug, Eq, PartialEq)]
    enum GamePhase {
        SmallBlind,
        BigBlind,
        PreFlop,
        Flop,
        Turn,
        River,
        Finished,
    }

    type Output = Box<dyn FnMut(String)>; // TODO: Probably want something more concrete here

    pub enum Action {
        SmallBlind, // Potentially going All-In
        BigBlind, // Potentially going All-In
        Fold,
        CheckOrCall, // For call, includes going All-In
        Raise(usize), // TODO: Tick size?
    }

    #[derive(Debug)]
    struct Round {
        pot: Pot,
        cards: GameCards,
        phase: GamePhase,
        phase_ends_on_player: usize, // After big blind initially, the after dealer on later rounds, else the last player to raise
        next_player: usize, // the player we are currently waiting on
        folded_players: Vec<bool>,
    }

    impl Round {
        pub fn new(
            players: usize,
            small_blind: usize,
            small_blind_player: usize,
            after_big_blind_player: usize,
            rng: &mut impl Rng
        ) -> Round {
            Round {
                pot: Pot::new(players, small_blind),
                phase_ends_on_player: after_big_blind_player,
                next_player: small_blind_player,
                phase: GamePhase::SmallBlind,
                cards: GameCards::new(players, rng).unwrap(),
                folded_players: vec![false; players],
            }
        }
    }

    // TODO: How to handle players going all-in/running out of chips?
    pub struct Game<R: Rng> {
        player_chips: Vec<usize>,
        small_blind: usize, // TODO: Increase over time?
        dealer: usize,
        round: Round,
        rng: R,
        output: Output,
    }

    impl<R: Rng> Debug for Game<R> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("Game")
                .field("player_chips", &self.player_chips)
                .field("small_blind", &self.small_blind)
                .field("dealer", &self.dealer)
                .field("round", &self.round)
                .finish()
        }
    }

    impl Game<ThreadRng> {
        pub fn new_random(players: usize, initial_chips: usize, small_blind: usize, output: Output) -> Self {
            let rng = rand::rng();
            Game::new(players, initial_chips, small_blind, rng, output)
        }
    }

    impl<R: Rng> Game<R> {
        pub fn new(players: usize, initial_chips: usize, small_blind: usize, mut rng: R, output: Output) -> Game<R> {
            let dealer = 0; // TODO: Vary this?
            let small_blind_player = 1;
            let after_big_blind = 3 % players;
            let mut game = Game {
                player_chips: vec![initial_chips; players],
                small_blind,
                dealer,
                round: Round::new(players, small_blind, small_blind_player, after_big_blind, &mut rng),
                rng,
                output
            };

            game.play_initial_blinds();

            game
        }

        // TODO: In event both players end up all-in, this can automatically play through the whole game.
        //       Better to queue up follow up actions in some sort of PendingAction state? Then top-level can check that.
        fn play_initial_blinds(&mut self) {
            for (i, cards) in self.round.cards.player_cards.iter().enumerate() {
                (self.output)(format!("Player {i}: {}, {}", cards[0], cards[1]));
            }
            self.handle_action(Action::SmallBlind, self.round.next_player);
            self.handle_action(Action::BigBlind, self.round.next_player);
        }

        pub fn handle_action(&mut self, action: Action, player: usize) {
            assert!(player == self.round.next_player);
            assert!(self.round.phase != GamePhase::Finished);
            match action {
                // TODO: Need some output to indicate actions being played.
                //       At a minimum, 'Small/Big blind',
                //       ideally also chips played / if all in.
                Action::SmallBlind => {
                    assert_eq!(GamePhase::SmallBlind, self.round.phase);
                    let bet = self.round.pot.current_player_total.min(self.player_chips[player]);
                    (self.output)(format!("Small blind: Player {player} bet {bet}"));
                    assert!(bet > 0);
                    self.inc_bet(player, bet);
                    self.handle_end_of_phase();
                },
                Action::BigBlind => {
                    assert_eq!(GamePhase::BigBlind, self.round.phase);
                    let bet = self.round.pot.current_player_total.min(self.player_chips[player]);
                    assert!(bet > 0);
                    (self.output)(format!("Big blind: Player {player} bet {bet}"));
                    self.inc_bet(player, bet);
                    self.handle_end_of_phase();
                },
                Action::CheckOrCall => {
                    assert!(self.round.phase != GamePhase::SmallBlind && self.round.phase != GamePhase::BigBlind);
                    let bet = self.round.pot.current_player_total - self.round.pot.chips_by_player[player];
                    let bet = bet.min(self.player_chips[player]);
                    if bet > 0 {
                        self.inc_bet(player, bet);
                        (self.output)(format!("Player {player} called, bet {bet}"));
                    } else {
                        (self.output)(format!("Player {player} checked"));
                    }

                    // TODO: If only one player left, doesn't make sense to give them a turn!
                    // TODO: Need to de-dupe some of this next/player_after code.
                    let mut next = (player + 1) % self.player_chips.len();
                    while (self.player_chips[next] == 0 || self.round.folded_players[next])
                        && next != self.round.phase_ends_on_player {
                        next = (next + 1) % self.player_chips.len();
                    }
                    if next == self.round.phase_ends_on_player {
                        self.handle_end_of_phase();
                        return;
                    }

                    let mut player_after = (next + 1) % self.player_chips.len();
                    while (self.player_chips[player_after] == 0 || self.round.folded_players[next]) && player_after != next {
                        player_after = (player_after + 1) % self.player_chips.len();
                    }
                    if player_after == next {
                        self.handle_end_of_phase();
                        return;
                    }
                    self.round.next_player = next;
                },
                Action::Raise(chips) => {
                    assert!(chips > self.round.pot.current_raise);
                    let player_additional_chips = chips - self.round.pot.chips_by_player[player];
                    // TODO: Tick size
                    assert!(player_additional_chips <= self.player_chips[player]);
                    self.inc_bet(player, player_additional_chips);
                    (self.output)(format!("Player {player} raised to {chips}, bet {player_additional_chips}"));
                    self.round.pot.inc_current_raise(chips - self.round.pot.current_raise);
                    self.round.phase_ends_on_player = player;

                    let mut next = (player + 1) % self.player_chips.len();
                    while (self.player_chips[next] == 0 || self.round.folded_players[next])
                        && next != player {
                        next = (next + 1) % self.player_chips.len();
                    }
                    // TODO: Should guard against client attempting this?
                    if next == player { panic!("Raised but nobody else left playing?") }
                    self.round.next_player = next;
                },
                Action::Fold => {
                    assert!(self.round.phase != GamePhase::SmallBlind && self.round.phase != GamePhase::BigBlind);
                    self.round.folded_players[player] = true;
                    (self.output)(format!("Player {player} folded"));

                    let mut next = (player + 1) % self.player_chips.len();
                    while (self.player_chips[next] == 0 || self.round.folded_players[next])
                        && next != self.round.phase_ends_on_player {
                        next = (next + 1) % self.player_chips.len();
                    }
                    if next == self.round.phase_ends_on_player {
                        self.handle_end_of_phase();
                        return;
                    }

                    let mut player_after = (next + 1) % self.player_chips.len();
                    while (self.player_chips[player_after] == 0 || self.round.folded_players[next]) && player_after != next {
                        player_after = (player_after + 1) % self.player_chips.len();
                    }
                    if player_after == next {
                        self.handle_end_of_phase();
                        return;
                    }

                    self.round.next_player = next;
                }
            }
        }

        // TODO: A lot of this should live on Round rather than Game?
        // TODO: If all but one player is out (rather than All-In, more typical to just end there)?
        fn handle_end_of_phase(&mut self) {
            match self.round.phase {
                GamePhase::SmallBlind => {
                    self.round.phase = GamePhase::BigBlind;
                    self.round.pot.inc_current_raise(self.small_blind);

                    let mut next = (self.round.next_player + 1) % self.player_chips.len();
                    while self.player_chips[next] == 0 {
                        next = (next + 1) % self.player_chips.len();
                        if next == self.round.next_player {
                            panic!("Could not find big blind player");
                        }
                    }
                    self.round.next_player = next;
                },
                GamePhase::BigBlind => {
                    self.round.phase = GamePhase::PreFlop;

                    // TODO: Also doesn't make sense if only one player left in? See 'player_after' below.
                    let mut next = (self.round.next_player + 1) % self.player_chips.len();
                    while self.player_chips[next] == 0 {
                        next = (next + 1) % self.player_chips.len();
                        if next == self.round.next_player {
                            self.handle_end_of_phase();
                            return;
                        }
                    }
                    let mut player_after = (next + 1) % self.player_chips.len();
                    while self.player_chips[player_after] == 0 {
                        player_after = (player_after + 1) % self.player_chips.len();
                        if player_after == next {
                            self.handle_end_of_phase();
                            return;
                        }
                    }
                    self.round.next_player = next;
                },
                GamePhase::PreFlop => {
                    (self.output)(self.round.cards.community_cards[..3].iter().map(|card| card.to_string()).collect());
                    self.round.phase = GamePhase::Flop;
                    self.round.pot.reset_current_raise();
                    // TODO: Simplify tracking next player!
                    let initial_next = (self.dealer + 1) % self.player_chips.len();
                    let mut next = initial_next;
                    while self.player_chips[next] == 0 || self.round.folded_players[next] {
                        next = (next + 1) % self.player_chips.len();
                        if next == initial_next {
                            self.handle_end_of_phase();
                            return;
                        }
                    }

                    let mut player_after = (next + 1) % self.player_chips.len();
                    while (self.player_chips[player_after] == 0 || self.round.folded_players[next]) && player_after != next {
                        player_after = (player_after + 1) % self.player_chips.len();
                    }
                    if player_after == next {
                        self.handle_end_of_phase();
                        return;
                    }

                    self.round.next_player = next;
                    self.round.phase_ends_on_player = next;
                },
                GamePhase::Flop => {
                    (self.output)(self.round.cards.community_cards[3].to_string());
                    self.round.phase = GamePhase::Turn;
                    self.round.pot.reset_current_raise();
                    // TODO: Simplify tracking next player!
                    let initial_next = (self.dealer + 1) % self.player_chips.len();
                    let mut next = initial_next;
                    while self.player_chips[next] == 0 || self.round.folded_players[next] {
                        next = (next + 1) % self.player_chips.len();
                        if next == initial_next {
                            self.handle_end_of_phase();
                            return;
                        }
                    }

                    let mut player_after = (next + 1) % self.player_chips.len();
                    while (self.player_chips[player_after] == 0 || self.round.folded_players[next]) && player_after != next {
                        player_after = (player_after + 1) % self.player_chips.len();
                    }
                    if player_after == next {
                        self.handle_end_of_phase();
                        return;
                    }

                    self.round.next_player = next;
                    self.round.phase_ends_on_player = next;
                },
                GamePhase::Turn => {
                    (self.output)(self.round.cards.community_cards[4].to_string());
                    self.round.phase = GamePhase::River;
                    self.round.pot.reset_current_raise();
                    // TODO: Simplify tracking next player!
                    let initial_next = (self.dealer + 1) % self.player_chips.len();
                    let mut next = initial_next;
                    while self.player_chips[next] == 0 || self.round.folded_players[next] {
                        next = (next + 1) % self.player_chips.len();
                        if next == initial_next {
                            self.handle_end_of_phase();
                            return;
                        }
                    }

                    let mut player_after = (next + 1) % self.player_chips.len();
                    while (self.player_chips[player_after] == 0 || self.round.folded_players[next]) && player_after != next {
                        player_after = (player_after + 1) % self.player_chips.len();
                    }
                    if player_after == next {
                        self.handle_end_of_phase();
                        return;
                    }

                    self.round.next_player = next;
                    self.round.phase_ends_on_player = next;
                },
                GamePhase::River => {
                    let mut hands: Vec<(usize, FiveCardHand)> = self.round.pot.chips_by_player.iter().enumerate()
                        .filter(|(i, chips)| !self.round.folded_players[*i] && **chips > 0)
                        .map(|(i, _)| (i, self.round.cards.get_hand(i)))
                        .collect();
                    hands.sort_by(|(_, first_hand), (_, second_hand)| first_hand.cmp(second_hand));
                    hands.reverse();

                    // Repeatedly distribute chips up to the smallest amount any winning player put in, then deduct, filter players, repeat.
                    while self.round.pot.total > 0 {
                        let mut winners = 1;
                        while winners < hands.len() && hands[winners].1.cmp(&hands[0].1) == Ordering::Equal { winners += 1 };
                        let winning_players: Vec<_> = hands[..winners].iter().map(|(i, _)| i).collect();
                        let max_player_chips = winning_players.iter().map(|&&i| self.round.pot.chips_by_player[i]).min().unwrap();
                        let mut chips_won = 0;
                        for chips in self.round.pot.chips_by_player.iter_mut() {
                            let deducted = max_player_chips.min(*chips);
                            *chips -= deducted;
                            self.round.pot.total -= deducted;
                            chips_won += deducted;
                        }
                        // Possible rounding here, excess chips go to the house!
                        let chips_won = chips_won / winners;
                        for &winner in winning_players {
                            (self.output)(format!("Player {winner} won {chips_won}"));
                            self.player_chips[winner] += chips_won;
                        }
                        hands = hands.into_iter().filter(|(i, _)| self.round.pot.chips_by_player[*i] > 0).collect();
                    }

                    // Game over
                    if self.player_chips.iter().filter(|&&chips| chips > 0).count() < 2 {
                        (self.output)("Game over".to_string());
                        self.round.phase = GamePhase::Finished;
                        return;
                    }

                    (self.output)("New round...".to_string());

                    // Otherwise, set up next round
                    // TODO: Find a better way of calculating next player!
                    let mut next_dealer = (self.dealer + 1) % self.player_chips.len();
                    while self.player_chips[next_dealer] == 0 {
                        next_dealer = (next_dealer + 1) % self.player_chips.len();
                    }
                    self.dealer = next_dealer;
                    let mut small_blind_player = (next_dealer + 1) % self.player_chips.len();
                    while self.player_chips[small_blind_player] == 0 {
                        small_blind_player = (small_blind_player + 1) % self.player_chips.len();
                    }
                    let mut big_blind_player = (small_blind_player + 1) % self.player_chips.len();
                    while self.player_chips[big_blind_player] == 0 {
                        big_blind_player = (big_blind_player + 1) % self.player_chips.len();
                    }
                    let mut after_big_blind = (big_blind_player + 1) % self.player_chips.len();
                    while self.player_chips[after_big_blind] == 0 {
                        after_big_blind = (after_big_blind + 1) % self.player_chips.len();
                    }
                    self.round = Round::new(
                        self.player_chips.len(),
                        self.small_blind,
                        small_blind_player,
                        after_big_blind,
                        &mut self.rng
                    );

                    self.play_initial_blinds();
                },
                GamePhase::Finished => panic!("Game already finished!"),
            }
        }

        fn inc_bet(&mut self, player: usize, bet: usize) {
            self.player_chips[player] -= bet;
            self.round.pot.inc_bet(player, bet);
        }
    }
}