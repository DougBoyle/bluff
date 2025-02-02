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