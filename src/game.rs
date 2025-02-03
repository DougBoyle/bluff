use std::{cell::RefCell, cmp::Ordering, collections::HashSet, fmt::Debug, rc::Rc};

use rand::{seq::SliceRandom, Rng, RngCore};

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
    chips_bet_by_player: Vec<usize>,
    total: usize,
}

impl Pot {
    fn new(players: usize, small_blind: usize) -> Pot {
        Pot { current_raise: small_blind, current_player_total: small_blind, chips_bet_by_player: vec![0; players], total: 0 }
    }

    fn inc_bet(&mut self, player: usize, bet: usize) {
        self.chips_bet_by_player[player] += bet;
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

pub enum Event {
    PlayerCards { player: usize, cards: [Card; 2] },
    CommunityCards {cards: Vec<Card> },
    SmallBlind { player: usize, chips_added: usize },
    BigBlind { player: usize, chips_added: usize },
    Check { player: usize },
    Call { player: usize, chips_added: usize },
    Raise { player: usize, round_new_raise: usize, chips_added: usize },
    Fold { player: usize },
    RoundWinner { player: usize, chips_won: usize },
    NewRoundStarting,
    GameOver { winning_player: usize },
    InvalidAction { player: usize, description: String },
}

pub trait Listener {
    fn handle_event(&self, event: &Event);
}

impl<T: Listener> Listener for Rc<RefCell<T>> {
    fn handle_event(&self, event: &Event) {
        self.borrow().handle_event(event);
    }
}

pub type Output = Box<dyn Listener>;

pub fn print_event(event: &Event) {
    println!("< {}", match event {
        Event::PlayerCards { player, cards } => format!("Player {player}: {}, {}", cards[0], cards[1]),
        Event::CommunityCards { cards } => cards.iter().map(|card| card.to_string()).collect::<String>(),
        Event::SmallBlind { player, chips_added } => format!("Small blind: Player {player}, added {chips_added} chips"),
        Event::BigBlind { player, chips_added } => format!("Big blind: Player {player}, added {chips_added} chips"),
        Event::Check { player } => format!("Player {player} checked"),
        Event::Call { player, chips_added } => format!("Player {player} called, added {chips_added} chips"),
        Event::Raise { player, round_new_raise, chips_added } => format!("Player {player} raised to {round_new_raise}, added {chips_added} chips"),
        Event::Fold { player } => format!("Player {player} folded"),
        Event::RoundWinner { player, chips_won } => format!("Player {player} won round, won {chips_won} chips"),
        Event::NewRoundStarting => "New round starting...".to_string(),
        Event::GameOver { winning_player } => format!("Game over, player {winning_player} won"),
        Event::InvalidAction { player, description } => format!("!! INVALID ACTION, Player={player}: {description}"),
    });
}

pub struct PrinterListener;

impl Listener for PrinterListener {
    fn handle_event(&self, event: &Event) {
        print_event(event);
    }
}

#[derive(Debug)]
pub enum Action {
    SmallBlind, // Potentially going All-In
    BigBlind, // Potentially going All-In
    Fold,
    CheckOrCall, // For call, includes going All-In
    Raise(usize), // In terms of the chips for this phase, not the additional amount being raised. TODO: Tick size?
}

#[derive(Debug)]
struct Round {
    pot: Pot,
    active_players: HashSet<usize>, // Players that still have chips + haven't folded
    cards: GameCards,
    phase: GamePhase,
    phase_ends_on_player: usize, // Meaningless until after big blind. Otherwise, the last player to raise, or first to go if no raises yet.
    next_player: usize, // the player we are currently waiting on
    folded_players: Vec<bool>,
}

impl Round {
    pub fn new(
        players: usize, // total number of players originally in game
        active_players: HashSet<usize>, // just those players that still have chips
        small_blind: usize,
        small_blind_player: usize,
        rng: &mut impl Rng
    ) -> Round {
        Round {
            pot: Pot::new(players, small_blind),
            active_players,
            phase_ends_on_player: 0,
            next_player: small_blind_player,
            phase: GamePhase::SmallBlind,
            cards: GameCards::new(players, rng).unwrap(),
            folded_players: vec![false; players],
        }
    }
}

type Random = Box<dyn RngCore>;

// TODO: How to handle players going all-in/running out of chips?
pub struct Game {
    player_chips: Vec<usize>,
    small_blind: usize, // TODO: Increase over time?
    dealer: usize,
    round: Round,
    rng: Random,
    output: Output,
}

impl Debug for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Game")
            .field("player_chips", &self.player_chips)
            .field("small_blind", &self.small_blind)
            .field("dealer", &self.dealer)
            .field("round", &self.round)
            .finish()
    }
}

impl Game {
    pub fn new_random(players: usize, initial_chips: usize, small_blind: usize, output: Output) -> Self {
        let rng = Box::new(rand::rng());
        Game::new(players, initial_chips, small_blind, rng, output)
    }
}

impl Game {
    pub fn new(players: usize, initial_chips: usize, small_blind: usize, mut rng: Random, output: Output) -> Game {
        let dealer = 0; // TODO: Vary this?
        let small_blind_player = 1;
        let active_players: HashSet<usize> = (0..players).collect();
        let mut game = Game {
            player_chips: vec![initial_chips; players],
            small_blind,
            dealer,
            round: Round::new(players, active_players, small_blind, small_blind_player, &mut rng),
            rng,
            output
        };

        game.play_initial_blinds();

        game
    }

    // TODO: In event both players end up all-in, this can automatically play through the whole game.
    //       Better to queue up follow up actions in some sort of PendingAction state? Then top-level can check that.
    fn play_initial_blinds(&mut self) {
        for (player, cards) in self.round.cards.player_cards.iter().enumerate() {
            self.output.handle_event(&Event::PlayerCards { player, cards: cards.clone() });
        }
        self.handle_action(Action::SmallBlind, self.round.next_player);
        self.handle_action(Action::BigBlind, self.round.next_player);
    }

    pub fn handle_action(&mut self, action: Action, player: usize) {
        assert!(player == self.round.next_player);
        assert!(self.round.phase != GamePhase::Finished);
        match action {
            // TODO: Output should indicate when a player has gone all in
            Action::SmallBlind => {
                assert_eq!(GamePhase::SmallBlind, self.round.phase);
                let chips_added = self.round.pot.current_player_total.min(self.player_chips[player]);
                self.output.handle_event(&Event::SmallBlind { player, chips_added });
                assert!(chips_added > 0);
                self.inc_bet(player, chips_added);
                self.handle_end_of_phase();
            },
            Action::BigBlind => {
                assert_eq!(GamePhase::BigBlind, self.round.phase);
                let chips_added = self.round.pot.current_player_total.min(self.player_chips[player]);
                assert!(chips_added > 0);
                self.output.handle_event(&Event::BigBlind { player, chips_added });
                self.inc_bet(player, chips_added);
                self.handle_end_of_phase();
            },
            Action::CheckOrCall => {
                assert!(self.round.phase != GamePhase::SmallBlind && self.round.phase != GamePhase::BigBlind);
                let chips_added = self.round.pot.current_player_total - self.round.pot.chips_bet_by_player[player];
                let chips_added = chips_added.min(self.player_chips[player]);
                if chips_added > 0 {
                    self.inc_bet(player, chips_added);
                    self.output.handle_event(&Event::Call { player, chips_added });
                } else {
                    self.output.handle_event(&Event::Check { player });
                }

                if self.round.active_players.len() < 2 {
                    self.handle_end_of_phase();
                    return;
                }

                match self.next_player_to_act_after(player) {
                    None => self.handle_end_of_phase(),
                    Some(next) => self.round.next_player = next,
                }
            },
            Action::Raise(round_new_raise) => {
                assert!(self.round.phase != GamePhase::SmallBlind && self.round.phase != GamePhase::BigBlind);
                assert!(round_new_raise > self.round.pot.current_raise, "Raise to {round_new_raise} not greater than current raise {}", self.round.pot.current_raise);
                self.round.pot.inc_current_raise(round_new_raise - self.round.pot.current_raise);
                let chips_added = self.round.pot.current_player_total - self.round.pot.chips_bet_by_player[player];
                // TODO: Tick size
                assert!(chips_added <= self.player_chips[player]);
                self.inc_bet(player, chips_added);
                self.output.handle_event(&Event::Raise { player, round_new_raise, chips_added });
                self.round.phase_ends_on_player = player;

                self.round.next_player = self.next_player_to_act_after(player).expect("Raised but nobody else left playing?");
            },
            Action::Fold => {
                assert!(self.round.phase != GamePhase::SmallBlind && self.round.phase != GamePhase::BigBlind);
                self.round.folded_players[player] = true;
                self.round.active_players.remove(&player);
                self.output.handle_event(&Event::Fold { player });

                if self.round.active_players.len() < 2 {
                    self.handle_end_of_phase();
                    return;
                }

                match self.next_player_to_act_after(player) {
                    None => self.handle_end_of_phase(),
                    Some(next) => self.round.next_player = next,
                }
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

                if self.round.active_players.len() < 2 {
                    self.handle_end_of_phase();
                    return;
                }

                let next = self.player_with_chips_not_folded_after(self.round.next_player).unwrap();
                self.round.next_player = next;
                self.round.phase_ends_on_player = next;
            },
            GamePhase::PreFlop => {
                self.output.handle_event(&Event::CommunityCards { cards: self.round.cards.community_cards[..3].iter().copied().collect() });
                self.round.phase = GamePhase::Flop;
                self.round.pot.reset_current_raise();

                if self.round.active_players.len() < 2 {
                    self.handle_end_of_phase();
                    return;
                }

                let next = self.player_with_chips_not_folded_after(self.dealer).unwrap();
                self.round.next_player = next;
                self.round.phase_ends_on_player = next;
            },
            GamePhase::Flop => {
                self.output.handle_event(&Event::CommunityCards { cards: self.round.cards.community_cards[..4].iter().copied().collect() });
                self.round.phase = GamePhase::Turn;
                self.round.pot.reset_current_raise();

                if self.round.active_players.len() < 2 {
                    self.handle_end_of_phase();
                    return;
                }

                let next = self.player_with_chips_not_folded_after(self.dealer).unwrap();
                self.round.next_player = next;
                self.round.phase_ends_on_player = next;
            },
            GamePhase::Turn => {
                self.output.handle_event(&Event::CommunityCards { cards: self.round.cards.community_cards.iter().copied().collect() });
                self.round.phase = GamePhase::River;
                self.round.pot.reset_current_raise();

                if self.round.active_players.len() < 2 {
                    self.handle_end_of_phase();
                    return;
                }

                let next = self.player_with_chips_not_folded_after(self.dealer).unwrap();
                self.round.next_player = next;
                self.round.phase_ends_on_player = next;
            },
            GamePhase::River => {
                let mut hands: Vec<(usize, FiveCardHand)> = self.round.pot.chips_bet_by_player.iter().enumerate()
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
                    let max_player_chips = winning_players.iter().map(|&&i| self.round.pot.chips_bet_by_player[i]).min().unwrap();
                    let mut chips_won = 0;
                    for chips in self.round.pot.chips_bet_by_player.iter_mut() {
                        let deducted = max_player_chips.min(*chips);
                        *chips -= deducted;
                        self.round.pot.total -= deducted;
                        chips_won += deducted;
                    }
                    // Possible rounding here, excess chips go to the house!
                    let chips_won = chips_won / winners;
                    for &player in winning_players {
                        self.output.handle_event(&Event::RoundWinner { player, chips_won });
                        self.player_chips[player] += chips_won;
                    }
                    hands = hands.into_iter().filter(|(i, _)| self.round.pot.chips_bet_by_player[*i] > 0).collect();
                }

                // Game over
                let active_players: HashSet<usize> = self.player_chips.iter().enumerate()
                    .filter(|(_, &chips)| chips > 0)
                    .map(|(i, _)| i)
                    .collect();

                if active_players.len() < 2 {
                    let winning_player = *active_players.iter().next().unwrap();
                    self.output.handle_event(&Event::GameOver { winning_player });
                    self.round.phase = GamePhase::Finished;
                    return;
                }

                self.output.handle_event(&Event::NewRoundStarting);

                // Otherwise, set up next round
                let next_dealer = self.player_with_chips_after(self.dealer).unwrap();
                let next_small_blind_player = self.player_with_chips_after(next_dealer).unwrap();

                self.dealer = next_dealer;
                self.round = Round::new(
                    self.player_chips.len(),
                    active_players,
                    self.small_blind,
                    next_small_blind_player,
                    &mut self.rng
                );

                self.play_initial_blinds();
            },
            GamePhase::Finished => panic!("Game already finished!"),
        }
    }

    /// Expects at least 2 players to have chips left (else we have a winner).
    /// Only use between rounds. Within a round, need to care if the player has folded.
    fn player_with_chips_after(&self, player: usize) -> Option<usize> {
        let mut next = (player + 1) % self.num_players();
        while self.player_chips[next] == 0 {
            next = (next + 1) % self.num_players();
            if next == player { return None; }
        }
        Some(next)
    }

    /// Should only be used between phases of a round.
    /// Cannot be used at the end of a round, when folded or not is meaningless.
    /// Cannot be used within a phase, when we need to consider the last player to act before advancing.
    fn player_with_chips_not_folded_after(&self, player: usize) -> Option<usize> {
        let mut next = (player + 1) % self.num_players();
        while !self.round.active_players.contains(&next) {
            next = (next + 1) % self.num_players();
            if next == player { return None; }
        }
        Some(next)
    }

    /// The next player to act within the current betting phase.
    /// Must have chips, have not folded, and be before the first player to act/latest player to raise.
    fn next_player_to_act_after(&self, player: usize) -> Option<usize> {
        let mut next = (player + 1) % self.num_players();
        if next == self.round.phase_ends_on_player { return None; }

        while !self.round.active_players.contains(&next) {
            next = (next + 1) % self.num_players();
            if next == self.round.phase_ends_on_player { return None; }
        }

        Some(next)
    }

    fn inc_bet(&mut self, player: usize, bet: usize) {
        self.player_chips[player] -= bet;
        if self.player_chips[player] == 0 {
            self.round.active_players.remove(&player);
        }
        self.round.pot.inc_bet(player, bet);
    }

    fn num_players(&self) -> usize {
        self.player_chips.len()
    }
}

/// TODO: Run the automated parts of the game, split out options/receive and validate inputs for the actions each player can perform.
/// In future, may dispatch these actions / waiting for responses to different subprocesses.
pub struct GameRunner {
    num_players: usize,
    players: Rc<RefCell<Vec<Box<dyn Player>>>>,
}

impl GameRunner {
    pub fn new(players: Vec<Box<dyn Player>>) -> Self {
        GameRunner { num_players: players.len(), players: Rc::new(RefCell::new(players)) }
    }

    pub fn run_game(&mut self, initial_chips: usize, small_blind: usize, rng: Random) {
        // TODO: Tidy up all the wrapper types
        struct RunnerListener(Rc<RefCell<Vec<Box<dyn Player>>>>);
        
        impl Listener for RunnerListener {
            fn handle_event(&self, event: &Event) {
                print_event(event);
                match event {
                    Event::PlayerCards { player, .. } => {
                        self.0.borrow_mut()[*player].receive_event(event);
                    },
                    _ => {
                        for player in self.0.borrow_mut().iter_mut() {
                            player.receive_event(event);
                        }
                    },
                }
            }
        }

        let listener = Rc::new(RefCell::new(RunnerListener(Rc::clone(&self.players))));

        let mut game = Game::new(self.num_players, initial_chips, small_blind, rng, Box::new(Rc::clone(&listener)));

        while game.round.phase != GamePhase::Finished {
            let player = game.round.next_player;
            let Pot { current_raise, current_player_total, chips_bet_by_player, .. } = &game.round.pot;
            let current_chips_bet = chips_bet_by_player[player];
            let chips_required = current_player_total - current_chips_bet;
            let available_chips = game.player_chips[player];

            let can_raise = game.round.active_players.len() > 1; // otherwise, all remaining players already All-In

            let action = loop {
                let action = self.players.borrow_mut()[player].select_action(PlayerInput::new(&game.round.pot, &game.player_chips, &game.round.active_players));
                let valid = match action {
                    Action::SmallBlind | Action::BigBlind => false, // automated, not an action a player can pick
                    Action::Fold | Action::CheckOrCall => true,
                    Action::Raise(new_raise_total) =>
                        can_raise && new_raise_total > *current_raise && (new_raise_total <= current_raise - chips_required + available_chips),
                };
                if valid {
                    break action;
                } else {
                    let error = Event::InvalidAction { player, description: format!("Invalid action {action:?}") };
                    listener.borrow().handle_event(&error);
                }
            };

            game.handle_action(action, player);
        }
    }
}

// TODO: Clean up what needs including here or not
pub struct PlayerInput<'a> {
    pot: &'a Pot,
    player_chips: &'a Vec<usize>,
    active_players: &'a HashSet<usize>,
}

impl<'a> PlayerInput<'a> {
    fn new(pot: &'a Pot, player_chips: &'a Vec<usize>, active_players: &'a HashSet<usize>) -> Self {
        PlayerInput { pot, player_chips, active_players }
    }
}

pub trait Player {
    fn receive_event(&mut self, event: &Event);
    fn select_action(&mut self, input: PlayerInput) -> Action;
}

pub struct RandomPlayer {
    player: usize,
    rng: Random,
}

impl RandomPlayer {
    pub fn new(player: usize) -> Self {
        RandomPlayer { player, rng: Box::new(rand::rng()) }
    }
}

impl Player for RandomPlayer {
    fn receive_event(&mut self, _: &Event) {
        // Plays randomly, no awareness of events besides playing valid actions
        //println!("Player {} saw event: ", self.player);
        //print_event(event);
    }

    fn select_action(&mut self, PlayerInput { pot, player_chips, active_players }: PlayerInput<'_>) -> Action {
        let Pot { current_raise, current_player_total, total, chips_bet_by_player, .. } = pot;
        let current_chips_bet = chips_bet_by_player[self.player];
        let chips_required = current_player_total - current_chips_bet;
        let available_chips = player_chips[self.player];

        let can_raise = active_players.len() > 1; // otherwise, all remaining players already All-In

        println!("Player {}: Pot = {total}, Current raise = {current_raise} (+{chips_required}), Chips remaining = {available_chips}", self.player);
        if chips_required >= available_chips {
            println!("Player {}: [All In ({available_chips})] [Fold]", self.player);
            if self.rng.random_bool(0.5) {
                Action::CheckOrCall
            } else {
                Action::Fold
            }
        } else {
            if can_raise {
                let max_raise_total = current_raise + available_chips - chips_required;
                if chips_required > 0 {
                    println!("Player {}: [Call ({chips_required})] [Raise (up to {max_raise_total} total / {available_chips} additional)] [Fold]", self.player);
                    if self.rng.random_bool(0.7) {
                        Action::CheckOrCall
                    } else if self.rng.random_bool(0.9) {
                        Action::Raise(self.rng.random_range(current_raise + 1..=max_raise_total))
                    } else {
                        Action::Fold
                    }
                } else {
                    println!("Player {}: [Check] [Raise (up to {max_raise_total} total / {available_chips} additional)] [Fold]", self.player);
                    if self.rng.random_bool(0.5) {
                        Action::CheckOrCall
                    } else if self.rng.random_bool(0.9) {
                        Action::Raise(self.rng.random_range(chips_required + 1..=available_chips))
                    } else {
                        Action::Fold
                    }
                }
            } else {
                if chips_required > 0 {
                    println!("Player {}: [Call ({chips_required})] [Fold]", self.player);
                    if self.rng.random_bool(0.9) {
                        Action::CheckOrCall
                    } else {
                        Action::Fold
                    }
                } else { // This case might never be possible?
                    println!("Player {}: [Check] [Fold]", self.player);
                    if self.rng.random_bool(0.95) {
                        Action::CheckOrCall
                    } else {
                        Action::Fold
                    }
                }
            }
        }
    }
}