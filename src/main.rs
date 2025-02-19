use bluff::game::{Action, Game, GameCards, GameRunner, Player, PrinterListener, RandomPlayer};
use rand::{rngs::StdRng, SeedableRng};

fn main() {
    let rng = StdRng::seed_from_u64(1);

    println!("Hello, world!");
    let game_cards = GameCards::new(5, rng).unwrap();
    println!("Game cards: {:#?}", game_cards);
    println!("Player 1 hand: {:#?}", game_cards.get_hand(0));

    let rng = Box::new(StdRng::seed_from_u64(1));

    println!("Poker game:");
    let mut game = Game::new(4, 100, 5, rng, Box::new(PrinterListener));

    game.handle_action(Action::CheckOrCall, 3);
    game.handle_action(Action::CheckOrCall, 0);
    game.handle_action(Action::CheckOrCall, 1);
    game.handle_action(Action::CheckOrCall, 2);

    // Flop

    game.handle_action(Action::CheckOrCall, 1);
    game.handle_action(Action::CheckOrCall,2);
    game.handle_action(Action::CheckOrCall, 3);
    game.handle_action(Action::CheckOrCall, 0);

    // Turn

    game.handle_action(Action::CheckOrCall, 1);
    game.handle_action(Action::CheckOrCall,2);
    game.handle_action(Action::CheckOrCall, 3);
    game.handle_action(Action::CheckOrCall, 0);

    // River

    game.handle_action(Action::CheckOrCall, 1);
    game.handle_action(Action::CheckOrCall,2);
    game.handle_action(Action::CheckOrCall, 3);
    game.handle_action(Action::CheckOrCall, 0);

    println!("Game state: {game:?}");

    let rng = Box::new(StdRng::seed_from_u64(1));
    let players = (0..4).map(|player| Box::new(RandomPlayer::new(player)) as Box<dyn Player>).collect();
    GameRunner::new(players).run_game(100, 5, rng);
}
