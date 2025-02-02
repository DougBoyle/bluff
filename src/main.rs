use rand::{rngs::StdRng, SeedableRng};

fn main() {
    println!("Hello, world!");
    let game = bluff::game::GameCards::new(5, StdRng::seed_from_u64(1)).unwrap();
    println!("Game: {:#?}", game);
    println!("Player 1 hand: {:#?}", game.get_hand(0));
}
