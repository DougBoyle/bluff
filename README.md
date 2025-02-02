
# Current State

`GameRunner` provides a simplified idea of how a UI might look, and randomly picks between the available actions for each player.
e.g.

```
< Player 0: [A♣], [K♡]
< Player 1: [4♢], [6♢]
< Player 2: [K♠], [Q♣]
< Player 3: [9♡], [J♠]
< Small blind: Player 1 bet 5
< Big blind: Player 2 bet 10
Player 3: Pot = 15, Current raise = 10 (+10), Chips remaining = 100
Player 3: [Call (10)] [Raise (up to 100 total / 100 additional)] [Fold]
< Player 3 called, bet 10
Player 0: Pot = 25, Current raise = 10 (+10), Chips remaining = 100
Player 0: [Call (10)] [Raise (up to 100 total / 100 additional)] [Fold]
< Player 0 called, bet 10
Player 1: Pot = 35, Current raise = 10 (+5), Chips remaining = 95
Player 1: [Call (5)] [Raise (up to 100 total / 95 additional)] [Fold]
< Player 1 called, bet 5
Player 2: Pot = 40, Current raise = 10 (+0), Chips remaining = 90
Player 2: [Check] [Raise (up to 100 total / 90 additional)] [Fold]
< Player 2 checked
< [8♠][A♡][8♡]
Player 1: Pot = 40, Current raise = 0 (+0), Chips remaining = 90
Player 1: [Check] [Raise (up to 90 total / 90 additional)] [Fold]
< Player 1 checked
Player 2: Pot = 40, Current raise = 0 (+0), Chips remaining = 90
Player 2: [Check] [Raise (up to 90 total / 90 additional)] [Fold]
< Player 2 checked
Player 3: Pot = 40, Current raise = 0 (+0), Chips remaining = 90
Player 3: [Check] [Raise (up to 90 total / 90 additional)] [Fold]
< Player 3 raised to 33, bet 33
Player 0: Pot = 73, Current raise = 33 (+33), Chips remaining = 90
Player 0: [Call (33)] [Raise (up to 90 total / 90 additional)] [Fold]
< Player 0 called, bet 33
Player 1: Pot = 106, Current raise = 33 (+33), Chips remaining = 90
Player 1: [Call (33)] [Raise (up to 90 total / 90 additional)] [Fold]
< Player 1 called, bet 33
```
