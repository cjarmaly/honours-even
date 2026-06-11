# Honours-Even

## Developer's Note

My first lecture of CS51 (Abstraction and Design in Computation) at Harvard was a 75 minute explanation from Professor Stuart Shieber about why his course would be taught exclusively in OCaml. Despite his paternalistic tone, I actually appreciate the two gifts this course gave me. The first is that code is a language, not much different from English, Spanish, ASL, or hieroglyphics; and, like all other languages, the right sequence of words, punctuation, and presentation can be beautiful. I've tried to take this lesson with me as I've progressed through Harvard's computer science curriculum, although I must admit I have yet to see anything as satisfying as the three-line implementation of Euclid's algorithm (the first slide of Shieber's second lecture). The second gift of CS51 was that there exist, at a minimum, some types of programming that I enjoy. I hope to indulge in that enjoyment with this project.

### Why Whist?

I write this more than a year later, fiddling with two decks of vintage Pasadena Rose Bowl Cards I bought my girlfriend for her birthday, and I can't help but be consumed by my love for card games. Poker, Canasta, Whist, Hearts, Cambio, you name it. So the idea is natural— translate one of the Armaly family favorites, Whist, with this language I am only so familiar with, OCaml, in hopes of intertwining the beauty of both. 

### Why OCaml?

Practically, OCaml is the right choice for this project. First, OCaml makes illegal states unrepresentable, so the language becomes my solver's proof of correctness. Second, side-stepping an interpreter introduces a bluntness I appreciate, and reduces total debugging time given my programming style. Third, it's fast. [Counterfactual Regret Minimization](https://martin.zinkevich.org/publications/regretpoker.pdf) might require billions of tree traversals, and I'd prefer to spend my time on the game theory as opposed to debugging memory (C) or waiting for slow runs to complete (Python). Lastly, I've taken a course entirely in OCaml. It might not be directly in my wheelhouse, but it's somewhere on the same street.

With all that said, there's no reason to be scared of OCaml. I hope you can find the beauty in both the code and the game that has brought my family love, laughter, and tears for as long as I can remember.


## (Bid) Whist Rules - Armaly Family Style

1. The game is played with a 54 card deck— a standard 52 card deck + 2 jokers. One joker is labeled "big", the other "little". There are two teams of two, with teammates seated across from one another.

2. To determine the first dealer, each player is dealt one random card without jokers in the draw. The highest card deals, with A < K < Q < ....< 3 < 2. Upon a tie, all tied candidates are dealt one additional card. Repeat until there is a round where one player is dealt a card higher than the others. 

3. To begin the first round, each player is dealt 12 cards, with the first card going to the left of the dealer. The remaining 6 form the kitty.

4. The player to the left of the dealer is first to bid. They bid a number, in the range 3-7, along with one of "Uptown", "Downtown", or "No Trump". The number represents the number of tricks their team commits to winning in addition to their expected half (6 tricks). 

"Uptown" indicates that, for this round, the card hierarchy is "Big" Joker > "Little" Joker > A > K > Q > ... > 3 > 2. "Downtown" indicates the opposite order, with jokers still the most dominant: "Big" Joker > "Little" Joker > 2 > 3 > 4 > ... > K > A. 

"No Trump" indicates that there will be no trump suit, and both jokers can never win a trick. 

The bid of X dominates a bid of Y if X > Y. The hierarchy for equal numbers is "No Trump" > "Downtown" > "Uptown".

If a player does not want to bid "more" than the player before them, they can pass. The auction ends after exactly one pass around the table. If the first 3 players pass, the dealer is forced to take the minimum bid. The dealer may "boss" the hand, bidding an equal bid in number and in type to the highest previous offer in order to take the hand.

5. Upon auction close, a "Uptown/Downtown" winner must immediately declare the trump suit. A "No Trump" bidder must immediately declare "No Trump High" or "No Trump Down", which indicates the hierarchy of the cards. 

Then the auction winner picks up the 6 cards in the kitty, which only they can see, then must discard 6 cards. These cards count as the first trick won for the declaring team, so there are 13 total tricks up for grabs, with the first always won by the declaring team.

6. The auction winner plays the first card. Every player plays one card per turn, sequentially. The highest card of the lead suit wins the trick, unless a trump suit is played, in which case the highest card of the trump suit wins. In "Uptown" and "Downtown" regimes, jokers are a part of the trump suit. If a player has a card of the lead suit, they must play it. Otherwise, they can play any other suit, including the trump suit. So jokers can only be played if a player is leading, if a trump is led, or if a player is void of the lead suit.

7. Under a "No Trump" regime, players with jokers must play one upon the first instance(s) where they do not have the lead suit and they have a joker in hand. Should they have two jokers, they must play one at a time. If a joker is led, then the first non-joker card played determines the lead suit. Jokers can always be led voluntarily in all regimes.

8. The winner of each trick leads the next trick. Repeat tricks until all players are out of cards.

9. If the auction winner's team wins a number of tricks equal to or greater than their bid, then they are awarded a number of points which is the difference between the number of tricks won and 6. If the bid is not met, then they lose points equal to the size of their bid. The defending team's score is always unchanged.

10. The dealer rotates clockwise after each round.

11. A team wins when they reach 7 total points, or their opponent reaches -7 points.

12. Boston Rule: taking every trick (all 12 + the kitty), regardless of the bid, is worth exactly 13 points. The lowest live scorescore is -6, so Boston's +13 always wins the game.

13. Under a "No Trump" regime, all points (including negative and Boston Rule) are doubled. 



