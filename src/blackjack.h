#ifndef BLACKJACK_H
#define BLACKJACK_H

#include <string>
#include <vector>
#include <random>

// Define structure of a card: rank, suit, and card's value
struct Card {
  std::string rank;
  std::string suit;
  int value;
};

// Represents the evaluated value of a blackjack hand.
//
// Fields:
//   total - The best blackjack point total for the hand (e.g., 21, 17, 12).
//           Aces are counted as 11 when possible, otherwise as 1.
//
//   soft  - True if the hand is "soft", meaning at least one Ace is still
//           counted as 11 after any necessary reductions to avoid busting.
//           False if the hand is "hard" (no Ace counted as 11).
//
//   code  - A short string describing the hand, such as "S17" for soft 17
//           or "H12" for hard 12. The first character is 'S' or 'H' and the
//           second part is the numeric total.
struct HandVal {
  int total;
  bool soft;
  std::string code;
};

// Function Headers

bool is_blackjack_c(const std::vector<Card>& hand);
HandVal evaluate_hand_c(const std::vector<Card>& hand);
std::vector<Card> create_shoe_c(int num_decks, std::mt19937_64& rng);
#endif
