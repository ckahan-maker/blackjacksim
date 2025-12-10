#include "card.h"

// Determine whether a hand is a natural blackjack.

// Parameters:
//   hand - A vector of Card objects representing the player's hand.
//          Expected to contain zero or more cards.
//
// Returns:
//   true  - if the hand is exactly two cards and forms a natural blackjack
//   false - otherwise
bool is_blackjack_c(const std::vector<Card>& hand) {
  if (hand.size() != 2) {
    return false;
  }
  int num_aces = 0;
  int total_value = 0;

  for (const auto& card : hand) {
    if (card.value == 11) ++num_aces;
    total_value += card.value;
  }

  // A natural blackjack occurs when the hand has exactly two cards,
  // one of which is an Ace, and their total value is 21.
  return (num_aces == 1 && total_value == 21);
}
