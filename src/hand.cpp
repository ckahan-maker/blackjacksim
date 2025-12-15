#include "blackjack.h"

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

/* Evaluate Blackjack Hand Value and State
 *
 * Calculates the total point value of a hand, automatically adjusting Aces
 * from 11 to 1 if the total exceeds 21. It provides the numeric score,
 * the "Soft/Hard" status, and a formatted string code.
 *
 * Parameters:
  * hand - A vector of Card structs representing the player's or dealer's hand.
*
* Returns:
* A HandVal struct containing:
  * total: The calculated point total (e.g., 21, 12).
  * soft:  true if an Ace is being counted as 11, false otherwise.
  * code:  A string code for display (e.g., "S17", "H12").
*/
HandVal evaluate_hand_c(const std::vector<Card>& hand) {
  int num_aces = 0;
  int total_value = 0;

  // Count Aces and sum raw values (Aces counted as 11 initially)
  for (const auto& card: hand) {
    if (card.value == 11) ++num_aces;
    total_value += card.value;
  }

  // If the hand is busted (>21) and contains Aces valued at 11, reduce them to 1
  while (total_value > 21 && num_aces > 0) {
    total_value -= 10; // Turn a 11 into a 1
    num_aces -= 1;
  }

  // If we still have an ace counting as 11, it is Soft
  bool is_soft = (num_aces > 0);

  // Create a readable string for printing
  std::string prefix = is_soft ? "S" : "H";
  std::string code = prefix + std::to_string(total_value);

  return (HandVal{total_value, is_soft, code});
}

// Create a Card object from a blackjack value.
//
// This helper is used in EV calculations where only the card's value
// matters. A dummy suit is assigned
// since suits are irrelevant for hand evaluation.
//
// Parameters:
//   v - Blackjack card value (2–11).
//
// Returns:
//   A Card with the specified value and corresponding rank.
//
Card create_card_helper(int v) {
  Card card;
  card.value = v;
  card.suit  = "♠";  // Suit is arbitrary for EV logic

  if (v == 11) {
    card.rank = "A";
  } else {
    card.rank = std::to_string(v);
  }

  return card;
}

