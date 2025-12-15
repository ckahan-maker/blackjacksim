#include "blackjack.h"

// Play out the dealer's hand
//
// The dealer draws cards from the shoe until they must stand.
// Cards are drawn starting at index `pos` in the shoe vector.
// The dealer's hand and card count array are updated in place.
//
// Parameters:
//   shoe - The full shuffled shoe of cards (read-only).
//   hand - The dealer's hand. Cards will be appended as the dealer draws.
//   dealer_stands_soft_17 - If true, dealer stands on soft 17 (S17).
//                           If false, dealer hits soft 17 (H17).
//   card_counts - Array tracking remaining card counts by value.
//                 This is decremented as cards are drawn.
//   pos - Current position in the shoe (index of next card to draw).
//
// Returns:
//   The updated shoe position after the dealer finishes drawing.
//
int dealer_play_c(
    const std::vector<Card>& shoe,
    std::vector<Card>& hand,
    bool dealer_stands_soft_17,
    std::array<int, 12>& card_counts,
    int pos
) {
  // Evaluate the dealer's current hand
  HandVal result = evaluate_hand_c(hand);

  // Dealer draws while total is less than 17,
  // or while on soft 17 if the table rule requires hitting
  while (result.total < 17 ||
         (result.code == "S17" && !dealer_stands_soft_17)) {

    // Draw the next card from the shoe
    const Card& card = shoe[pos];
    pos += 1;
    // Update remaining card counts
    card_counts[card.value] -= 1;
    // Add card to dealer's hand
    hand.push_back(card);
    // Re-evaluate hand after drawing
    result = evaluate_hand_c(hand);
  }

  // Return the updated shoe position
  return pos;
}
