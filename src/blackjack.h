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

// Rules governing when a player may double down.
enum class DoubleRule {
  ANY,          // Double on any two cards
  NINE_TEN_ELEVEN, // Double only on hard 9, 10, or 11
  TEN_ELEVEN    // Double only on hard 10 or 11
};

// Blackjack table rules and configuration.
struct BlackjackRules {
  bool dealer_stands_soft_17;   // True if dealer stands on soft 17 (S17), false if hits (H17)
  int num_decks;                // Number of standard 52-card decks in the shoe
  bool allow_insurance;         // Whether insurance is offered when dealer shows an Ace
  bool dealer_peeks;            // Whether dealer peeks at hole card for blackjack (American rules)
  DoubleRule double_on;        // Doubling restriction rule (e.g., "any", "9,10,11", "10,11")
  bool double_after_split;      // Whether doubling is allowed after splitting a pair
  int max_splits;               // Maximum number of splits allowed in a round
  bool resplit_aces;            // Whether aces may be resplit if another ace is drawn
  bool hit_split_aces;          // Whether the player may hit hands formed by split aces
};


// Function Headers

bool is_blackjack_c(const std::vector<Card>& hand);
HandVal evaluate_hand_c(const std::vector<Card>& hand);
std::vector<Card> create_shoe_c(int num_decks, std::mt19937_64& rng);
int dealer_play_c(
    const std::vector<Card>& shoe,
    std::vector<Card>& hand,
    bool dealer_stands_soft_17,
    std::array<int, 12>& card_counts,
    int pos
);

#endif
