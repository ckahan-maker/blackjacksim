#include <Rcpp.h>
#include <random>
#include <algorithm>
#include "card.h"
using namespace Rcpp;

// Create and shuffle a multi-deck blackjack shoe.
//
// Builds a complete shoe consisting of `num_decks` standard 52-card decks.
// Each card stores a rank, suit, and blackjack
// point value (Ace = 11, face cards = 10, numeric cards use their face
// values). Cards are generated in deterministic order and the shoe is
// shuffled using the RNG passed in by reference.
//
std::vector<Card> create_shoe_c(int num_decks, std::mt19937_64& rng) {
  // Suits, ranks, and blackjack point values
  static const std::vector<std::string> suits  = {"♠", "♥", "♦", "♣"};
  static const std::vector<std::string> ranks  = {"A", "J", "Q", "K",
                                                  "2", "3", "4", "5", "6",
                                                  "7", "8", "9", "10"};
  static const std::vector<int> values         = {11, 10, 10, 10,   // A, J, Q, K
                                                  2, 3, 4, 5, 6,
                                                  7, 8, 9, 10};      // 2–10

  // Preallocate full shoe (52 cards per deck)
  std::vector<Card> shoe;
  shoe.reserve(52 * num_decks);

  // Build the shoe
  for (int d = 0; d < num_decks; ++d) {
    for (std::size_t r = 0; r < ranks.size(); ++r) {
      const std::string& rank = ranks[r];
      int value = values[r];

      for (const auto& suit : suits) {
        shoe.push_back(Card{rank, suit, value});
      }
    }
  }

  // Shuffle using the provided RNG
  std::shuffle(shoe.begin(), shoe.end(), rng);

  return shoe;
}

