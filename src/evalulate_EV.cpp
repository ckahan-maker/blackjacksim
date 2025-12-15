#include "blackjack.h"
#include <Rcpp.h>
using namespace Rcpp;

// Evaluates the Expected Value (EV) of the Surrender action.
// [[Rcpp::export]]
double eval_surrender_c() {
  // The EV of surrendering is always -0.5 because
  // you are guaranteed to lose 50% of the bet.
  return -0.5;
}

// Evaluates the Expected Value (EV) of the Insurance action.
// card_counts: An integer vector of length 12 tracking card values.
// [[Rcpp::export]]
double eval_insurance_c(Rcpp::IntegerVector card_counts) {

  // Calculate number of 10-value cards in remaining deck
  double tens_remaining = (double)card_counts[10];

  double total = 0.0;
  for (int i = 2; i <= 11; ++i) total += card_counts[i];

  // Calculate probability the dealer has a 10
  double p_ten = tens_remaining / total;
  double p_not_ten = 1.0 - p_ten;

  // EV calculation
  return (p_ten * 2.0) - (p_not_ten * 1.0);
}


// Recursively compute the EV if player stands.
// Parameters:
//   dealer_hand  - Dealer's current hand.
//   player_total - Player's final hand total.
//   card_counts  - Remaining card counts in the shoe.
//   rules        - Table rules.
double eval_stand_c(std::vector<Card> dealer_hand, int player_total,
                    std::array<int, 12> card_counts, const BlackjackRules& rules) {

  HandVal hv = evaluate_hand_c(dealer_hand);

  // Base Case: Dealer busts
  if (hv.total > 21) {
    return 1.0; // Player wins (EV = 1.0)
  }

  // Calculate number of cards remaining in the deck
  double num_cards = 0.0;
  for (int i = 2; i <= 11; ++i) num_cards += card_counts[i];

  // Dealer must hit if below 17, or on soft 17 when H17 rules apply
  bool dealer_stands = (hv.total > 17) ||
    (hv.total == 17 && (rules.dealer_stands_soft_17 || !hv.soft));

  if (dealer_stands) {
    if (hv.total < player_total) return 1.0;  // Player wins
    if (hv.total > player_total) return -1.0; // Player loses
    return 0.0; // Push
  }

  // Recursive Step: Dealer must hit
  double expected_value = 0.0;

  for (int v = 2; v <= 11; ++v) {
    if (card_counts[v] > 0) {
      // Probability of drawing this specific card value
      double p_card = static_cast<double>(card_counts[v]) / num_cards;

      // Simulate drawing the card
      std::array<int, 12> next_card_counts = card_counts;
      next_card_counts[v]--;


      Card card;
      if (v != 11) {
        card = Card{std::to_string(v), "♠", v};
      } else {
        card = Card{"A", "♠", v};
      }
      // Add card to dealer's hand
      dealer_hand.push_back(card);
      // Recursively compute EV for this draw and weight it by its probability.
      expected_value += p_card * eval_stand_c(dealer_hand, player_total,
                                            next_card_counts, rules);
      dealer_hand.pop_back();
    }
  }

  return expected_value;
}
// Recursively compute the Expected Value (EV) of the Double Down action.
// Parameters:
//   dealer_hand  - Dealer's current hand.
//   player_hand  - Player's current hand before doubling.
//   card_counts  - Remaining card counts in the shoe.
//   rules        - Blackjack table rules.
//
double eval_double_c(std::vector<Card> dealer_hand, std::vector<Card> player_hand,
                     std::array<int, 12> card_counts, const BlackjackRules& rules) {

  double expected_value = 0.0;
  // Total number of cards remaining (used for draw probabilities)
  double total_cards = 0;
  for (int i = 2; i <= 11; ++i) total_cards += card_counts[i];

  // Double down: player draws exactly one card, then stands with a doubled bet
  for (int v = 2; v <= 11; ++v) {
    if (card_counts[v] > 0) {
      // Calculate the probability of drawing a card
      double p_card = static_cast<double>(card_counts[v]) / total_cards;

      Card card;
      if (v != 11) {
        card = Card{std::to_string(v), "♠", v};
      } else {
        card = Card{"A", "♠", v};
      }
      // Add the drawn card to the player's hand
      player_hand.push_back(card);

      // Evaluate player's final total after the one-card draw
      HandVal hv = evaluate_hand_c(player_hand);
      auto next_counts = card_counts;
      next_counts[v]--;

      if (hv.total > 21) {
        // Player busts: loses 2 units because the bet was doubled
        expected_value += p_card * -2.0;
      }
      else {
        // Player stands: dealer plays out; outcome is worth 2 units
        double stand_ev = eval_stand_c(dealer_hand, hv.total,
                                        next_counts, rules);
        expected_value += p_card * (2.0 * stand_ev);
      }
      // Undo mutation for the next branch
      player_hand.pop_back();
    }
  }

  return expected_value;
}

// Recursively compute the Expected Value (EV) of choosing to hit.
//
// After drawing one card, the player continues optimally by choosing the
// higher-EV action (stand vs hit again) until the hand ends.
//
// Parameters:
//   dealer_hand  - Dealer's current hand.
//   player_hand  - Player's current hand before drawing a card.
//   card_counts  - Remaining card counts in the shoe.
//   rules        - Blackjack table rules.
//
double eval_hit_c(std::vector<Card> dealer_hand, std::vector<Card> player_hand,
                  std::array<int, 12> card_counts, const BlackjackRules& rules) {

  double expected_value = 0.0;

  // Total number of cards remaining (used for draw probabilities)
  double total_cards = 0;
  for (int i = 2; i <= 11; ++i) total_cards += card_counts[i];

  // Enumerate all possible next-card values
  for (int v = 2; v <= 11; ++v) {
    if (card_counts[v] > 0) {
      // Calculate probability of drawing a card
      double p_card = static_cast<double>(card_counts[v]) / total_cards;

      Card card;
      if (v != 11) {
        card = Card{std::to_string(v), "♠", v};
      } else {
        card = Card{"A", "♠", v};
      }
      // Add the drawn card to the player's hand
      player_hand.push_back(card);

      // Evaluate player's total after the card draw
      HandVal hv = evaluate_hand_c(player_hand);
      auto next_counts = card_counts;
      next_counts[v]--;

      if (hv.total > 21) {
        // Player busts: loses 1 unit
        expected_value += p_card * -1.0;
      }
      else if (hv.total == 21) {
        // Player must stand on 21
        expected_value += p_card * eval_stand_c(dealer_hand, 21, next_counts, rules);
      }
      else {
        // Player chooses the better of Standing or Hitting again
        double ev_stand = eval_stand_c(dealer_hand, hv.total, next_counts, rules);
        double ev_hit_again = eval_hit_c(dealer_hand, player_hand, next_counts, rules);

        // The player will always pick the move with higher EV
        expected_value += p_card * std::max(ev_stand, ev_hit_again);
      }

      // Undo mutation for the next loop iteration
      player_hand.pop_back();
    }
  }

  return expected_value;
}



