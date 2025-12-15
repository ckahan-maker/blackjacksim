#include "blackjack.h"
#include <algorithm>

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
      card = create_card_helper(v);

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
      card = create_card_helper(v);

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
      card = create_card_helper(v);

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

// Compute Expected Values (EVs) for a specified set of player actions.
//
// Parameters:
//   rules_obj        - R list representing a blackjack_rules object.
//   player_hand_df   - R data.frame with the player's current hand.
//   dealer_hand_df   - R data.frame with the dealer's current hand.
//   card_counts_r    - Integer vector of remaining card counts by value.
//   actions          - Character vector of actions to evaluate
//                      (e.g., "stand", "hit", "double", "surrender", "insure").
//
// [[Rcpp::export]]
Rcpp::List get_specific_evs_rcpp(Rcpp::List rules_obj,
                           Rcpp::DataFrame player_hand_df,
                           Rcpp::DataFrame dealer_hand_df,
                           Rcpp::IntegerVector card_counts_r,
                           Rcpp::CharacterVector actions) {

  // Parse blackjack rules from R into a C++ rules struct
  BlackjackRules rules = parse_rules(rules_obj);
  // Convert R data.frames into C++ card vectors
  std::vector<Card> player_hand = df_to_cards(player_hand_df);
  std::vector<Card> dealer_hand = df_to_cards(dealer_hand_df);
  // Copy remaining card counts into a fixed-size C++ array
  std::array<int, 12> card_counts;
  for(int i = 0; i < 12; ++i) card_counts[i] = card_counts_r[i];

  // Container for EV results keyed by action name
  Rcpp::List ev_results;
  HandVal player_hv = evaluate_hand_c(player_hand);

  // Loop over requested actions and compute EV for each
  for(int i = 0; i < actions.size(); ++i) {
    std::string action = Rcpp::as<std::string>(actions[i]);

    if (action == "stand") {
      // EV if the player stands immediately
      ev_results["stand"] = eval_stand_c(dealer_hand, player_hv.total,
                                          card_counts, rules);
    }
    else if (action == "hit") {
      // EV if the player hits and then plays optimally
      ev_results["hit"] = eval_hit_c(dealer_hand, player_hand, card_counts, rules);
    }
    else if (action == "double") {
      // EV if the player doubles down (one card then stand)
      ev_results["double"] = eval_double_c(dealer_hand, player_hand, card_counts, rules);
    }
    else if (action == "surrender") {
      // EV of surrender (fixed at -0.5 units)
      ev_results["surrender"] = eval_surrender_c();
    }
    else if (action == "insure") {
      // EV of taking insurance given remaining card composition
      ev_results["insure"] = eval_insurance_c(card_counts_r);
    }
  }

  return ev_results;
}


