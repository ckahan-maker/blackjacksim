#include "blackjack.h"

 /* Converts an R 'blackjack_rules' S3 object (which is internally a List)
 * into a C++ BlackjackRules struct.
 */
BlackjackRules parse_rules(Rcpp::List rules) {
  BlackjackRules rules_c;

  rules_c.dealer_stands_soft_17 = Rcpp::as<bool>(rules["dealer_stands_soft_17"]);
  rules_c.num_decks             = Rcpp::as<int>(rules["num_decks"]);
  rules_c.allow_insurance       = Rcpp::as<bool>(rules["allow_insurance"]);
  rules_c.dealer_peeks          = Rcpp::as<bool>(rules["dealer_peeks"]);
  rules_c.double_after_split    = Rcpp::as<bool>(rules["double_after_split"]);
  rules_c.max_splits            = Rcpp::as<int>(rules["max_splits"]);
  rules_c.resplit_aces          = Rcpp::as<bool>(rules["resplit_aces"]);
  rules_c.hit_split_aces        = Rcpp::as<bool>(rules["hit_split_aces"]);

  std::string double_val = Rcpp::as<std::string>(rules["double_on"]);
  if (double_val == "any") {
    rules_c.double_on = DoubleRule::ANY;
  } else if (double_val == "9,10,11") {
    rules_c.double_on = DoubleRule::NINE_TEN_ELEVEN;
  } else if (double_val == "10,11") {
    rules_c.double_on = DoubleRule::TEN_ELEVEN;
  }

  return rules_c;
}

// Convert an R data frame of cards into a C++ vector of Card objects.
//
// Parameters:
//   df - An R data.frame representing a hand or set of cards. Expected to contain
//   columns: rank (character), suit (character), value (integer)
//
// Returns:
//   A std::vector<Card> containing the converted cards.
//
std::vector<Card> df_to_cards(Rcpp::DataFrame df) {
  // Extract columns from the DataFrame as Rcpp vectors
  Rcpp::CharacterVector ranks = df["rank"];
  Rcpp::CharacterVector suits = df["suit"];
  Rcpp::IntegerVector values  = df["value"];

  int n = df.nrows();
  std::vector<Card> hand;
  hand.reserve(n);

  // Build Card objects row by row
  for(int i = 0; i < n; ++i) {
    Card c;
    c.rank  = Rcpp::as<std::string>(ranks[i]);
    c.suit  = Rcpp::as<std::string>(suits[i]);
    c.value = values[i];
    hand.push_back(c);
  }

  return hand;
}

// Determine whether the player may double down.
//
// Parameters:
//   player_hand - The player's current hand (must contain exactly two cards).
//   rules - Blackjack table rules.
//   split_hand - Indicates whether this hand resulted from a split.
//
// Returns:
//   true  - if doubling is allowed under the current rules
//   false - otherwise
//
bool can_double_c(const std::vector<Card>& player_hand,
                  const BlackjackRules& rules,
                  bool split_hand) {

  if (player_hand.size() != 2) return false;
  if (split_hand && !rules.double_after_split) return false;

  HandVal hv = evaluate_hand_c(player_hand);
  bool is_hard = !hv.soft;

  switch (rules.double_on) {
    case DoubleRule::ANY:
      return true;
    case DoubleRule::NINE_TEN_ELEVEN:
      return is_hard && (hv.total >= 9 && hv.total <= 11);
    case DoubleRule::TEN_ELEVEN:
      return is_hard && (hv.total == 10 || hv.total == 11);
    default:
      return false;
  }
}

// Checks if a player is allowed to take another card (Hit)
//
// Parameters:
//   player_hand - The player's current hand
//   rules - Blackjack table rules.
//   split_aces - Indicates if this hand resulted from splitting Aces.
//
// Returns:
//   true  - if hitting is allowed under the current rules
//   false - otherwise
//
bool can_hit_c(const std::vector<Card>& player_hand,
               const BlackjackRules& rules,
               bool split_aces) {

  // If the hand came from split Aces and the rule 'hit_split_aces' is false,
  // the player is stuck with whatever they were dealt.
  if (split_aces && !rules.hit_split_aces) {
    return false;
  }

  return true;
}
