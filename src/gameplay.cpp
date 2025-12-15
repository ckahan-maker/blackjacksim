#include "blackjack.h"
#include <Rcpp.h>
using namespace Rcpp;


 /* Converts an R 'blackjack_rules' S3 object (which is internally a List)
 * into a C++ BlackjackRules struct.
 */
BlackjackRules parse_rules(List rules) {
  BlackjackRules rules_c;

  rules_c.dealer_stands_soft_17 = as<bool>(rules["dealer_stands_soft_17"]);
  rules_c.num_decks             = as<int>(rules["num_decks"]);
  rules_c.allow_insurance       = as<bool>(rules["allow_insurance"]);
  rules_c.dealer_peeks          = as<bool>(rules["dealer_peeks"]);
  rules_c.double_after_split    = as<bool>(rules["double_after_split"]);
  rules_c.max_splits            = as<int>(rules["max_splits"]);
  rules_c.resplit_aces          = as<bool>(rules["resplit_aces"]);
  rules_c.hit_split_aces        = as<bool>(rules["hit_split_aces"]);

  std::string double_val = as<std::string>(rules["double_on"]);
  if (double_val == "any") {
    rules_c.double_on = DoubleRule::ANY;
  } else if (double_val == "9,10,11") {
    rules_c.double_on = DoubleRule::NINE_TEN_ELEVEN;
  } else if (double_val == "10,11") {
    rules_c.double_on = DoubleRule::TEN_ELEVEN;
  }

  return rules_c;
}
