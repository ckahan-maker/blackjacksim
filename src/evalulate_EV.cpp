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
