blackjack_rules = function(
  payout = 1.5, # 3:2 payout
  dealer_stands_soft_17 = TRUE, # does dealer stand or hit on soft 17
  num_decks = 6, # number of decks used by dealer
  penetration = 0.75, # percentage of cards in deck before dealer reshuffles
  burn_cards = 1, # number of top cards discarded after reshuffle
  allow_insurance = TRUE, # whether user is allowed to make a sidebet about dealer having blackjack
  surrender = "none", # allowed options: early or late (depends on whether dealer can look at hole card)
  dealer_peeks = TRUE, # is dealer allowed to peak at hole card to check for blackjack
  european_no_hole = FALSE, # is the game played according to european rules: no hole card
  double_on = "any",
  double_after_split = TRUE,
  max_splits = 3,
  resplit_aces = FALSE,
  hit_split_aces = FALSE,
  allow_blackjack_after_split = FALSE # after splitting, can you get a blackjack payout
) {
  # Validate numeric inputs
  if (num_decks < 1) stop("num_decks must be at least 1")
  if (penetration <= 0 || penetration >= 1) stop("penetration must be between 0 and 1")
  # Validate string choices
  valid_surrender <- c("none", "early", "late")
  surrender <- match.arg(surrender, valid_surrender)
  valid_double <- c("any", "9,10,11", "10,11")
  double_on <- match.arg(double_on, valid_double)
  # Check that choices are compatible
  if (european_no_hole && dealer_peeks) {
    stop("Conflict: 'dealer_peeks' cannot be TRUE when 'european_no_hole' is TRUE (there is no card to peek at).")
  }

  if (european_no_hole && surrender != "none") {
    stop("Conflict: Surrender is not allowed under European No Hole rules.")
  }
  rules_obj <- structure(
    list(
      payout = payout,
      dealer_stands_soft_17 = dealer_stands_soft_17,
      num_decks = num_decks,
      penetration = penetration,
      burn_cards = burn_cards,
      allow_insurance = allow_insurance,
      surrender = surrender,
      dealer_peeks = dealer_peeks,
      european_no_hole = european_no_hole,
      double_on = double_on,
      double_after_split = double_after_split,
      max_splits = max_splits,
      resplit_aces = resplit_aces,
      hit_split_aces = hit_split_aces,
      allow_blackjack_after_split = allow_blackjack_after_split
    ),
    class = "blackjack_rules"
  )
  return(rules_obj)
}

