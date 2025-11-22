#' Define Blackjack Table Configuration
#'
#' Creates a configuration object specifying the rules and mechanics for a Blackjack game simulation.
#' This includes dealer behavior, payout ratios, deck penetration, and allowed player actions.
#'
#' @param payout Numeric. The payout ratio for a natural Blackjack. Common values are 1.5 (3:2) or 1.2 (6:5). Default is \code{1.5}.
#' @param dealer_stands_soft_17 Logical. If \code{TRUE}, the dealer must stand on a Soft 17 (A-6). If \code{FALSE}, the dealer hits on Soft 17. Default is \code{TRUE}.
#' @param num_decks Integer. The number of 52-card decks used in the shoe. Default is \code{6}.
#' @param penetration Numeric. A value between 0 and 1 representing the percentage of the shoe dealt before reshuffling. Default is \code{0.75}.
#' @param burn_cards Integer. The number of cards discarded from the top of the shoe after a shuffle. Default is \code{1}.
#' @param allow_insurance Logical. Whether the player is offered Insurance when the dealer shows an Ace. Default is \code{TRUE}.
#' @param surrender Character. Options are \code{"none"}, \code{"early"}, or \code{"late"}. Defines if/when a player can forfeit their hand for half their bet. Default is \code{"none"}.
#' @param dealer_peeks Logical. If \code{TRUE}, the dealer checks their hole card for Blackjack before the player acts. Default is \code{TRUE}.
#' @param european_no_hole Logical. If \code{TRUE}, the game is played with "European No Hole Card" rules (dealer receives no second card until player finishes). Default is \code{FALSE}.
#' @param double_on Character. Specifies which starting totals allow doubling down. Options: \code{"any"}, \code{"9,10,11"}, or \code{"10,11"}. Default is \code{"any"}.
#' @param double_after_split Logical. If \code{TRUE}, doubling down is permitted after splitting a pair. Default is \code{TRUE}.
#' @param max_splits Integer. The maximum number of times a hand can be split. Default is \code{3} (allowing up to 4 hands).
#' @param resplit_aces Logical. If \code{TRUE}, a player can split Aces again if they draw another Ace. Default is \code{FALSE}.
#' @param hit_split_aces Logical. If \code{TRUE}, a player can hit after splitting Aces (usually \code{FALSE}, meaning split Aces get only one card). Default is \code{FALSE}.
#' @param allow_blackjack_after_split Logical. If \code{TRUE}, a 10-value card dealt to a split Ace counts as a natural Blackjack (usually \code{FALSE}, counting as a standard 21). Default is \code{FALSE}.
#'
#' @return A list of class \code{"blackjack_rules"} containing the specified game parameters
#' @export
#'
#' @examples
#' #1. Standard Vegas Strip Rules (Dealer hits Soft 17, Late Surrender allowed)
#' vegas_rules <- blackjack_rules(
#'   num_decks = 6,
#'   dealer_stands_soft_17 = FALSE,
#'   surrender = "late",
#'   payout = 1.5
#' )
#' # 2. Sleazy Casino Rules (High House Edge)
#' # Features: 6:5 payout, 8 decks, H17, restricted doubling, bad penetration
#' sleazy_casino <- blackjack_rules(
#'   payout = 1.2,                 # 6:5 Payout (Massive house edge increase)
#'   dealer_stands_soft_17 = FALSE,# Dealer hits Soft 17
#'   num_decks = 8,                # 8 Decks makes counting harder
#'   penetration = 0.5,            # 50% cut card (terrible for counting)
#'   double_on = "10,11",          # Can only double on hard 10 or 11
#'   double_after_split = FALSE,   # No doubling after splitting
#'   surrender = "none",           # Forced to play out bad hands
#' )
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

