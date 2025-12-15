#' Check Whether Surrender Is Allowed
#'
#' Determines whether the player is permitted to surrender the current hand
#' under the specified blackjack table rules.
#'
#' @param rules A \code{blackjack_rules} object specifying the table rules.
#' @param hand Data frame representing the player's current hand. Must contain
#'   exactly two cards for surrender to be allowed.
#' @param num_hands Integer. Total number of player hands currently in play.
#'   If greater than 1 (i.e., after a split), surrender is not allowed.
#'
#' @return Logical. Returns \code{TRUE} if surrender is allowed in the current
#'   game state, and \code{FALSE} otherwise.
#'
#' @keywords internal
#'
#' @examples
#' rules <- blackjack_rules(surrender = "late")
#'
#' hand <- data.frame(
#'   rank = c("10", "6"),
#'   suit = c("♠", "♠"),
#'   stringsAsFactors = FALSE
#' )
#'
#' can_surrender(rules, hand, num_hands = 1)
#'
can_surrender <- function(rules, hand, num_hands) {
  if (rules$surrender == "none") return(FALSE)
  if (nrow(hand) != 2) return(FALSE)      # cannot surrender after drawing a card
  if (num_hands > 1) return(FALSE)        # no surrender after a split
  return(TRUE)
}

#' Check Whether Insurance Is Allowed
#'
#' Determines whether the player is permitted to insure the current hand
#' under the specified blackjack table rules. Insurance is only offered when the dealer's
#' upcard is an Ace and must be taken before any player actions.
#'
#' @param rules A \code{blackjack_rules} object specifying the table rules.
#' @param hand Data frame representing the player's current hand. Must contain
#'   exactly two cards for insurance to be allowed.
#' @param dealer_upcard_rank Character. Rank of the dealer's visible upcard
#'   (e.g., \code{"A"}, \code{"10"}).
#' @param num_hands Integer. Total number of player hands currently in play.
#'   If greater than 1 (i.e., after a split), insurance is not allowed.
#' @param insurance_taken Logical. Indicates whether the player has already
#'   taken (or declined) insurance for the current hand. If \code{TRUE},
#'   insurance is no longer available.
#'
#' @return Logical. Returns \code{TRUE} if insurance is allowed in the current
#'   game state, and \code{FALSE} otherwise.
#'
#' @keywords internal
#'
#' @examples
#' rules <- blackjack_rules(allow_insurance = TRUE)
#'
#' hand <- data.frame(
#'   rank = c("10", "6"),
#'   suit = c("♠", "♠"),
#'   stringsAsFactors = FALSE
#' )
#'
#' can_insure(rules, hand, num_hands = 1)
#'
can_insure <- function(rules, hand, dealer_upcard_rank,
                       num_hands, insurance_taken = FALSE) {
  if (!rules$allow_insurance) return(FALSE)
  if (insurance_taken) return(FALSE) # you can't take insurance twice
  if (dealer_upcard_rank != "A") return(FALSE)  # insurance only vs dealer Ace
  if (nrow(hand) != 2) return(FALSE) # cannot insure after drawing a card
  if (num_hands > 1) return(FALSE) # no insurance after a split
  return(TRUE)
}
