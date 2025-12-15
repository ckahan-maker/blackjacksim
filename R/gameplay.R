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
#' under the specified blackjack table rules.
#'
#' @param rules A \code{blackjack_rules} object specifying the table rules.
#' @param hand Data frame representing the player's current hand. Must contain
#'   exactly two cards for insurance to be allowed.
#' @param num_hands Integer. Total number of player hands currently in play.
#'   If greater than 1 (i.e., after a split), insurance is not allowed.
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
can_insure <- function(rules, hand, num_hands) {
  if (!allow_insurance) return(FALSE)
  if (nrow(hand) != 2) return(FALSE) # cannot insure after drawing a card
  if (num_hands > 1) return(FALSE) # no insurance after a split
  return(TRUE)
}
