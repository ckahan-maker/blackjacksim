#' Evaluate Blackjack Hand Value and State
#'
#' Calculates the total point value of a hand, automatically adjusting Aces
#' from 11 to 1 if the total exceeds 21. It provides the numeric score,
#' the "Soft/Hard" status, and a formatted string code.
#'
#' @param hand A data frame of cards (subset of the shoe) containing `rank` and `value` columns.
#'
#' @return A named list containing:
#' \describe{
#'   \item{total}{Numeric. The best possible point total (e.g., 21, 17, 12).}
#'   \item{soft}{Logical. \code{TRUE} if the hand contains an Ace counted as 11 (Soft), \code{FALSE} otherwise.}
#'   \item{code}{Character. A string representation for display (e.g., "S17" for Soft 17, "H12" for Hard 12).}
#' }
#' @export
#'
#' @examples
#' # 1. Hard Hand (10 + 7)
#' # Returns: list(total=17, soft=FALSE, code="H17")
#' evaluate_hand(hand_df)
#'
#' # 2. Soft Hand (Ace + 6)
#' # Returns: list(total=17, soft=TRUE, code="S17")
#' evaluate_hand(soft_df)
evaluate_hand <- function(hand) {
  # count how many Aces are in the hand
  num_aces <- sum(hand$rank == "A")
  # sum the raw values (assuming Aces are 11 initially)
  total_value <- sum(hand$value)

  while (total_value > 21 && num_aces > 0) {
  # If the hand is busted (>21) and contains Aces valued at 11, reduce them to 1
    total_value <- total_value - 10 # Turn an 11 into a 1
    num_aces <- num_aces - 1
  }

  # If we still have an ace counting as 11, it is Soft
  is_soft <- (num_aces > 0)

  # Create a readable string for printing
  code <- paste0(if(is_soft) "S" else "H", total_value)

  return(list(
    total = total_value,  # Use this for checking > 21
    soft = is_soft,       # Use this for strategy (S17 vs H17)
    code = code           # Use this for printing ("S17")
  ))
}

#' Determine Whether a Hand Is a Natural Blackjack
#'
#' @param hand A data frame representing a player's hand. It must contain at
#'   least two columns:
#'   \describe{
#'     \item{\code{rank}}{A character vector giving card ranks (e.g., "A", "K", "7").}
#'     \item{\code{value}}{A numeric vector giving card values with Aces counted as 11.}
#'   }
#'
#' @return A logical value: \code{TRUE} if the hand is a natural blackjack,
#'   \code{FALSE} otherwise.
#'
#' @examples
#' hand <- data.frame(rank = c("A", "K"), value = c(11, 10))
#' is_blackjack(hand)  # TRUE
#'
#' hand2 <- data.frame(rank = c("10", "5", "6"), value = c(10, 5, 6))
#' is_blackjack(hand2) # FALSE
#'
#' @keywords internal
is_blackjack <- function(hand) {
  num_aces <- sum(hand$rank == "A")
  total_value <- sum(hand$value)

  # A natural blackjack occurs when the hand has exactly two cards,
  # one of which is an Ace, and their total value is 21.
  return(nrow(hand) == 2 && num_aces == 1 && total_value == 21)
}
