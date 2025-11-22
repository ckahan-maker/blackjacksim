#' Generate a Shuffled Blackjack Shoe
#'
#' Creates a randomized multi-deck shoe of playing cards using standard 52-card decks.
#' The resulting data frame includes the card's rank, suit (using Unicode symbols), and Blackjack point value.
#'
#' @param num_decks Integer. The number of 52-card decks to combine into the shoe.
#'
#' @return A data frame with \code{52 * num_decks} rows and 3 columns:
#' \itemize{
#'   \item \code{rank}: Character (e.g., "A", "K", "7").
#'   \item \code{suit}: Character (Unicode symbols: "♠", "♥", "♦", "♣").
#'   \item \code{value}: Numeric point value (Ace = 11, Face cards = 10, Number cards = face value).
#' }
#' @export
#'
#' @examples
#' # 1. Create a standard 6-deck shoe
#' shoe <- create_shoe(num_decks = 6)
#'
#' # View the top 5 cards
#' head(shoe)
#'
#' # Check total cards (should be 312)
#' nrow(shoe)
create_shoe <- function(num_decks){
  suits <- c("♠", "♥", "♦", "♣")
  ranks <- c("A", "J", "Q", "K", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  values <- c(11, rep(10, 3), 2:10)
  base_deck <- expand.grid(rank = ranks, suit = suits, stringsAsFactors = FALSE)

  names(values) <- ranks
  base_deck$value <- values[base_deck$rank]

  shoe <- base_deck[rep(1:52, times = num_decks), ]
  shuffled_shoe <- shoe[sample(nrow(shoe)), ]
  rownames(shuffled_shoe) <- NULL

  return(shuffled_shoe)
}
