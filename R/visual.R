library(grid)

#' Draw a Playing Card
#'
#' Draws a single playing card at a given position on the screen. The card
#' shows its rank in the top-left and bottom-right corners, and the suit
#' symbol in the center. Hearts and diamonds are drawn in red; clubs and
#' spades are drawn in black.
#'
#' @param rank Character. The rank of the card (e.g., `"A"`, `"K"`, `"7"`).
#' @param suit Character. The suit symbol (e.g., `"♠"`, `"♥"`, `"♦"`, `"♣"`).
#' @param x Numeric. X-coordinate for the center of the card (in NPC units).
#' @param y Numeric. Y-coordinate for the center of the card (in NPC units).
#' @param width Numeric. The width of the card as a proportion of the drawing area.
#'   Default is \code{0.12}.
#' @param height Numeric. The height of the card as a proportion of the drawing area.
#'   Default is \code{0.18}.
#'
#' @return Invisibly returns \code{NULL}. The card is drawn as a side effect.
#'
#' @keywords internal
#'
#' @examples
#' grid::grid.newpage()
#' draw_card("A", "♠", x = 0.5, y = 0.5)
#'
draw_card <- function(rank, suit, x, y,
                      width = 0.12, height = 0.18) {

  # background color of the card
  fill_col <- "white"

  # draw card outline
  grid.rect(
    x = x, y = y, width = width, height = height,
    gp = gpar(col = "black", fill = fill_col, lwd = 1.5)
  )

  # diamonds/hearts are red; clubs/spades are black
  col <- if (suit %in% c("♥", "♦")) "red" else "black"

  # spacing from edges for the corner rank text
  text_padding <- 0.005

  # rank: top-left corner
  grid.text(
    rank,
    x = x - width/2 + text_padding,
    y = y + height/2 - text_padding,
    just = c("left", "top"),
    gp = gpar(col = col, fontsize = 10, fontface = "bold")
  )

  # rank: bottom-right corner
  grid.text(
    rank,
    x = x + width/2 - text_padding,
    y = y - height/2 + text_padding,
    just = c("right", "bottom"),
    gp = gpar(col = col, fontsize = 10, fontface = "bold")
  )

  # suit symbol in the center
  grid.text(
    suit,
    x = x, y = y,
    gp = gpar(col = col, fontsize = 20)
  )
}

