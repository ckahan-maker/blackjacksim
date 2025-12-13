library(grid)

#' Draw a Playing Card
#'
#' Draws a single playing card at a given position on the screen. The card
#' shows its rank in the top-left and bottom-right corners, and the suit
#' symbol in the center. Hearts and diamonds are drawn in red; clubs and
#' spades are drawn in black. If \code{face_down = TRUE}, a
#' diamond-patterned card back is drawn instead.
#'
#' @param rank Character. The rank of the card (e.g., `"A"`, `"K"`, `"7"`).
#' @param suit Character. The suit symbol (e.g., `"♠"`, `"♥"`, `"♦"`, `"♣"`).
#' @param x Numeric. X-coordinate for the center of the card (in NPC units).
#' @param y Numeric. Y-coordinate for the center of the card (in NPC units).
#' @param width Numeric. The width of the card as a proportion of the drawing area.
#'   Default is \code{0.12}.
#' @param height Numeric. The height of the card as a proportion of the drawing area.
#'   Default is \code{0.18}.
#' @param face_down Logical. If \code{TRUE}, draws the back of the card with a
#'   diamond pattern. If \code{FALSE}, draws the card face. Default is \code{FALSE}.
#' @return Invisibly returns \code{NULL}. The card is drawn as a side effect.
#'
#' @keywords internal
#'
#' @examples
#' grid::grid.newpage()
#' draw_card("A", "♠", x = 0.5, y = 0.5)
#'
draw_card <- function(rank, suit, x, y,
                      width = 0.12, height = 0.18, face_down = FALSE) {

  # background color of the card
  fill_col <- "white"

  # draw card outline
  grid.rect(
    x = x, y = y, width = width, height = height,
    gp = gpar(col = "black", fill = fill_col, lwd = 1.5)
  )
  if (face_down) {
    # Determine number of diamonds on back of card horizontally and vertically
    diam_h <- 15
    diam_v <- 40
    # Work in card-local coordinates and make sure diamonds don't extend past the border of the card
    pushViewport(viewport(x = x, y = y, width = width, height = height, clip = "on"))
    # Create sequence of diamonds on back of card
    xs <- seq(0, 1, length.out = diam_h)
    ys <- seq(0, 1, length.out = diam_v)
    # generate coordinates of all diamonds on back of card
    diamond_pts <- expand.grid(x = xs, y = ys)
    # Identify row indices to create a staggered/offset effect
    row_idx <- rep(seq_len(diam_v), each = diam_h)
    # Calculate horizontal offset (half the distance between diamonds)
    x_shift <- (diamond_pts$x[2] - diamond_pts$x[1]) / 2
    # Shift even-numbered rows to create diagonal lattice pattern
    diamond_pts$x[row_idx %% 2 == 0] <- diamond_pts$x[row_idx %% 2 == 0] + x_shift
    # Render the diamond pattern
    grid.points(
      x = diamond_pts$x,
      y = diamond_pts$y,
      pch = 18, # the diamond character
      size = unit(1/diam_h, "npc"),
      gp = gpar(col = "darkred")
    )
    # Restore the previous plotting state
    popViewport()

    return(invisible(NULL))
  }

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

draw_hand <- function(hand){

}
