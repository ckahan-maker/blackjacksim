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
