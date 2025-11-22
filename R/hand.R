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
