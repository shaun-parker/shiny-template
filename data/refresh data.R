numbers = 1:37
n_games = 100

starting_money = 1000
stake = 10
odds = 35
win = stake * odds + stake

number = 32


fn <- function(n_games) {
  
  df = data.frame("game" = as.integer(),
                  "winning_number" = as.integer(),
                  current_money = as.integer())
  
  for (i in 1:n_games) {
    current_game = i # game number
    current_winner = sample(x = numbers, size = 1) # what number came in?
    
    
    df <- rbind(df, data.frame(current_game, current_winner, "current_money" = 0))
  }
  
  # Calculate first row
  df[1, 3] <- ifelse(
    df[1, 2] == number,
    starting_money + win,
    starting_money - stake
  )
  
  for (i in 2:nrow(df)) {
    if (df[i, 2] == number) {
      df[i, 3] <- df[i - 1, 3] + win
    } else { 
      df[i, 3] <- df[i - 1, 3] - stake
    }
  }
  
  return(df)
}

n_games = 10000
n_simulations = 100

final_df <- lapply(rep(n_games, n_simulations), fn)

final_df <- do.call(rbind, final_df)
final_df$simulation <- rep(1:n_simulations, times = 1, each = n_games)

saveRDS(final_df, "./data/data.rds")
