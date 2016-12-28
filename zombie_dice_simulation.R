# Zombie Dice Simulation
# Justin Meyer
# 11.17.15

# NOTES: 
# 1. THIS WORKS BUT DOESN'T ACCURATELY SIMULATE ZOMBIE DICE
# IT DOESN'T ACCOUNT FOR DICE BEING HELD OUT AFTER THEY ROLL A BRAIN OR SHOTGUN
# 2. ALSO, MORE STRATEGIES NEED TO BE ADDED. THE CURRENT STRATEGY IS TO ROLL
# UNTIL LOSING (3 SHOTGUNS) OR WINNING (13 BRAINS) THE GAME.

# Set strategy (this part doesn't work)
# 1 = no strategy, continue rolling until win or lose
# 2 = stop round if one brain
# 3 = stop round if two brains
# 4 = stop round if one shotgun
# 5 = stop round if two shotguns
# strategy <- 1

# Set the number of games to play
games <- 10000

# Create a data frame to store the game results in
game_results <- as.data.frame(matrix(NA, games, 2))

library(data.table)
setnames(game_results, c("V1", "V2"), c("outcome", "round"))

game_results$outcome <- as.factor(game_results$outcome)
levels(game_results$outcome) <- c("lost", "won")

# Create a loop that simulates the specified number of games

for (i in 1:games) {

        print(paste0("############ GAME: ", i, " STARTED ############"))
        
        # Create a loop that runs as long as the game hasn't been won or lost
        round <- 0
        count_brains <- 0
        count_shotguns <- 0
        
        while (count_brains < 13 & count_shotguns < 3) {
        
                # Create matrix representing the 13 dice
                # Brain = 1, runner = 0, shotgun = -1
                dice <- matrix(NA, 13, 6)
                
                dice[1, ] <- c(1, 1, 1, 0, 0, -1) # green
                dice[2, ] <- c(1, 1, 1, 0, 0, -1) # green
                dice[3, ] <- c(1, 1, 1, 0, 0, -1) # green
                dice[4, ] <- c(1, 1, 1, 0, 0, -1) # green
                dice[5, ] <- c(1, 1, 1, 0, 0, -1) # green
                dice[6, ] <- c(1, 1, 1, 0, 0, -1) # green
                dice[7, ] <- c(1, 1, 0, 0, -1, -1) # yellow
                dice[8, ] <- c(1, 1, 0, 0, -1, -1) # yellow
                dice[9, ] <- c(1, 1, 0, 0, -1, -1) # yellow
                dice[10, ] <- c(1, 1, 0, 0, -1, -1) # yellow
                dice[11, ] <- c(1, 0, 0, -1, -1, -1) # red
                dice[12, ] <- c(1, 0, 0, -1, -1, -1) # red
                dice[13, ] <- c(1, 0, 0, -1, -1, -1) # red
                
                # Simulate a round by randomly drawing three dice from the 13
                selected_dice <- dice[sample(1:nrow(dice), 3, replace = FALSE),]
                
                # Simulate rolling the three selected dice
                roll_result_1 <- sample(selected_dice[1, ], 1)
                roll_result_2 <- sample(selected_dice[2, ], 1)
                roll_result_3 <- sample(selected_dice[3, ], 1)
                rm(selected_dice)
                
                # Combine the results
                round_results <- as.data.frame(rbind(roll_result_1, roll_result_2, roll_result_3))
                rm(roll_result_1, roll_result_2, roll_result_3)
                
                # Determine if the player won or lost
                round_results$count <- 1
                round_brains <- sum(round_results$count[round_results$V1 == 1])
                count_brains <- count_brains + round_brains
        
                round_shotguns <- sum(round_results$count[round_results$V1 == -1])
                count_shotguns <- count_shotguns + round_shotguns
        
                rm(round_brains, round_shotguns)
                        
                # Add one to round and print number of rounds
                round <- round + 1
                print(paste0("###### Round: ", round, " ######"))
                
                # Report brains count and shotguns count
                print(paste0("Brains: ", count_brains))
                print(paste0("Shotguns: ", count_shotguns))
                
                # Report status of game at end of each round
                if(count_brains >= 13 & count_shotguns < 3){print(paste0("Won in ", round, " rounds."))
                                                          game_results[i, 1] <- "won"
                                                          game_results[i, 2] <- round}
                
                else if(count_shotguns >= 3){print(paste0("Lost in ", round, " rounds."))
                                             game_results[i, 1] <- "lost"
                                             game_results[i, 2] <- round}
                else {print(paste0("Play another round."))}
                
                rm(round_results)
                
                }
        
        print(paste0("############ GAME: ", i, " COMPLETE. ############"))
        
        }

summary(game_results)
table(game_results$round)



