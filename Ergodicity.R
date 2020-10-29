#### Ergodicity ####
# a game give 50% chance of 50% gain and 50% chance of 40% loss

# 1. one individual play the game for 365 round with initial value of 1
# sample the 365 rounds of the game: 1 for gain and 0 for loss
gain_or_loss <- rbinom(365, 1, 0.5)
# outcome for ONE individual play the game for 365 rounds
outcome <- 1.5 ^ (sum(gain_or_loss)) * 0.6 ^ (365 -sum(gain_or_loss))


# 2. many people play the game
gain_or_loss_many <- rbinom(1000, 365, 0.5)
outcome_many <- 1.5 ^ (gain_or_loss_many) * 0.6 ^ (365 - gain_or_loss_many)
summary(outcome_many)

# rbinom can be use to simulate one bernoulli or many bernoulli experiments, 
# see: http://www.programmingr.com/examples/neat-tricks/sample-r-function/r-rbinom/


# 3. how many people we need to 
# make the average across people converge to the expectatino 
# encapsulate the process into a function 
outcome_many <- function(people, round, prob = 0.5, gain, loss) {
  gain_or_loss_many <- rbinom(people, round, prob)
  outcome_many <- (1 + gain) ^ (gain_or_loss_many) * (1 - loss) ^ (round - gain_or_loss_many)
  return(outcome_many)
}

a <- outcome_many(2000000, 52, 0.5, 0.8, 0.6)

# trying with differnt # of people, we find that as the # of people increases, the mean outcome also increase, 
# moving closer to the expectation from the lower values.
# thus, we do need enough people to experience all possible scenarios so that the average statistics give us the expectation

# to reduce the computation load, we could reduce the number of round

summary(a)
hist(log(a))
