##########################################################################
### 2020 IM Data Conference: Pre-conference Training
### Why Data Analytics Can Be Misleading: Context, Method, and Validity
### Zhen (Richard) Tang, zhen.tang@lmu.edu, Loyola Marymount University
##########################################################################
library("dplyr")
library("ggplot2")

############ The Simpson's Paradox ############
# Assuming we want to study the relation between workload and happiness
# Simulate one hypothetical reality
# setup basic parameters
sample_size = 100
low_hour_range = 40
high_hour_range = 80
low_base = 100
high_base = 300
true_beta = -2
error = rnorm(sample_size, mean = 0, sd = 10)

# simulate low workload group
hours <- sample.int(low_hour_range, size = sample_size, replace = T)
happiness <- low_base + true_beta * hours + error
low_group <- as.data.frame(cbind(happiness, hours))
low_group$group <- c("low")

# simulate high workload group
hours <- hours + (high_hour_range - low_hour_range)
happiness <- high_base + true_beta * hours + error
high_group <- as.data.frame(cbind(happiness, hours))
high_group$group <- c("high")

# the full dataset
happiness_and_work <- rbind(low_group, high_group)

# visulize the simposon's paradox
ggplot(data = happiness_and_work, aes(x = hours, y = happiness)) + geom_point(aes(color = group)) + 
  geom_smooth(method = "lm", se = F) + geom_smooth(method = "lm", se = F, aes(color = group))


# solve the Paradox by adding group fixed-effect
# a wrong model, suggesting more working hours lead to more happiness  :-)
summary(lm(happiness ~ hours, data = happiness_and_work))
# a correct model, suggesting the opposite, which is consistent with the reality we simulated. 
summary(lm(happiness ~ hours + group, data = happiness_and_work))





############ The Selction Effect ############
######## ___Generate Cadidate Pool ########
# generate Leadership Score from a normal distribution with mean = 80, s.d. = 10
Leadership <- round(rnorm(200, 80, 10), digit = 0)
summary(Leadership)
hist(Leadership)

# generate I.Q. Score with a normal distribution with mean = 100, s.d. = 10
I.Q. <-  round(rnorm(200, 100, 10), digit = 0)
hist(I.Q.)

# note that the leadership and I.Q. scores are drawn from two independent simulation
# thus, the two scores are independent too. 
cor.test(Leadership, I.Q.)

# generate Total.Score
Total.Score = Leadership + I.Q.
hist(Total.Score)
# note that the sum of two independent normal distributions is still a normal distribution


# generate Candidate.ID
Candidate.ID <- c(1:200)
# combine information into data.frame
Candidates <- as.data.frame(cbind(Candidate.ID, Leadership, I.Q., Total.Score))


# sava a sample of candidates to excel
# write.csv(Candidates[c(1:15),] ,"Candidate Pool.csv")

# visualize the original candidate pool
ggplot(Candidates, aes(x = Leadership, y = I.Q.)) + geom_point() + geom_smooth(method = "lm", se = F )
# key: there is not correlation between the values of Leadership and I.Q.


######## ___Hiring Decisions ########
# simulate hiring decisions according to the total score of Leadership and I.Q.
Candidates <- Candidates %>% mutate (Hired = ifelse(Total.Score > 190, "Yes", "No")) %>% 
  mutate(Hired_at_tier = ifelse(Total.Score > 190, "One (> 190)", ifelse(Total.Score > 170, "Two (> 170)", "Three (the rest)"))) %>%
  mutate(Hired_at_tier = factor(Hired_at_tier, levels = c("One (> 190)", "Two (> 170)", "Three (the rest)")))


######## Visualizing the Selection Effect  
# superfical negative correlation between Leadership and I.Q. among those who are hired
ggplot(Candidates, aes(x = Leadership, y = I.Q., color = Hired)) + geom_point() +  
  geom_smooth(data = Candidates %>% filter(Hired =="Yes"), method = "lm", se = F ) + scale_color_manual(values = c("green", "red"))

# superfical negative correlation between Leadership and I.Q. in EVERY tier of the companies
ggplot(Candidates, aes(x = Leadership, y = I.Q., color = Hired_at_tier)) + geom_point() +  
  geom_smooth(data = Candidates, method = "lm", se = F ) 

######## ___More data is better? ####
# we know that in the reality we simulated, I.Q. and Leadership is independent
# the  model below is a correct model, regrssing Leadership on I.Q. only
summary(lm(Leadership ~ I.Q., data = Candidates))
# the model below is a wrong one, putting an extra variable totally biased the reality,
# suggesting a negative relationship between I.Q. and Leadership.
# Therefore, more data/variables might not always be good
summary(lm(Leadership ~ I.Q. + Hired_at_tier, data = Candidates))



######## ___One Interesting Extension: Simpson's Paradox ####
# assuming you got the model right, but if you did not sample correctly, your analytics still failed
# if you using only highest and lowest tier companies, you will see simpson's paradox
ggplot(data = Candidates %>% filter(Hired_at_tier == "One (> 190)" | Hired_at_tier == "Three (the rest)"), aes(x = Leadership, y = I.Q.)) + 
  geom_point(aes(color = Hired_at_tier)) +  geom_smooth( method = "lm", se = F ) + geom_smooth(method = "lm", se =F, aes(color = Hired_at_tier))


# In sum, Data Analytics Can Be Misleading in Many Ways





############ Will You Play the Game: Ergodicity ############
# setup of the game, which gives you 50% chance of winning 80% and 50% chance of lossing 60%
gain = 0.8
loss = 0.6
chance_of_gain = 0.5
initial_value = 1000

###### Scenario 1. one individual plays the game for N round with the initial value
# sample the N rounds of the game: 1 for gain and 0 for loss
# here we choose N = 100
N = 100
# rbinom draw a random set of samples from binomial distribution to mimic the gains and losses 
# rbinom can be use to simulate one bernoulli or many bernoulli experiments, 
# see: http://www.programmingr.com/examples/neat-tricks/sample-r-function/r-rbinom/
gain_or_loss <- rbinom(N, 1, chance_of_gain)
summary(gain_or_loss)

# outcome for ONE individual play the game for N rounds
# review what happened in the N rounds
gain_or_loss
# the in chain of N events, 1 represents 80% gain and 0 represents 60% loss
# we then translate the chain of events into a chain of real rates of gains and losses
gain_or_loss_new <- gain_or_loss * 1.4 + 0.4
gain_or_loss_new
return_rate <- cumprod(gain_or_loss_new)[N]
print(paste("return rate is: ", return_rate))

# since the order of the multiplication doesn't matter, we can just get the return rate with:
return_rate <- (1 + gain) ^ (sum(gain_or_loss)) * (1 - loss) ^ (N -sum(gain_or_loss))
print(paste("return rate is: ", return_rate))

final_value <- return_rate * initial_value
print(paste("final value is: ", final_value))

# after N round (here N = 100), you only have a small fraction of your intial value (i.e., 1,000) left. 
# so, with only one player, the solution B is closer to the truth. 




###### scenario 2. a group of people play the game
group_size = 1000000
gain_or_loss_group <- rbinom(group_size, N, chance_of_gain)
# the occurance of loss and gain is consistent with the law of large number: 50% gains and 50% loss   
summary(gain_or_loss_group)


# the distribution of the group players' return rate
# very righ-skewed, a fraction of total players win a lot but the majority are lossing money. 
return_rate_group <- (1 + gain) ^ (gain_or_loss_group) * (1 - loss) ^ (N - gain_or_loss_group)
summary(return_rate_group)
hist(return_rate_group)
# to show the tiny righ tail of the distribution, we take logarithm of the return rate
hist(log(return_rate_group))
print(paste("mean return rate is: ", mean(return_rate_group)))

# see the mean final value of the group of players
final_value_group <- return_rate_group * initial_value
summary(final_value_group)
hist(log(final_value_group))

print(paste("mean final value is: ", mean(final_value_group)))

# so, wtih many players, the solution A is closer to the truth.


