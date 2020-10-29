##############################################
### WAM Training Section5
##############################################


#### The Selction Effect ####

library("dplyr")
library("ggplot2")

setwd("C:/LMU OneDrive/OneDrive - lmu.edu/Training_WAM")


#### Generate Cadidate Pool ####
# generate Leadership Score
Leadership <- round(rnorm(200, 80, 10), digit = 0)
hist(Leadership)

# generate I.Q. Score
I.Q. <-  round(rnorm(200, 100, 10), digit = 0)
hist(I.Q.)

# generate Total.Score
Total.Score = Leadership + I.Q.
hist(Total.Score)

# generate Candidate.ID
Candidate.ID <- c(1:200)
# combine information into data.frame
Candidates <- as.data.frame(cbind(Candidate.ID, Leadership, I.Q., Total.Score))

# check the correlaton between Leadership and I.Q.
cor.test(Candidates$I.Q.,Candidates$Leadership)

# sava a sample of candidates to excel
write.csv(Candidates[c(1:15),] ,"Candidate Pool.csv")

# visualize the original candidate pool
ggplot(Candidates, aes(x = Leadership, y = I.Q.)) + geom_point() + geom_smooth(method = "lm", se = F )
# key: there is not correlation between the values of Leadership and I.Q.


#### Hiring Decisios ####
# simulate hiring decisions according to the total score of Leadership and I.Q.
Candidates <- Candidates %>% mutate (Hired = ifelse(Total.Score > 190, "Yes", "No")) %>% 
  mutate(Hired_at_tire = ifelse(Total.Score > 190, "One (> 190)", ifelse(Total.Score > 170, "Two (> 170)", "Three (the rest)"))) %>%
  mutate(Hired_at_tire = factor(Hired_at_tire, levels = c("One (> 190)", "Two (> 170)", "Three (the rest)")))
                                       

#### Visualizing the Selection Effect #### 
# superfical negative correlation between Leadership and I.Q. among those who are hired
ggplot(Candidates, aes(x = Leadership, y = I.Q., color = Hired)) + geom_point() +  
  geom_smooth(data = Candidates %>% filter(Hired =="Yes"), method = "lm", se = F ) + scale_color_manual(values = c("green", "red"))

# superfical negative correlation between Leadership and I.Q. in EVERY tire of the companies
ggplot(Candidates, aes(x = Leadership, y = I.Q., color = Hired_at_tire)) + geom_point() +  
  geom_smooth(data = Candidates, method = "lm", se = F ) 


#### One Interesting Extension: Simpson's Paradox ####
# if you using only highest and lowest tier companies, you will see simpson's paradox
ggplot(data = Candidates %>% filter(Hired_at_tire == "One (> 190)" | Hired_at_tire == "Three (the rest)"), aes(x = Leadership, y = I.Q.)) + 
  geom_point(aes(color = Hired_at_tire)) +  geom_smooth( method = "lm", se = F ) + geom_smooth(method = "lm", se =F, aes(color = Hired_at_tire))



