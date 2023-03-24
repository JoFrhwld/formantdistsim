library(ggplot2)
change1 <- sim_prob_change(max_year = 100, pop_size = 100, phi = 3)
ggplot(change1)+
  geom_point(aes(dob, prob_obs))+
  geom_line(aes(dob, prob_ml))
