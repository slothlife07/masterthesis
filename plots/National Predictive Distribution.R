# national posterior distribution


# 2010 National Posterior Distribution Y.Pred
## create df to store predcited valid votes for vvd
national.2010.df <- as.data.frame(matrix(data = NA, nrow = 1600, ncol = 12))

## fill df with 600 vote count predictions per province
for (i in (1:12)){
  for(s in 1:1600){
    national.2010.df[s,i] <- run.2010$BUGSoutput$sims.list$Y.pred[s,i]*valid.votes.2010[i,1]
  }
}

# add row sum as another column
PredVoteCount <- rowSums(national.2010.df[,1:12])

# add PredVoteCount & TotValidVotes to national.2017.df
national.2010.df <- cbind(national.2010.df, PredVoteCount, TotValidVotes.2010)

# calculate NationalPredVoteShare
national.2010.df <-  national.2010.df %>% mutate(PredNatVoteShare = PredVoteCount/TotValidVotes.2010)
# name variable for plotting 
posterior.ypred.national.2010 <- national.2010.df$PredNatVoteShare
#Compute HDI and Quantile CI
ci_hdi.2010 <- ci(posterior.ypred.national.2010, method = "HDI")
# ggplot y.pred.nat 2010
posterior.ypred.national.2010 %>% 
  estimate_density(extend = TRUE) %>% 
  ggplot(aes(x = x, y = y)) +
  labs(y = "Posterior", x = "Vote Share") +
  geom_area(fill = "grey") +
  theme_classic() + 
  # title(s)
  ggtitle(label = "Predictions 2010") +
  # election result
  geom_vline(xintercept = 0.2049, color = "green", size = 0.3) +
  geom_vline(xintercept = ci_hdi.2010$CI_low, color = "red", size = 0.25) +
  geom_vline(xintercept = ci_hdi.2010$CI_high, color = "red", size = 0.25) +
  geom_vline(xintercept = mean(posterior.ypred.national.2010), color = "black", size = 0.25, linetype = 1)



# 2012 National Posterior Distribution Y.Pred
# df validvotes, pro, year
valid.votes.2012
# number total valid votes nl 2017
TotValidVotes.2012
# START
## create df to store predcited valid votes for vvd
national.2012.df <- as.data.frame(matrix(data = NA, nrow = 1600, ncol = 12))

## fill df with 600 vote count predictions per province
for (i in (1:12)){
  for(s in 1:1600){
    national.2012.df[s,i] <- run.2012$BUGSoutput$sims.list$Y.pred[s,i]*valid.votes.2012[i,1]
  }
}

# add row sum as another column
PredVoteCount <- rowSums(national.2012.df[,1:12])

# add PredVoteCount & TotValidVotes to national.2017.df
national.2012.df <- cbind(national.2012.df, PredVoteCount, TotValidVotes.2012)

# calculate NationalPredVoteShare
national.2012.df <-  national.2012.df %>% mutate(PredNatVoteShare = PredVoteCount/TotValidVotes.2012)
# name variable for plotting 
posterior.ypred.national.2012 <- national.2012.df$PredNatVoteShare
#Compute HDI and Quantile CI
ci_hdi.2012 <- ci(posterior.ypred.national.2012, method = "HDI")
# ggplot y.pred.nat 2017
plot2 <- posterior.ypred.national.2012 %>% 
  estimate_density(extend = TRUE) %>% 
  ggplot(aes(x = x, y = y)) +
  labs(y = "Posterior", x = "Vote Share") +
  geom_area(fill = "grey") +
  theme_classic() + 
  # title(s)
  ggtitle(label = "Predictions 2012") +
  # election result
  geom_vline(xintercept = 0.2658, color = "green", size = 0.3)+
  #geom_text(aes(x= 0.23, label="Result",), color = "green")+
  geom_vline(xintercept = ci_hdi.2012$CI_low, color = "red", size = 0.25) +
  geom_vline(xintercept = ci_hdi.2012$CI_high, color = "red", size = 0.25) +
  geom_vline(xintercept = mean(posterior.ypred.national.2012), color = "black", size = 0.25, linetype = 1)


# 2017 National Posterior Distribution Y.Pred
# df validvotes, pro, year
valid.votes.2017
# number total valid votes nl 2017
TotValidVotes.2017
# START
## create df to store predcited valid votes for vvd
national.2017.df <- as.data.frame(matrix(data = NA, nrow = 1600, ncol = 12))

## fill df with 600 vote count predictions per province
for (i in (1:12)){
  for(s in 1:1600){
    national.2017.df[s,i] <- run.2017$BUGSoutput$sims.list$Y.pred[s,i]*valid.votes.2017[i,1]
  }
}

# add row sum as another column
PredVoteCount <- rowSums(national.2017.df[,1:12])

# add PredVoteCount & TotValidVotes to national.2017.df
national.2017.df <- cbind(national.2017.df, PredVoteCount, TotValidVotes.2017)

# calculate NationalPredVoteShare
national.2017.df <-  national.2017.df %>% mutate(PredNatVoteShare = PredVoteCount/TotValidVotes.2017)
# name variable for plotting 
posterior.ypred.national.2017 <- national.2017.df$PredNatVoteShare
#Compute HDI and Quantile CI
ci_hdi.2017 <- ci(posterior.ypred.national.2017, method = "HDI")
# ggplot y.pred.nat 2017
plot3 <- posterior.ypred.national.2017 %>% 
  estimate_density(extend = TRUE) %>% 
  ggplot(aes(x = x, y = y)) +
  labs(y = "Posterior", x = "Vote Share") +
  geom_area(fill = "grey") +
  theme_classic() + 
  # title(s)
  ggtitle(label = "Predictions 2017")+
  # election result
  geom_vline(xintercept = 0.2129, color = "green", size = 0.3) +
  geom_vline(xintercept = ci_hdi.2017$CI_low, color = "red", size = 0.25) +
  geom_vline(xintercept = ci_hdi.2017$CI_high, color = "red", size = 0.25) + 
  geom_vline(xintercept = mean(posterior.ypred.national.2017), color = "black", size = 0.25, linetype = 1)



# 2021 National Posterior Distribution Y.Pred

## create df to store predcited valid votes for vvd
national.2021.df <- as.data.frame(matrix(data = NA, nrow = 1600, ncol = 12))

## fill df with 600 vote count predictions per province
for (i in (1:12)){
  for(s in 1:1600){
    national.2021.df[s,i] <- run.2021$BUGSoutput$sims.list$Y.pred[s,i]*valid.votes.2021[i,1]
  }
}

# add row sum as another column
PredVoteCount <- rowSums(national.2021.df[,1:12])

# add PredVoteCount & TotValidVotes to national.2017.df
national.2021.df <- cbind(national.2021.df, PredVoteCount, TotValidVotes.2021)

# calculate NationalPredVoteShare
national.2021.df <-  national.2021.df %>% mutate(PredNatVoteShare = (PredVoteCount/TotValidVotes.2021))
# name variable for plotting 
posterior.ypred.national.2021 <- national.2021.df$PredNatVoteShare
#Compute HDI and Quantile CI
ci_hdi.2021 <- ci(posterior.ypred.national.2021, method = "HDI")

# plot posterior 2021
posterior.ypred.national.2021 %>% 
  estimate_density(extend = TRUE) %>% 
  ggplot(aes(x = x, y = y)) +
  labs(y = "Posterior", x = "Vote Share") +
  geom_area(fill = "grey") +
  theme_classic() + 
  # title(s)
  ggtitle(label = "Predictions 2021")+
  # election result
  geom_vline(xintercept = 0.2187, color = "green", size = 0.3) +
  geom_vline(xintercept = ci_hdi.2021$CI_low, color = "red", size = 0.25) +
  geom_vline(xintercept = ci_hdi.2021$CI_high, color = "red", size = 0.25) + 
  geom_vline(xintercept = mean(posterior.ypred.national.2021), color = "black", size = 0.25, linetype = 1)
mean(posterior.ypred.national.2021)

