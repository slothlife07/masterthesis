# model code
# load data set
sub <- fread("~/Desktop/Maastricht University/BISS/Master Thesis/dataset_final.csv")




## standardize with basic scale option
sub$EligibleVoters <- scale(sub$EligibleVoters)
sub$TotalPopulation_1 <- scale(sub$Males_2)
sub$Females_3 <- scale(sub$Females_3)
sub$NeverMarried_5 <- scale(sub$NeverMarried_5)
sub$Married_6 <- scale(sub$Married_6)
sub$Widowed_7 <- scale(sub$Widowed_7)
sub$Divorced_8 <- scale(sub$Divorced_8)
sub$YoungerThan20Years_10 <- scale(sub$YoungerThan20Years_10)
sub$k_20To40Years_11 <- scale(sub$k_20To40Years_11)
sub$k_40To65Years_12 <- scale(sub$k_40To65Years_12)
sub$k_65To80Years_13 <- scale(sub$k_65To80Years_13)
sub$k_80YearsOrOlder_14 <- scale(sub$k_80YearsOrOlder_14)
sub$TotalDemographicPressure_20 <- scale(sub$TotalDemographicPressure_20)
sub$GDP_Growth <- scale(sub$GDP_Growth)
sub$Inflation_rate <- scale(sub$Inflation_rate)
sub$inc <- scale(sub$inc)
sub$inc_cabinet <- scale(sub$inc_cabinet)
sub$PastElectionVoteShare <- scale(sub$PastElectionVoteShare)
sub$PastElection2VoteShare <- scale(sub$PastElection2VoteShare)





# create train/test set
## # 2021
# train 
train.2021 <- sub %>% filter(Year <= 2020)
# test 
test.2021 <- sub %>% filter(Year == 2021, Party == "VVD", ElectionType == "Second Chamber")


## # 2017

# train 
train.2017 <- sub %>% filter(Year <= 2016)
# test 
test.2017 <- sub %>% filter(Year == 2017, Party == "VVD", ElectionType == "Second Chamber")



## 2012
# train 
train.2012 <- sub %>% filter(Year <= 2011)
# train 
test.2012 <- sub %>% filter(Year == 2012, Party == "VVD", ElectionType == "Second Chamber")


## 2010

# train 
train.2010 <- sub %>% filter(Year <= 2009)
# train 
test.2010 <- sub %>% filter(Year == 2010, Party == "VVD", ElectionType == "Second Chamber")




## Define Model & Parameters
parameter.function <- function(train, test){
  sub.train <- train #later change the input df for the definition of the variables 
  sub.test <- test #later change the input df for the definition of the variables 
  output <- list(Y = train$VoteShare,
                 # political 
                 voters = sub.train$EligibleVoters,
                 inc = sub.train$inc,
                 inc_cabinet = sub.train$inc_cabinet,
                 # past elections
                 past.VoteShare = sub.train$PastElectionVoteShare,
                 past2.VoteShare = sub.train$PastElection2VoteShare,
                 # demographics
                 pop = sub.train$TotalPopulation_1,
                 male_share = sub.train$Males_2,
                 married = sub.train$Married_6,
                 widowed = sub.train$Widowed_7,
                 divorced = sub.train$Divorced_8,
                 from20t039 = sub.train$k_20To40Years_11,
                 from40to64 = sub.train$k_40To65Years_12,
                 from65to80 = sub.train$k_65To80Years_13,
                 older80 = sub.train$k_80YearsOrOlder_14,
                 demographic_pressure = sub.train$TotalDemographicPressure_20,
                 # economic
                 gdp_growth  = sub.train$GDP_Growth,
                 inflation_rate = sub.train$Inflation_rate,
                 # define number of iterations 
                 N = dim(sub.train)[1], 
                 
                 # Prediction / Testing test 
                 # no need to define outcome variable, comes from prediction 
                 # political 
                 voters.pred  = sub.test$EligibleVoters,
                 inc.pred  = sub.test$inc,
                 inc_cabinet.pred  = sub.test$inc_cabinet,
                 # past elections
                 past.VoteShare.pred = sub.test$PastElectionVoteShare,
                 past2.VoteShare.pred = sub.test$PastElection2VoteShare,
                 # demographics
                 pop.pred  = sub.test$TotalPopulation_1,
                 male_share.pred  = sub.test$Males_2,
                 married.pred  = sub.test$Married_6,
                 widowed.pred  = sub.test$Widowed_7,
                 divorced.pred  = sub.test$Divorced_8,
                 from20t039.pred  = sub.test$k_20To40Years_11,
                 from40to64.pred  = sub.test$k_40To65Years_12,
                 from65to80.pred  = sub.test$k_65To80Years_13,
                 older80.pred  = sub.test$k_80YearsOrOlder_14,
                 demographic_pressure.pred  = sub.test$TotalDemographicPressure_20,
                 # economic
                 gdp_growth.pred   = sub.test$GDP_Growth,
                 inflation_rate.pred = sub.test$Inflation_rate,
                 N.pred = dim(sub.test)[1])
  return(output)
}

# run over function for different elections 
parameters.2021 <- parameter.function(train.2021, test.2021)
parameters.2017 <- parameter.function(train.2017, test.2017)
parameters.2012 <- parameter.function(train.2012, test.2012)
parameters.2010 <- parameter.function(train.2010, test.2010)


# model code: bayesian (linear regression)
model_code = "model {

# modeling 
for(i in 1:N) {
 Y[i] ~ dnorm(mu[i], sd^(-2))
mu[i] = int + beta1*voters[i] + beta2*inc[i] + beta3*inc_cabinet[i] + beta4*past.VoteShare[i] + beta5*past2.VoteShare[i] + beta6*gdp_growth[i] + beta7*inflation_rate[i] + beta8*pop[i] + beta9*male_share[i] +  
beta10*married[i] + beta11*widowed[i] + beta12*divorced[i] + beta13*from20t039[i] + beta14*from40to64[i] + beta15*from65to80[i] + beta16*older80[i] + 
beta17*demographic_pressure[i] 

# mu is N
res[i] = Y[i] - mu[i]
}



# prediction 
for(i in 1:N.pred) {
Y.pred[i] ~ dnorm(mu.pred[i],sd^(-2))
mu.pred[i] = int + beta1*voters.pred[i] + beta2*inc.pred[i] + beta3*inc_cabinet.pred[i] + beta4*past.VoteShare.pred[i] + beta5*past2.VoteShare.pred[i] + beta6*gdp_growth.pred[i] + beta7*inflation_rate.pred[i] + beta8*pop.pred[i] + beta9*male_share.pred[i] + beta10*married.pred[i] +  beta11*widowed.pred[i] + beta12*divorced.pred[i] +  beta13*from20t039.pred[i] + beta14*from40to64.pred[i] + beta15*from65to80.pred[i] + beta16*older80.pred[i] + beta17*demographic_pressure.pred[i] 
}


# non-informative priors on the coefficients 
int ~ dnorm(0,1)
beta1 ~ dnorm(0,1)
beta2 ~ dnorm(0,1)
beta3 ~ dnorm(0,1)
beta4 ~ dnorm(0,1)
beta5 ~ dnorm(0,1)
beta6 ~ dnorm(0,1)
beta7 ~ dnorm(0,1)
beta8 ~ dnorm(0,1)
beta9 ~ dnorm(0,1)
beta10 ~ dnorm(0,1)
beta11 ~ dnorm(0,1)
beta12 ~ dnorm(0,1)
beta13 ~ dnorm(0,1)
beta14 ~ dnorm(0,1)
beta15 ~ dnorm(0,1)
beta16 ~ dnorm(0,1)
beta17 ~ dnorm(0,1)


 
# prior on the standard deviation 
sd ~ dunif(0,0.15) 


}"


# temporary path to run the model
tmpf=tempfile()
tmps=file(tmpf,"w")
cat(model_code,file=tmps)
close(tmps)

# Choose the parameters to watch
pararmeters.to.save =  c("Y.pred","int", 
                         "beta1",
                         "beta2",
                         "beta3",
                         "beta4",
                         "beta5",
                         "beta6",
                         "beta7",
                         "beta8",
                         "beta9",
                         "beta10",
                         "beta11",
                         "beta12",
                         "beta13",
                         "beta14",
                         "beta15",
                         "beta16",
                         "beta17",
                         "sd",
                         "res")



run.model.function <- function(sub.n){
  output <-  jags(
    data = sub.n, # data - needs to be list with model parameters 
    parameters.to.save = pararmeters.to.save, # vector of parameters
    model.file = tmpf,
    n.chains = 4, # number of Markov chains 
    n.iter = 1000, # number of total iterations 
    n.burnin = 200, # length of burn in
    n.thin = 2, # thinning rate - keep every n observation
    DIC = FALSE)
  return(output)
}
run.2021 <- run.model.function(parameters.2021)
run.2017 <- run.model.function(parameters.2017)
run.2012 <- run.model.function(parameters.2012)
run.2010 <- run.model.function(parameters.2010)

