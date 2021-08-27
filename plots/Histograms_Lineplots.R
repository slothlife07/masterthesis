# histograms & lineplots
# index: vvd & best other development, performance BRM and US

### vvd development & best other
vvddevelopment <- as.data.frame(matrix(data=NA, nrow = 10, ncol = 3))
# party
vvddevelopment[1:5,1] <- "VVD"
vvddevelopment[6:10,1] <- "Best Other"
# election year
vvddevelopment[,2] <- c(2003, 2006, 2010, 2012, 2017, 2003, 2006, 2010, 2012, 2017)
# vvd vote share
vvddevelopment[1:5,3] <- c(17.91, 14.67, 20.49, 26.58, 21.29)
# second best party
vvddevelopment[6:10,3] <- c(28.62,26.51,19.63,24.84,13.06)
# 
colnames(vvddevelopment)[1] <- "Party"
colnames(vvddevelopment)[2] <- "Year"
colnames(vvddevelopment)[3] <- "VoteShare"

plot1.vvd <- ggplot(data=vvddevelopment, aes(x=Year, y=VoteShare, group=Party)) +
  geom_line(aes(color = Party)) +
  geom_point() +
  ggtitle("Vote Share Development") +
  theme_classic()+
  scale_x_continuous(breaks = Year) +
  xlab("Election Year") +
  ylab("Vote Share")


diff.vvd <- diff(vvddevelopment[1:5,3])
diff.bestOther <- diff(vvddevelopment[6:10,3])
diff <- as.data.frame(matrix(data=NA, ncol =1, nrow = 10))
diff[c(1,6),] <- 0
diff[c(2:5),] <- diff.vvd
diff[c(7:10),] <- diff.bestOther
vvddevelopment.diff <- cbind(vvddevelopment,diff)
vvddevelopment.diff <- vvddevelopment.diff[c(2:5,7:10),] 
colnames(vvddevelopment.diff)[4] <- "Change"

plot2.vvd <- ggplot(data=vvddevelopment.diff, aes(x=Year, y=Change, group=Party)) +
  geom_col(aes(color = Party),)+
  geom_point()+
  ggtitle("Change To Previous Elections") +
  theme_classic() +
  scale_x_continuous(breaks = Year) +
  xlab("Election Year") +
  ylab("% change to previous election")
par(mfrow = c(1,2))
plot1.vvd
plot2.vvd

# empty df
uniform.swing.df <-  as.data.frame(matrix(data = NA, nrow = 4, ncol = 7))
# add column names
uniform.colnames <- c("Year", "Party", "ElecDate", "PollDate", "PollVoteShare", "PastElecVoteShare","Refernce")
colnames(uniform.swing.df) <- uniform.colnames
# fill df
uniform.swing.df$Year <- c(2010, 2012, 2017, 2021)
uniform.swing.df$Party <- "VVD"
uniform.swing.df$ElecDate <- c("09-06-2010","12-09-2021","15-03-2017","17-03-2021")
uniform.swing.df$PollDate <- c("11-05-2010","14-08-2021","14-02-2017","16-02-2021")
uniform.swing.df$PollVoteShare <- c(0.1984,0.2397,0.2467,0.2496)
uniform.swing.df$PastElecVoteShare <- c(0.1467, 0.2049, 0.2658, 0.2129)
uniform.swing.df$Refernce <- c("hist", "hist", "hist","new")

# compute diff between poll and elec results
uniform.swing.df <- uniform.swing.df %>% mutate(NationalSwing = PollVoteShare - PastElecVoteShare)

# load dataset sub
sub <- fread("~/Desktop/Maastricht University/BISS/Master Thesis/dataset_final.csv")
sub.baseline.model <- sub
# filter for relevant years
sub.baseline.model <- sub.baseline.model %>% filter(Year >= 2010, ElectionType == "Second Chamber", Party == "VVD") 
# select only relevant cols 
sub.baseline.model <- sub.baseline.model %>% dplyr::select(Year, Province, PastElectionVoteShare)


# add national swing results 
sub.baseline.model <- sub.baseline.model %>% left_join(uniform.swing.df, by = "Year")
# US prediction calculation
sub.baseline.model <- sub.baseline.model %>% mutate(UniformSwingPrediction = PastElectionVoteShare + NationalSwing)
# add final elec result
provinces.results <- sub %>% filter(Party == "VVD", Year >= 2010, ElectionType == "Second Chamber")
provinces.results <- provinces.results[,c(10)]
sub.baseline.model <- cbind(sub.baseline.model, provinces.results )

# compute differences of baseline models to final results
sub.baseline.model <- sub.baseline.model %>% mutate(PredErrorUS = (VoteShare - UniformSwingPrediction)*100)

# baseline model per year
sub.baseline.model.2010 <- sub.baseline.model %>% filter(Year == 2010)
sub.baseline.model.2012 <- sub.baseline.model %>% filter(Year == 2012)
sub.baseline.model.2017 <- sub.baseline.model %>% filter(Year == 2017)
sub.baseline.model.2021 <- sub.baseline.model %>% filter(Year == 2021)



votes.baseline.df <- sub %>% dplyr::select(ValidVotes, Province, Year, Party, ElectionType) %>% filter(Party == "VVD", Year >=2010, ElectionType == "Second Chamber") ## VVD to not have dublicates
# delete party column
valid.votes.baseline <- valid.votes.baseline.df %>% dplyr::select(ValidVotes)
# add valid votes 
sub.baseline.model <- cbind(sub.baseline.model, valid.votes.baseline)
# calculation
# 2010 valid votes and total valid votes
valid.votes.2010 <- valid.votes.baseline.df %>% filter(Year == 2010)
TotValidVotes.2010 <- sum(valid.votes.2010$ValidVotes)
# 2012
valid.votes.2012 <- valid.votes.baseline.df %>% filter(Year == 2012)
TotValidVotes.2012 <- sum(valid.votes.2012$ValidVotes)
# 2017
valid.votes.2017 <- valid.votes.baseline.df %>% filter(Year == 2017)
TotValidVotes.2017 <- sum(valid.votes.2017$ValidVotes)
# 2021
valid.votes.2021 <- valid.votes.baseline.df %>% filter(Year == 2021)
TotValidVotes.2021 <- sum(valid.votes.2021$ValidVotes)

TotalValidVotes <- as.data.frame(matrix(data=NA, ncol =1, nrow = 48))

# add total per election
TotalValidVotes[1:12,] <- TotValidVotes.2010
TotalValidVotes[13:24,] <- TotValidVotes.2012
TotalValidVotes[25:36,] <- TotValidVotes.2017
TotalValidVotes[37:48,] <- TotValidVotes.2021

sub.baseline.model <- cbind(sub.baseline.model, TotalValidVotes)
colnames(sub.baseline.model)[15] <- "TotValidVotes"
# national vote share prediction US
sub.baseline.model <- sub.baseline.model %>% mutate(step1 = UniformSwingPrediction*ValidVotes)


sub.baseline.model.2010 <- sub.baseline.model %>% filter(Year == 2010)
sub.baseline.model.2012 <- sub.baseline.model %>% filter(Year == 2012)
sub.baseline.model.2017 <- sub.baseline.model %>% filter(Year == 2017)
sub.baseline.model.2021 <- sub.baseline.model %>% filter(Year == 2021)
# final national prediction US
predictionUS <- c()
predictionUS[1] <- (sum(sub.baseline.model.2010$step1))/TotValidVotes.2010
predictionUS[2] <-
  (sum(sub.baseline.model.2012$step1))/TotValidVotes.2012
predictionUS[3] <-
  (sum(sub.baseline.model.2017$step1))/TotValidVotes.2017
predictionUS[4] <-
  (sum(sub.baseline.model.2021$step1))/TotValidVotes.2021

results <- c(0.2049, 0.2658, 0.2129, 0.2187)

national.US <- as.data.frame(cbind(predictionUS, results))
national.US <- national.US %>% mutate(PredErrorUS = (predictionUS - results)*100)

MLE <- c(0.2144, 0.2468, 0.2983 ,0.2427) # update
national.US <- cbind(national.US, MLE)
national.US <- national.US %>% mutate(PredErrorBRM = (MLE-results)*100)
national.US$Year <- c(2010,2012,2017,2021)
national.USmelt <- national.US %>% dplyr::select(Year, PredErrorUS, PredErrorBRM)
national.USmelt <- melt(national.USmelt, id.vars = "Year", variable.name = "Model", value.name = "PredError")


#plot differences

ggplot(data=national.USmelt, aes(x = Year, y = PredError, fill = Model)) +
  geom_bar(position = "dodge", stat = "identity") + 
  theme_bw() + 
  ggtitle("Prediction Error")+
  theme_classic()+
  scale_x_continuous(breaks = Year) +
  xlab("Election Year") +
  ylab("Prediction Error (in %)")


