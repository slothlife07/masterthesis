# data preparation auxiliary variables



base <- fread("~/Desktop/Maastricht University/BISS/Master Thesis/Data/Politic/elections_combined.csv")

### Population data add
# load cbs package - works with API 
library(cbsodataR)
# retrieve a table of contents with all SN tables
ds <- cbs_get_datasets("Language" = "en")
head(ds)
# look for dfs with population information
pop_search <- cbs_search("demographic", language="en")
# load df 37296eng
pop <- cbs_get_data("37296eng")
# adjust period to get Year variable 
pop <- cbind(Year = substring(pop$Periods, 0,4), pop)
# test class of variable
class(pop$Year)
# change to integer
pop$Year <- as.integer( pop$Year)



# add pop data to base data frane
# creeate subset to add only relevant variables
pop_add <- pop[,c(1,3:10,12:16,22)]
base <- base %>% 
  left_join(pop_add, by = "Year")

# from now in sub as name of the df used for the analysis
sub <-  base
sub <-  sub %>% filter(sub$Province != "Niet-provinciaal ingedeeld")


# Add Province Identifier
# exclude "Niet-provinciaal ingedeeld"
#Province.df <- sub %>% filter(sub$Province != "Niet-provinciaal ingedeeld")
Province.df <- sub
# vector with all provinces 
Province <- unique(Province.df$Province)
# add numerical identifier
ProvinceID <- c(1:12)
ProvinceID <- as.data.frame(cbind(Province, ProvinceID))

# add to sub
sub <-  sub %>% left_join(ProvinceID, by = c("Province" = "Province"))
# move identifier column to province column -> later 



## economic data

### GDP Growth (Updated)
# https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG?locations=NL
# load (new) gdp worldbank data
setwd("~/Desktop/Maastricht University/BISS/Master Thesis/Data/Economic")
gdp_growth <- fread("gdp_growth1961_2020.csv", header = TRUE)
# wb new file
# change column name so its possible to filter
colnames(gdp_growth)[2] <- "countrycode"
# filter to nld
gdp_growth <- gdp_growth %>% filter(countrycode == "NLD")
# transpose for merging to sub 
# keep only relevant /gdp only available from 1961-2019
gdp_growth <- gdp_growth[,-c(1:5,65:66)]
# transpose
gdp_growth <- t(gdp_growth)
# row names to first column
d <- gdp_growth
names <- rownames(d)
rownames(d) <- NULL
gdp_growth <- cbind(names,d)
# change column names 
colnames.gdp_growth <- c("Year","GDP_Growth")
colnames(gdp_growth) <- colnames.gdp_growth
gdp_growth <- as.data.frame(gdp_growth)
gdp_growth$Year <- as.integer(gdp_growth$Year)
gdp_growth$GDP_Growth <- as.numeric(gdp_growth$GDP_Growth)/100
# add GDP 2020 & 2021
# add two rows to gdp_growth df
gdp_growth[nrow(gdp_growth) + 2,] = NA
# manually enter new data (add reference)
gdp_growth[60,1] <- 2020
gdp_growth[61,1] <- 2021
gdp_growth[60,2] <- -0.03741
gdp_growth[61,2] <- 0.00355

# merge to sub df 
sub <- sub %>% left_join(gdp_growth,by ="Year") # missing 1959-1960, df also doesnt contain 2020,2021






### inflation rate (Consumer prices)
# load data
setwd("~/Desktop/Maastricht University/BISS/Master Thesis/Data/Economic")
inflation <- fread("inflationrate_1960_2019.csv", header = TRUE)

# change column name so its possible to filter
colnames(inflation)[2] <- "countrycode"
# filter to nld
inflation <- inflation %>% filter(countrycode == "NLD")
# transpose for merging to sub 
# keep only relevant inflation only available from 1960-2019
inflation <- inflation[,-c(1:4,65:66)]
# transpose
inflation <- t(inflation)
# row names to first column
d <- inflation
names <- rownames(d)
rownames(d) <- NULL
inflation <- cbind(names,d)
# change column names 
colnames.inflation <- c("Year","Inflation_rate")
colnames(inflation) <- colnames.inflation
inflation <- as.data.frame(inflation)
inflation$Year <- as.integer(inflation$Year)
inflation$Inflation_rate <- as.numeric(inflation$Inflation_rate)/100

# add inflation 2020 & 2021
# add two rows to gdp_growth df
inflation[nrow(inflation) + 2,] = NA
# manually enter new data (add reference)
inflation[61,1] <- 2020
inflation[62,1] <- 2021
inflation[61,2] <- 0.0112 # statista
inflation[62,2] <- 0.0338# statista # 0.0318 real
# merge to sub df 
sub <- sub %>% left_join(inflation,by ="Year")






### population cbs -> relative values instead of integer

# use percentages instead of absolute 
sub$Males_2 <- sub$Males_2/sub$TotalPopulation_1
sub$Females_3 <- sub$Females_3/sub$TotalPopulation_1
sub$NeverMarried_5 <- sub$NeverMarried_5/sub$TotalPopulation_1
sub$Married_6 <- sub$Married_6/sub$TotalPopulation_1
sub$Widowed_7 <- sub$Widowed_7/sub$TotalPopulation_1
sub$Divorced_8 <- sub$Divorced_8/sub$TotalPopulation_1
# age groups / 100 
sub$YoungerThan20Years_10 <- sub$YoungerThan20Years_10/sub$TotalPopulation_1
sub$k_20To40Years_11 <- sub$k_20To40Years_11/sub$TotalPopulation_1
sub$k_40To65Years_12 <- sub$k_40To65Years_12/sub$TotalPopulation_1
sub$k_65To80Years_13 <- sub$k_65To80Years_13/sub$TotalPopulation_1
sub$k_80YearsOrOlder_14 <- sub$k_80YearsOrOlder_14/sub$TotalPopulation_1



### election results from past two elections (national elections)

# subset vector of election years
election.vec <- sub %>% filter(EligibleVoters!= 0, ElectionType == "Second Chamber")
# vector of election years
Year <- unique(election.vec$Year) 

# remove 1959 election
#Year <- Year[2:19]
# vector of past election years
PastElection <- c(1948,Year[1:20])
# vector of second previous election (two elections back)
# deleting the last year observation since it should be a vector with the length of national election years
PastElection2 <- c(1946,PastElection[1:20]) # check if really last election (not relevant anyways #since not in data set)
# bind three vectors 
election.cycle <- as.data.frame(cbind(Year,PastElection, PastElection2))
# add to main df 
sub <- sub %>% left_join(election.cycle, by = "Year")
# works-> I could do for european elections as well or do i even have to????

# create small df to add past results
past.election.vote.share <- sub %>% select(Year, Province, Party, VoteShare) %>% mutate(PastElectionVoteShare = VoteShare, PastElection2VoteShare = VoteShare)
# delete voteShare 
past.election.vote.share <- past.election.vote.share[,-4]
# add PastElection to sub df
sub <-  sub %>% left_join(select(past.election.vote.share, Year,Province, Party, PastElectionVoteShare), by = c("PastElection"="Year", "Province", "Party"))
# add PastElection2 to sub df
sub <-  sub %>% left_join(select(past.election.vote.share,Year,Province, Party, PastElection2VoteShare), by = c("PastElection2"="Year", "Province", "Party"))




# Moving last observation
All values of non-economic variables from 2021 will be taken from the year 2020. Due to the large variances in economic performance due to corona, economic factors are updated for 2021 but are projections of the true value (current stand)


# create empty df and add all population data 
df.moving <- matrix(data = NA, nrow = 1, ncol = 1)
df.moving <- as.data.frame(df.moving)
colnames(df.moving)[1] <- "Year"
df.moving[1,1] <- 2020
# add pop data
df.moving <- df.moving %>% left_join(pop_add, by = "Year")
# data manipulation pop data (as above)
# use percentages instead of absolute 
df.moving$Males_2 <- df.moving$Males_2/df.moving$TotalPopulation_1
df.moving$Females_3 <- df.moving$Females_3/df.moving$TotalPopulation_1
df.moving$NeverMarried_5 <- df.moving$NeverMarried_5/df.moving$TotalPopulation_1
df.moving$Married_6 <- df.moving$Married_6/df.moving$TotalPopulation_1
df.moving$Widowed_7 <- df.moving$Widowed_7/df.moving$TotalPopulation_1
df.moving$Divorced_8 <- df.moving$Divorced_8/df.moving$TotalPopulation_1
# age groups
df.moving$YoungerThan20Years_10 <- df.moving$YoungerThan20Years_10/df.moving$TotalPopulation_1
df.moving$k_20To40Years_11 <- df.moving$k_20To40Years_11/df.moving$TotalPopulation_1
df.moving$k_40To65Years_12 <- df.moving$k_40To65Years_12/df.moving$TotalPopulation_1
df.moving$k_65To80Years_13 <- df.moving$k_65To80Years_13/df.moving$TotalPopulation_1
df.moving$k_80YearsOrOlder_14 <- df.moving$k_80YearsOrOlder_14/df.moving$TotalPopulation_1

# change Year to 2021
df.moving[1,1] <- 2021


# fill in the data into main df
# CAREFUL SINCE ITS ROW NRS -> CHECK IN THE END IF CORRECT
# rows for 2021 observations filled with the info from 2020
sub[5229:5683,11:24] <- df.moving[1,2:15]




## add abbreviations

# load party overview
party_overview <- read_excel("~/Desktop/Maastricht University/BISS/Master Thesis/Data/Politic/parties_nl_overview1.xlsx")

# remoce whitespaces in order to enable left join
# create new df for this
sub$Party <- str_replace_all(sub$Party," ", "")
# merge abbreviation and translation of most relevant parties
sub <- sub %>% 
  left_join(party_overview, by = "Party")


# add inc 

# load excel file
incumb <- read_excel("~/Desktop/Maastricht University/BISS/Master Thesis/Data/Politic/cabinets_overview.xlsx")
# add to base
sub <- sub %>% 
  left_join(select(incumb, year, party, inc_cabinet, inc), by = c("Year"="year",
                                                                  "Abbreviation"="party"))
# if NA = 0 since uploaded excel only includes parties that have been inc or inc_cabinet
sub$inc_cabinet[is.na(sub$inc_cabinet)] <- 0
sub$inc[is.na(sub$inc)] <- 0
# NA = 0 if not taken place on election
sub$PastElectionVoteShare[is.na(sub$PastElectionVoteShare)] <- 0
sub$PastElection2VoteShare[is.na(sub$PastElection2VoteShare)] <- 0



sub <- sub %>% filter(Year >= 1963)
# write CSV file
write_csv(sub,"~/Desktop/Maastricht University/BISS/Master Thesis/dataset_final.csv")
```