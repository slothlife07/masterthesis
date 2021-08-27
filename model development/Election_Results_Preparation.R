# election result data load and preparation

# support docs
overview_provinces1 <- read_excel("overview_provinces1.xlsx")
as.data.frame(overview_provinces1)
col_change_overview <- c("ElectoralDistrict","Province") #change headline in original file
colnames(overview_provinces1) <- col_change_overview



## National Elections
#### Columnnames
change_column_names <- c("Year", "Province", "EligibleVoters", "Votes1", 
                         "ValidVotes","Party","Count")


###2021
# 2021 - Last General Election
nl_2021 <- fread("nl_2021.csv") 
# add province level
nl_2021 <-  nl_2021 %>% 
  inner_join(overview_provinces1, by = c("OuderRegioNaam" = "ElectoralDistrict"))
# group all counts on province level
nl_2021 <- nl_2021 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_2021 <- cbind(Year = 2021, nl_2021)
# transpose the data in long format
nl_2021 <- melt(nl_2021, id = c(1:8), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_2021 <- nl_2021[,-c(3,6,7)]
# change column names
colnames(nl_2021) <- change_column_names



###2017

# 2017 - Last General Election
nl_2017 <- fread("nl_2017.csv") 
# add province level
nl_2017 <-  nl_2017 %>% 
  inner_join(overview_provinces1, by = c("OuderRegioNaam" = "ElectoralDistrict"))
# group all counts on province level
nl_2017 <- nl_2017 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_2017 <- cbind(Year = 2017, nl_2017)
# transpose the data in long format
nl_2017 <- melt(nl_2017, id = c(1:8), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_2017 <- nl_2017[,-c(3,6,7)]
# change column names
colnames(nl_2017) <- change_column_names




# 2012 
nl_2012 <- fread("nl_2012.csv") 
# add province level
nl_2012 <-  nl_2012 %>% 
  inner_join(overview_provinces1, by = c("OuderRegioNaam" = "ElectoralDistrict"))
# group all counts on province level
nl_2012 <- nl_2012 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_2012 <- cbind(Year = 2012, nl_2012)
# transpose the data in long format
nl_2012 <- melt(nl_2012, id = c(1:8), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_2012 <- nl_2012[,-c(3,6,7)]
# change column names
colnames(nl_2012) <- change_column_names



# 2010
nl_2010 <- fread("nl_2010.csv") 
# change column name 4 to province
colnames(nl_2010)[4] <- "Province"
# group all counts on province level
nl_2010 <- nl_2010 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_2010 <- cbind(Year = 2010, nl_2010)
# transpose the data in long format
nl_2010 <- melt(nl_2010, id = c(1:8), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_2010 <- nl_2010[,-c(3,6,7)]
# change column names
colnames(nl_2010) <- change_column_names


# 2006
nl_2006 <- fread("nl_2006.csv") 
# change column name 4 to province
colnames(nl_2006)[4] <- "Province"
# group all counts on province level
nl_2006 <- nl_2006 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_2006 <- cbind(Year = 2006, nl_2006)
# transpose the data in long format
nl_2006 <- melt(nl_2006, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_2006 <- nl_2006[,-c(3,6)]
# change column names
colnames(nl_2006) <- change_column_names


# 2003
nl_2003 <- fread("nl_2003.csv") 
# change column name 4 to province
colnames(nl_2003)[4] <- "Province"
# group all counts on province level
nl_2003 <- nl_2003 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_2003 <- cbind(Year = 2003, nl_2003)
# transpose the data in long format
nl_2003 <- melt(nl_2003, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_2003 <- nl_2003[,-c(3,6)]
# change column names
colnames(nl_2003) <- change_column_names




# 2002
nl_2002 <- fread("nl_2002.csv") 
# change column name 4 to province
colnames(nl_2002)[4] <- "Province"
# group all counts on province level
nl_2002 <- nl_2002 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_2002 <- cbind(Year = 2002, nl_2002)
# transpose the data in long format
nl_2002 <- melt(nl_2002, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_2002 <- nl_2002[,-c(3,6)]
# change column names
colnames(nl_2002) <- change_column_names

# 1998
nl_1998 <- fread("nl_1998.csv") 
# change column name 4 to province
colnames(nl_1998)[4] <- "Province"
# group all counts on province level
nl_1998 <- nl_1998 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_1998 <- cbind(Year = 1998, nl_1998)
# transpose the data in long format
nl_1998 <- melt(nl_1998, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_1998 <- nl_1998[,-c(3,6)]
# change column names
colnames(nl_1998) <- change_column_names


# 1994
nl_1994 <- fread("nl_1994.csv") 
# change column name 4 to province
colnames(nl_1994)[4] <- "Province"
# group all counts on province level
nl_1994 <- nl_1994 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_1994 <- cbind(Year = 1994, nl_1994)
# transpose the data in long format
nl_1994 <- melt(nl_1994, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_1994 <- nl_1994[,-c(3,6)]
# change column names
colnames(nl_1994) <- change_column_names


# 1989
nl_1989 <- fread("nl_1989.csv") 
# change column name 4 to province
colnames(nl_1989)[4] <- "Province"
# group all counts on province level
nl_1989 <- nl_1989 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_1989 <- cbind(Year = 1989, nl_1989)
# transpose the data in long format
nl_1989 <- melt(nl_1989, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_1989 <- nl_1989[,-c(3,6)]
# change column names
colnames(nl_1989) <- change_column_names


# 1986
nl_1986 <- fread("nl_1986.csv") 
# change column name 4 to province
colnames(nl_1986)[4] <- "Province"
# group all counts on province level
nl_1986 <- nl_1986 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_1986 <- cbind(Year = 1986, nl_1986)
# transpose the data in long format
nl_1986 <- melt(nl_1986, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_1986 <- nl_1986[,-c(3,6)]
# change column names
colnames(nl_1986) <- change_column_names



# Province Flevoland was established in 1986, therefore not appearing in elections before
# 1982
nl_1982 <- fread("nl_1982.csv") 
# change column name 4 to province
colnames(nl_1982)[4] <- "Province"
# group all counts on province level
nl_1982 <- nl_1982 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_1982 <- cbind(Year = 1982, nl_1982)
# transpose the data in long format
nl_1982 <- melt(nl_1982, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_1982 <- nl_1982[,-c(3,6)]
# change column names
colnames(nl_1982) <- change_column_names


# 1981
nl_1981 <- fread("nl_1981.csv") 
# change column name 4 to province
colnames(nl_1981)[4] <- "Province"
# group all counts on province level
nl_1981 <- nl_1981 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_1981 <- cbind(Year = 1981, nl_1981)
# transpose the data in long format
nl_1981 <- melt(nl_1981, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_1981 <- nl_1981[,-c(3,6)]
# change column names
colnames(nl_1981) <- change_column_names


# 1977
nl_1977 <- fread("nl_1977.csv") 
# change column name 4 to province
colnames(nl_1977)[4] <- "Province"
# group all counts on province level
nl_1977 <- nl_1977 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_1977 <- cbind(Year = 1977, nl_1977)
# transpose the data in long format
nl_1977 <- melt(nl_1977, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_1977 <- nl_1977[,-c(3,6)]
# change column names
colnames(nl_1977) <- change_column_names


# 1972
nl_1972 <- fread("nl_1972.csv") 
# change column name 4 to province
colnames(nl_1972)[4] <- "Province"
# group all counts on province level
nl_1972 <- nl_1972 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_1972 <- cbind(Year = 1972, nl_1972)
# transpose the data in long format
nl_1972 <- melt(nl_1972, id = c(1:8), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_1972 <- nl_1972[,-c(3,6,7)]
# change column names
colnames(nl_1972) <- change_column_names


# 1971
nl_1971 <- fread("nl_1971.csv") 
# change column name 4 to province
colnames(nl_1971)[4] <- "Province"
# group all counts on province level
nl_1971 <- nl_1971 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_1971 <- cbind(Year = 1971, nl_1971)
# transpose the data in long format
nl_1971 <- melt(nl_1971, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_1971 <- nl_1971[,-c(3,6)]
# change column names
colnames(nl_1971) <- change_column_names



# 1967
nl_1967 <- fread("nl_1967.csv") 
# change column name 4 to province
colnames(nl_1967)[4] <- "Province"
# group all counts on province level
nl_1967 <- nl_1967 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_1967 <- cbind(Year = 1967, nl_1967)
# transpose the data in long format
nl_1967 <- melt(nl_1967, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_1967 <- nl_1967[,-c(3,6)]
# change column names
colnames(nl_1967) <- change_column_names



# 1963
nl_1963 <- fread("nl_1963.csv") 
# change column name 4 to province
colnames(nl_1963)[4] <- "Province"
# group all counts on province level
nl_1963 <- nl_1963 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
nl_1963 <- cbind(Year = 1963, nl_1963)
# transpose the data in long format
nl_1963 <- melt(nl_1963, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_1963 <- nl_1963[,-c(3,6)]
# change column names
colnames(nl_1963) <- change_column_names


# 1959
nl_1959 <- fread("nl_1959.csv") 
# change column name 4 to province
colnames(nl_1959)[4] <- "Province"
# group all counts on province level
nl_1959 <- nl_1959 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE)) 
# add Year column
nl_1959 <- cbind(Year = 1959, nl_1959)
# transpose the data in long format
nl_1959 <- melt(nl_1959, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_1959 <- nl_1959[,-c(3,6)]
# change column names
colnames(nl_1959) <- change_column_names



# 1956
nl_1956 <- fread("nl_1956.csv") 
# change column name 4 to province
colnames(nl_1956)[4] <- "Province"
# group all counts on province level
nl_1956 <- nl_1956 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE)) 
# add Year column
nl_1956 <- cbind(Year = 1956, nl_1956)
# transpose the data in long format
nl_1956 <- melt(nl_1956, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_1956 <- nl_1956[,-c(3,6)]
# change column names
colnames(nl_1956) <- change_column_names


# 1952
nl_1952 <- fread("nl_1952.csv") 
# change column name 4 to province
colnames(nl_1952)[4] <- "Province"
# group all counts on province level
nl_1952 <- nl_1952 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE)) 
# add Year column
nl_1952 <- cbind(Year = 1952, nl_1952)
# transpose the data in long format
nl_1952 <- melt(nl_1952, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
nl_1952 <- nl_1952[,-c(3,6)]
# change column names
colnames(nl_1952) <- change_column_names
         


## European Elections



# 2019 - Last European Election
eu_nl_2019<- fread("eu_nl_2019.csv") 
# add province level
eu_nl_2019 <-  eu_nl_2019 %>% 
  inner_join(overview_provinces1, by = c("OuderRegioNaam" = "ElectoralDistrict"))
# group all counts on province level
eu_nl_2019 <- eu_nl_2019 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
eu_nl_2019 <- cbind(Year = 2019, eu_nl_2019)
# transpose the data in long format
eu_nl_2019 <- melt(eu_nl_2019, id = c(1:8), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
eu_nl_2019 <- eu_nl_2019[,-c(3,6,7)]
# change column names
colnames(eu_nl_2019) <- change_column_names



# 2014
eu_nl_2014<- fread("eu_nl_2014.csv") 
# add province level
eu_nl_2014 <-  eu_nl_2014 %>% 
  inner_join(overview_provinces1, by = c("OuderRegioNaam" = "ElectoralDistrict"))
# group all counts on province level
eu_nl_2014 <- eu_nl_2014 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
eu_nl_2014 <- cbind(Year = 2014, eu_nl_2014)
# transpose the data in long format
eu_nl_2014 <- melt(eu_nl_2014, id = c(1:8), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
eu_nl_2014 <- eu_nl_2014[,-c(3,6,7)]
# change column names
colnames(eu_nl_2014) <- change_column_names



# 2009
eu_nl_2009<- fread("eu_nl_2009.csv") 
# add province level
eu_nl_2009 <-  eu_nl_2009 %>% 
  inner_join(overview_provinces1, by = c("OuderRegioNaam" = "ElectoralDistrict"))
# group all counts on province level
eu_nl_2009 <- eu_nl_2009 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
eu_nl_2009 <- cbind(Year = 2009, eu_nl_2009)
# transpose the data in long format
eu_nl_2009 <- melt(eu_nl_2009, id = c(1:8), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
eu_nl_2009 <- eu_nl_2009[,-c(3,6,7)]
# change column names
colnames(eu_nl_2009) <- change_column_names



# 2004
eu_nl_2004<- fread("eu_nl_2004.csv") 
# change column name 4 to province
colnames(eu_nl_2004)[4] <- "Province"
# group all counts on province level
eu_nl_2004 <- eu_nl_2004 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
eu_nl_2004 <- cbind(Year = 2004, eu_nl_2004)
# transpose the data in long format
eu_nl_2004 <- melt(eu_nl_2004, id = c(1:8), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
eu_nl_2004 <- eu_nl_2004[,-c(3,6,7)]
# change column names
colnames(eu_nl_2004) <- change_column_names


# 1999
eu_nl_1999<- fread("eu_nl_1999.csv") 
# change column name 4 to province
colnames(eu_nl_1999)[4] <- "Province"
# group all counts on province level
eu_nl_1999 <- eu_nl_1999 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
eu_nl_1999 <- cbind(Year = 1999, eu_nl_1999)
# transpose the data in long format
eu_nl_1999 <- melt(eu_nl_1999, id = c(1:8), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
eu_nl_1999 <- eu_nl_1999[,-c(3,6,7)]
# change column names
colnames(eu_nl_1999) <- change_column_names



# 1994
eu_nl_1994<- fread("eu_nl_1994.csv") 
# change column name 4 to province
colnames(eu_nl_1994)[4] <- "Province"
# group all counts on province level
eu_nl_1994 <- eu_nl_1994 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
eu_nl_1994 <- cbind(Year = 1994, eu_nl_1994)
# transpose the data in long format
eu_nl_1994 <- melt(eu_nl_1994, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
eu_nl_1994 <- eu_nl_1994[,-c(3,6)]
# change column names
colnames(eu_nl_1994) <- change_column_names



# 1989
eu_nl_1989 <- fread("eu_nl_1989.csv") 
# change column name 4 to province
colnames(eu_nl_1989)[4] <- "Province"
# group all counts on province level
eu_nl_1989 <- eu_nl_1989 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
eu_nl_1989 <- cbind(Year = 1989, eu_nl_1989)
# transpose the data in long format
eu_nl_1989 <- melt(eu_nl_1989, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
eu_nl_1989 <- eu_nl_1989[,-c(3,6)]
# change column names
colnames(eu_nl_1989) <- change_column_names



# 1984
eu_nl_1984 <- fread("eu_nl_1984.csv") 
# change column name 4 to province
colnames(eu_nl_1984)[4] <- "Province"
# group all counts on province level
eu_nl_1984 <- eu_nl_1984 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
eu_nl_1984 <- cbind(Year = 1984, eu_nl_1984)
# transpose the data in long format
eu_nl_1984 <- melt(eu_nl_1984, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
eu_nl_1984 <- eu_nl_1984[,-c(3,6)]
# change column names
colnames(eu_nl_1984) <- change_column_names



# 1979
eu_nl_1979 <- fread("eu_nl_1979.csv") 
# change column name 4 to province
colnames(eu_nl_1979)[4] <- "Province"
# group all counts on province level
eu_nl_1979 <- eu_nl_1979 %>%  group_by(Province) %>% 
  summarise(across(.cols = is.numeric, .fns = sum, na.rm = TRUE))
# add Year column
eu_nl_1979 <- cbind(Year = 1979, eu_nl_1979)
# transpose the data in long format
eu_nl_1979 <- melt(eu_nl_1979, id = c(1:7), variable.name = "Party",value.name = "Count")
# delete irrelevant columns
eu_nl_1979 <- eu_nl_1979[,-c(3,6)]
# change column names
colnames(eu_nl_1979) <- change_column_names

# aggregate national elections
# bind all national elections into one dataframe
nationalelections <- rbind(nl_1952, nl_1956, nl_1959, nl_1963, nl_1967, nl_1971,
                           nl_1972, nl_1977,nl_1981, nl_1982, nl_1986, nl_1989, 
                           nl_1994, nl_1998, nl_2002, nl_2003, nl_2006, nl_2010, 
                           nl_2012, nl_2017, nl_2021)

# add election type
nationalelections <- cbind(ElectionType = "Second Chamber", nationalelections)

# aggregate european elections
# bind all european elections into one dataframe
europeanelections <- rbind(eu_nl_1979, eu_nl_1984, eu_nl_1989, eu_nl_1994, eu_nl_1999,
                           eu_nl_2004, eu_nl_2009, eu_nl_2014, eu_nl_2019)
# add election type
europeanelections <- cbind(ElectionType = "European Parliament", europeanelections)




# bind national and european elections into one dataframe
elections <- rbind(nationalelections, europeanelections)
# add turnout rate 
elections <- elections %>% mutate(Turnout = ValidVotes/EligibleVoters,
                                  VoteShare = Count/ValidVotes)
# write elections_combined csv
write_csv(elections,"~/Desktop/Maastricht University/BISS/Master Thesis/Data/Politic/elections_combined.csv")
