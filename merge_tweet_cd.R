library(tidycensus)
library(dplyr)
var <- load_variables(2020, "pl", cache = TRUE)

pop20_state <- get_decennial(geography = "state", 
                          variables = "P1_001N", 
                          year = 2020,
                          sumfile = "pl")
pop20_black <- get_decennial(geography = "state", 
                             variables = "P1_004N", 
                             year = 2020,
                             sumfile = "pl")
pop20_white <- get_decennial(geography = "state", 
                             variables = "P1_003N", 
                             year = 2020,
                             sumfile = "pl")

pop20_cd <- get_decennial(geography = "congressional district", 
                       variables = "P1_001N", 
                       year = 2020,
                       sumfile = "pl")
pop20_black_cd <- get_decennial(geography = "congressional district", 
                             variables = "P1_004N", 
                             year = 2020,
                             sumfile = "pl")
pop20_white_cd <- get_decennial(geography = "congressional district", 
                                variables = "P1_003N", 
                                year = 2020,
                                sumfile = "pl")

# row bind House and Senate members #
pop20_all <- bind_rows(pop20_state,pop20_cd)
names(pop20_all)[4] <- "all_pop"
pop_all <- pop20_all[,-3]
pop20_black <- bind_rows(pop20_black,pop20_black_cd)
names(pop20_black)[4] <- "black_pop"
pop_black <- pop20_black[,c(1,4)]
pop20_white <- bind_rows(pop20_white,pop20_white_cd)
names(pop20_white)[4] <- "white_pop"
pop_white <- pop20_white[,c(1,4)]
# merge all population, black, and white population for each state and congressional districts #
all_black <- inner_join(pop_all, pop_black, by="GEOID")
cd_race <- inner_join(all_black, pop_white, by="GEOID")
cd_race$black_percent <- round(cd_race$black_pop/cd_race$all_pop*100, digits = 2)
cd_race$nonwhite_percent <- round((cd_race$all_pop-cd_race$white_pop)/cd_race$all_pop*100, digits = 2)

# merge politicians and (geographic) demographic data #
library(readxl)
tweet <- read_excel("politician_tweet_count.xlsx")
library(stringr)
tweet_1 <- tweet[1:100,]
tweet_1$GEOID <- str_pad(tweet_1$GEOID, 2, pad="0")
tweet_2 <- tweet[101:518,]
tweet_2$GEOID <- str_pad(tweet_2$GEOID, 4, pad="0")
politician <- bind_rows(tweet_1, tweet_2)
merge_data <- left_join(politician, cd_race, by="GEOID")
merged_data <- select(merge_data, -NAME)
library(writexl)
write_xlsx(merge_data, "politician_cd_data.xlsx")
