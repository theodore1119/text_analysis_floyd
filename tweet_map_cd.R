library(ggplot2)
library(readxl)
library(dplyr)
library(stringr)

data <- read_excel("politician_tweet_count.xlsx")
floyd_map <- data[!is.na(data$geoid),]
floyd_map$geoid <- str_pad(floyd_map$geoid, 4, pad="0")
floyd_map <- floyd_map[,-5]

library(USAboundaries)
# ?us_congressional to see other options
cong.map <- us_congressional(resolution = "high")
ggplot(cong.map) +
  geom_sf()
cong.map <- filter(cong.map, state_name %in% state.name & !(state_name %in% c("Hawaii", "Alaska")))
ggplot(cong.map) +
  geom_sf()
floyd.map <- left_join(cong.map, floyd_map, by = "geoid")
names(floyd.map)[4] <- "fips"
floyd.map$tweet_quart <- NA
floyd.map$tweet_quart[which(floyd.map$no_tweets %in% c(0,1))] <- "0-1"
floyd.map$tweet_quart[which(floyd.map$no_tweets %in% c(2,3))] <- "2-3"
floyd.map$tweet_quart[which(floyd.map$no_tweets %in% c(4,5,6,7))] <- "4-7"
floyd.map$tweet_quart[which(floyd.map$no_tweets > 7)] <- "8 or more"
floyd.map$tweet_quart[which(is.na(floyd.map$no_tweets))] <- "NA"

floyd.map %>% 
  ggplot(aes(fill = tweet_quart)) +
  geom_sf(color="black", size=.2, alpha=.5) +
  scale_fill_manual(values=c("white","#BDD7E7","#6BAED6","#08519C","#D3D3D3"))+
  labs(fill="Number of Floyd Tweets") +
  theme_void()
