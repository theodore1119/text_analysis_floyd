library(readxl)
library(MASS)
tweet <- read_excel("politician_cd_data.xlsx")

tweet$party <- factor(tweet$party)
tweet$black <- factor(tweet$black)
tweet$nonwhite <- factor(tweet$nonwhite)
tweet$black_prop <- tweet$black_percent/100
tweet$nonwhite_prop <- tweet$nonwhite_percent/100
tweet$clinton_2016_prop <- tweet$clinton_2016/100

m1 <- glm.nb(no_tweets ~ party + black + black_prop, data = tweet)
summary(m1)
m2 <- glm.nb(no_tweets ~ party + nonwhite + nonwhite_prop, data = tweet)
summary(m2)
m3 <- glm.nb(no_tweets ~ party + black + black_prop + clinton_2016_prop, data = tweet)
summary(m3)
m4 <- glm.nb(no_tweets ~ party + nonwhite + nonwhite_prop + clinton_2016_prop, data = tweet)
summary(m4)

# Coefficient plots
library(dotwhisker)
library(dplyr)
dwplot(list(m1, m2, m3, m4),
       vline = geom_vline(
         xintercept = 0,
         colour = "grey60",
         linetype = 2
       ), 
       dot_args = list(size=2),
       whisker_args = list(size=0.8),
       model_order = c("Model 1","Model 2","Model 3","Model 4")) %>%
  relabel_predictors(
    c(
      partyR = "Republican",
      partyI = "Independent",
      black1 = "Black member",
      nonwhite1 = "Nonwhite member",
      black_prop = "Black share",
      nonwhite_prop = "Nonwhite share",
      clinton_2016_prop = "Clinton 2016 vote"
    )
  ) + xlab("Coefficient Estimate") + ylab("") +
  theme_light() +
  theme(
    axis.text = element_text(size=15),
    axis.title = element_text(size=18),
    legend.key.size = unit(0.8,'cm'),
    legend.text = element_text(size=15),
    legend.position = c(0.993, 0.99),
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "grey80"),
    legend.title = element_blank()
  )

# Categorize districts by the share of black population
tweet$`Share of Black Population`[which(tweet$black_prop<=0.12065)] <- "< 12.1%"
tweet$`Share of Black Population`[which(tweet$black_prop>0.12065 & tweet$black_prop<=0.5)] <- "12.1% - 50%"
tweet$`Share of Black Population`[which(tweet$black_prop>0.5)] <- "> 50%"
tweet$`Share of Black Population` <- factor(tweet$`Share of Black Population`, levels = c("< 12.1%","12.1% - 50%","> 50%"))
tweet_1 <- tweet[!is.na(tweet$`Share of Black Population`),]

# Marginal plot for party
library(sjPlot)
plot_model(m3, type = "pred", terms = "party[D,R]", line.size = 1.2) +
  geom_point(size=4) +
  xlab("Party") +
  ylab("Predicted Number of Floyd Tweets") +
  scale_x_discrete(limits=c("D","R"), labels=c("Democrat","Republican")) +
  ylim(0,7) +
  theme_light() +
  theme(aspect.ratio = 1) +
  theme(title = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20))

# Scatter plot
library(ggplot2)
ggplot(data=tweet_1, aes(x=clinton_2016_prop, y=no_tweets, shape=`Share of Black Population`)) +
  xlab("Clinton's 2016 Vote Share") +
  ylab("Number of Floyd Tweets") +
  geom_point(aes(colour=`Share of Black Population`, alpha=`Share of Black Population`),size=2) +
  scale_x_continuous(limits = c(0,1), labels = scales::percent) +
  scale_color_manual(values = c("#FED100","#009B3A","black")) +
  scale_alpha_manual(values = c(0.36, 0.5, 1)) +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15)) +
  guides(color=guide_legend(override.aes = list(size=3)))
