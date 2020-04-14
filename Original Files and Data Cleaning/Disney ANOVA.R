### ANOVA for Brewer HUB ###
### Authored by Morgan Middlebrooks ###

##### ANOVA INFO ###### 

#  Understand what an ANOVA is even doing/ what sort of data is appropriate for it. 

## It's important to understand what TYPE of data you need in ANOVA. An ANOVA is
## basically just seeing if there is statistically significant difference
## between the groups of a IV on some outcome variable. 

## So you need the IV to be categorical and the DV to be continuous. 

##### Libraries #####
# install.packages("tidyverse")
# install.packages("ggplot")

### Load Libraries
library(tidyverse)
library(ggplot2)


### Load the Data in ###

load(file = "Movie Data.Rdata")
movie_data <- movie_data[complete.cases(movie_data$genre),]


#### Subsetting Data ####


## We're going to pick the genres with the largest cell sizes that are closest
## to equal.

group_by(movie_data, genre) %>%
  summarise(
    count = n())

## It looks like Adventure, Comedy, and Drama are our closest to equal cell
## sizes


anova.df <- subset(movie_data, genre %in% c('Adventure','Comedy', 'Drama'))

group_by(anova.df, genre) %>%
  summarise(
    count = n())

boxplot(total_gross ~ genre, data = anova.df, main = "Gross Revenue",
        ylab = "Total Gross Revenue", xlab = "Genre", col = "salmon")

outlier.invest <- ggplot(anova.df, aes(genre, total_gross)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(fill = "steelblue", colour = "black", outlier.colour = "red", outlier.shape = 4) +
  ggtitle("Outlier Investigation") + 
  theme_classic() +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = .5, size = 12, colour = "black"))

outlier.invest

## looks like we have so pretty serious outliers. 


#### Dealing w/ Outliers...sort of #####

outliers <- boxplot(anova.df$total_gross, plot=FALSE)$out

print(outliers)

anova.df.out <- anova.df[-which(anova.df$total_gross %in% outliers),]

outlier.invest.2 <- ggplot(anova.df.out, aes(genre, total_gross)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(fill = "steelblue", colour = "black", outlier.colour = "red", outlier.shape = 4) +
  ggtitle("Outlier Investigation") + 
  theme_classic() +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = .5, size = 12, colour = "black"))

outlier.invest.2

## We still have some after a basic outlier removal that look like outliers, but
## were not classified this way. There are lots of fancy ways you can check if
## those are TRUE outliers (using Cook's d for example), but we're not doing
## that today.

## Notice how the bars are stretched more similarly now. 


#### Levene Test ####

car::leveneTest(total_gross ~ genre, data = anova.df.out)

## There is a difference, but barely. A better scientist would not let this
## slide. The groups should have equal variance!

#### THE ANOVA FINALLY ####

mod <- aov(total_gross ~ genre, data = anova.df.out)

summary(mod)

## There is a difference in genre....but WHERE YOU ASK?

#### Post Hoc ####

TukeyHSD(mod)

## Difference between comedy + adventure
## Difference between drama + adventure
## No difference in drama + comedy

group_by(anova.df, genre) %>%
  summarise(
    count = n(),
    mean = mean(total_gross, na.rm = TRUE),
    sd = sd(total_gross, na.rm = TRUE)
  )


## adventure makes way more money here


## Check out boxplots below to really send message home that you're going to
## make more money on adventure

bp <- ggplot(anova.df.out, aes(genre, total_gross)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(fill = "steelblue", colour = "black", outlier.colour = "red", outlier.shape = 4) +
  ggtitle("Movie Monies") + 
  theme_classic() +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = .5, size = 12, colour = "red"))

## looking at this plot you can see that Adventure's mean is higher than both
## comedy and drama, but comedy and drama aren't that different
