# Intro to R workshop working script

#setup

load(file = "Movie Data.Rdata")
head(movie_data)

# look at structure
str(movie_data)

#summary stats
summary(movie_data)

# looks like we havve NAs (no value) in the genre category which we are interested in

movie_data <- movie_data[complete.cases(movie_data$genre),]
summary(movie_data)

# we should only look at genres with adequate samples
xtabs(~genre, data = movie_data)

#Black Comedy, Concert/Performance, Horror, and Western have less than 10 cases...

movie_data <- subset()


# using a package - explain
# simple plot

library("tidyverse")

ggplot(data = movie_data) +
  geom_boxplot(aes(x = genre, y = total_gross)) +
  theme(axis.text.x  = element_text(angle=75, vjust=0.7, size=9))
