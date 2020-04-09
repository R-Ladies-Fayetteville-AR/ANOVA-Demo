# Intro to R workshop working script

#setup

load(file = "Movie Data.Rdata")
head(movie_data)

# look at structure
str(movie_data)

#summary stats
summary(movie_data)

# looks like we have NAs (no value) in the genre category which we are interested in

movie_data <- movie_data[complete.cases(movie_data$genre),]
summary(movie_data)

# we should only look at genres with adequate samples
xtabs(~genre, data = movie_data)

#Black Comedy, Concert/Performance, Horror, and Western have less than 10 cases...

large_sample_genre <- filter(movie_data, genre != "Black Comedy" & genre != "Concert/Performance" & genre != "Horror" & genre != "Western")
summary(large_sample_genre$genre)

## adding two columns to divide by 1 billion
large_sample_genre$total_gross_billion <- large_sample_genre$total_gross/1000000000
large_sample_genre$adj_gross_billion <- large_sample_genre$inflation_adjusted_gross/1000000000

# using a package - explain

library("tidyverse")

# simple boxplot

ggplot(data = large_sample_genre) +
  geom_boxplot(aes(x = genre, y = total_gross_billion)) +
  theme(axis.text.x  = element_text(angle=75, vjust=0.7, size=9)) +
  labs(x = "Genre Category", y = "Total Gross Revenue (Billions)")

# plot bar graphs of total gross vs adjusted

#need to sum within the genre category to get the totals for each
total_sum <- aggregate(total_gross_billion ~ genre,data = large_sample_genre, sum)
adj_sum <- aggregate(adj_gross_billion ~ genre, data = large_sample_genre, sum)
#combine the two revenue types
genre_sum <- merge(total_sum, adj_sum, by = "genre")
#convert to "long" format for graph
pivot_genre_sum <- pivot_longer(genre_sum, cols = c('total_gross_billion', 'adj_gross_billion'), names_to='Revenue_Type', 
                                values_to="Revenue")
#bar graph
ggplot(data = pivot_genre_sum, aes(x = genre, y = Revenue, fill = Revenue_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x  = element_text(angle=75, vjust=0.7, size=9)) +
  scale_fill_discrete(labels = c("Inflation Adjusted Gross","Total Gross")) +
  labs(x = "Genre Category", y = "Gross Revenue (Billions)", fill = "Revenue Type")
