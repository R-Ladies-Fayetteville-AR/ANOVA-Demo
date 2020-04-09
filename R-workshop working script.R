# Intro to R workshop working script

############# Programming in R intro #############

## R in its most basic form can be used as a calculator:

5 + 3 # addition

5+3   # looks cluttered without spacing, right? right?!

5 - 3 # subtraction

5 * 3 # multiplication

5 / 3 # division

5 %% 3      # find the remainder

3^2         # exponentiation

sqrt(25)    # square root

### you can use R to assign values to variables ###

x = 5						# assigns 5 to x

x								# checks the value of x

y <- 4					# another was to assign 4 to y

10 -> z					# yet one more way to assign 10 to z

x4 = 6					# assigns 6 to the variable x4

x * x4  				# a calculation performed on variables

test_object             # checking the value of a non-existent variable throws an error

test_object <- 1.5

test_object_2 <- 2 

test_object_2 + test_object # can add objects together

###### you can also test logical operations ########

z > x  # is z greater than x

x < y  # is x less than y

z >= x  # is z greater than or equal to x

z == y  # is z equal to y

z - 6 == y  # is the value of z - 6 equal to y

z != y  # is z not equal to y

########### objects in R are stored as certain data types #########

# 1 numeric
typeof(x)

# 2 character
l <- "this is a list"
typeof(l)

# 3 logical
typeof(TRUE)

# 4 list (contains multiple data types)
my_list <-list("a" = 2.5, "b" = TRUE, "c" = 1:3)
typeof(my_list)

# these are all vectors (1 dimension objects)
# there are also 2 dimensional objects

# 1 matrix (contains rows and columns)

my_matrix <- matrix(data = c(1, 2, 3,
              4, 5, 6,
              7, 8, 9), nrow = 3, ncol = 3)
my_matrix
str(my_matrix)

# 2 data frame (contains observations (rows) and variables (columns))
my_dataframe <- as.data.frame(my_matrix)
str(my_dataframe)

# what sets this apart from matrices is that you can call on variables by their name using "$"
my_dataframe$V1






######## disney movie data exploration setup ###########

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
