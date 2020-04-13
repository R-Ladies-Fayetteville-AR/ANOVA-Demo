### Intro to R workshop script cleaned up


############# Section 1 ##########################

############# Programming in R intro #############

############# authored by Stephanie Long

## R in its most basic form can be used as a calculator:

5 + 3 # addition

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


############# Section 2 ##########################

##### ANOVA INFO ###### 

##### Authored by Morgan Middlebrooks

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



############# Section 3 ##########################

######## Data Visualization of Movie Genre Revenue ###########

######## Authored by Stephanie Long


# we will continue to use the movie_data

# removing genre groups with less than 10 movies
large_sample_genre <- filter(movie_data, genre != "Black Comedy" & genre != "Concert/Performance" & genre != "Horror" & genre != "Western")
summary(large_sample_genre$genre)

## adding two columns to divide by 1 billion, so the y-axis of the plots makes more sense
large_sample_genre$total_gross_billion <- large_sample_genre$total_gross/1000000000
large_sample_genre$adj_gross_billion <- large_sample_genre$inflation_adjusted_gross/1000000000

##### Plot 1: simple boxplot

plot1 <- ggplot(data = large_sample_genre) +
  geom_boxplot(aes(x = genre, 
                   y = total_gross_billion, 
                   fill = genre)) +
  theme(axis.text.x  = element_text(angle=75, vjust=0.5, size=9),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Genre Category", 
       y = "Total Gross Revenue (Billions)",
       title = "Movie Genre Total Gross Revenue")
plot1

ggsave("Movie Genre Boxplot.jpeg", plot1)


##### Plot 2: two barplots together

#loading in cleaned data for plot
load("Plot 2 cleaned data.Rdata")

plot2 <- ggplot(data = plot_data2, aes(x = genre, 
                              y = MRevenue, 
                              fill = Revenue_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=75, vjust=0.5, size=9),
        legend.position = "none") +
  facet_wrap(~Revenue_Type ~.) +
  labs(x = "Genre Category", 
       y = "Mean Gross Revenue (Billions)", 
       fill = "Revenue Type") +
  geom_errorbar(aes(ymin=MRevenue-SE, ymax=MRevenue+SE), width=.2, position=position_dodge(.9))
plot2

ggsave("Comparing Mean Gross Revenue for Inflation Adjustment across Movie Genre.jpeg", plot2)


### Plot 3: Comparison of Top 25 grossing movies

library("patchwork") # package for combining plots

#pulling just the top 25 total grossing movies
top_total_25 <- movie_data %>% 
  arrange(desc(total_gross)) %>%
  slice(1:25)

#pulling just the top 25 grossing movies adjusted for inflation
top_adj_25 <- movie_data %>% 
  arrange(desc(inflation_adjusted_gross)) %>%
  slice(1:25)

#combining the data and converting for graphing (don't worry about this part)
top_25 <- merge(top_total_25[,c(1,5)], top_adj_25[,c(1,6)], all = T, by = "movie_title")
top_25_pivot <- pivot_longer(top_25, cols = c("total_gross", "inflation_adjusted_gross"), names_to = "Type", values_to = "Revenue")

# plot 3A
total <- ggplot(data = top_total_25, 
                aes(x = reorder(movie_title, total_gross), y = (total_gross/1000000))) +
  geom_bar(stat = "identity", position = "dodge", fill = "turquoise2") +
  theme_bw() +
  coord_flip() +
  theme(axis.title.y = element_blank(), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))+
  labs(y = "Total Revenue (Millions)",
       title = "Top 25 Movies Based on Total Revenue")
total

# plot 3B
adj <- ggplot(data = top_adj_25, 
              aes(x = reorder(movie_title, inflation_adjusted_gross), y = (inflation_adjusted_gross/1000000000))) +
  geom_bar(stat = "identity", position = "dodge", fill = "salmon") +
  theme_bw() +
  coord_flip() +
  theme(axis.title.y = element_blank(), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))+
  labs(y = "Total Revenue (Billions)",
       title = "Top 25 Movies Based on Inflation Adjusted Revenue")
adj

# combining plot 3A and 3B
plot3 <- total | adj
plot3

ggsave("Comparing Top 25 Grossing Movies Adjusting for Inflation.jpeg", plot3, width = 20, height = 11)
