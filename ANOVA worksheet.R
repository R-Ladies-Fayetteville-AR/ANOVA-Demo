### ANOVA Example


# 1: Load the dataset into R & read a little bit about the dataset. 

df <- PlantGrowth  ## creates a data frame from this plant growth data that is just built into R

?PlantGrowth ## brings up information on the dataset. You can do this for datasets that are built into R


# 2: Understand what an ANOVA is even doing/ what sort of data is appropriate for it. 

## It's important to understand what TYPE of data you need in ANOVA. An ANOVA is
## basically just seeing if there is statistically significant difference
## between the groups of a IV on some outcome variable. 

## So you need the IV to be categorical and the DV to be continuous. 

str(df) 
## check the structure of the data. What sort of variables do we have? Are they
## factor 

levels(df$group)


## this lets you look at the different levels of treatment groups for these plants. 


boxplot(weight ~ group, data = PlantGrowth, main = "PlantGrowth data",
        ylab = "Dried weight of plants", col = "salmon")



## a cuter  way: 
  
library(ggplot2)

bp <-  qplot(group,  weight, data = df, 
      geom=c("boxplot", "jitter"), fill = group)

bp + ggtitle("Plant growth with\ndifferent treatments") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = .5, size = 12, colour = "salmon"))



## This lets you look at the difference in the groups graphically 


# 3: Do the actual ANOVA analysis

### FINALLY THE ANOVA ### 


mod <- aov(weight ~ group, data = df)

?aov


summary(mod)

## There is a significant difference between groups. BUT WHAT GROUPS?
library(tidyverse)
library(dplyr)
group_by(df, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

TukeyHSD(mod)

# diff: difference between means of the two groups
# lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
# p adj: p-value after adjustment for the multiple comparisons.


## From the Tukey Model output you can see that there is only a sig difference
## between trt 1 and trt 2



## Testing assummptions of an ANOVA

## Homogeniety of Variance: variance of groups does not differ significantly
## from each other

plot(mod, 1)

## This looks mostly fine but there are some outliers. What you do with outliers
## sort of depends.

car::leveneTest(weight ~ group, data = df)

## Though there may be some violations of homogeneity of variance, the groups
## ultimately are significantly different.

plot(mod, 2)

## Normality plot of residuals. In the plot below, the quantiles of the
## residuals are plotted against the quantiles of the normal distribution. A
## 45-degree reference line is also plotted.

## The normal probability plot of residuals is used to check the assumption that
## the residuals are normally distributed. It should approximately follow a
## straight line.


