library(ggplot2)
library(plyr)
library(dplyr) 
library(ggpubr)
library(car)

### START ###

data1 <- read.csv('Book333.csv')

plot(TMRM ~ substance, data=data1)

my_data <- data1

# Show the levels

levels(my_data$substance)

my_data$substance <- ordered(my_data$substance,
                             levels = c("Ctrl(-)", "Ctrl(+)", "PBS", "0.1%N ", 
                                        "1%N " , "10%N ", "1%U", "10%U"))

plot(TMRM ~ substance, data=my_data)

# normality test
# Kruskal Wallis Test One Way Anova by Ranks 

kruskal.test(my_data$TMRM ~ my_data$substance) # where y1 is numeric and A is a factor

# Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.

library("ggpubr")

ggdensity(my_data$TMRM, 
          main = "Density plot of TMRM",
          xlab = "TMRM")

ggdensity(my_data$TMRM[my_data$substance != "Ctrl(-)"], 
          main = "Density plot of TMRM",
          xlab = "TMRM")

# Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution. 

library(ggpubr)
ggqqplot(my_data$TMRM)
ggqqplot(my_data$TMRM[my_data$substance != "Ctrl(-)"])

library("car")
qqPlot(my_data$TMRM)
qqPlot(my_data$TMRM[my_data$substance != "Ctrl(-)"])

# perform the Shapiro-Wilk test of normality for one variable (univariate):

shapiro.test(my_data$TMRM)

shapiro.test(my_data$TMRM[my_data$substance != "Ctrl(-)"])

# Compute summary statistics by groups - count, mean, sd:u
# From the output, the p-value > 0.05 implying that 
# the distribution of the data are not significantly different from normal distribution. 
# In other words, we can assume the normality.


# Perform pairwise comparisons

?compare_means


compare_means(TMRM ~ substance,  data = my_data, method = "wilcox.test")

wilcox <- compare_means(TMRM ~ substance,  data = my_data)

View(wilcox)


compare_means(TMRM ~ substance,  data = my_data, method = "t.test")

t_test <- compare_means(TMRM ~ substance,  data = my_data, method = "t.test")

View(t_test)

# Visualize: Specify the comparisons you want

my_comparisons <- list( c( "1%N " ,"1%U"), c(  "10%N ","10%U"))

ggboxplot(my_data, x = "substance", y = "TMRM",
          color = "substance", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons)+ 
  stat_compare_means(label.y = 50)


# Visualize: Specify the comparisons you want

my_comparisons <- list( c( "1%N " ,"1%U"), c(  "10%N ","10%U"))

ggboxplot(my_data, x = "substance", y = "TMRM")+ 
  stat_compare_means(comparisons = my_comparisons)+ 
  stat_compare_means(label.y = 50) 


### END ###







group_by(my_data, substance) %>%
  summarise(
    #count = n(),
    mean = mean(TMRM, na.rm = TRUE),
    sd = sd(TMRM, na.rm = TRUE)
  )

??group_by

group_by(my_data, substance)


df <- group_by(my_data, substance)
View(df)

## Compute summary statistics for the variable len organized into groups by the variable dose:

group_by(df, substance) %>%
  summarise(
    sd = sd(TMRM, na.rm = TRUE),
    TMRM = mean(TMRM)
  )

df.summary <- group_by(df, substance) %>%
  summarise(
    sd = sd(TMRM, na.rm = TRUE),
    TMRM = mean(TMRM)
  )

df.summary

# (2) Bar plot

ggplot(df.summary, aes(substance, TMRM)) +
  geom_bar(stat = "identity", fill = "lightgray", 
           color = "black") +
  geom_errorbar(aes(ymin = TMRM, ymax = TMRM+sd), width = 0.2) 

# (2) Bar plots of means + individual jitter points + errors

df <- my_data

ggplot(df, aes(substance, TMRM)) +
  geom_bar(stat = "identity", data = df.summary,
           fill = NA, color = "black") +
  geom_jitter( position = position_jitter(0.2),
               color = "black") + 
  geom_errorbar(
    aes(ymin = TMRM-sd, ymax = TMRM+sd),
    data = df.summary, width = 0.2) 



# If you still want to use R base graphs, type the following scripts:
# Box plot
boxplot(TMRM ~ substance, data = my_data,
        xlab = "Treatment", ylab = "TMRM fluorescence intensity, a.u.",
        frame = FALSE, col = c("#0000ff", "#0000ff", "#0000ff", "#ff0000",
                               "#ff0000", "#ff0000", "#00FF00", "#00FF00"))

#?boxplot

# plotmeans

library("gplots")
plotmeans(TMRM ~ substance, data = my_data, frame = FALSE,
          xlab = "Treatment", ylab = "Weight",
          main="Mean Plot with 95% CI") 


# Compute the analysis of variance
res.aov <- aov(TMRM ~ substance, data = my_data)

# Summary of the analysis

summary(res.aov)

TukeyHSD(res.aov)

par(mar = c(4.5, 8, 4.5, 4.5))

plot(TukeyHSD(res.aov), las = 1)




# Randomized Block Design - Friedman Test 
# friedman.test(my_data$TMRM~A|B)
# where y are the data values, A is a grouping factor
# and B is a blocking factor

