library(ggplot2)
library(plyr)
library(dplyr) 
library(ggpubr)

# probes: c("Ctrl(-)", "Ctrl(+)", "PBS", "0.1%N ", "1%N " , "10%N ", "1%U", "10%U") 

mydata <- read.csv('Book11.csv', sep=";")

mydata$substance

levels(mydata$substance)

mydata$substance <- ordered(mydata$substance,
                             levels = c("Ctrl(-)", "Ctrl(+)", "PBS", "0.1%N ", 
                                        "1%N " , "10%N ", "1%U", "10%U"))

levels(mydata$substance)

mydata$substance

plot(TMRM ~ substance, data=mydata)

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

## Compute summary statistics for the variable len organized into groups by the variable dose:

group_by(mydata, substance)
df.summary1 <- group_by(mydata, substance)
df.summary1


df.summary <- mydata %>%
  group_by(substance) %>%
  summarise(
    sd = sd(TMRM, na.rm = TRUE),
    TMRM = mean(TMRM)
  )

df.summary


# (2) Bar plot

ggplot(df.summary, aes(TMRM, substance)) +
  geom_bar(stat = "identity", fill = "lightgray", 
           color = "black") +
  geom_errorbar(aes(ymin = TMRM-sd, ymax = TMRM+sd), width = 0.3)

# (2) Bar plots of means + individual jitter points + errors

df <- mydata

ggplot(df, aes(substance, TMRM)) +
  geom_bar(stat = "identity", data = df.summary,
           fill = NA, color = "black") +
  geom_jitter( position = position_jitter(0.2),
               color = "black") + 
  geom_errorbar(
    aes(ymin = TMRM-sd, ymax = TMRM+sd),
    data = df.summary, width = 0.2) 

# Perform pairwise comparisons
# Summary of the analysis

summary(res.aov)

TukeyHSD(res.aov)

par(mar = c(4.5, 8, 4.5, 4.5))
plot(TukeyHSD(res.aov), las = 1)

# Perform pairwise comparisons

compare_means(TMRM ~ substance,  data = mydata, method = "wilcox.test")

wilcox <- compare_means(TMRM ~ substance,  data = mydata, method = "wilcox.test")

View(wilcox)


# ?compare_means


compare_means(TMRM ~ substance,  data = mydata, method = "t.test")

t_test <- compare_means(TMRM ~ substance,  data = mydata, method = "t.test")

View(t_test)


# Visualize: Specify the comparisons you want

my_comparisons <- list( c( "1%N " ,"1%U"), c(  "10%N ","10%U"))

ggboxplot(mydata, x = "substance", y = "TMRM")+ 
  stat_compare_means(comparisons = my_comparisons)+ 
  stat_compare_means(label.y = 50)  

# Visualize: Specify the comparisons you want color
my_comparisons <- list( c(  "10%N ","10%U") )

ggboxplot(mydata, x = "substance", y = "TMRM",
          color = "substance", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons)+ 
  stat_compare_means(label.y = 50)  



