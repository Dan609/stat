library(ggplot2)
library(plyr)
library(dplyr) 
library(ggpubr)
library(car)
library(stringi)
library(Hmisc)
library(gplots)

#data0 <- read.csv('coloc.csv')

data1 <- read.csv('coloc1.csv')

#data0$probe <- ordered(data0$probe,
#                            levels = c("p7", "p9", "p12", 
#                                       "p15", "p18", "p20", 
#                                       "p21", "p25", "p27", 
#                                       "p28"))

data1$probe <- ordered(data1$probe,
                       levels = c("p7", "p9", "p12", 
                                  "p15", "p18", 
                                  "p21", "p25", "p27", 
                                  "p28"))
# normality test
# perform the Shapiro-Wilk test of normality for one variable (univariate):

shapiro.test(data1$Rvalue)

# Kruskal Wallis Test One Way Anova by Ranks 

kruskal.test(data1$Rvalue ~ data1$probe)


# Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.


ggdensity(data1$Rvalue, 
          main = "Density plot of Rvalue",
          xlab = "Rvalue")


# Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution. 

ggqqplot(data1$Rvalue)

qqPlot(data1$Rvalue)


# Perform pairwise comparisons
# ?compare_means

compare_means(Rvalue ~ probe,  data = data1, method = "t.test")
compare_means(Rvalue ~ probe,  data = data1, method = "wilcox.test")

write.csv(compare_means(Rvalue ~ probe,  data = data1, method = "t.test"), file="t.test.csv")
write.csv(compare_means(Rvalue ~ probe,  data = data1, method = "wilcox.test"), file="wilcox.test.csv")

#View(compare_means(Rvalue ~ probe,  data = data1, method = "t.test"))
#View(compare_means(Rvalue ~ probe,  data = data1, method = "wilcox.test"))

## Compute summary statistics for the variable len organized into groups by the variable dose:

df.summary <- group_by(data1, probe) %>%
  summarise(
    sd = sd(Rvalue, na.rm = TRUE),
    Rvalue = mean(Rvalue)
  )


df.summary

# (2) Bar plots of means + individual jitter points + errors

df <- data1

ggplot(df, aes(probe, Rvalue)) +
  geom_bar(stat = "identity", data = df.summary,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
               color = "black") + 
  geom_errorbar(
    aes(ymin = Rvalue-sd, ymax = Rvalue+sd),
    data = df.summary, width = 0.2)

# add line connecting means:

qplot(probe, Rvalue, data=data1) + 
  stat_summary(fun.y=mean, colour="red", geom="line", aes(group = 1))

# (2) Bar plot

ggplot(df.summary, aes(probe, Rvalue)) +
  geom_bar(stat = "identity", fill = "lightgray", 
           color = "black") +
  geom_errorbar(aes(ymin = Rvalue-sd, ymax = Rvalue+sd), width = 0.2)


# One-way ANOVA

# If you still want to use R base graphs, type the following scripts:

# Box plot
boxplot(Rvalue ~ probe, data = data1,
        xlab = "Passage", ylab = "Pearson R value",
        frame = FALSE, col = c("#ffffff"))

# Visualize: Specify the comparisons you want color
my_comparisons <- list( c("p12","p15"), c("p21","p28"), c("p15", "p18"))

ggboxplot(data1, x = "probe", y = "Rvalue")+ 
  stat_compare_means(comparisons = my_comparisons)+ 
  stat_compare_means(label.y = 1)

# simple ggplot

ggplot(data1, aes(x = probe, y = Rvalue)) + 
  geom_boxplot()

#my_comparisons <- list( c("p21","p28"), c("p15","p12"))
#ggboxplot(data1, x = "probe", y = "Rvalue",
#          color = "probe", palette = "jco")+ 
#  stat_compare_means(comparisons = my_comparisons)+ 
#  stat_compare_means(label.y = 1)
#data1$probe <- as.factor(data1$probe)
#obj <- ggplot(data1, aes(x = as.factor(probe), y = Rvalue, col = probe, group = probe))+
#  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
#  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
#  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
#obj

# Compute the analysis of variance

res.aov <- aov(Rvalue ~ probe, data = data1)

# Summary of the analysis

summary(res.aov)

TukeyHSD(res.aov)

par(mar = c(4.5, 8, 4.5, 4.5))

plot(TukeyHSD(res.aov), las = 1)

# plotmeans

plotmeans(Rvalue ~ probe, data = data1, frame = FALSE, 
          mean.labels=FALSE, connect=TRUE,
          n.label=TRUE, text.n.label="n = ",
          xlab = "Passages", ylab = "Pearson R value",
          main="Colocalization of Myosin-9 and F-actin in WJMSC-1 cells, 
          \nMean Plot with 95% CI")

#

savehistory(file='myscript.R')

# where plots saved:

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

# Now, you can copy these files to your desired directory, as follows:
# file.copy(from=plots.png.paths, to="path_to_folder")
