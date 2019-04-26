# Statistical analysis of colocalozation coefficients, Dan Bobkov, 2019
# Firstly, calculate coef in ImajeJ :
# Kendall's Tau-b rank correlation value = bTau
# Spearman's rank correlation value = Rs
# Manders' coefficients = tM1 and tM2
# Pearson's R value (above threshold) = Rval

# Import libraries

library(ggplot2)
library(plyr)
library(dplyr) 
library(ggpubr)
library(car)
library(stringi)
library(Hmisc)
library(gplots)
library(PMCMRplus)
library(dunn.test)
library(DescTools)
library(ggsignif)
# Load data

data1 <- read.csv('coloc3.csv')

hist(data1)

# Name dependent variables

data1$probe <- as.factor(data1$probe)

t.test <- compare_means(bTau ~ probe,  data = data1, method = "t.test", ref.group = 'p07')
wilcox.test <- compare_means(bTau ~ probe,  data = data1, method = "wilcox.test", ref.group = 'p07')


# order levels
data1$probe <- ordered(data1$probe,
                       levels = c("p07", "p09", "p12", 
                                  "p15", "p18", 
                                  "p21", "p25", "p27", 
                                  "p28", "p35", "p36"))

#

#

#####################################
# Kendall's Tau-b rank correlation value
#####################################

# normality test
# perform the Shapiro-Wilk test of normality for one variable (univariate):
shapiro.test(data1$bTau)

# Kruskal Wallis Test One Way Anova by Ranks
kruskal.test(data1$bTau ~ data1$probe)


# Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.
ggdensity(data1$bTau, 
          main = "Density plot of bTau",
          xlab = "bTau")


# Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution. 
# qqPlot(data1$bTau)
ggqqplot(data1$bTau)


# Perform pairwise comparisons
compare_means(bTau ~ probe,  data = data1, method = "anova")
compare_means(bTau ~ probe,  data = data1, method = "kruskal.test")


compare_means(bTau ~ probe,  data = data1, method = "t.test")
compare_means(bTau ~ probe,  data = data1, method = "wilcox.test")

write.csv(compare_means(bTau ~ probe,  data = data1, method = "t.test"), file="t.test.bTau.csv")
write.csv(compare_means(bTau ~ probe,  data = data1, method = "wilcox.test"), file="wilcox.test.bTau.csv")


# One-way ANOVA
# Compute the analysis of variance

res.aov <- aov(bTau ~ probe, data = data1)

# Summary of the analysis

summary(res.aov)
TukeyHSD(res.aov)
par(mar = c(4.5, 8, 4.5, 4.5))
plot(TukeyHSD(res.aov), las = 1)
u

# Bar plot with signifiers 

df.summary <- group_by(data1, probe) %>%
  summarise(
    sd = sd(bTau, na.rm = TRUE),
    bTau = mean(bTau)
  )

df.summary

## 

ggplot(df.summary, aes(probe, bTau)) +
  geom_bar(stat = "identity", fill = 'gray', 
           color = "black", size= 1, show.legend=TRUE) +
  geom_errorbar(aes(ymin = bTau-sd, ymax = bTau+sd), width = 0.2, size=1) +
  theme(
    # Change axis lines
    axis.line = element_line(size = 1),
    # Change axis ticks text labels: font color, size and face
    axis.text.x = element_text(face = "bold",
                               size = 12, angle = 90),     # Change x axis tick labels only
    axis.text.y = element_text(face = "bold", 
                               size = 12, angle = 0),     # Change y axis tick labels only
    # Change axis ticks line: font color, size, linetype and length
    axis.ticks = element_line(),      # Change ticks line fo all axes
    axis.ticks.x = element_line(),    # Change x axis ticks only
    axis.ticks.y = element_line(),    # Change y axis ticks only
    axis.ticks.length = unit(3, "pt") # Change the length of tick marks
  ) +
  geom_point() +
  ylim(0, 1) + 
  ggtitle("Colocalization of myosin-9 and F-actin in MSCWJ-1 cells") + 
  labs(y="Kendall's Tau-b rank correlation value", x = "Experimental groups") +
  # xmin / xmax positions should match the x-axis labels' positions
  geom_signif(y_position = c(0.95),
              xmin = c(5),
              xmax = c(9),
              annotation = "***", 
              tip_length = 0.04)  +
  # xmin / xmax positions should match the x-axis labels' positions
  geom_signif(y_position = c(0.85),
              xmin = c(2),
              xmax = c(4),
              annotation = "*", 
              tip_length = 0.04) +
  # xmin / xmax positions should match the x-axis labels' positions
  geom_signif(y_position = c(0.9),
              xmin = c(9),
              xmax = c(11),
              annotation = "*", 
              tip_length = 0.04) +
  # xmin / xmax positions should match the x-axis labels' positions
  geom_signif(y_position = c(0.77),
              xmin = c(4),
              xmax = c(6),
              annotation = "**", 
              tip_length = 0.04)


##
# (1) Compute summary statistics for the variable probe
# (2) Bar plots of means + individual jitter points + errors
# Kendall's Tau-b rank correlation value

df.summary.bTau <- group_by(data1, probe) %>%
  summarise(
    sd = sd(bTau, na.rm = TRUE),
    bTau = mean(bTau)
  )

df.summary.bTau

df.bTau <- data1

ggplot(df.bTau, aes(probe, bTau)) +
  geom_bar(stat = "identity", data = df.summary.bTau,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = bTau-sd, ymax = bTau+sd),
    data = df.summary.bTau, width = 0.2)

# plotmeans

plotmeans(bTau ~ probe, data = data1, frame = FALSE, ylim = c(0, 1),
          mean.labels=FALSE, connect=TRUE,
          n.label=TRUE, text.n.label="n = ",
          xlab = "Passages", ylab = "Kendall's Tau-b rank correlation value",
          main="Colocalization of Myosin-9 and F-actin in WJMSC-1 cells, 
          \nMean Plot with 95% CI") + scale_x_discrete(name ="Passages", 
                                                       limits=c("p07", "p09", "p12", 
                                                                "p15", "p18", 
                                                                "p21", "p25", "p27", 
                                                                "p28", "p35", "p36")) +
  scale_y_continuous(name="Kendall's Tau-b rank correlation value", limits=c(0, 1))






# where plots saved:

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

# Now, you can copy these files to your desired directory, as follows:
# file.copy(from=plots.png.paths, to="path_to_folder")
savehistory(file='myscript.R')
#####################################

#

#
# 
# boxplot(bTau ~ probe, data1)
# boxplot(Rval ~ probe, data1)
# boxplot(Rs ~ probe, data1)
# boxplot(tM1 ~ probe, data1)
# boxplot(tM2 ~ probe, data1)
#
