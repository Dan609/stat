# Statistical analysis of MTT results, Dan Bobkov, 2019
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

### Start analysis

data1 <- read.csv('mtt1.csv', sep = ';')

data1$probe <- as.factor(data1$probe)

data1$probe <- ordered(data1$probe,
                       levels = c("control", "control-water", "control-PBS", 
                                  "N_0.02", "N_0.2", "N_2", "N_20", "N_100",
                                  "U_0.02", "U_0.2", "U_2", "U_20", "U_100"))

# normality test
# perform the Shapiro-Wilk test of normality for one variable (univariate):
# shapiro.test(data1$MTT.6h)

shapiro.test(data1$MTT.6h[data1$probe =='control'])
shapiro.test(data1$MTT.6h[data1$probe =='control-water'])
shapiro.test(data1$MTT.6h[data1$probe =='control-PBS'])
shapiro.test(data1$MTT.6h[data1$probe =='N_0.02'])
shapiro.test(data1$MTT.6h[data1$probe =='N_0.2'])
shapiro.test(data1$MTT.6h[data1$probe =='N_2'])
shapiro.test(data1$MTT.6h[data1$probe =='N_20'])
shapiro.test(data1$MTT.6h[data1$probe =='N_100'])
shapiro.test(data1$MTT.6h[data1$probe =='U_0.02'])
shapiro.test(data1$MTT.6h[data1$probe =='U_0.2'])
shapiro.test(data1$MTT.6h[data1$probe =='U_2'])
shapiro.test(data1$MTT.6h[data1$probe =='U_20'])
shapiro.test(data1$MTT.6h[data1$probe =='U_100'])

# Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.
ggdensity(data1$MTT.6h, 
          main = "Density plot of MTT.6h",
          xlab = "MTT.6h")

# Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution. 
# qqPlot(data1$MTT.6h)
ggqqplot(data1$MTT.6h)

# normality test
# perform the Shapiro-Wilk test of normality for one variable (univariate):
# shapiro.test(data1$MTT.24h)

shapiro.test(data1$MTT.24h[data1$probe =='control'])
shapiro.test(data1$MTT.24h[data1$probe =='control-water'])
shapiro.test(data1$MTT.24h[data1$probe =='control-PBS'])
shapiro.test(data1$MTT.24h[data1$probe =='N_0.02'])
shapiro.test(data1$MTT.24h[data1$probe =='N_0.2'])
shapiro.test(data1$MTT.24h[data1$probe =='N_2'])
shapiro.test(data1$MTT.24h[data1$probe =='N_20'])
shapiro.test(data1$MTT.24h[data1$probe =='N_100'])
shapiro.test(data1$MTT.24h[data1$probe =='U_0.02'])
shapiro.test(data1$MTT.24h[data1$probe =='U_0.2'])
shapiro.test(data1$MTT.24h[data1$probe =='U_2'])
shapiro.test(data1$MTT.24h[data1$probe =='U_20'])
shapiro.test(data1$MTT.24h[data1$probe =='U_100'])

# Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.
ggdensity(data1$MTT.24h, 
          main = "Density plot of MTT.24h",
          xlab = "MTT.24h")

# Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution. 
# qqPlot(data1$MTT.6h)
ggqqplot(data1$MTT.24h)

# Bartlett test of homogeneity of variances
# bartlett.test(MTT.6h ~ probe, data1)
# bartlett.test(MTT.24h ~ probe, data1)

# Levene’s test is an alternative to Bartlett’s test when the data is not normally distributed.
leveneTest(MTT.6h ~ probe, data1)
leveneTest(MTT.24h ~ probe, data1)

# One-way ANOVA
# Compute the analysis of variance Tukey

res.aov <- aov(MTT.6h ~ probe, data = data1)
summary(res.aov)
#export it to wordfile
capture.output(summary(res.aov),file="res.aov.6h.doc")
# TukeyHSD(res.aov)
# par(mar = c(4.5, 8, 4.5, 4.5))
# plot(TukeyHSD(res.aov), las = 1)

res.aov <- aov(MTT.24h ~ probe, data = data1)
summary(res.aov)
#export it to wordfile
capture.output(summary(res.aov),file="res.aov.24h.doc")
# TukeyHSD(res.aov)
# par(mar = c(4.5, 8, 4.5, 4.5))
# plot(TukeyHSD(res.aov), las = 1)

# Post hoc
# Calculate pairwise comparisons between group levels with corrections for multiple testing

pairwise.t.test(data1$MTT.6h, data1$probe, p.adj = "holm")$p.value
pairwise.t.test(data1$MTT.24h, data1$probe, p.adj = "holm")$p.value


t.test.6h <- pairwise.t.test(data1$MTT.6h, data1$probe, p.adj = "holm")
t.test.6h$p.value
write.csv(t.test.6h$p.value, file="t.test.6h.csv")

t.test.24h <- pairwise.t.test(data1$MTT.24h, data1$probe, p.adj = "holm")
t.test.24h$p.value
write.csv(t.test.24h$p.value, file="t.test.24h.csv")

# Kruskal Wallis Test One Way Anova by Ranks
# kruskal.test(data1$MTT.6h ~ data1$probe)
# kruskal.test(data1$MTT.24h ~ data1$probe)

# Perform pairwise comparisons
# compare_means(MTT.6h ~ probe,  data = data1, method = "t.test", 
#              p.adjust.method = 'holm')
# compare_means(MTT.24h ~ probe,  data = data1, method = "t.test", 
#              p.adjust.method = 'holm')
# write.csv(compare_means(MTT.6h ~ probe,  data = data1, method = "t.test", 
#                        p.adjust.method = 'holm'), 
#          file="t.test.MTT.6h.csv")

# write.csv(compare_means(MTT.24h ~ probe,  data = data1, method = "t.test", 
#                        p.adjust.method = 'holm'), 
#          file="t.test.MTT.24h.csv")


# Bar plot 1

df.summary.MTT.6h <- group_by(data1, probe) %>%
  summarise(
    sd = sd(MTT.6h, na.rm = TRUE),
    MTT.6h = mean(MTT.6h)
  )

df.summary.MTT.6h
summary_6h <- df.summary.MTT.6h

##

ggplot(df.summary.MTT.6h, aes(probe, MTT.6h)) +
  geom_bar(stat = "identity", fill = 'gray', 
           color = "black", size= 1, show.legend=TRUE) +
  geom_errorbar(aes(ymin = MTT.6h-sd, ymax = MTT.6h+sd), width = 0.2, size=1) +
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
  ylim(0, 0.3) + 
  ggtitle("MTT, 6 h of incubation") + 
  labs(y="540 nm absorbance (a.u.)", x = "Experimental groups") +
  # xmin / xmax positions should match the x-axis labels' positions
  geom_signif(y_position = c(0.27),
              xmin = c(3),
              xmax = c(6),
              annotation = "p = 0.8", 
              tip_length = 0.04)


# Bar plot 2

df.summary.MTT.24h <- group_by(data1, probe) %>%
  summarise(
    sd = sd(MTT.24h, na.rm = TRUE),
    MTT.24h = mean(MTT.24h)
  )

df.summary.MTT.24h
summary_24h <- df.summary.MTT.24h

## 

ggplot(df.summary.MTT.24h, aes(probe, MTT.24h)) +
  geom_bar(stat = "identity", fill = 'gray', 
           color = "black", size= 1, show.legend=TRUE) +
  geom_errorbar(aes(ymin = MTT.24h-sd, ymax = MTT.24h+sd), width = 0.2, size=1) +
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
  ylim(0, 0.3) + 
  ggtitle("MTT, 24 h of incubation") + 
  labs(y="540 nm absorbance (a.u.)", x = "Experimental groups") +
  # xmin / xmax positions should match the x-axis labels' positions
  geom_signif(y_position = c(0.27),
              xmin = c(2),
              xmax = c(5),
              annotation = "p = 0.3", 
              tip_length = 0.04)


##