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

# Load data

data1 <- read.csv('coloc2.csv')

# Name dependent variables

data1$probe <- as.factor(data1$probe)
data1$probe <- ordered(data1$probe,
                       levels = c("p7", "p9", "p12", 
                                  "p15", "p18", 
                                  "p21", "p25", "p27", 
                                  "p28"))

#

#

#####################################
# Kendall's Tau-b rank correlation value
#####################################

# normality test
# perform the Shapiro-Wilk test of normality for one variable (univariate):
# shapiro.test(data1$bTau)

shapiro.test(data1$bTau[data1$probe =='p7'])
shapiro.test(data1$bTau[data1$probe =='p9'])
shapiro.test(data1$bTau[data1$probe =='p12'])
shapiro.test(data1$bTau[data1$probe =='p15'])
shapiro.test(data1$bTau[data1$probe =='p18'])
shapiro.test(data1$bTau[data1$probe =='p21'])
shapiro.test(data1$bTau[data1$probe =='p25'])
shapiro.test(data1$bTau[data1$probe =='p27'])
shapiro.test(data1$bTau[data1$probe =='p28'])

# Calculate pairwise comparisons between group levels with corrections for multiple testing

pairwise.t.test(data1$bTau, data1$probe, p.adj = "bonf")

# Performs Anderson-Darling all-pairs comparison test.

adAllPairsTest(bTau ~ probe, data1)
out <- adAllPairsTest(bTau ~ probe, data1, p.adjust="holm")
summary(out)
summaryGroup(out)

## data set InsectSprays

if (F){
  ans <- kwAllPairsNemenyiTest(bTau ~ probe, data1)
  plot(ans)
  plot(ans, col="red",main="My title", xlab="Spray", "Count")
}

# post hoc analysis

dunn.test(data1$bTau, data1$probe,
          method="bonferroni",
          wrap=TRUE, kw=TRUE, label=TRUE,
          alpha=0.05, altp=TRUE)

#  post-hoc test after having calculated an ANOVA

PostHocTest(aov(data1$bTau ~ data1$probe, data = data1), method = "scheffe")

r.aov <- aov(data1$bTau ~ data1$probe, data = data1)

# only p-values by setting conf.level to NA

PostHocTest(aov(data1$bTau ~ data1$probe, data = data1), method = "hsd",
            conf.level=NA)

# Kruskal Wallis Test One Way Anova by Ranks ???????????????? ???????????????? — ??????????????

kruskal.test(data1$bTau ~ data1$probe)

if(FALSE){
  kruskal.test(data1$bTau ~ data1$probe=='p7')
  kruskal.test(data1$bTau ~ data1$probe=='p9')
  kruskal.test(data1$bTau ~ data1$probe=='p12')
  kruskal.test(data1$bTau ~ data1$probe=='p15')
  kruskal.test(data1$bTau ~ data1$probe=='p18')
  kruskal.test(data1$bTau ~ data1$probe=='p21')
  kruskal.test(data1$bTau ~ data1$probe=='p25')
  kruskal.test(data1$bTau ~ data1$probe=='p27')
  kruskal.test(data1$bTau ~ data1$probe=='p28')
}

# Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.


ggdensity(data1$bTau, 
          main = "Density plot of bTau",
          xlab = "bTau")


# Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution. 

ggqqplot(data1$bTau)

qqPlot(data1$bTau)

# Perform pairwise comparisons
# ?compare_means

compare_means(bTau ~ probe,  data = data1, method = "t.test")
compare_means(bTau ~ probe,  data = data1, method = "wilcox.test")

write.csv(compare_means(bTau ~ probe,  data = data1, method = "t.test"), file="t.test.bTau.csv")
write.csv(compare_means(bTau ~ probe,  data = data1, method = "wilcox.test"), file="wilcox.test.bTau.csv")
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


# add line connecting means:

qplot(probe, bTau, data=data1) + 
  stat_summary(fun.y=mean, colour="red", geom="line", aes(group = 1))


# (2) Bar plot

ggplot(df.summary.bTau, aes(probe, bTau)) +
  geom_bar(stat = "identity", fill = "lightgray", 
           color = "black") +
  geom_errorbar(aes(ymin = bTau-sd, ymax = bTau+sd), width = 0.2)

# Visualize: Specify the comparisons you want color
my_comparisons <- list( c("p12","p15"), c("p15","p21"), c("p21", "p28"))

ggboxplot(data1, x = "probe", y = "bTau")+ 
  stat_compare_means(comparisons = my_comparisons)+ 
  stat_compare_means(label.y = 1)


####

ggboxplot(data1, x = "probe", y = "bTau")+ 
  stat_compare_means(comparisons = my_comparisons)+ 
  stat_compare_means( aes(label = ..p.signif..), 
                      label.x = 1.5,label.y = 1)

# ggplot with axis names
ggplot(data1, aes(x = probe, y = bTau)) + 
  geom_boxplot() + scale_x_discrete(name ="Passages", 
                                    limits=c("p7", "p9", "p12", 
                                             "p15", "p18", 
                                             "p21", "p25", "p27", 
                                             "p28")) +
  scale_y_continuous(name="Kendall's Tau-b rank correlation value", limits=c(0, 1))

# One-way ANOVA
# Compute the analysis of variance

res.aov <- aov(bTau ~ probe, data = data1)

# Summary of the analysis

summary(res.aov)

TukeyHSD(res.aov)

par(mar = c(4.5, 8, 4.5, 4.5))

plot(TukeyHSD(res.aov), las = 1)

# plotmeans

plotmeans(bTau ~ probe, data = data1, frame = FALSE, 
          mean.labels=FALSE, connect=TRUE,
          n.label=TRUE, text.n.label="n = ",
          xlab = "Passages", ylab = "Kendall's Tau-b rank correlation value",
          main="Colocalization of Myosin-9 and F-actin in WJMSC-1 cells, 
          \nMean Plot with 95% CI") + scale_x_discrete(name ="Passages", 
                                                       limits=c("p7", "p9", "p12", 
                                                                "p15", "p18", 
                                                                "p21", "p25", "p27", 
                                                                "p28")) +
  scale_y_continuous(name="Kendall's Tau-b rank correlation value", limits=c(0, 1))



savehistory(file='myscript.R')

# where plots saved:

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

# Now, you can copy these files to your desired directory, as follows:
# file.copy(from=plots.png.paths, to="path_to_folder")

#####################################

