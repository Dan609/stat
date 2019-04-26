# (2) Bar plot

ggplot(df.summary.bTau, aes(probe, bTau)) +
  geom_bar(stat = "identity", fill = "lightgray", 
           color = "black") +
  geom_errorbar(aes(ymin = bTau-sd, ymax = bTau+sd), width = 0.2)

# add line connecting means:

qplot(probe, bTau, data=data1) + 
  stat_summary(fun.y=mean, colour="red", geom="line", aes(group = 1))

# (2) Bar plot


ggplot(df.summary.bTau, aes(probe, bTau)) +
  geom_bar(stat = "identity", fill = 'gray', 
           color = "black", size= 1, show.legend=TRUE) +
  geom_errorbar(aes(ymin = bTau-sd, ymax = bTau+sd), width = 0.2, size=1) 


##

ggplot(df.summary.bTau, aes(probe, bTau)) +
  geom_bar(stat = "identity", fill = 'gray', 
           color = "black", size= 1, show.legend=TRUE) +
  geom_errorbar(aes(ymin = bTau-sd, ymax = bTau+sd), width = 0.2, size=1) +
  theme(
    # Change axis lines
    axis.line = element_line(size = 1),
    
    # Change axis ticks text labels: font color, size and face
    axis.text = element_text(),       # Change tick labels for all axes
    axis.text.x = element_text(face = "bold",
                               size = 12),     # Change x axis tick labels only
    axis.text.y = element_text(face = "bold", 
                               size = 12, angle = 0),     # Change y axis tick labels only
    
    # Change axis ticks line: font color, size, linetype and length
    axis.ticks = element_line(),      # Change ticks line fo all axes
    axis.ticks.x = element_line(),    # Change x axis ticks only
    axis.ticks.y = element_line(),    # Change y axis ticks only
    axis.ticks.length = unit(3, "pt") # Change the length of tick marks
  ) +
  geom_point() +
  ylim(0, 1)


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

###################################
# Spearman's rank correlation value
###################################

# normality test
# perform the Shapiro-Wilk test of normality for one variable (univariate):

shapiro.test(data1$Rs)

shapiro.test(data1$Rs[data1$probe =='p7'])
shapiro.test(data1$Rs[data1$probe =='p9'])
shapiro.test(data1$Rs[data1$probe =='p12'])
shapiro.test(data1$Rs[data1$probe =='p15'])
shapiro.test(data1$Rs[data1$probe =='p18'])
shapiro.test(data1$Rs[data1$probe =='p21'])
shapiro.test(data1$Rs[data1$probe =='p25'])
shapiro.test(data1$Rs[data1$probe =='p27'])
shapiro.test(data1$Rs[data1$probe =='p28'])


# Kruskal Wallis Test One Way Anova by Ranks 

kruskal.test(data1$Rs ~ data1$probe)

if(FALSE) {
  kruskal.test(data1$Rs ~ data1$probe=='p7')
  kruskal.test(data1$Rs ~ data1$probe=='p9')
  kruskal.test(data1$Rs ~ data1$probe=='p12')
  kruskal.test(data1$Rs ~ data1$probe=='p15')
  kruskal.test(data1$Rs ~ data1$probe=='p18')
  kruskal.test(data1$Rs ~ data1$probe=='p21')
  kruskal.test(data1$Rs ~ data1$probe=='p25')
  kruskal.test(data1$Rs ~ data1$probe=='p27')
  kruskal.test(data1$Rs ~ data1$probe=='p28')
}

# F-test

var.test(Rs ~ probe==c("p7", "p28"), data1)
var.test(Rs ~ probe==c("p7", "p15"), data1)
var.test(Rs ~ probe==c("p28", "p15"), data1)

# Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.


ggdensity(data1$Rs, 
          main = "Density plot of Rs",
          xlab = "Rs")


# Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution. 

ggqqplot(data1$Rs)

qqPlot(data1$Rs)

# Perform pairwise comparisons
# ?compare_means

compare_means(Rs ~ probe,  data = data1, method = "t.test")
compare_means(Rs ~ probe,  data = data1, method = "wilcox.test")

write.csv(compare_means(Rs ~ probe,  data = data1, method = "t.test"), file="t.test.Rs.csv")
write.csv(compare_means(Rs ~ probe,  data = data1, method = "wilcox.test"), file="wilcox.test.Rs.csv")

# (1) Compute summary statistics for the variable probe
# (2) Bar plots of means + individual jitter points + errors

df.summary.Rs <- group_by(data1, probe) %>%
  summarise(
    sd = sd(Rs, na.rm = TRUE),
    Rs = mean(Rs)
  )

df.summary.Rs

df.Rs <- data1

ggplot(df.Rs, aes(probe, Rs)) +
  geom_bar(stat = "identity", data = df.summary.Rs,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = Rs-sd, ymax = Rs+sd),
    data = df.summary.Rs, width = 0.2)


# add line connecting means:

qplot(probe, Rs, data=data1) + 
  stat_summary(fun.y=mean, colour="red", geom="line", aes(group = 1))


# (2) Bar plot

ggplot(df.summary.Rs, aes(probe, Rs)) +
  geom_bar(stat = "identity", fill = "lightgray", 
           color = "black") +
  geom_errorbar(aes(ymin = Rs-sd, ymax = Rs+sd), width = 0.2)

# Visualize: Specify the comparisons you want color
my_comparisons <- list( c("p12","p15"), c("p15","p21"), c("p21", "p28"))

ggboxplot(data1, x = "probe", y = "Rs")+ 
  stat_compare_means(comparisons = my_comparisons)+ 
  stat_compare_means(label.y = 1)


# ggplot with axis names
ggplot(data1, aes(x = probe, y = Rs)) + 
  geom_boxplot() + scale_x_discrete(name ="Passages", 
                                    limits=c("p7", "p9", "p12", 
                                             "p15", "p18", 
                                             "p21", "p25", "p27", 
                                             "p28")) +
  scale_y_continuous(name="Spearman's rank correlation value", limits=c(0, 1))

# One-way ANOVA
# Compute the analysis of variance

res.aov <- aov(Rs ~ probe, data = data1)

# Summary of the analysis

summary(res.aov)

TukeyHSD(res.aov)

par(mar = c(4.5, 8, 4.5, 4.5))

plot(TukeyHSD(res.aov), las = 1)

# plotmeans

plotmeans(Rs ~ probe, data = data1, frame = FALSE, 
          mean.labels=FALSE, connect=TRUE,
          n.label=TRUE, text.n.label="n = ",
          xlab = "Passages", ylab = "Spearman's rank correlation value",
          main="Colocalization of Myosin-9 and F-actin in WJMSC-1 cells, 
          \nMean Plot with 95% CI") + scale_x_discrete(name ="Passages", 
                                                       limits=c("p7", "p9", "p12", 
                                                                "p15", "p18", 
                                                                "p21", "p25", "p27", 
                                                                "p28")) +
  scale_y_continuous(name="Spearman's rank correlation value", limits=c(0, 1))



savehistory(file='myscript.R')

# where plots saved:

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

# Now, you can copy these files to your desired directory, as follows:
# file.copy(from=plots.png.paths, to="path_to_folder")

#













# Pearson's R value (above threshold)

df.summary.Rval <- group_by(data1, probe) %>%
  summarise(
    sd = sd(Rval, na.rm = TRUE),
    Rval = mean(Rval)
  )

df.summary.Rval

df.Rval <- data1

ggplot(df.Rval, aes(probe, Rval)) +
  geom_bar(stat = "identity", data = df.summary.Rval,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = Rval-sd, ymax = Rval+sd),
    data = df.summary.Rval, width = 0.2)

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

# Manders' tM1

df.summary.tM1 <- group_by(data1, probe) %>%
  summarise(
    sd = sd(tM1, na.rm = TRUE),
    tM1 = mean(tM1)
  )

df.summary.tM1

df.tM1 <- data1

ggplot(df.tM1, aes(probe, tM1)) +
  geom_bar(stat = "identity", data = df.summary.tM1,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = tM1-sd, ymax = tM1+sd),
    data = df.summary.tM1, width = 0.2)


# Manders' tM2

df.summary.tM2 <- group_by(data1, probe) %>%
  summarise(
    sd = sd(tM2, na.rm = TRUE),
    tM2 = mean(tM2)
  )

df.summary.tM2

df.tM2 <- data1

ggplot(df.tM2, aes(probe, tM2)) +
  geom_bar(stat = "identity", data = df.summary.tM2,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = tM2-sd, ymax = tM2+sd),
    data = df.summary.tM2, width = 0.2)


qplot(probe, Rval, data=data1) + 
  stat_summary(fun.y=mean, colour="red", geom="line", aes(group = 1))

qplot(probe, tM1, data=data1) + 
  stat_summary(fun.y=mean, colour="red", geom="line", aes(group = 1))

qplot(probe, tM2, data=data1) + 
  stat_summary(fun.y=mean, colour="red", geom="line", aes(group = 1))

qplot(probe, bTau, data=data1) + 
  stat_summary(fun.y=mean, colour="red", geom="line", aes(group = 1))

ggplot(df.summary.Rval, aes(probe, Rval)) +
  geom_bar(stat = "identity", fill = "lightgray", 
           color = "black") +
  geom_errorbar(aes(ymin = Rval-sd, ymax = Rval+sd), width = 0.2)

ggplot(df.summary., aes(probe, Rs)) +
  geom_bar(stat = "identity", fill = "lightgray", 
           color = "black") +
  geom_errorbar(aes(ymin = Rs-sd, ymax = Rs+sd), width = 0.2)

ggplot(df.summary.Rs, aes(probe, Rs)) +
  geom_bar(stat = "identity", fill = "lightgray", 
           color = "black") +
  geom_errorbar(aes(ymin = Rs-sd, ymax = Rs+sd), width = 0.2)

ggplot(df.summary.Rs, aes(probe, Rs)) +
  geom_bar(stat = "identity", fill = "lightgray", 
           color = "black") +
  geom_errorbar(aes(ymin = Rs-sd, ymax = Rs+sd), width = 0.2)


# If you still want to use R base graphs, type the following scripts:

# Box plot
boxplot(Rval ~ probe, data = data1,
        xlab = "Passage", ylab = "Pearson R value",
        frame = FALSE, col = c("#ffffff"))

# Box plot
boxplot(tM1 ~ probe, data = data1,
        xlab = "Passage", ylab = "Manders' tM1 (Above autothreshold of Ch2)",
        frame = FALSE, col = c("#ffffff"))

# Box plot
boxplot(tM2 ~ probe, data = data1,
        xlab = "Passage", ylab = "Manders' tM2 (Above autothreshold of Ch1)",
        frame = FALSE, col = c("#ffffff"))

# Box plot
boxplot(bTau ~ probe, data = data1,
        xlab = "Passage", ylab = "Kendall's Tau-b rank correlation value",
        frame = FALSE, col = c("#ffffff"))

# Box plot
boxplot(Rs + Rval + bTau + tM1 + tM2 ~ probe, data = data1,
        xlab = "Passage", ylab = "Pearson R value",
        frame = FALSE, col = c("#ffffff"))

# comparison
my_comparisons <- list( c("p12","p15"), c("p15","p21"), c("p21", "p18"))

ggboxplot(data1, x = "probe", y = "Rs") + 
  stat_compare_means(comparisons = my_comparisons)+ 
  stat_compare_means(label.y = 1) + 
  scale_x_discrete(name ="Passages", 
                   limits=c("p7", "p9", "p12", "p15", "p18",
                            "p21", "p25", "p27", "p28")) +
  scale_y_continuous(name="Spearman's rank correlation value", 
                     limits=c(0, 1.2))


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


plotmeans(Rval ~ probe, data = data1, frame = FALSE, 
          mean.labels=FALSE, connect=TRUE,
          n.label=TRUE, text.n.label="n = ",
          xlab = "Passages", ylab = "Spearman's rank correlation value",
          main="Colocalization of Myosin-9 and F-actin in WJMSC-1 cells, 
          \nMean Plot with 95% CI")



# plotmeans

plotmeans(bTau ~ probe, data = data1, frame = FALSE, 
          mean.labels=FALSE, connect=TRUE,
          n.label=TRUE, text.n.label="n = ",
          xlab = "Passages", ylab = "Kendall's Tau-b rank correlation value",
          main="Colocalization of Myosin-9 and F-actin in WJMSC-1 cells, 
          \nMean Plot with 95% CI")


# plotmeans

plotmeans(Rval ~ probe, data = data1, frame = FALSE, 
          mean.labels=FALSE, connect=TRUE,
          n.label=TRUE, text.n.label="n = ",
          xlab = "Passages", ylab = "Pearson's R value (above threshold)",
          main="Colocalization of Myosin-9 and F-actin in WJMSC-1 cells, 
          \nMean Plot with 95% CI")



shapiro.test(data1$Rval)
shapiro.test(data1$tM1)
shapiro.test(data1$tM2)
shapiro.test(data1$bTau)

# simple ggplot

ggplot(data1, aes(x = probe, y = Rs)) + 
  geom_boxplot() 

#View(compare_means(Rvalue ~ probe,  data = data1, method = "t.test"))
#View(compare_means(Rvalue ~ probe,  data = data1, method = "wilcox.test"))

# (2) Bar plot

ggplot(df.summary.Rs, aes(probe, Rs)) +
  geom_bar(stat = "identity", fill = "lightgray",
           color = "black") +
  geom_errorbar(aes(ymin = Rs-sd, ymax = Rs+sd), width = 0.2) + 
  scale_x_discrete(name ="Passages", 
                   limits=c("p7", "p9", "p12", "p15", "p18",
                            "p21", "p25", "p27", "p28")) +
  scale_y_continuous(name="Spearman's rank correlation value", 
                     limits=c(0, 1.2)) + 
  stat_compare_means(comparisons = my_comparisons)+ 
  stat_compare_means(label.y = 1)


# Box plot
boxplot(Rs ~ probe, data = data1,
        xlab = "Passage", ylab = "Spearman's rank correlation value",
        frame = FALSE, col = c("#ffffff"))

#data0 <- read.csv('coloc.csv')
#data0$probe <- ordered(data0$probe,
#                            levels = c("p7", "p9", "p12", 
#                                       "p15", "p18", "p20", 
#                                       "p21", "p25", "p27", 
#                                       "p28"))



# Calculate pairwise comparisons between group levels with corrections for multiple testing

pairwise.t.test(data1$bTau, data1$probe, p.adj = "bonf")

pairwise.t.test(data1$bTau, data1$probe, p.adj = "holm")


pairwise.t.test(data1$bTau, data1$probe, p.adj = "fdr")
pairwise.t.test(data1$bTau, data1$probe, p.adj = "fdr", pool.sd=FALSE)

pairwise.t.test(data1$bTau, data1$probe, p.adj = "hochberg")

pairwise.t.test(data1$bTau, data1$probe, p.adj = "hommel")

pairwise.t.test(data1$bTau, data1$probe, p.adj = "BH")

pairwise.t.test(data1$bTau, data1$probe, p.adj = "BY")

pairwise.t.test(data1$bTau, data1$probe, p.adj = "none")

# Performs Anderson-Darling all-pairs comparison test.

adAllPairsTest(bTau ~ probe, data1)
out <- adAllPairsTest(bTau ~ probe, data1, p.adjust="holm")
summary(out)
summaryGroup(out)

adAllPairsTest(bTau ~ probe, data1)
out <- adAllPairsTest(bTau ~ probe, data1, p.adjust="fdr")
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
          method="bh",
          wrap=TRUE, kw=TRUE, label=TRUE,
          alpha=0.05, altp=TRUE)

#  post-hoc test after having calculated an ANOVA

PostHocTest(aov(data1$bTau ~ data1$probe, data = data1), method = "scheffe")

r.aov <- aov(data1$bTau ~ data1$probe, data = data1)


PostHocTest(aov(data1$bTau ~ data1$probe, data = data1), method = "lsd")

r.aov <- aov(data1$bTau ~ data1$probe, data = data1)

# only p-values by setting conf.level to NA

PostHocTest(aov(data1$bTau ~ data1$probe, data = data1), method = "hsd",
            conf.level=NA)













shapiro.test(data1$Rs)


shapiro.test(data1$bTau[data1$probe =='p07'])
shapiro.test(data1$bTau[data1$probe =='p09'])
shapiro.test(data1$bTau[data1$probe =='p12'])
shapiro.test(data1$bTau[data1$probe =='p15'])
shapiro.test(data1$bTau[data1$probe =='p18'])
shapiro.test(data1$bTau[data1$probe =='p21'])
shapiro.test(data1$bTau[data1$probe =='p25'])
shapiro.test(data1$bTau[data1$probe =='p27'])
shapiro.test(data1$bTau[data1$probe =='p28'])



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
