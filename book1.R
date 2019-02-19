data(package='faraway')

data(coagulation, package='faraway')

data1 <- read.csv('Book1.csv')


data1 <- read.csv('Book1.csv')

View(data1)

plot(coag ~ diet, data=coagulation)

plot(MTT ~ substance, data=data1)

summary(coagulation)

summary(data1)


coagulation$diet


data1$substance

ourModel = lm(coag~diet-1, coagulation)

ourModel1 = lm(MTT~substance-1, data1)

summary(ourModel)

summary(ourModel1)

?lm

anova(ourModel1)

# http://www.sthda.com/english/wiki/one-way-anova-test-in-r

my_data <- data1

# Show a random sample
set.seed(1234)
dplyr::sample_n(my_data, 10)

# Show the levels
levels(my_data$substance)

my_data$substance <- ordered(my_data$substance,
                         levels = c("DMSO", "control", "neoton1", "neoton10", 
                                    "unifusol2" , "unifusol20", "PBS20"))

# Compute summary statistics by groups - count, mean, sd:

library(dplyr)
group_by(my_data, substance) %>%
  summarise(
    count = n(),
    mean = mean(MTT, na.rm = TRUE),
    sd = sd(MTT, na.rm = TRUE)
  )




install.packages("backports")
install.packages("ggpubr")

# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
library("ggpubr")

ggboxplot(my_data, x = "substance", y = "MTT", 
          color = "group", palette = c("#000000", "#000000", "#ff0000",
                                       "#ff0000", "#00FF00", "#00FF00", "#000000"),
          order = c("DMSO", "control", "neoton1", "neoton10", 
                    "unifusol2" , "unifusol20", "PBS20"),
          ylab = "570 nm absorbance", xlab = "Treatment")

# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")

ggline(my_data, x = "substance", y = "MTT",  
       add = c("mean_se", "jitter"), 
       order = c("DMSO", "control", "neoton1", "neoton10", 
                 "unifusol2" , "unifusol20", "PBS20"),
       ylab = "570 nm absorbance", xlab = "Treatment")


# If you still want to use R base graphs, type the following scripts:
# Box plot
boxplot(weight ~ group, data = my_data,
        xlab = "Treatment", ylab = "Weight",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
# plotmeans
library("gplots")
plotmeans(weight ~ group, data = my_data, frame = FALSE,
          xlab = "Treatment", ylab = "Weight",
          main="Mean Plot with 95% CI") 


# Compute the analysis of variance
res.aov <- aov(MTT ~ substance, data = my_data)
# Summary of the analysis
summary(res.aov)

TukeyHSD(res.aov)

par(mar = c(4.5, 8, 4.5, 4.5))
plot(TukeyHSD(res.aov), las = 1)

# write.csv(TukeyHSD(res.aov), file = "TukeyHSD.csv") not working
# excell data import from text


plate1 <- read.csv('plate1.csv')
plate2 <- read.csv('plate2.csv')

summary(plate1)
summary(plate2)

plate1$substance
plate2$substance


ourModel1 = lm(MTT~substance-1, plate1)


ourModel2 = lm(MTT~substance-1, plate2)

summary(ourModel1)

summary(ourModel2)
