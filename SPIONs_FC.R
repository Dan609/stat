# Flow cytometry results analysis
# Staining with TMRM and H2DCFDA
# Dan Bobkov, 2021
# mailto: bobkov@incras.ru
# https://github.com/dan609/

# Import libraries
library(xlsx)
library(dplyr)
library(rJava)
library(readxl)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(ggsignif)
library(xlsxjars)
library(DescTools)
library(tidyverse)
library(PMCMRplus)

# Set working directory
setwd("~/Yandex.Disk.localized/ExData/SPIONs/flowcytometry2")


#
###
###### Bar
############## Plots
############################
#################################
######################################

data <- read.csv('titr.csv', sep = ';')

data$probe <- as.factor(data$probe)

data$probe <- ordered(data$probe,
                      levels = c("Ctr", "SPION0", "SPION50",
                                 "SPION100", "SPION150", "SPION300"))

head(data)


########## Name X labels
CellSciGuylabs <- c("Ctr", "SPION0", "SPION50",
                    "SPION100", "SPION150", "SPION300")


#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable to be summariezed
# groupnames : vector of column names to be used as grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


# Summarize the TMRM data :
df_mito <- data_summary(data, varname = "mito", 
                    groupnames = c("probe"))
# Convert dose to a factor variable
df_mito$probe=as.factor(df_mito$probe)
head(df_mito)


# Summarize the H2DCFDA data :
df_ros <- data_summary(data, varname = "ros", 
                        groupnames = c("probe"))
# Convert dose to a factor variable
df_ros$probe=as.factor(df_ros$probe)
head(df_ros)


## Kruskal-Wallis rank sum
kruskal.test(data$mito ~ data$probe)

## Pairwise comparisons
compare_means(mito ~ probe, data, method = 't.test')
mito_t.test <- compare_means(mito ~ probe, data, method = 't.test')
write.xlsx(mito_t.test, file = "mito_t.test.xlsx",
           sheetName = "t.test", append = FALSE)

########### bar plot mito
ggplot(df_mito, aes(x=probe, y=mito, fill="black")) + 
  
  geom_bar(stat="identity",
           position=position_dodge(),
           fill = 'gray') +
  
  geom_errorbar(aes(ymin=mito-sd, ymax=mito+sd), width=.2,
                position=position_dodge(.9)) +
  
  scale_x_discrete(labels= CellSciGuylabs) +

  scale_y_continuous(name = "TMRM fluorescence\n intensity, a.u.",
                     labels = function(x) format(x, scientific = TRUE),
                     breaks = c(seq(0, 3000000, 500000)), 
                     limits = c(0, 3200000)) +
  
  labs(y = 'TMRM fluorescence\n intensity, a.u.',
       x = "",
       #title = "",
       caption = "Kruskal-Wallis p-value = 0.0009")  +
  
  theme_classic(base_size=14)  +
  
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1,     
                                   size = 12, face="bold",   
                                   colour="black" ),
        axis.text.y = element_text(color = "grey20", size = 8)) +
  
  
  geom_signif(y_position = c(2000000),
              xmin = c(3),
              xmax = c(4),
              annotation = "*", 
              tip_length = 0.04) +
  
  
  geom_signif(y_position = c(2200000),
              xmin = c(2),
              xmax = c(3),
              annotation = "ns", 
              tip_length = 0.04) +
  
  
  geom_signif(y_position = c(2200000),
              xmin = c(4),
              xmax = c(5),
              annotation = "**", 
              tip_length = 0.04) +
  
  
  geom_signif(y_position = c(2000000),
              xmin = c(5),
              xmax = c(6),
              annotation = "ns", 
              tip_length = 0.04) +
  
  
  geom_signif(y_position = c(2500000),
              xmin = c(2),
              xmax = c(4),
              annotation = "ns", 
              tip_length = 0.04) +            
  
  geom_signif(y_position = c(2750000),
              xmin = c(2),
              xmax = c(5),
              annotation = "*", 
              tip_length = 0.04) +

  geom_signif(y_position = c(3050000),
              xmin = c(2),
              xmax = c(6),
              annotation = "**", 
              tip_length = 0.04) +

  geom_signif(y_position = c(2000000),
              xmin = c(2),
              xmax = c(1),
              annotation = "****", 
              tip_length = 0.04)



#dev.print(pdf, 'filename.pdf')
#dev.print(png, 'BarPlotTMRM.png', width=400)
ggsave("BarPlotTMRM.png")


############ H2DCFDA ##############
## Kruskal-Wallis rank sum
kruskal.test(data$ros ~ data$probe)
## Pairwise comparisons
compare_means(ros ~ probe, data, method = 't.test')
ros_t.test <- compare_means(ros ~ probe, data, method = 't.test')
write.xlsx(ros_t.test, file = "ros_t.test.xlsx",
           sheetName = "t.test", append = FALSE)


########### Bar plot ROS
ggplot(df_ros, aes(x=probe, y=ros, fill="black")) + 
  
  geom_bar(stat="identity",
           position=position_dodge(),
           fill = 'gray') +
  
  geom_errorbar(aes(ymin=ros-sd, ymax=ros+sd), width=.2,
                position=position_dodge(.9)) +
  
  scale_x_discrete(labels= CellSciGuylabs) +

  scale_y_continuous(name = "H2DCFDA fluorescence\n intensity, a.u.",
                     labels = function(x) format(x, scientific = TRUE),
                     breaks = c(seq(0, 650000, 150000)), 
                     limits = c(0, 310000)) +
  
  labs(y = 'H2DCFDA fluorescence\n intensity, a.u.',
       x = "",
       #title = "",
       caption = "Kruskal-Wallis p-value = 0.0007") +
  
  theme_classic(base_size=14)  +
  
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1,     
                                   size = 12, face="bold", 
                                   colour="black" ),
        axis.text.y = element_text(color = "grey20", size = 8)) +

  geom_signif(y_position = c(160000),
              xmin = c(2),
              xmax = c(3),
              annotation = "*", 
              tip_length = 0.04) +


  geom_signif(y_position = c(180000),
              xmin = c(3),
              xmax = c(4),
              annotation = "ns", 
              tip_length = 0.04) +
  
  
  geom_signif(y_position = c(160000),
              xmin = c(4),
              xmax = c(5),
              annotation = "*", 
              tip_length = 0.04) +
  
  
  geom_signif(y_position = c(225000),
              xmin = c(5),
              xmax = c(6),
              annotation = "****", 
              tip_length = 0.04) +
  
  
  geom_signif(y_position = c(220000),
              xmin = c(2),
              xmax = c(4),
              annotation = "*", 
              tip_length = 0.04) +            
  
  geom_signif(y_position = c(265000),
              xmin = c(2),
              xmax = c(5),
              annotation = "**", 
              tip_length = 0.04) +

  geom_signif(y_position = c(300000),
              xmin = c(2),
              xmax = c(6),
              annotation = "****", 
              tip_length = 0.04) +

  geom_signif(y_position = c(135000),
              xmin = c(2),
              xmax = c(1),
              annotation = "****", 
              tip_length = 0.04)

  

#dev.print(png, 'BarPlotROS.png', width=400)
ggsave("BarPlotROS.png")




#
###
###### Linear
############## Regression
############################
#################################
######################################
data_lm <- read.csv('spion.csv', sep = ';')



################ TMRM ###################
# Calculate Linear regression
fit_mito <- lm(mito ~ spion, data = data_lm)
summary(fit_mito)

# Plot results
ggplot(data_lm, aes(x = spion, y = mito)) + 
  
  theme_classic(base_size=14) +
  
  geom_jitter(position = position_jitter(.1),
              cex = .9,
              shape = 16) +
  
  theme(legend.position = "none") +
  
  theme(axis.text.x = element_text(angle = 0, hjust = 0)) + 
  
  # geom_hline(yintercept=0, linetype="dashed", color = "red") + # add zero level
  
  labs(title = "MSCs Flow Cytometry",
       caption = "Slope = -388.3; R-squared = 0.3; p = 0.007") +
  
  geom_smooth(method='lm', formula= y ~ x) + # add regression line
  
  scale_x_continuous(name = "SPION concentration, mkg/ml", 
                     breaks = c(seq(0, 300, 50)), 
                     limits = c(-10, 310)) +
  
  scale_y_continuous(name = "TMRM fluorescence\n intensity, a.u.",
                     labels = function(x) format(x, scientific = TRUE),
                     breaks = c(seq(1500000, 2550000, 200000)), 
                     limits = c(1500000, 2000000))


ggsave("Regression_TMRM.png")






##############  H2DCFDA  #################
# Calculate linear regression
fit_ros <- lm(ros ~ spion, data = data_lm)
summary(fit_ros)

# Plot results
ggplot(data_lm, aes(x = spion, y = ros)) + 
  
  theme_classic(base_size=14) +
  
  geom_jitter(position = position_jitter(.1),
              cex = .9,
              shape = 16) +
  
  theme(legend.position = "none") +
  
  theme(axis.text.x = element_text(angle = 0, hjust = 0)) + 
  
  # geom_hline(yintercept=0, linetype="dashed", color = "red") + # add zero level
  
  labs(title = "MSCs Flow Cytometry",
       caption = "Slope = 330.6; R-squared = 0.82; p = 2.255e-08") +
  
  geom_smooth(method='lm', formula= y ~ x) + # add linear regression
  
  scale_x_continuous(name = "SPION concentration, mkg/ml", 
                     breaks = c(seq(0, 300, 50)), 
                     limits = c(-10, 310)) +
  
  scale_y_continuous(name = "H2DCFDA fluorescence\n intensity, a.u.",
                     labels = function(x) format(x, scientific = TRUE),
                     breaks = c(seq(55000, 250000, 50000)), 
                     limits = c(55000, 250000))


ggsave("Regression_H2DCFDA.png")









######################################
########### Mean and SD ##############
mean(data$mito[data$probe == "Ctr"])
sd(data$mito[data$probe == "Ctr"])

mean(data$mito[data$probe == "SPION0"])
sd(data$mito[data$probe == "SPION0"])

mean(data$mito[data$probe == "SPION50"])
sd(data$mito[data$probe == "SPION50"])

mean(data$mito[data$probe == "SPION100"])
sd(data$mito[data$probe == "SPION100"])

mean(data$mito[data$probe == "SPION150"])
sd(data$mito[data$probe == "SPION150"])

mean(data$mito[data$probe == "SPION300"])
sd(data$mito[data$probe == "SPION300"])


mean(data$ros[data$probe == "Ctr"])
sd(data$ros[data$probe == "Ctr"])

mean(data$ros[data$probe == "SPION0"])
sd(data$ros[data$probe == "SPION0"])

mean(data$ros[data$probe == "SPION50"])
sd(data$ros[data$probe == "SPION50"])

mean(data$ros[data$probe == "SPION100"])
sd(data$ros[data$probe == "SPION100"])

mean(data$ros[data$probe == "SPION150"])
sd(data$ros[data$probe == "SPION150"])

mean(data$ros[data$probe == "SPION300"])
sd(data$ros[data$probe == "SPION300"])
#########################


### Test for normality ####
shapiro.test(data$mito[data$probe =='Ctr'])
shapiro.test(data$mito[data$probe =='SPION0'])
shapiro.test(data$mito[data$probe =='SPION50'])
shapiro.test(data$mito[data$probe =='SPION100'])
shapiro.test(data$mito[data$probe =='SPION150'])
shapiro.test(data$mito[data$probe =='SPION300'])

shapiro.test(data$ros[data$probe =='Ctr'])
shapiro.test(data$ros[data$probe =='SPION0'])
shapiro.test(data$ros[data$probe =='SPION50'])
shapiro.test(data$ros[data$probe =='SPION100'])
shapiro.test(data$ros[data$probe =='SPION150'])
shapiro.test(data$ros[data$probe =='SPION300'])


## Bartlett test of homogeneity of variances
# bartlett.test(mito ~ probe, data)
# bartlett.test(ros ~ probe, data)

## Compute the analysis of variance
# res.aov.mito <- aov(mito ~ probe, data = data)
## Summary of the analysis
# summary(res.aov.mito)

## Compute the analysis of variance
#res.aov.ros <- aov(ros ~ probe, data = data)
## Summary of the analysis
#summary(res.aov.ros)

# summary(dunnettTest(data$mito, data$probe, alternative = "two.sided"))