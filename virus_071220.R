# Статистическая обработка результатов изучения 
# специфической фармакологической активности 
# на модели летальной гриппозной пневмонии 
# in vivo на лабораторных животных
# Created by Dan Bobkov, 2020
# mailto: dan.bobkov@gmail.com

library(readxl)
library(rJava)
library(xlsxjars)
library(DescTools)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(xlsx)
library(ggsignif)
library(PMCMRplus)

library(tidyverse)

##############################################################################
##############################################################################
##############################################################################
###### 1 - титры вируса в лёгких
data <- read.csv('titr_gripp.csv', sep = ';', dec = ',', header = FALSE)
##############################################################################
##############################################################################
##############################################################################


data <- read.csv('titr_gripp.csv', sep = ';', dec = ',', header = FALSE)

colnames(data) <- c("probe", "value", "conc")

data$probe <- as.factor(data$probe)

data$conc <- as.factor(data$conc)

data$probe <- ordered(data$probe,
                       levels = c("Placebo", "Tamiflu",
                                  "1", "2", "3", "4", "5", "6", "7", "8", "9",
                                  "10", "11", "12", "13", "14", "15", "16", "17", "18"))

data$conc <- ordered(data$conc, levels = c("30", "60", "90"))
head(data)
tail(data)


#
# Вначале была проведена проверка данных на соответствие 
# нормальному распределению для того, 
# чтобы обосновать выбор теста, 
# выявляющего различия между выборками. 
# Для проверки данных на нормальность 
# применялся тест Шапиро-Уилка, 
# результаты которого представлены в таблицe 1.
#

options(scipen = 999)

shapiro.test(data$value[data$probe =='1'])
shapiro.test(data$value[data$probe =='2'])
shapiro.test(data$value[data$probe =='3'])
shapiro.test(data$value[data$probe =='4'])
shapiro.test(data$value[data$probe =='5'])
shapiro.test(data$value[data$probe =='6'])
shapiro.test(data$value[data$probe =='7'])
shapiro.test(data$value[data$probe =='8'])
shapiro.test(data$value[data$probe =='9'])
shapiro.test(data$value[data$probe =='10'])
shapiro.test(data$value[data$probe =='11'])
shapiro.test(data$value[data$probe =='12'])
shapiro.test(data$value[data$probe =='13'])
shapiro.test(data$value[data$probe =='14'])
shapiro.test(data$value[data$probe =='15'])
shapiro.test(data$value[data$probe =='16'])
shapiro.test(data$value[data$probe =='17'])
shapiro.test(data$value[data$probe =='18'])
shapiro.test(data$value[data$probe =='Tamiflu'])
shapiro.test(data$value[data$probe =='Placebo'])


ggqqplot(data$value)

ggqqplot(data$value[data$probe =='2'])
ggqqplot(data$value[data$probe =='Tamiflu'])
ggqqplot(data$value[data$probe =='Placebo'])


# Bartlett test of homogeneity of variances
bartlett.test(value ~ probe, data)
#
# Таким образом, параметрические тесты неприменимы,
# и для выявления межгрупповых различий мы применили 
# критерий Краскела – Уоллиса, 
# результаты представлены в таблицe 2

kruskal.test(data$value ~ data$probe)

kruskal.test(data$value[data$conc == '30'] ~ data$probe[data$conc == '30'])

kruskal.test(data$value[data$conc == '60'] ~ data$probe[data$conc == '60'])

kruskal.test(data$value[data$conc == '90'] ~ data$probe[data$conc == '90'])



# два разных плацебо, поэтому считаем так и так


data$probe <- ordered(data$probe,
                      levels = c("Placebo", "Tamiflu",
                                 "1", "2", "3", "4", "5", "6", "7", "8", "9",
                                 "10", "11", "12", "13", "14", "15", "16", "17", "18"))






########## Dunnett test

summary(dunnettTest(data$value, data$probe, alternative = "less"))



summary(dunnettTest(data$value[data$conc == '30'],
                    data$probe[data$conc == '30'],
                    alternative = "less"))

summary(dunnettTest(data$value[data$conc == '60'],
                    data$probe[data$conc == '60'],
                    alternative = "less"))

summary(dunnettTest(data$value[data$conc == '90'],
                    data$probe[data$conc == '90'],
                    alternative = "less"))

########### Mean and SD
mean(data$value[data$probe == "Placebo"])
sd(data$value[data$probe == "Placebo"])

mean(data$value[data$probe == "Tamiflu"])
sd(data$value[data$probe == "Tamiflu"])

mean(data$value[data$probe == "1"])
sd(data$value[data$probe == "1"])

mean(data$value[data$probe == "2"])
sd(data$value[data$probe == "2"])

mean(data$value[data$probe == "3"])
sd(data$value[data$probe == "3"])

mean(data$value[data$probe == "4"])
sd(data$value[data$probe == "4"])

mean(data$value[data$probe == "5"])
sd(data$value[data$probe == "5"])

mean(data$value[data$probe == "6"])
sd(data$value[data$probe == "6"])

mean(data$value[data$probe == "7"])
sd(data$value[data$probe == "7"])

mean(data$value[data$probe == "8"])
sd(data$value[data$probe == "8"])


mean(data$value[data$probe == "9"])
sd(data$value[data$probe == "9"])

mean(data$value[data$probe == "10"])
sd(data$value[data$probe == "10"])

mean(data$value[data$probe == "11"])
sd(data$value[data$probe == "11"])

mean(data$value[data$probe == "12"])
sd(data$value[data$probe == "12"])

mean(data$value[data$probe == "13"])
sd(data$value[data$probe == "13"])

mean(data$value[data$probe == "14"])
sd(data$value[data$probe == "14"])

mean(data$value[data$probe == "15"])
sd(data$value[data$probe == "15"])

mean(data$value[data$probe == "16"])
sd(data$value[data$probe == "16"])

mean(data$value[data$probe == "17"])
sd(data$value[data$probe == "17"])

mean(data$value[data$probe == "18"])
sd(data$value[data$probe == "18"])


######################################### plot

CellSciGuylabs <- c("Плацебо", "Осельтамивир\n фосфат, 20 мг/кг",
                    "1", "2", "3", "4", "5", "6", "7", "8", "9",
                    "10", "11", "12", "13", "14", "15", "16", "17", "18" )



# plot

ggplot(data, aes(x = probe, y = value)) +
  
  ylim(c(1, 8)) +
  
  theme_classic(base_size=14) +
  
  geom_jitter(position = position_jitter(.1),
              cex = .9,
              shape = 16) +
  
  theme(legend.position = "none") +
  
  scale_x_discrete(labels= CellSciGuylabs) +
  
  labs(y = 'Титр вируса, lg ТИД50',
       x = "") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   size = 12, face="bold",
                                  colour="black" ))



#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
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



# Summarize the data :

df3 <- data_summary(data, varname = "value", 
                    groupnames = c("probe"))
# Convert dose to a factor variable
df3$probe=as.factor(df3$probe)
head(df3)



CellSciGuylabs <- c("Плацебо", "Тамифлю, 20 мг/кг",
                    "11926007, 90мг/кг", "11926007, 60мг/кг", "11926007, 30мг/кг",
                    "11926011, 90мг/кг", "11926011, 60мг/кг", "11926011, 30мг/кг",
                    "11926158, 90мг/кг", "11926158, 60мг/кг", "11926158, 30мг/кг",
                    "11926163, 90мг/кг", "11926163, 60мг/кг", "11926163, 30мг/кг", 
                    "12026078, 90мг/кг", "12026078, 60мг/кг", "12026078, 30мг/кг",
                    "12026163, 90мг/кг", "12026163, 60мг/кг", "12026163, 30мг/кг" )

########### bar plot
ggplot(df3, aes(x=probe, y=value, fill="black")) + 
  
  geom_bar(stat="identity",
           position=position_dodge(),
           fill = 'gray') +
  
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9)) +

  scale_x_discrete(labels= CellSciGuylabs) +
  
  labs(y = 'Титр вируса, lg ТИД50',
       x = "")  +
  
  theme_classic(base_size=14)  +
  
theme(legend.position = "none",
 axis.text.x = element_text(angle = 60, hjust = 1,     
                            size = 12, face="bold",   colour="black" )) +


geom_signif(y_position = c(7.7),
            xmin = c(1),
            xmax = c(12),
            annotation = "P = 0,057", 
            tip_length = 0.04) 




























########## REPEAT 2 @@@@@@@@@@@@@   #########


###### 1 - титры вируса в лёгких

data <- read.csv('titr_gripp2.csv', sep = ';', dec = ',', header = FALSE)

colnames(data) <- c("probe", "value")

data$probe <- as.factor(data$probe)

data$probe <- ordered(data$probe,
                      levels = c("Placebo1", "Placebo2", "Tamiflu",
                                 "1", "2", "3", "4", "5", "6", "7", "8"))

head(data)
tail(data)


#
# Вначале была проведена проверка данных на соответствие 
# нормальному распределению для того, 
# чтобы обосновать выбор теста, 
# выявляющего различия между выборками. 
# Для проверки данных на нормальность 
# применялся тест Шапиро-Уилка, 
# результаты которого представлены в таблицe 1.
#

options(scipen = 999)

shapiro.test(data$value[data$probe =='1'])
shapiro.test(data$value[data$probe =='2'])
shapiro.test(data$value[data$probe =='3'])
shapiro.test(data$value[data$probe =='4'])
shapiro.test(data$value[data$probe =='5'])
shapiro.test(data$value[data$probe =='6'])
shapiro.test(data$value[data$probe =='7'])
shapiro.test(data$value[data$probe =='8'])
shapiro.test(data$value[data$probe =='Tamiflu'])
shapiro.test(data$value[data$probe =='Placebo1'])
shapiro.test(data$value[data$probe =='Placebo2'])

ggqqplot(data$value)

ggqqplot(data$value[data$probe =='2'])
ggqqplot(data$value[data$probe =='Tamiflu'])
ggqqplot(data$value[data$probe =='Placebo1'])
ggqqplot(data$value[data$probe =='Placebo2'])


# Bartlett test of homogeneity of variances
bartlett.test(value ~ probe, data)
#
# Таким образом, параметрические тесты неприменимы,
# и для выявления межгрупповых различий мы применили 
# критерий Краскела – Уоллиса, 
# результаты представлены в таблицe 2

kruskal.test(data$value ~ data$probe)





# два разных плацебо, поэтому считаем так и так

data$probe <- ordered(data$probe,
                      levels = c("Placebo1", "Placebo2", "Tamiflu",
                                 "1", "2", "3", "4", "5", "6", "7", "8"))


data$probe <- ordered(data$probe,
                      levels = c("Placebo2", "Placebo1", "Tamiflu",
                                 "1", "2", "3", "4", "5", "6", "7", "8"))



########## Dunnett test

summary(dunnettTest(data$value, data$probe, alternative = "less"))


compare_means(value ~ probe, data, method = 'wilcox.test')
comparison <- compare_means(value ~ probe, data, method = 'wilcox.test')

# ,p.adjust.method = 'bonferroni'

########### Mean and SD
mean(data$value[data$probe == "Placebo1"])
sd(data$value[data$probe == "Placebo1"])

mean(data$value[data$probe == "Placebo2"])
sd(data$value[data$probe == "Placebo2"])

mean(data$value[data$probe == "Tamiflu"])
sd(data$value[data$probe == "Tamiflu"])

mean(data$value[data$probe == "1"])
sd(data$value[data$probe == "1"])

mean(data$value[data$probe == "2"])
sd(data$value[data$probe == "2"])

mean(data$value[data$probe == "3"])
sd(data$value[data$probe == "3"])

mean(data$value[data$probe == "4"])
sd(data$value[data$probe == "4"])

mean(data$value[data$probe == "5"])
sd(data$value[data$probe == "5"])

mean(data$value[data$probe == "6"])
sd(data$value[data$probe == "6"])

mean(data$value[data$probe == "7"])
sd(data$value[data$probe == "7"])

mean(data$value[data$probe == "8"])
sd(data$value[data$probe == "8"])



######################################### plot

CellSciGuylabs <- c("Плацебо 1", "Плацебо 2", "Тамифлю",
                    "1", "2", "3", "4", "5", "6", "7", "8")



# plot

ggplot(data, aes(x = probe, y = value)) +
  
  ylim(c(1, 8)) +
  
  theme_classic(base_size=14) +
  
  geom_jitter(position = position_jitter(.1),
              cex = .9,
              shape = 16) +
  
  theme(legend.position = "none") +
  
  scale_x_discrete(labels= CellSciGuylabs) +
  
  labs(y = 'Титр вируса, lg ТИД50',
       x = "") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   size = 12, face="bold",
                                   colour="black" ))



#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
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



# Summarize the data :

df3 <- data_summary(data, varname = "value", 
                    groupnames = c("probe"))
# Convert dose to a factor variable
df3$probe=as.factor(df3$probe)
head(df3)




########### bar plot
ggplot(df3, aes(x=probe, y=value, fill="black")) + 
  
  geom_bar(stat="identity",
           position=position_dodge(),
           fill = 'gray') +
  
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9)) +
  
  scale_x_discrete(labels= CellSciGuylabs) +
  
  labs(y = 'Титр вируса, lg ТИД50',
       x = "")  +
  
  theme_classic(base_size=14)  +
  
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1,     
                                   size = 12, face="bold",   colour="black" )) +
  
  
  geom_signif(y_position = c(8.7),
              xmin = c(1),
              xmax = c(7),
              annotation = "0,03", 
              tip_length = 0.04) +
  
  
  geom_signif(y_position = c(9.4),
              xmin = c(1),
              xmax = c(8),
              annotation = "0,00008", 
              tip_length = 0.04) + 
  
  
  geom_signif(y_position = c(10.1),
              xmin = c(1),
              xmax = c(9),
              annotation = "0,00008", 
              tip_length = 0.04)


##############################################################################
##############################################################################
##############################################################################
###### 2 - результаты ИФА
data1 <- read.csv('virus_gripp.csv', sep = ';', dec = ',', header = TRUE)
##############################################################################
##############################################################################
##############################################################################


# Constructing Quadratic Formula
result <- function(a,b,c){
  if(delta(a,b,c) > 0){ # first case D>0
    x_1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x_2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    result = c(x_1,x_2)
  }
  else if(delta(a,b,c) == 0){ # second case D=0
    x = -b/(2*a)
  }
  else {"There are no real roots."} # third case D<0
}

# Constructing delta
delta<-function(a,b,c){
  b^2-4*a*c
}

### solution
a <- result(1,-2,1); a


####### calibration equation
x = 7.8 # test

y = -2*(10^(-6))*(x^2) + 0.0024*x + 0.0323 # 0.05 ok



############# calibration test data

y  = 0.05
a <- result(-2*(10^(-6)), 0.0024, 0.0323 - y); a

y  = 0.06
a <- result(-2*(10^(-6)), 0.0024, 0.0323 - y); a

y  = 0.11
a <- result(-2*(10^(-6)), 0.0024, 0.0323 - y); a

y  = 0.19
a <- result(-2*(10^(-6)), 0.0024, 0.0323 - y); a

y  = 0.31
a <- result(-2*(10^(-6)), 0.0024, 0.0323 - y); a

y  = 0.49
a <- result(-2*(10^(-6)), 0.0024, 0.0323 - y); a


y  = 0.752
a <- result(-2*(10^(-6)), 0.0024, 0.0323 - y); a[1]

################# 

df2 %>% 
  mutate(rate = cons/total,
         rate2 = rate*rate)

###################


data1 <- read.csv('virus.csv', sep = ';', dec = ',', header = TRUE)


data1 %>%
  mutate(value = result(-2*(10^(-6)), 0.0024, 0.0323 - data1$OD)[1]) -> data2 


data1 %>%
  mutate(value = 0 - OD) -> data3 

############ OD to ng/ml



data <- read.csv('virus_gripp.csv', sep = ';', dec = ',', header = TRUE)


data$probe <- as.factor(data$probe)

data$conc <- as.factor(data$conc)

data$probe <- ordered(data$probe,
                      levels = c("Placebo", "Tamiflu",
                                 "1", "2", "3", "4", "5", "6", "7", "8", "9",
                                 "10", "11", "12", "13", "14", "15", "16", "17", "18"))

data$conc <- ordered(data$conc, levels = c("30", "60", "90"))
head(data)
tail(data)


#
# Вначале была проведена проверка данных на соответствие 
# нормальному распределению для того, 
# чтобы обосновать выбор теста, 
# выявляющего различия между выборками. 
# Для проверки данных на нормальность 
# применялся тест Шапиро-Уилка, 
# результаты которого представлены в таблицe 1.
#

options(scipen = 999)

shapiro.test(data$value[data$probe =='1'])
shapiro.test(data$value[data$probe =='2'])
shapiro.test(data$value[data$probe =='3'])
shapiro.test(data$value[data$probe =='4'])
shapiro.test(data$value[data$probe =='5'])
shapiro.test(data$value[data$probe =='6'])
shapiro.test(data$value[data$probe =='7'])
shapiro.test(data$value[data$probe =='8'])
shapiro.test(data$value[data$probe =='9'])
shapiro.test(data$value[data$probe =='10'])
shapiro.test(data$value[data$probe =='11'])
shapiro.test(data$value[data$probe =='12'])
shapiro.test(data$value[data$probe =='13'])
shapiro.test(data$value[data$probe =='14'])
shapiro.test(data$value[data$probe =='15'])
shapiro.test(data$value[data$probe =='16'])
shapiro.test(data$value[data$probe =='17'])
shapiro.test(data$value[data$probe =='18'])
shapiro.test(data$value[data$probe =='Tamiflu'])
shapiro.test(data$value[data$probe =='Placebo'])


ggqqplot(data$value)

ggqqplot(data$value[data$probe =='2'])
ggqqplot(data$value[data$probe =='Tamiflu'])
ggqqplot(data$value[data$probe =='Placebo'])


# Bartlett test of homogeneity of variances
bartlett.test(value ~ probe, data)
#
# Таким образом, параметрические тесты неприменимы,
# и для выявления межгрупповых различий мы применили 
# критерий Краскела – Уоллиса, 
# результаты представлены в таблицe 2

kruskal.test(data$value ~ data$probe)

kruskal.test(data$value[data$conc == '30'] ~ data$probe[data$conc == '30'])

kruskal.test(data$value[data$conc == '60'] ~ data$probe[data$conc == '60'])

kruskal.test(data$value[data$conc == '90'] ~ data$probe[data$conc == '90'])



# 


data$probe <- ordered(data$probe,
                      levels = c("Placebo", "Tamiflu",
                                 "1", "2", "3", "4", "5", "6", "7", "8", "9",
                                 "10", "11", "12", "13", "14", "15", "16", "17", "18"))






########## Dunnett test

summary(dunnettTest(data$value, data$probe, alternative = "less"))



summary(dunnettTest(data$value[data$conc == '30'],
                    data$probe[data$conc == '30'],
                    alternative = "less"))

summary(dunnettTest(data$value[data$conc == '60'],
                    data$probe[data$conc == '60'],
                    alternative = "less"))

summary(dunnettTest(data$value[data$conc == '90'],
                    data$probe[data$conc == '90'],
                    alternative = "less"))

########### Mean and SD
mean(data$value[data$probe == "Placebo"])
sd(data$value[data$probe == "Placebo"])

mean(data$value[data$probe == "Tamiflu"])
sd(data$value[data$probe == "Tamiflu"])

mean(data$value[data$probe == "1"])
sd(data$value[data$probe == "1"])

mean(data$value[data$probe == "2"])
sd(data$value[data$probe == "2"])

mean(data$value[data$probe == "3"])
sd(data$value[data$probe == "3"])

mean(data$value[data$probe == "4"])
sd(data$value[data$probe == "4"])

mean(data$value[data$probe == "5"])
sd(data$value[data$probe == "5"])

mean(data$value[data$probe == "6"])
sd(data$value[data$probe == "6"])

mean(data$value[data$probe == "7"])
sd(data$value[data$probe == "7"])

mean(data$value[data$probe == "8"])
sd(data$value[data$probe == "8"])


mean(data$value[data$probe == "9"])
sd(data$value[data$probe == "9"])

mean(data$value[data$probe == "10"])
sd(data$value[data$probe == "10"])

mean(data$value[data$probe == "11"])
sd(data$value[data$probe == "11"])

mean(data$value[data$probe == "12"])
sd(data$value[data$probe == "12"])

mean(data$value[data$probe == "13"])
sd(data$value[data$probe == "13"])

mean(data$value[data$probe == "14"])
sd(data$value[data$probe == "14"])

mean(data$value[data$probe == "15"])
sd(data$value[data$probe == "15"])

mean(data$value[data$probe == "16"])
sd(data$value[data$probe == "16"])

mean(data$value[data$probe == "17"])
sd(data$value[data$probe == "17"])

mean(data$value[data$probe == "18"])
sd(data$value[data$probe == "18"])


######################################### plot

CellSciGuylabs <- c("Плацебо", "Осельтамивир\n фосфат, 20 мг/кг",
                    "1", "2", "3", "4", "5", "6", "7", "8", "9",
                    "10", "11", "12", "13", "14", "15", "16", "17", "18" )



# plot

ggplot(data, aes(x = probe, y = value)) +
  
  ylim(c(1, 1000)) +
  
  theme_classic(base_size=14) +
  
  geom_jitter(position = position_jitter(.1),
              cex = .9,
              shape = 16) +
  
  theme(legend.position = "none") +
  
  scale_x_discrete(labels= CellSciGuylabs) +
  
  labs(y = 'Титр вируса, lg ТИД50',
       x = "") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   size = 12, face="bold",
                                   colour="black" ))



#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
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



# Summarize the data :

df3 <- data_summary(data, varname = "value", 
                    groupnames = c("probe"))
# Convert dose to a factor variable
df3$probe=as.factor(df3$probe)
head(df3)



CellSciGuylabs <- c("Плацебо", "Тамифлю, 20 мг/кг",
                    "11926007, 90мг/кг", "11926007, 60мг/кг", "11926007, 30мг/кг",
                    "11926011, 90мг/кг", "11926011, 60мг/кг", "11926011, 30мг/кг",
                    "11926158, 90мг/кг", "11926158, 60мг/кг", "11926158, 30мг/кг",
                    "11926163, 90мг/кг", "11926163, 60мг/кг", "11926163, 30мг/кг", 
                    "12026078, 90мг/кг", "12026078, 60мг/кг", "12026078, 30мг/кг",
                    "12026163, 90мг/кг", "12026163, 60мг/кг", "12026163, 30мг/кг" )

########### bar plot
ggplot(df3, aes(x=probe, y=value, fill="black")) + 
  
  geom_bar(stat="identity",
           position=position_dodge(),
           fill = 'gray') +
  
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9)) +
  
  scale_x_discrete(labels= CellSciGuylabs) +
  
  labs(y = 'Нагрузка вируса, нг/мл',
       x = "")  +
  
  theme_classic(base_size=14)  +
  
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 60, hjust = 1,     
                                   size = 12, face="bold",   colour="black" )) +
  
  
  geom_signif(y_position = c(777),
              xmin = c(1),
              xmax = c(2),
              annotation = "***", 
              tip_length = 0.04) +
  
  
  geom_signif(y_position = c(727),
              xmin = c(1),
              xmax = c(8),
              annotation = "*", 
              tip_length = 0.04) 











