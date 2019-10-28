# Cell movement track analysis by Dan Bobkov, 2019 # dan.bobkov@gmail.com

library(trajr)
library(tibble)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(xlsx)

# Set names
alltracks <- setNames(data.frame(matrix(ncol = 15, nrow = 0)), 
                      c("track", 
                         "length",
                         "distance",
                         "straight",
                         "square_displacement",
                         "mean_speed",
                         "sd_speed",
                         "max_speed",
                         "min_speed",
                         "sinuosity",
                         "emax",
                         "DC",
                         "SDDC",
                         "mean_angle",
                         "probe"))
tracks_p09 <- alltracks
tracks_p15 <- alltracks
tracks_p36 <- alltracks
#---------------------------
# Remove outliers function
outliers.rm <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.75))
  q <- unname(q)
  for (i in x) 
  {
    if ( i < q[1] - 1.5*IQR(x) || 
         i > q[2] + 1.5*IQR(x)) { 
      x <- x[x != i]
      return(x)
    }
    else {
      return(x)
    }
  }
}
#---------------------------
### Load Track analysis functions
### p09
traj_analysis_p09 <- function(input) {
  
  data <- read.csv(input)
  
  traj_params <- setNames(data.frame(matrix(ncol = 15, nrow = 0)), 
                          c("track", 
                            "length",
                            "distance",
                            "straight",
                            "square_displacement",
                            "mean_speed",
                            "sd_speed",
                            "max_speed",
                            "min_speed",
                            "sinuosity",
                            "emax",
                            "DC",
                            "SDDC",
                            "mean_angle",
                            "probe"))
  
  for (i in unique(data$Track)) {
    
    # Define x, y, and time coordinates
    coords <- data.frame(x = data$X[data$Track == i], 
                         y = data$Y[data$Track == i], 
                         # times = c(1:96))
                         timeCol = data$Slice[data$Track == i],
                         spatialUnits = "pixels", timeUnits = "hours")
    
    trj <- TrajFromCoords(coords, spatialUnits = "pixels", timeUnits = "seconds", fps = 95/3600/24)
    TrajDuration(trj) # Returns the temporal duration of the trajectory (or a portion)
    # A 1.315789 object had length 1 pixels in the video, scale to micrometres
    trj <- TrajScale(trj, 1.3158 / 1, "micrometer")
    TrajGetUnits(trj) # Returns the spatial units of a trajectory
    TrajGetTimeUnits(trj)	#Returns the temporal units of a trajectory
    TrajStepLengths(trj)	#Returns the lengths of each step within the trajectory
    # Rediscretization
    # The function TrajResampleTime linearly interpolates points along a trajectory 
    # to create a new trajectory with fixed step time intervals.
    trj <- TrajResampleTime(trj, 901)
    TrajDuration(trj) # Returns the temporal duration of the trajectory (or a portion)
    TrajGetFPS(trj)
    par(mar=c(5,5,5,5))
    # Plot it
    plot(trj, lwd = 2)
    points(trj, draw.start.pt = FALSE, pch = 21, col = "black", cex = 1.2)
    # Trajectory analysis
    # The TrajDerivatives function calculates linear speed and acceleration along a Trajectory
    derivs <- TrajDerivatives(trj)
    
    traj_params <- add_row(traj_params, 
                           track = i,
                           # total length of the trajectory
                           length = TrajLength(trj),
                           # straight-line distance from the start to the end of the trajectory
                           distance = TrajDistance(trj),
                           # Straightness index
                           straight = TrajStraightness(trj), # D/L ratio
                           # expected square displacement of a correlated random walk
                           square_displacement = TrajExpectedSquareDisplacement(trj),
                           # Measures of speed
                           mean_speed = mean(derivs$speed),
                           sd_speed = sd(derivs$speed),
                           max_speed = max(derivs$speed),
                           min_speed = min(derivs$speed),
                           # Measures of straightness
                           sinuosity = TrajSinuosity(trj),
                           emax = TrajEmax(trj),
                           SDDC  =  sd(TrajDirectionalChange(trj)),
                           DC = mean(TrajDirectionalChange(trj)),
                           mean_angle = TrajMeanVectorOfTurningAngles(trj),
                           probe = 'p09')
    
    head(traj_params)
    
  }
  
  # print(traj_params)
  write.csv(traj_params, file = 'traj_p09.csv')
  tracks <<- traj_params
  return(traj_params)
  
} # 96 frames
#---------------------------
### p15
traj_analysis_p15 <- function(input) {
  
  data <- read.csv(input)
  
  traj_params <- setNames(data.frame(matrix(ncol = 15, nrow = 0)), 
                          c("track", 
                            "length",
                            "distance",
                            "straight",
                            "square_displacement",
                            "mean_speed",
                            "sd_speed",
                            "max_speed",
                            "min_speed",
                            "sinuosity",
                            "emax",
                            "DC",
                            "SDDC",
                            "mean_angle",
                            "probe"))
  
  for (i in unique(data$Track)) {
    
    # Define x, y, and time coordinates
    coords <- data.frame(x = data$X[data$Track == i], 
                         y = data$Y[data$Track == i], 
                         # times = c(1:96))
                         timeCol = data$Slice[data$Track == i],
                         spatialUnits = "pixels", timeUnits = "hours")
    
    trj <- TrajFromCoords(coords, spatialUnits = "pixels", timeUnits = "seconds", fps = 95/3600/24)
    TrajDuration(trj) # Returns the temporal duration of the trajectory (or a portion)
    # A 1.315789 object had length 1 pixels in the video, scale to micrometres
    trj <- TrajScale(trj, 1.3158 / 1, "micrometer")
    TrajGetUnits(trj) # Returns the spatial units of a trajectory
    TrajGetTimeUnits(trj)	#Returns the temporal units of a trajectory
    TrajStepLengths(trj)	#Returns the lengths of each step within the trajectory
    # Rediscretization
    # The function TrajResampleTime linearly interpolates points along a trajectory 
    # to create a new trajectory with fixed step time intervals.
    trj <- TrajResampleTime(trj, 901)
    TrajDuration(trj) # Returns the temporal duration of the trajectory (or a portion)
    TrajGetFPS(trj)
    par(mar=c(5,5,5,5))
    # Plot it
    plot(trj, lwd = 2)
    points(trj, draw.start.pt = FALSE, pch = 21, col = "black", cex = 1.2)
    # Trajectory analysis
    # The TrajDerivatives function calculates linear speed and acceleration along a Trajectory
    derivs <- TrajDerivatives(trj)
    
    traj_params <- add_row(traj_params, 
                           track = i,
                           # total length of the trajectory
                           length = TrajLength(trj),
                           # straight-line distance from the start to the end of the trajectory
                           distance = TrajDistance(trj),
                           # Straightness index
                           straight = TrajStraightness(trj),
                           # expected square displacement of a correlated random walk
                           square_displacement = TrajExpectedSquareDisplacement(trj),
                           # Measures of speed
                           mean_speed = mean(derivs$speed),
                           sd_speed = sd(derivs$speed),
                           max_speed = max(derivs$speed),
                           min_speed = min(derivs$speed),
                           # Measures of straightness
                           sinuosity = TrajSinuosity(trj),
                           emax = TrajEmax(trj),
                           SDDC  =  sd(TrajDirectionalChange(trj)),
                           DC = mean(TrajDirectionalChange(trj)),
                           mean_angle = TrajMeanVectorOfTurningAngles(trj),
                           probe = 'p15')
    
    head(traj_params)
    
  }
  
  # print(traj_params)
  write.csv(traj_params, file = 'traj_p15.csv')
  tracks <<- traj_params
  return(traj_params)
  
} # 96 frames
#---------------------------
### p36
# modified for 24 frames-data, includes time rediscritization:
traj_analysis_p36 <- function(input) {
  
  data <- read.csv(input)
  
  traj_params <- setNames(data.frame(matrix(ncol = 15, nrow = 0)), 
                          c("track", 
                            "length",
                            "distance",
                            "straight",
                            "square_displacement",
                            "mean_speed",
                            "sd_speed",
                            "max_speed",
                            "min_speed",
                            "sinuosity",
                            "emax",
                            "DC",
                            "SDDC",
                            "mean_angle",
                            "probe"))
  
  for (i in unique(data$Track)) {
    
    # Define x, y, and time coordinates
    coords <- data.frame(x = data$X[data$Track == i], 
                         y = data$Y[data$Track == i], 
                         #times = c(1:24))
                         timeCol = data$Slice[data$Track == i],
                         spatialUnits = "pixels", timeUnits = "hours")
    
    trj <- TrajFromCoords(coords, spatialUnits = "pixels", timeUnits = "seconds", fps = 23/3600/24)
    
    TrajDuration(trj) # Returns the temporal duration of the trajectory (or a portion)
    
    trj <- TrajScale(trj, 1.3158 / 1, "micrometer")
    
    TrajGetUnits(trj) # Returns the spatial units of a trajectory
    TrajGetTimeUnits(trj)	#Returns the temporal units of a trajectory
    TrajStepLengths(trj)	#Returns the lengths of each step within the trajectory
    
    par(mar=c(5,5,5,5))
    
    # Rediscretization
    # The function TrajResampleTime linearly interpolates points along a trajectory 
    # to create a new trajectory with fixed step time intervals.
    trj <- TrajResampleTime(trj, 901)
    
    TrajDuration(trj) # Returns the temporal duration of the trajectory (or a portion)
    TrajGetFPS(trj)
    
    plot(trj, lwd = 2)
    points(trj, draw.start.pt = FALSE, pch = 21, col = "black", cex = 1.2)
    
    
    # Plot rediscretized trajectory in red
    lines(trj, col = "#FF0000A0", lwd = 2)
    points(trj, type = 'p', col = "#FF0000A0", pch = 16)
    
    legend("topright", c("Original", "Resampled"), col = c("black", "red"), 
           lwd = 2, inset = c(0.01, 0.02))
    
    
    TrajDuration(trj) # Returns the temporal duration of the trajectory (or a portion)
    TrajGetUnits(trj) # Returns the spatial units of a trajectory
    TrajGetTimeUnits(trj)	#Returns the temporal units of a trajectory
    TrajStepLengths(trj)	#Returns the lengths of each step within the trajectory
    TrajMeanVectorOfTurningAngles(trj) # eturns the mean vector of the turning angles
    TrajAngles(trj) # Returns the turning angles (radians) of a trajectory
    TrajMeanVelocity(trj) # Returns the mean velocity vector of the trajectory (or a portion)
    TrajGetTimeUnits(trj) # Returns the temporal units of a trajectory
    
    # The TrajDerivatives function calculates linear speed and acceleration along a Trajectory
    derivs <- TrajDerivatives(trj)
    
    traj_params <- add_row(traj_params, 
                           track = i,
                           # total length of the trajectory
                           length = TrajLength(trj),
                           # straight-line distance from the start to the end of the trajectory
                           distance = TrajDistance(trj),
                           # Straightness index
                           straight = TrajStraightness(trj),
                           # expected square displacement of a correlated random walk
                           square_displacement = TrajExpectedSquareDisplacement(trj),
                           # Measures of speed
                           mean_speed = mean(derivs$speed),
                           sd_speed = sd(derivs$speed),
                           max_speed = max(derivs$speed),
                           min_speed = min(derivs$speed),
                           # Measures of straightness
                           sinuosity = TrajSinuosity(trj),
                           emax = TrajEmax(trj),
                           SDDC  =  sd(TrajDirectionalChange(trj)),
                           DC = mean(TrajDirectionalChange(trj)),
                           mean_angle = TrajMeanVectorOfTurningAngles(trj),
                           probe = 'p36')
    
    head(traj_params)
    
  }
  
  # print(traj_params)
  write.csv(traj_params, file = 'traj_p36.csv')
  tracks <<- traj_params
  return(traj_params)
  
} # 24 frames, TrajRediscretize
#---------------------------


# Choose dir
file_list_p09 <- list.files(path = , choose.dir(default = "", 
                                            caption = "Select folder"),
                        pattern = "csv", 
                        all.files = FALSE,
                        full.names = TRUE, recursive = TRUE,
                        ignore.case = FALSE, include.dirs = FALSE,
                        no.. = FALSE)
# Choose dir
file_list_p15 <- list.files(path = , choose.dir(default = "", 
                                            caption = "Select folder"),
                        pattern = "csv", 
                        all.files = FALSE,
                        full.names = TRUE, recursive = TRUE,
                        ignore.case = FALSE, include.dirs = FALSE,
                        no.. = FALSE)
# Choose dir
file_list_p36 <- list.files(path = , choose.dir(default = "", 
                                            caption = "Select folder"),
                        pattern = "csv", 
                        all.files = FALSE,
                        full.names = TRUE, recursive = TRUE,
                        ignore.case = FALSE, include.dirs = FALSE,
                        no.. = FALSE)
#----------------------------------------
# Call to function
# Start scan for p09
for (file_name in file_list_p09) {
  traj_analysis_p09(file_name)
  tracks_p09 <- rbind(tracks_p09, tracks)
}
# Start scan for p15
for (file_name in file_list_p15) {
  traj_analysis_p15(file_name)
  tracks_p15 <- rbind(tracks_p15, tracks)
}
# Start scan for p36
for (file_name in file_list_p36) {
  traj_analysis_p36(file_name)
  tracks_p36 <- rbind(tracks_p36, tracks)
}
#----------------------------------------
# Merge all tracks
alltracks <- rbind(tracks_p09, tracks_p15, tracks_p36) # collect all tracks

#-----------------

summary(alltracks)

#----save results

write.csv(alltracks, file = 'alltracks.csv')

# Order probe levels

alltracks$probe <- as.factor(alltracks$probe)

alltracks$probe <- ordered(alltracks$probe,
                      levels = c("p09", "p15", "p36"))

# stat analisys

all.h <- alltracks

# set time to hours
all.h <- cbind(all.h[,c(6,7,8,9)]*3600, all.h[,c(2,3,4,5,10,11,12,13,14,15)])

data <- all.h

data$probe <- as.factor(data$probe)
data$probe <- ordered(data$probe,
                           levels = c("p09", "p15", "p36"))

head(data)
# plot(data)

# Perform pairwise comparisons

compare_means(distance ~ probe,  data = data, method = "anova")
compare_means(distance ~ probe,  data = data, method = "kruskal.test")
compare_means(distance ~ probe,  data = data, method = "t.test")

compare_means(distance ~ probe,  data = data, method = "wilcox.test")
compare_means(straight ~ probe,  data = data, method = "wilcox.test")
compare_means(mean_speed ~ probe,  data = data, method = "wilcox.test")

write.xlsx(compare_means(ros ~ probe,  data = data, method = "kruskal.test"), 
           file = 'kruskal.test.xlsx')

write.xlsx(compare_means(ros ~ probe, data = data, method = "anova"),
           file = 'anova.xlsx')

write.xlsx(compare_means(ros ~ probe,  data = data, method = "t.test"),
           file = 't.test.xlsx')

write.xlsx(compare_means(ros ~ probe,  data = data, method = "wilcox.test"),
           file = 'wilcox.test.xlsx')

# One-way ANOVA
# Compute the analysis of variance
res.aov <- aov(distance ~ probe, data = data)

# Summary of the analysis
par(mar = c (5,15,5,5))
summary(res.aov)
TukeyHSD(res.aov)
par(mar = c(4.5, 7, 4.5, 2))
plot(TukeyHSD(res.aov), las = 1)

# data[data$probe=="p09",c(2:10)]


ggqqplot(data$length, main = 'lenght')
ggqqplot(data$distance, main = 'distance')
ggqqplot(data$square_displacement, main = 'square_displacement')
ggqqplot(data$mean_speed, main = 'mean_speed')
ggqqplot(data$sd_speed, main = 'sd_speed')
ggqqplot(data$max_speed, main = 'max_speed')
ggqqplot(data$min_speed, main = 'min_speed')
ggqqplot(data$sinuosity, main = 'sinuosity')
ggqqplot(data$emax, main = 'emax')
ggqqplot(data$DC, main = 'DC')
ggqqplot(data$SDDC, main = 'SDDC')

colnames(data)

# Bar plot with signifiers 

compare_means(distance ~ probe,  data = data, method = "t.test")

df.summary <- group_by(data, probe) %>%
  summarise(
    sd = sd(distance, na.rm = TRUE),
    distance = mean(distance)
  )

df.summary


ggplot(df.summary, aes(probe, distance)) +
  geom_bar(stat = "identity", fill = 'gray', 
           color = "black", size= 1, show.legend=TRUE) +
  geom_errorbar(aes(ymin = distance-sd, ymax = distance+sd), width = 0.2, size=1) +
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
  ylim(0, 600) + 
  ggtitle("MSC-WJ1, 24h distance") + 
  labs(y="Distance, micrometers", x = "Passage") +
  # xmin / xmax positions should match the x-axis labels' positions
  geom_signif(y_position = c(580),
              xmin = c(1),
              xmax = c(2),
              annotation = "****", 
              tip_length = 0.04) +
  # xmin / xmax positions should match the x-axis labels' positions
  geom_signif(y_position = c(500),
              xmin = c(1),
              xmax = c(3),
              annotation = "***", 
              tip_length = 0.04) +
  # xmin / xmax positions should match the x-axis labels' positions
  geom_signif(y_position = c(420),
              xmin = c(2),
              xmax = c(3),
              annotation = "*", 
              tip_length = 0.04)

#------One plot for all-------------------------------------

# pdf(file = 'mfrow_eg.pdf', width = 6, height = 4)
# dev.off()
# (1) Compute summary statistics for the variable probe
# (2) Bar plots of means + individual jitter points + errors
#-------------

df.summary.length <- group_by(data, probe) %>%
  summarise(
    sd = sd(length, na.rm = TRUE),
    length = mean(length)
  )

df.summary.length

df.length <- data

ggplot(df.length, aes(probe, length)) +
  geom_bar(stat = "identity", data = df.summary.length,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = length-sd, ymax = length+sd),
    data = df.summary.length, width = 0.2) + ggtitle('length')

#-----------------

df.summary.distance <- group_by(data, probe) %>%
  summarise(
    sd = sd(distance, na.rm = TRUE),
    distance = mean(distance)
  )

df.summary.distance

df.distance <- data

ggplot(df.distance, aes(probe, distance)) +
  geom_bar(stat = "identity", data = df.summary.distance,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = distance-sd, ymax = distance+sd),
    data = df.summary.distance, width = 0.2) #+ ggtitle('distance')+
  # xmin / xmax positions should match the x-axis labels' positions
  geom_signif(y_position = c(580),
              xmin = c(1),
              xmax = c(2),
              annotation = "**", 
              tip_length = 0.04)# +
  # xmin / xmax positions should match the x-axis labels' positions
  geom_signif(y_position = c(500),
              xmin = c(1),
              xmax = c(3),
              annotation = "***", 
              tip_length = 0.04)

#-----------------

df.summary.square_displacement <- group_by(data, probe) %>%
  summarise(
    sd = sd(square_displacement, na.rm = TRUE),
    square_displacement = mean(square_displacement)
  )

df.summary.square_displacement

df.square_displacement <- data

ggplot(df.square_displacement, aes(probe, square_displacement)) +
  geom_bar(stat = "identity", data = df.summary.square_displacement,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = square_displacement-sd, ymax = square_displacement+sd),
    data = df.summary.square_displacement, width = 0.2) + ggtitle('square_displacement') 


#-------------------------------------------------

df.summary.mean_speed <- group_by(data, probe) %>%
  summarise(
    sd = sd(mean_speed, na.rm = TRUE),
    mean_speed = mean(mean_speed)
  )

df.summary.mean_speed

df.mean_speed <- data

ggplot(df.mean_speed, aes(probe, mean_speed)) +
  geom_bar(stat = "identity", data = df.summary.mean_speed,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = mean_speed-sd, ymax = mean_speed+sd),
    data = df.summary.mean_speed, width = 0.2) + ggtitle('mean_speed')



#------------------

df.summary.sd_speed <- group_by(data, probe) %>%
  summarise(
    sd = sd(sd_speed, na.rm = TRUE),
    sd_speed = mean(sd_speed)
  )

df.summary.sd_speed

df.sd_speed <- data

ggplot(df.sd_speed, aes(probe, sd_speed)) +
  geom_bar(stat = "identity", data = df.summary.sd_speed,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = sd_speed-sd, ymax = sd_speed+sd),
    data = df.summary.sd_speed, width = 0.2) + ggtitle('sd_speed')

#------------------------

df.summary.max_speed <- group_by(data, probe) %>%
  summarise(
    sd = sd(max_speed, na.rm = TRUE),
    max_speed = mean(max_speed)
  )

df.summary.max_speed

df.max_speed <- data

ggplot(df.max_speed, aes(probe, max_speed)) +
  geom_bar(stat = "identity", data = df.summary.max_speed,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = max_speed-sd, ymax = max_speed+sd),
    data = df.summary.max_speed, width = 0.2) + ggtitle('max_speed')

#------------------------

df.summary.min_speed <- group_by(data, probe) %>%
  summarise(
    sd = sd(min_speed, na.rm = TRUE),
    min_speed = mean(min_speed)
  )

df.summary.min_speed

df.min_speed <- data

ggplot(df.min_speed, aes(probe, min_speed)) +
  geom_bar(stat = "identity", data = df.summary.min_speed,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = min_speed-sd, ymax = min_speed+sd),
    data = df.summary.min_speed, width = 0.2) + ggtitle('min_speed')


#-------------------------

df.summary.sinuosity <- group_by(data, probe) %>%
  summarise(
    sd = sd(sinuosity, na.rm = TRUE),
    sinuosity = mean(sinuosity)
  )

df.summary.sinuosity

df.sinuosity <- data


ggplot(df.sinuosity, aes(probe, sinuosity)) +
  geom_bar(stat = "identity", data = df.summary.sinuosity,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = sinuosity-sd, ymax = sinuosity+sd),
    data = df.summary.sinuosity, width = 0.2) + ggtitle('sinuosity')

#-------------------------------------------------------



df.summary.emax <- group_by(data, probe) %>%
  summarise(
    sd = sd(emax, na.rm = TRUE),
    emax = mean(emax)
  )

df.summary.emax

df.emax <- data

ggplot(df.emax, aes(probe, emax)) +
  geom_bar(stat = "identity", data = df.summary.emax,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = emax-sd, ymax = emax+sd),
    data = df.summary.emax, width = 0.2) + ggtitle('emax')

#----------------------------------------------------------

df.summary.DC <- group_by(data, probe) %>%
  summarise(
    sd = sd(DC, na.rm = TRUE),
    DC = mean(DC)
  )

df.summary.DC

df.DC <- data

ggplot(df.DC, aes(probe, DC)) +
  geom_bar(stat = "identity", data = df.summary.DC,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = DC-sd, ymax = DC+sd),
    data = df.summary.DC, width = 0.2) + ggtitle('DC')

#----------------------------------------------------------

df.summary.SDDC <- group_by(data, probe) %>%
  summarise(
    sd = sd(SDDC, na.rm = TRUE),
    SDDC = mean(SDDC)
  )

df.summary.SDDC

df.SDDC <- data

ggplot(df.SDDC, aes(probe, SDDC)) +
  geom_bar(stat = "identity", data = df.summary.SDDC,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = SDDC-sd, ymax = SDDC+sd),
    data = df.summary.SDDC, width = 0.2) + ggtitle('SDDC')

#----------------------------------------------------------
#-------------------------------------------------------


df.summary.mean_angle <- group_by(data, probe) %>%
  summarise(
    sd = sd(mean_angle, na.rm = TRUE),
    mean_angle = mean(mean_angle)
  )

df.summary.mean_angle

df.mean_angle <- data

ggplot(df.mean_angle, aes(probe, mean_angle)) +
  geom_bar(stat = "identity", data = df.summary.mean_angle,
           fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2),
              color = "black") + 
  geom_errorbar(
    aes(ymin = mean_angle-sd, ymax = mean_angle+sd),
    data = df.summary.mean_angle, width = 0.2) + ggtitle('mean_angle')



#------------------------------storage
# functions without resampling

### p15
traj_analysis_p15 <- function(input) {
  
  data <- read.csv(input)
  
  traj_params <- setNames(data.frame(matrix(ncol = 14, nrow = 0)), 
                          c("track", 
                            "length",
                            "distance",
                            "square_displacement",
                            "mean_speed",
                            "sd_speed",
                            "max_speed",
                            "min_speed",
                            "sinuosity",
                            "emax",
                            "DC",
                            "SDDC",
                            "mean_angle",
                            "probe"))
  
  for (i in unique(data$Track)) {
    
    # Define x, y, and time coordinates
    coords <- data.frame(x = data$X[data$Track == i], 
                         y = data$Y[data$Track == i], 
                         # times = c(1:96))
                         timeCol = data$Slice[data$Track == i],
                         spatialUnits = "pixels", timeUnits = "hours")
    
    trj <- TrajFromCoords(coords, spatialUnits = "pixels", timeUnits = "seconds", fps = 95/3600/24)
    TrajDuration(trj) # Returns the temporal duration of the trajectory (or a portion)
    # A 1.315789 object had length 1 pixels in the video, scale to micrometres
    trj <- TrajScale(trj, 1.3158 / 1, "micrometer")
    TrajGetUnits(trj) # Returns the spatial units of a trajectory
    TrajGetTimeUnits(trj)	#Returns the temporal units of a trajectory
    TrajStepLengths(trj)	#Returns the lengths of each step within the trajectory
    TrajDuration(trj) # Returns the temporal duration of the trajectory (or a portion)
    TrajGetFPS(trj)
    par(mar=c(5,5,5,5))
    # Plot it
    plot(trj, lwd = 2)
    points(trj, draw.start.pt = FALSE, pch = 21, col = "black", cex = 1.2)
    # Trajectory analysis
    # The TrajDerivatives function calculates linear speed and acceleration along a Trajectory
    derivs <- TrajDerivatives(trj)
    
    traj_params <- add_row(traj_params, 
                           track = i,
                           # total length of the trajectory
                           length = TrajLength(trj),
                           # straight-line distance from the start to the end of the trajectory
                           distance = TrajDistance(trj),
                           # expected square displacement of a correlated random walk
                           square_displacement = TrajExpectedSquareDisplacement(trj),
                           # Measures of speed
                           mean_speed = mean(derivs$speed),
                           sd_speed = sd(derivs$speed),
                           max_speed = max(derivs$speed),
                           min_speed = min(derivs$speed),
                           # Measures of straightness
                           sinuosity = TrajSinuosity(trj),
                           emax = TrajEmax(trj),
                           SDDC  =  sd(TrajDirectionalChange(trj)),
                           DC = mean(TrajDirectionalChange(trj)),
                           mean_angle = TrajMeanVectorOfTurningAngles(trj),
                           probe = 'p15')
    
    head(traj_params)
    
  }
  
  # print(traj_params)
  write.csv(traj_params, file = 'traj_p15.csv')
  tracks <<- traj_params
  return(traj_params)
  
} # 96 frames

### p36
# modified for 24 frames-data, includes time rediscritization:
traj_analysis_p36 <- function(input) {
  
  data <- read.csv(input)
  
  traj_params <- setNames(data.frame(matrix(ncol = 14, nrow = 0)), 
                          c("track", 
                            "length",
                            "distance",
                            "square_displacement",
                            "mean_speed",
                            "sd_speed",
                            "max_speed",
                            "min_speed",
                            "sinuosity",
                            "emax",
                            "DC",
                            "SDDC",
                            "mean_angle",
                            "probe"))
  
  for (i in unique(data$Track)) {
    
    # Define x, y, and time coordinates
    coords <- data.frame(x = data$X[data$Track == i], 
                         y = data$Y[data$Track == i], 
                         #times = c(1:24))
                         timeCol = data$Slice[data$Track == i],
                         spatialUnits = "pixels", timeUnits = "hours")
    
    trj <- TrajFromCoords(coords, spatialUnits = "pixels", timeUnits = "seconds", fps = 23/3600/24)
    
    TrajDuration(trj) # Returns the temporal duration of the trajectory (or a portion)
    
    trj <- TrajScale(trj, 1.3158 / 1, "micrometer")
    
    TrajGetUnits(trj) # Returns the spatial units of a trajectory
    TrajGetTimeUnits(trj)	#Returns the temporal units of a trajectory
    TrajStepLengths(trj)	#Returns the lengths of each step within the trajectory
    
    par(mar=c(5,5,5,5))
    
    # Rediscretization
    # The function TrajResampleTime linearly interpolates points along a trajectory 
    # to create a new trajectory with fixed step time intervals.
    trj <- TrajResampleTime(trj, 901)
    
    TrajDuration(trj) # Returns the temporal duration of the trajectory (or a portion)
    TrajGetFPS(trj)
    
    plot(trj, lwd = 2)
    points(trj, draw.start.pt = FALSE, pch = 21, col = "black", cex = 1.2)
    
    
    # Plot rediscretized trajectory in red
    lines(trj, col = "#FF0000A0", lwd = 2)
    points(trj, type = 'p', col = "#FF0000A0", pch = 16)
    
    legend("topright", c("Original", "Resampled"), col = c("black", "red"), 
           lwd = 2, inset = c(0.01, 0.02))
    
    
    TrajDuration(trj) # Returns the temporal duration of the trajectory (or a portion)
    TrajGetUnits(trj) # Returns the spatial units of a trajectory
    TrajGetTimeUnits(trj)	#Returns the temporal units of a trajectory
    TrajStepLengths(trj)	#Returns the lengths of each step within the trajectory
    TrajMeanVectorOfTurningAngles(trj) # eturns the mean vector of the turning angles
    TrajAngles(trj) # Returns the turning angles (radians) of a trajectory
    TrajMeanVelocity(trj) # Returns the mean velocity vector of the trajectory (or a portion)
    TrajGetTimeUnits(trj) # Returns the temporal units of a trajectory
    
    # The TrajDerivatives function calculates linear speed and acceleration along a Trajectory
    derivs <- TrajDerivatives(trj)
    
    traj_params <- add_row(traj_params, 
                           track = i,
                           # total length of the trajectory
                           length = TrajLength(trj),
                           # straight-line distance from the start to the end of the trajectory
                           distance = TrajDistance(trj),
                           # expected square displacement of a correlated random walk
                           square_displacement = TrajExpectedSquareDisplacement(trj),
                           # Measures of speed
                           mean_speed = mean(derivs$speed),
                           sd_speed = sd(derivs$speed),
                           max_speed = max(derivs$speed),
                           min_speed = min(derivs$speed),
                           # Measures of straightness
                           sinuosity = TrajSinuosity(trj),
                           emax = TrajEmax(trj),
                           SDDC  =  sd(TrajDirectionalChange(trj)),
                           DC = mean(TrajDirectionalChange(trj)),
                           mean_angle = TrajMeanVectorOfTurningAngles(trj),
                           probe = 'p36')
    
    head(traj_params)
    
  }
  
  # print(traj_params)
  write.csv(traj_params, file = 'traj_p36.csv')
  tracks <<- traj_params
  return(traj_params)
  
} # 24 frames, TrajRediscretize

#----------------------


kruskal.test(data$length ~ data$probe)

ggdensity(data$length, 
          main = "Density plot of length in WJ1",
          xlab = "length")



#---------------------------------

#------------
# par(mfrow = c(2,2))
png()

qplot(probe, length, data = alltracks,
      geom = c("jitter", "boxplot"), alpha = I(0.6), log = "y",
      main = "WJ1, 24h")

qplot(probe, distance, data = alltracks,
      geom = c("jitter", "boxplot"), alpha = I(0.6), log = "y",
      main = "WJ1, 24h")

qplot(probe, square_displacement, data = alltracks,
      geom = c("jitter", "boxplot"), alpha = I(0.6), log = "y",
      main = "WJ1, 24h")


compare_means(length ~ probe,  data = data, method = "wilcox.test")
compare_means(distance ~ probe,  data = data, method = "wilcox.test")
compare_means(mean_speed ~ probe,  data = data, method = "wilcox.test")

qplot(probe, mean_speed, data = data,
      geom = c("jitter", "boxplot"), alpha = I(0.6), log = "y",
      main = "WJ1, 24h") + 
      labs(y = 'Mean speed, micrometers per hour',
           x = "Cell passage") +
  # xmin / xmax positions should match the x-axis labels' positions
  geom_signif(y_position = c(2.2),
              xmin = c(1),
              xmax = c(2),
              annotation = "****", 
              tip_length = 0.04) +
  # xmin / xmax positions should match the x-axis labels' positions
  geom_signif(y_position = c(2),
              xmin = c(2),
              xmax = c(3),
              annotation = "****", 
              tip_length = 0.04) 


qplot(probe, sd_speed, data = alltracks,
      geom = c("jitter", "boxplot"), alpha = I(0.6), log = "y",
      main = "WJ1, 24h")

qplot(probe, max_speed, data = alltracks,
      geom = c("jitter", "boxplot"), alpha = I(0.6), log = "y",
      main = "WJ1, 24h")

qplot(probe, min_speed, data = alltracks,
      geom = c("jitter", "boxplot"), alpha = I(0.6), log = "y",
      main = "WJ1, 24h")

qplot(probe, sinuosity, data = alltracks,
      geom = c("jitter", "boxplot"), alpha = I(0.6), log = "y",
      main = "WJ1, 24h")

qplot(probe, DC, data = alltracks,
      geom = c("jitter", "boxplot"), alpha = I(0.6), log = "y",
      main = "WJ1, 24h")

qplot(probe, SDDC, data = alltracks,
      geom = c("jitter", "boxplot"), alpha = I(0.6), log = "y",
      main = "WJ1, 24h")

qplot(probe, emax, data = alltracks,
      geom = c("jitter", "boxplot"), alpha = I(0.6), log = "y",
      main = "WJ1, 24h")

qplot(probe, straight, data = alltracks,
      geom = c("jitter", "boxplot"), alpha = I(0.6), log = "y",
      main = "Straightness, WJ1, 24h")

# dev.off()


#------------------------------------------------
all <- read.csv('alltracks.csv')

b <- ggplot(all, aes(x = probe, y = emax))

b + geom_point()
b + geom_jitter()

b + geom_boxplot(aes(color = probe))
b + geom_boxplot(aes(fill = probe))
b + geom_boxplot(aes(fill = probe)) + scale_fill_grey()

b + geom_boxplot() + coord_flip()
b + geom_boxplot(notch = TRUE)

b + stat_boxplot()

b + geom_violin()
b + geom_line()

b + geom_dotplot(binaxis = "y", stackdir = "center",
                 stackratio = 1, dotsize = 0.2)

#----------------------------------------------

all$probe=='p09'

all[all$probe=='p09', "length"]

length(all[all$probe=='p36', "emax"])
length(outliers.rm(all[all$probe=='p36', "emax"]))

#-------------

head(data)

install.packages("GGally")
library(GGally)

mydata <- data[,-12]
head(mydata)

ggcorr(mydata, palette = "RdBu", label = TRUE)

ggpairs(mydata)
