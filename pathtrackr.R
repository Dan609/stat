library(trajr)
library(tibble)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(xlsx)
#---------------------------------------------------------------
traj_params <- setNames(data.frame(matrix(ncol = 12, nrow = 0)), 
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
                          "mean_angle",
                          "probe"))
#-------START----------------------

data <- read.csv(file.choose())

# data <- read.csv('Results from F0001 in µm per min.csv')

i = 1

coords <- data.frame(x = data$X[data$Track == i], 
                     y = data$Y[data$Track == i], 
                     # times = c(1:96))
                     timeCol = data$Slice[data$Track == i],
                     spatialUnits = "pixels", timeUnits = "hours")

trj <- TrajFromCoords(coords, spatialUnits = "pixels", timeUnits = "seconds", fps = 95/3600/24)

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
                       mean_angle = TrajMeanVectorOfTurningAngles(trj),
                       probe = 'p36')

head(traj_params)



#---------Generated trajectories-----------------------------
# Generate trajectory with a point every 1 hours and highly variable speed (which equates to step length)
trj <- TrajGenerate(23, stepLength = 1, fps = 1, timeUnits = "hours", linearErrorSd = .8)
plot(trj, lwd = 2)
points(trj, draw.start.pt = FALSE, pch = 21, col = "black", cex = 1.2)

trj <- TrajGenerate(95, stepLength = 1, fps = 4, timeUnits = "hours", linearErrorSd = .8)
plot(trj, lwd = 2)
points(trj, draw.start.pt = FALSE, pch = 21, col = "black", cex = 1.2)



