# Animal trajectory analysis with trajr, Dan Bobkov, 2019
# https://cran.rstudio.com/web/packages/trajr/vignettes/trajr-vignette.html
# 

library("trajr")


# Load the data from experiment
data <- read.csv('F0001.csv')


# Define x, y, and time coordinates
coords <- data.frame(x = data$X[data$Track == 1], 
                     y = data$Y[data$Track == 1], 
                     times = c(1:24))
# Create a trajectory from the coordinates
trj <- TrajFromCoords(coords, spatialUnits = "pixels", timeUnits = "hours")
# A 1.315789 object had length 1 pixels in the video, scale to micrometres
trj <- TrajScale(trj, 1.3158 / 1, "??m")
# Plot it
plot(trj, lwd = 2)

# Trajectory analysis
resampled <- TrajRediscretize(trj, 3)
# The TrajDerivatives function calculates linear speed and acceleration along a Trajectory
derivs <- TrajDerivatives(trj)
# total length of the trajectory
TrajLength(trj)
# straight-line distance from the start to the end of the trajectory
TrajDistance(trj)
# expected square displacement of a correlated random walk
TrajExpectedSquareDisplacement(trj)
# Measures of speed
mean(derivs$speed)
sd(derivs$speed)
max(derivs$speed)
min(derivs$speed)
# Measures of straightness
TrajSinuosity(trj)
TrajEmax(resampled)


# Return a list with all of the statistics for this trajectory
traj_params <- list(lenght = TrajLength(trj),
     distance = TrajDistance(trj),
     square_displacement = TrajExpectedSquareDisplacement(trj),
     mean_speed = mean(derivs$speed),
     sd_speed = sd(derivs$speed),
     max_speed = max(derivs$speed),
     min_speed = min(derivs$speed),
     sinuosity = TrajSinuosity(trj),
     emax = TrajEmax(resampled)
)


traj_params

tracks <- as.data.frame(rbind(
  traj_params,
  traj_params
),

stringsAsFactors = FALSE)








#### BRUTE FORCE APPROACH ###
### need to fix it and wrap in function ###






data <- read.csv('F0001.csv')


# Define x, y, and time coordinates
coords <- data.frame(x = data$X[data$Track == 1], 
                     y = data$Y[data$Track == 1], 
                     times = c(1:24))
# Create a trajectory from the coordinates
trj <- TrajFromCoords(coords, spatialUnits = "pixels")

# A 1.315789 object had length 1 pixels in the video, scale to micrometres
trj <- TrajScale(trj, 1.3158 / 1, "??m")

# Plot it
plot(trj, lwd = 2)

# Trajectory analysis
resampled <- TrajRediscretize(trj, 3)
# The TrajDerivatives function calculates linear speed and acceleration along a Trajectory
derivs <- TrajDerivatives(trj)
# total length of the trajectory
TrajLength(trj)
# straight-line distance from the start to the end of the trajectory
TrajDistance(trj)
# expected square displacement of a correlated random walk
TrajExpectedSquareDisplacement(trj)
# Measures of speed
mean(derivs$speed)
sd(derivs$speed)
max(derivs$speed)
min(derivs$speed)
# Measures of straightness
TrajSinuosity(trj)
TrajEmax(resampled)




# Define x, y, and time coordinates
coords <- data.frame(x = data$X[data$Track == 3], 
                     y = data$Y[data$Track == 3], 
                     times = c(1:24))
# Create a trajectory from the coordinates
trj <- TrajFromCoords(coords, spatialUnits = "pixels")

# A 1.315789 object had length 1 pixels in the video, scale to micrometres
trj <- TrajScale(trj, 1.3158 / 1, "??m")

# Plot it
plot(trj, lwd = 2)

# Trajectory analysis
resampled <- TrajRediscretize(trj, 3)
# The TrajDerivatives function calculates linear speed and acceleration along a Trajectory
derivs <- TrajDerivatives(trj)
# total length of the trajectory
TrajLength(trj)
# straight-line distance from the start to the end of the trajectory
TrajDistance(trj)
# expected square displacement of a correlated random walk
TrajExpectedSquareDisplacement(trj)
# Measures of speed
mean(derivs$speed)
sd(derivs$speed)
max(derivs$speed)
min(derivs$speed)
# Measures of straightness
TrajSinuosity(trj)
TrajEmax(resampled)



# Define x, y, and time coordinates
coords <- data.frame(x = data$X[data$Track == 5], 
                     y = data$Y[data$Track == 5], 
                     times = c(1:24))
# Create a trajectory from the coordinates
trj <- TrajFromCoords(coords, spatialUnits = "pixels")

# A 1.315789 object had length 1 pixels in the video, scale to micrometres
trj <- TrajScale(trj, 1.3158 / 1, "??m")

# Plot it
plot(trj, lwd = 2)

# Trajectory analysis
resampled <- TrajRediscretize(trj, 3)
# The TrajDerivatives function calculates linear speed and acceleration along a Trajectory
derivs <- TrajDerivatives(trj)
# total length of the trajectory
TrajLength(trj)
# straight-line distance from the start to the end of the trajectory
TrajDistance(trj)
# expected square displacement of a correlated random walk
TrajExpectedSquareDisplacement(trj)
# Measures of speed
mean(derivs$speed)
sd(derivs$speed)
max(derivs$speed)
min(derivs$speed)
# Measures of straightness
TrajSinuosity(trj)
TrajEmax(resampled)


# Define x, y, and time coordinates
coords <- data.frame(x = data$X[data$Track == 7], 
                     y = data$Y[data$Track == 7], 
                     times = c(1:24))
# Create a trajectory from the coordinates
trj <- TrajFromCoords(coords, spatialUnits = "pixels")

# A 1.315789 object had length 1 pixels in the video, scale to micrometres
trj <- TrajScale(trj, 1.3158 / 1, "??m")

# Plot it
plot(trj, lwd = 2)

# Trajectory analysis
resampled <- TrajRediscretize(trj, 3)
# The TrajDerivatives function calculates linear speed and acceleration along a Trajectory
derivs <- TrajDerivatives(trj)
# total length of the trajectory
TrajLength(trj)
# straight-line distance from the start to the end of the trajectory
TrajDistance(trj)
# expected square displacement of a correlated random walk
TrajExpectedSquareDisplacement(trj)
# Measures of speed
mean(derivs$speed)
sd(derivs$speed)
max(derivs$speed)
min(derivs$speed)
# Measures of straightness
TrajSinuosity(trj)
TrajEmax(resampled)


# Define x, y, and time coordinates
coords <- data.frame(x = data$X[data$Track == 9], 
                     y = data$Y[data$Track == 9], 
                     times = c(1:24))
# Create a trajectory from the coordinates
trj <- TrajFromCoords(coords, spatialUnits = "pixels")

# A 1.315789 object had length 1 pixels in the video, scale to micrometres
trj <- TrajScale(trj, 1.3158 / 1, "??m")

# Plot it
plot(trj, lwd = 2)


# Trajectory analysis
resampled <- TrajRediscretize(trj, 3)
# The TrajDerivatives function calculates linear speed and acceleration along a Trajectory
derivs <- TrajDerivatives(trj)
# total length of the trajectory
TrajLength(trj)
# straight-line distance from the start to the end of the trajectory
TrajDistance(trj)
# expected square displacement of a correlated random walk
TrajExpectedSquareDisplacement(trj)
# Measures of speed
mean(derivs$speed)
sd(derivs$speed)
max(derivs$speed)
min(derivs$speed)
# Measures of straightness
TrajSinuosity(trj)
TrajEmax(resampled)







#######








# Define x, y, and time coordinates
coords1 <- data.frame(x = data$X[data$Track == 1], 
                     y = data$Y[data$Track == 1], 
                     times = c(1:24))
# Create a trajectory from the coordinates
trj1 <- TrajFromCoords(coords1, spatialUnits = "pixels")

# Define x, y, and time coordinates
coords3 <- data.frame(x = data$X[data$Track == 3], 
                      y = data$Y[data$Track == 3], 
                      times = c(1:24))
# Create a trajectory from the coordinates
trj3 <- TrajFromCoords(coords3, spatialUnits = "pixels")





tracks <- as.data.frame(rbind(
  trj1,
  trj3
  ),
  
  stringsAsFactors = FALSE)

# colnames(tracks) <- c("filename", "species", "category", "col")

tracks <- na.omit(tracks)



characteriseTrajectory <- function(trj) {
  # Measures of speed
  derivs <- TrajDerivatives(trj)
  mean_speed <- mean(derivs$speed)
  sd_speed <- sd(derivs$speed)
  
  # Measures of straightness
  sinuosity <- TrajSinuosity(trj)
  resampled <- TrajRediscretize(trj, .001)
  Emax <- TrajEmax(resampled)
  
  # Periodicity
  corr <- TrajDirectionAutocorrelations(resampled, 60)
  first_min <- TrajDAFindFirstMinimum(corr)
  
  # Return a list with all of the statistics for this trajectory
  list(mean_speed = mean_speed,
       sd_speed = sd_speed,
       sinuosity = sinuosity,
       Emax = Emax,
       min_deltaS = first_min[1],
       min_C = first_min[2]
  )
}

characteriseTrajectory(trj3)






##### function ...




traj_analysis <- function(input) {
  
  data <- input
  
  for (i in unique(data$Track)) {
    # Define x, y, and time coordinates
    coords <- data.frame(x = data$X[data$Track == i], 
                         y = data$Y[data$Track == i], 
                         times = c(1:24))
    # Create a trajectory from the coordinates
    trj <- TrajFromCoords(coords, spatialUnits = "pixels")
    
    # A 1.315789 object had length 1 pixels in the video, scale to micrometres
    trj <- TrajScale(trj, 1.3158 / 1, "??m")
    
    # Plot it
    # plot(trj, lwd = 2)
    
    # Trajectory analysis
    resampled <- TrajRediscretize(trj, 3)
    # The TrajDerivatives function calculates linear speed and acceleration along a Trajectory
    derivs <- TrajDerivatives(trj)
    # total length of the trajectory
    TrajLength(trj)
    # straight-line distance from the start to the end of the trajectory
    TrajDistance(trj)
    # expected square displacement of a correlated random walk
    TrajExpectedSquareDisplacement(trj)
    # Measures of speed
    mean(derivs$speed)
    sd(derivs$speed)
    max(derivs$speed)
    min(derivs$speed)
    # Measures of straightness
    TrajSinuosity(trj)
    TrajEmax(resampled)
    
    
    # Return a list with all of the statistics for this trajectory
    traj_params <- list(lenght = TrajLength(trj),
                        distance = TrajDistance(trj),
                        square_displacement = TrajExpectedSquareDisplacement(trj),
                        mean_speed = mean(derivs$speed),
                        sd_speed = sd(derivs$speed),
                        max_speed = max(derivs$speed),
                        min_speed = min(derivs$speed),
                        sinuosity = TrajSinuosity(trj),
                        emax = TrajEmax(resampled)
    )
    
    
    return(traj_params)
    
    
  }
  
  tracks <- as.data.frame(rbind(
    traj_params,
    traj_params
  ), stringsAsFactors = FALSE)
  
  return(tracks)
}





traj_analysis(read.csv('F0001.csv'))








# Plot acceleration and speed

plot(derivs$acceleration ~ derivs$accelerationTimes, type = 'l', col = 'red', 
     yaxt = 'n',
     xlab = 'Time (h)',
     ylab = expression(paste('Acceleration (', m/h^2, ')')))

axis(side = 2, col = "red")
lines(derivs$speed ~ derivs$speedTimes, col = 'blue')
axis(side = 4, col = "blue")
mtext('Speed (m/s)', side = 4, line = 3)
abline(h = 0, col = 'lightGrey')