library("trajr")

# Load the data from experiment
data <- read.csv('F0001.csv')
# Define x, y, and time coordinates
coords <- data.frame(x = data$X[data$Track == 5], 
                     y = data$Y[data$Track == 5], 
                     times = c(1:24))
# Create a trajectory from the coordinates
trj <- TrajFromCoords(coords, spatialUnits = "pixels", timeUnits = "hours")
# A 1.315789 object had length 1 pixels in the video, scale to micrometres
trj <- TrajScale(trj, 1.3158 / 1, "μm")
# Plot it
plot(trj, lwd = 2)
# Trajectory analysis
resampled <- TrajRediscretize(trj, 3)
# The TrajDerivatives function calculates linear speed and acceleration along a Trajectory
derivs <- TrajDerivatives(trj)

# Return a list with all of the statistics for this trajectory
traj_params <- list(lenght = TrajLength(trj), # total length of the trajectory
                    distance = TrajDistance(trj),
                    # straight-line distance from the start to the end of the trajectory
                    square_displacement = TrajExpectedSquareDisplacement(trj), 
                    # expected square displacement of a correlated random walk
                    mean_speed = mean(derivs$speed), # Measures of speed
                    sd_speed = sd(derivs$speed),
                    max_speed = max(derivs$speed),
                    min_speed = min(derivs$speed),
                    sinuosity = TrajSinuosity(trj), # Measures of straightness
                    emax = TrajEmax(resampled)
)

traj_params
