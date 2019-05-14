library(trajr)
library(tibble)
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

##### Track analysis function ###

traj_analysis <- function(input) {
  
  data <- read.csv(input)
  
  traj_params <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), 
                          c("track", 
                            "lenght",
                            "distance",
                            "square_displacement",
                            "mean_speed",
                            "sd_speed",
                            "max_speed",
                            "min_speed",
                            "sinuosity",
                            "emax"))
  
  for (i in unique(data$Track)) {

    # Define x, y, and time coordinates
    coords <- data.frame(x = data$X[data$Track == i], 
                         y = data$Y[data$Track == i], 
                         times = c(1:24))
    # Create a trajectory from the coordinates
    trj <- TrajFromCoords(coords, spatialUnits = "pixels")
    
    # A 1.315789 object had length 1 pixels in the video, scale to micrometres
    trj <- TrajScale(trj, 1.3158 / 1, "£gm")
    
    # Plot it
    plot(trj, lwd = 2)
    
    # Trajectory analysis
    resampled <- TrajRediscretize(trj, 3)
    # The TrajDerivatives function calculates linear speed and acceleration along a Trajectory
    derivs <- TrajDerivatives(trj)
    
    traj_params <- add_row(traj_params, 
            track = i,
            # total length of the trajectory
            lenght = TrajLength(trj),
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
            emax = TrajEmax(resampled)
            )
    
  }

  # print(traj_params)
  write.csv(traj_params, file = 'track.csv')
  a <<- traj_params
  return(traj_params)
  
}


# call 
traj_analysis('F0001.csv')


scatterplot(a$track, a$length)
