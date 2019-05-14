library("trajr")


##### Track analysis function ###


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
    trj <- TrajScale(trj, 1.3158 / 1, "£gm")
    
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