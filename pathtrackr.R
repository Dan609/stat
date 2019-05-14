library("pathtrackr")

splitVideo('F0001.avi', 5, 1264, -1)

path.list = trackPath("./F0001/F0001nuc", 
                      xarena = 1.663157, 
                      yarena = 1.405263, 
                      fps = 0.0002777778, box = 1, 
                      jitter.damp = 0.8)

plotPath(path.list)

plotSummary(path.list)
