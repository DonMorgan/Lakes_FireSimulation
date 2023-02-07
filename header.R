OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
figsOutDir <- file.path(OutDir,'figures')
SpatialDir <- file.path('data','spatial')
DataDir <- file.path('data')
spatialOutDir <- file.path(OutDir,'spatial')

GISLibrary<- file.path('/Users/darkbabine/ProjectLibrary/Library/GISFiles/BC')
NALibrary<- file.path('/Users/darkbabine/Dropbox (BVRC)/_dev/Biodiversity/data')
RoadDir <- file.path('/Users/darkbabine/Dropbox (BVRC)/_dev/PROVdata/Disturbance')
BioLibrary<- file.path('/Users/darkbabine/Dropbox (BVRC)/_dev/Biodiversity/data')
WaterLibrary<- file.path('/Users/darkbabine/Dropbox (BVRC)/_dev/Water/data')
#LakesLibrary<- file.path('/Users/darkbabine/Dropbox (BVRC)/_dev/Biodiversity/Lakes_FireSimulation/data/spatiaal')

dir.create(file.path(OutDir), showWarnings = FALSE)
dir.create(file.path(dataOutDir), showWarnings = FALSE)
dir.create(file.path(spatialOutDir), showWarnings = FALSE)
dir.create(file.path(figsOutDir), showWarnings = FALSE)
dir.create(DataDir, showWarnings = FALSE)
dir.create("tmp", showWarnings = FALSE)
dir.create("tmp/AOI", showWarnings = FALSE)

options(scipen=999)

