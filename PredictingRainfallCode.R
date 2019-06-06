#load Precip.df file into environment

# coordinates of the marina
pbmCoordinates <- c(-85.78, 41.34)
cmCoordinates <- c(-86.39, 41.19)
mmCoordinates <- c(-86.03, 40.09)

#libraries necessary for running the code
library("geosphere")
library("dplyr")

#groups coordinates into condensed matrix
coordinateVectors <- dplyr::select(Precip.df, Longitude, Latitude)

#gets vectors of distances from each station to each marina
pbmDistances <- apply(coordinateVectors, 1, distm, pbmCoordinates, fun=distHaversine)/1609.34
cmDistances <- apply(coordinateVectors, 1, distm, cmCoordinates, fun=distHaversine)/1609.34
mmDistances <- apply(coordinateVectors, 1, distm, mmCoordinates, fun=distHaversine)/1609.34

#creates marina-specific dataframe
pbmDataFrame <- data.frame(Precip.df$Total.Precip, Precip.df$Station.Number, pbmDistances)
cmDataFrame <- data.frame(Precip.df$Total.Precip, Precip.df$Station.Number, cmDistances)
mmDataFrame <- data.frame(Precip.df$Total.Precip, Precip.df$Station.Number, mmDistances)

#omits NA values (whole rows)
pbmDataFrame <- na.omit(pbmDataFrame)
cmDataFrame <- na.omit(cmDataFrame)
mmDataFrame <- na.omit(mmDataFrame)

#sorts the dataframes by ascending distance values
pbmDataFrame <- pbmDataFrame[order(pbmDataFrame$pbmDistances),]
cmDataFrame <- cmDataFrame[order(cmDataFrame$cmDistances),]
mmDataFrame <- mmDataFrame[order(mmDataFrame$mmDistances),]

#grabs the first five for the 5-knn approach
pbmTop5 <- pbmDataFrame[1:5,]
cmTop5 <- cmDataFrame[1:5,]
mmTop5 <- mmDataFrame[1:5,]

#5-knn prediction, using the precipitation and inverse square function for weighting
pbm5nnPrediction <- weighted.mean(pbmTop5$Precip.df.Total.Precip, (1/pbmTop5$pbmDistances^2)/sum(1/pbmTop5$pbmDistances^2))
cm5nnPrediction <- weighted.mean(cmDataFrame$Precip.df.Total.Precip[1:5], (1/cmTop5$cmDistances^2)/sum(1/cmTop5$cmDistances^2))
mm5nnPrediction <- weighted.mean(mmDataFrame$Precip.df.Total.Precip[1:5], (1/mmTop5$mmDistances^2)/sum(1/mmTop5$mmDistances^2))

#grabs all stations within a 25 mile radius per marina
pbm25mileRadius <- pbmDataFrame[pbmDataFrame$pbmDistances<25,]
cm25mileRadius <- cmDataFrame[cmDataFrame$cmDistances<25,]
mm25mileRadius <- mmDataFrame[mmDataFrame$mmDistances<25,]

#25-mile radius prediction, using precipitation and inverse square function for weighting
pbm25milesPrediction <- weighted.mean(pbm25mileRadius$Precip.df.Total.Precip, 1/pbm25mileRadius$pbmDistances^2/sum(1/pbm25mileRadius$pbmDistances^2))
cm25milesPrediction <- weighted.mean(cm25mileRadius$Precip.df.Total.Precip, 1/cm25mileRadius$cmDistances^2/sum(1/cm25mileRadius$cmDistances^2))
mm25milesPrediction <- weighted.mean(mm25mileRadius$Precip.df.Total.Precip, 1/mm25mileRadius$mmDistances^2/sum(1/mm25mileRadius$mmDistances^2))

