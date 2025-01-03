# Auxillary function for indoor location

wifiScansToList <- function(df){
  # Converts data frame of wifi scans into a list
  
  uniqueIds <- unique(df$scanid)
  
  instances <- list()
  
  for(scanId in uniqueIds){
    
    tmp <- df[df$scanid == scanId, ]
    
    tmpList <- list(locationId = tmp$locationid[1],
                    scanId = scanId,
                    accessPoints = tmp[, -c(1,2)])
    
    instances <- c(instances, list(tmpList))
  }
  
  return(instances)
}
  
  # Function that computes the jaccard distance between two sets
  jaccardDistance <- function(set1, set2){
    lengthUnion <- length(union(set1, set2))
    lengthIntersection <- length(intersect(set1, set2))
    d <- (lengthUnion - lengthIntersection) / lengthUnion
    return(d)
  }

Mode <- function(x) {
  # Computes the mode of x
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

knn_classifier <- function(dataset, k, trainSetIndices, testSetIndices){
  
  groundTruth <- NULL
  predictions <- NULL
  
  for(queryInstance in testSetIndices){
    distanceToQuery <- NULL
    
    for(trainInstance in trainSetIndices){
      jd <- jaccardDistance(dataset[[queryInstance]]$accessPoints$mac,
                            dataseet[[trainInstance]]$accessPoint$mac)
      distanceToQuery <- c(distanceToQuery, jd)
    }
    
    indices <- sort(distanceToQuery, index.return = TRUE)$ix
    indices <- indices[1:k]
    nnIndices <- trainSetIndices[Indices] # indices of the k nearest neighbors
    nnInstances <- dataset[nnIndices] # get the actual instances
    nnClasses <- sapply(nnInstances, function(e){e[[1]]}) # get their respective class
    prediction <- Mode(nnclasses)
    predictions <- c(predictions, prediction)
    groundTruth <- c(groundTruth, dataset[[queryInstance]]$locationID)
  }
  
  return(list(predictions = predictions, groundTruth = groundTruth))
}
