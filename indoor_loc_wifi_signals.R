source(file.path("globals.R"))
source(file.path("indoor_auxillary.R"))
library(caret)

datapath_pc <- file.path(dataset_path_pc, "indoor_location", "wifi.csv", fsep = "\\")

df <- read.csv(datapath, stringsAsFactors = F)

dataset <- wifiScansToList(df)

uniqueIds <- unique(df$scanid)

instances <- list()

for(scanId in uniqueIds){
  
  tmp <- df[df$scanid == scanId, ]
  
  tmpList <- list(locationId = tmp$locationid[1],
                  scanId = scanId,
                  accessPoints = tmp[, -c(1,2)])
  
  instances <- c(instances, list(tmpList))
}
