###############################################################################
#Fun_MinDistv1: find the closest 25m RNA point to ncc loaction and return the 
#               corresponding serving cell information
#Fun_MinDistv1: return additional TMSpercent comparing with Fun_MinDist
#
#arguments:
#    rowind: rowindex. uses to get the NCC locations
#    sring: search ring. Only filter the samples within sring from RNA table
#    ncc: ncc location data
#    gps: mesh4 RNA table
#
#return: "ELEMENTNAME", "COUNTBEST", "DL_EARFCN", "AVGRSRP", "AVGRSRQ", 
#        "TMSPERCENTAGE", "dist", "ncclon", "ncclat"
###############################################################################

library(geosphere)

#sring: search ring. 0.001=100m 0.0001=10m 0.000001=1m


MinDist = function(rowind, sring, ncc, gps){
  #filter the points in sring
  ncclat <- ncc[rowind, "lat"]
  ncclon <- ncc[rowind, "lon"]
  temp <- subset(gps, LATITUDE <= ncclat + sring & 
                      LATITUDE >= ncclat - sring &
                      LONGITUDE <= ncclon + sring &
                      LONGITUDE >= ncclon - sring)
  
  if (nrow(temp) == 0){
    c("out of Bound", NA, NA, NA, NA, NA, NA, NA, NA)
  }
  else {
    temp$ncclon <- ncc[rowind, "lon"]
    temp$ncclat <- ncc[rowind, "lat"]
    temp$dist <- distCosine(cbind(temp$ncclon, temp$ncclat), 
                            cbind(temp$LONGITUDE, temp$LATITUDE))
    
    temp[which.min(temp$dist), c("ELEMENTNAME", "COUNTBEST", "DL_EARFCN", 
                                 "AVGRSRP", "AVGRSRQ", "TMSPERCENTAGE",
                                 "dist", "ncclon", "ncclat")]
    
  }
  
}

