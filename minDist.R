minDist = function(gpsAddr, nccAddr, csvAddr, sring){
  #mesh4_Main3 is faster than mesh4_Main4!!!
  t1 <- Sys.time()
  #include MinDist function
  source("E:\\rf\\NCC\\Fun_MinDistv1.R")
  library(data.table)
  
  #input arguments
  ##gpsAddr: the directories of raw RNA tables 
  #gpsAddr <- "E:\\rf\\NCC\\tables\\RNA\\L700\\Rna"
  #gpsAddr <- "E:\\rf\\NCC\\tables\\RNA\\Rna"
  ##nccAddr: the directory of NCC locations.
  ###columns: Index | lon | lat | Date
  ###column Date specify the start data of collection date
  #nccAddr <- "E:\\rf\\NCC\\tables\\NccMapingResults_South-2020V4.csv"
  ##csvAddr: the direcotry that the result will be wrote to
  #csvAddr <- "E:\\rf\\NCC\\tables\\NccMapingResults_South-2020V4_Result.csv"
  #sring: search ring. 0.001=100m 0.0001=10m 0.000001=1m
  #sring <- 0.005
  
  ###main###
  
  ncc <- read.csv(nccAddr)
  
  dayRanges <- unique(ncc$Date)
  
  t = 1
  
  for (day in dayRanges){
    print(day)
    tempNcc <- subset(ncc, Date == day)
    tempGpsAddr <- paste(gpsAddr, day, ".csv", sep = '')
    tempGps <- fread(tempGpsAddr)
    
    tResult <- sapply(1:nrow(tempNcc), function(x) MinDist(rowind = x, 
                                                           sring = sring, 
                                                           ncc = tempNcc, 
                                                           gps = tempGps))
    tResult <- as.data.frame(matrix(unlist(tResult), byrow = T, ncol = 9), 
                             stringsAsFactors = F)
    colnames(tResult) <- c("ELEMENTNAME", "COUNTBEST","DL_EARFCN", 
                           "avgRSRP", "avgRSRQ", "TMSpercent", "dist",
                           "ncclon", "ncclat")
    
    tResult <- cbind(tempNcc, tResult)
    
    if (t == 1){
      result <- tResult
    }
    else {
      result <- rbind(result, tResult)
    }
    t = t + 1
  }
  
  #write.csv(result, csvAddr, row.names = F, na = "")
  fwrite(result, csvAddr)
  print(csvAddr)
  Sys.time() - t1
  
}
