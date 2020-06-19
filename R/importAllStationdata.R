#' Import Canadian snow station information data from .dly file
#' @param fileLoc File path to .dly data
#' @return nicely organized dataframe of snow data. ID is station ID, Name is station name, Lat is station latitude, Lon is station longitude, Elev is station elevation, Sdate is start date of observations (YYYYMM), Edate is end date of observations (YYYYMM), Nobs is number of observations
#' @export
importAllStationdata<-function(fileLoc){
  SnowDataUpdated <- read.delim(fileLoc, header=FALSE, stringsAsFactors=FALSE)
  SnowDataUpdated<-SnowDataUpdated$V1
  l<-lapply(SnowDataUpdated, processStationinfo)
  l<-do.call(rbind,l)
  data.frame(ID=l[,1],Name=l[,2],Lat=as.numeric(l[,3]),Lon=as.numeric(l[,4]),Elev=as.numeric(l[,5]),Sdate=as.numeric(l[,6]),Edate=as.numeric(l[,7]),Nobs=as.numeric(l[,8]))
}

processStationinfo<-function(curstr){
  if(stringr::str_length(curstr)==77){
    id=.Internal(substr(curstr,1L,7L))
    Name=.Internal(substr(curstr,9L,38L))
    Lat=.Internal(substr(curstr,40L,45L))
    Lon=.Internal(substr(curstr,47L,53L))
    Elev=.Internal(substr(curstr,55L,58L))
    Sdate=.Internal(substr(curstr,60L,65L))
    Edate=.Internal(substr(curstr,67L,72L))
    Nobs=.Internal(substr(curstr,74L,77L))
    c(id,Name,Lat,Lon,Elev,Sdate,Edate,Nobs)
  }
}
