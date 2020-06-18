#' Import Canadian snow station information data from .dly file
#' @param fileLoc File path to .dly data
#' @return nicely organized dataframe of snow data
#' @export
library(stringr)
importAllStationdata<-function(fileLoc){
  SnowDataUpdated <- read.delim(fileLoc, header=FALSE, stringsAsFactors=FALSE)
  SnowDataUpdated<-SnowDataUpdated$V1
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
  l<-lapply(SnowDataUpdated, processStationinfo)
  l<-do.call(rbind,l)
  l<-as.data.frame(l)
  colnames(l)<-c("ID","Name","Lat","Lon","Elev","Sdate","Edate","Nobs")
  l$Lat<-as.numeric(l$Lat)
  l$Lon<-as.numeric(l$Lon)
  l$Elev<-as.numeric(l$Elev)
  l$Nobs<-as.numeric(l$Nobs)
  l
}
