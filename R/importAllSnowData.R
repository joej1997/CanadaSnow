#' Import Canadian snow station information data from .dly file
#' @param fileLoc File path to .dly data
#' @return nicely organized dataframe of snow data
#' @export
importAllSnowdata<-function(fileLoc){
  monthlymat<-matrix("",nrow = 31,ncol = 7)
  monthlymat[,4]=as.character(1:31)
  l<-lapply(SnowDataUpdated, processStationdata)
  l<-do.call(rbind,l)
  l<-as.data.frame(l)
  colnames(l)<-c("ID","Year","Month","Day","SnowDepth","QC","CA")
  l$Year<-as.numeric(l$Year)
  l$Month<-as.numeric(l$Month)
  l$Day<-as.numeric(l$Day)
  l$SnowDepth<-as.numeric(l$SnowDepth)
}

processStationdata<-function(curstr){
  if(stringr::str_length(curstr)!=77){
    monthlymat[,1]=.Internal(substr(curstr,1L,7L))
    monthlymat[,2]=.Internal(substr(curstr,9L,12L))
    monthlymat[,3]=.Internal(substr(curstr,13L,14L))
    for(j in 1:31){
      cur<-as.integer((j-1)*10)
      monthlymat[j,5]<-.Internal(substr(curstr,cur+16L,cur+18L))
      monthlymat[j,6]<-.Internal(substr(curstr,cur+20L,cur+22L))
      monthlymat[j,7]<-.Internal(substr(curstr,cur+24L,cur+24L))
    }
    monthlymat
  }
}
