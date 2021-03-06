#' Import Canadian snow station information data from .dly file
#' @param fileLoc File path to .dly data
#' @return nicely organized dataframe of snow data. ID is station ID, Year is the year of observation, Month is the month of observation, Day is the day of observation, SnowDepth is the depth of snow on the ground measured in centimeters, QC is a quality control flag where 0 is a valid observation and 999 is missing where the numbers inbetween indicate how accurate the estimate is (smaller is more accurate), CA is Climate archive flag where C= snow depth manually changed based on nearby stations | E= estimated | M=missing | T= trace (<.5 cm) | Z= value set to missing or filling of missing zero snow depths from snow cover
#' @export
importAllSnowdata<-function(fileLoc){
  monthlymat<-matrix("",nrow = 31,ncol = 7)
  monthlymat[,4]=as.character(1:31)

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

  SnowDataUpdated <- read.delim(fileLoc, header=FALSE, stringsAsFactors=FALSE)
  SnowDataUpdated<-SnowDataUpdated$V1
  l<-lapply(SnowDataUpdated, processStationdata)
  l<-do.call(rbind,l)
  data.frame(ID=l[,1],Year=as.numeric(l[,2]),Month=as.numeric(l[,3]),Day=as.numeric(l[,4]),SnowDepth=as.numeric(l[,5]),QC=l[,6],CA=l[,7])
}
