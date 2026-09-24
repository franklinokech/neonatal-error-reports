require(magrittr)
require(dplyr)
require(stringr)

timeIsValid<- function(timeVar){
  timeVar<- gsub(' ', "", timeVar)
  isPm<- grepl("pm", timeVar, ignore.case = T)
  isAm<- grepl("am", timeVar, ignore.case = T)
  isHrs<- grepl("hrs", timeVar, ignore.case = T)
  if(isPm & isAm){
    return(F)
  }
  if(isPm & isHrs) {
    return(F)
  }
  if(isAm & isHrs){
    return(F)
  }
  if(grepl("\\.|\\,|\\;", timeVar)){
    return(F)
  }
  tm<-(strsplit(timeVar, "\\:") %>% unlist())
  hrs<- suppressWarnings(
    as.numeric(
      str_trim(tm[1L])))
  HH<-as.numeric(substr(hrs, 1, 2))
 if(!is.na(HH)){
   if(!isPm & !isAm & !isHrs & HH<12 & HH!=0L){
     return(F)
   }
 }

  if(length(timeVar)!=0L && grepl("^[0-9]"
                                  , timeVar)){
    if(grepl("\\:([0-9])?([a-zA-Z])?", timeVar)){
     
      mins=(tm[2L])
      mins<-suppressWarnings(
        as.numeric(
          str_trim(
            gsub("pm|am|hrs|''", ""
                 , mins,ignore.case = T))))
      if(is.na(hrs) || is.na(mins)){
        return(F)
      }else if(hrs>23L 
               #|| (hrs>12 & any(isPm | isAm))
               ){
        return(F)
      }else if((nchar(str_trim(gsub("pm|am|hrs|''", "", tm[2L],ignore.case = T)))!=2L & mins!=0L) || mins>59L){
        return(F)
      }else if(as.numeric(substr(hrs, 1, 2))>12 & isAm){
        return(F)
      }else{
        return(T)
      }
    } else {
      hrs=suppressWarnings(
       # as.numeric
        (
          str_trim(
            gsub("pm|am|hrs|''", ""
                 , timeVar,ignore.case = T))))
      if(is.na(hrs) || (nchar(hrs)==4 & as.numeric(substr(hrs, 1, 2))>23)){
        return(F)
      }else{
        if(is.na(hrs) || (nchar(hrs)==4 & as.numeric(substr(hrs, 1, 2))>12 & isHrs)){
          return(T)
        }else if (as.numeric(substr(hrs, 1, 2))<12){
          return(T)
        }else{
          return(T)
        }
        
      }
    }
  }else{
    return(F)
  }
}


