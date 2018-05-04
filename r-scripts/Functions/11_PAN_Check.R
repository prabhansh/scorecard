pan_check <- read.csv("PAN_check.csv", stringsAsFactors = F)
###################### Function Check Pan Series with year of Birth and Return whether it is Risky ############
############################along with Text Note to be shown on app.indifi ###################

check_pan <- function( year , pan_first_char ){
  tryCatch({
  pan_first_char <- toupper(substr(as.character(pan_first_char),1,1))
  year <- as.numeric(year)
  pan_check_select <- toupper(pan_check$starts_with[ pan_check$year_of_birth == year ])
  if(is.null(pan_first_char)){
    return(c(0,""))
  }
  if(is.null(pan_check_select[1]) ){
    return(c(0,""))
  }
  if(is.na(pan_first_char)){
    return(c(0,""))
  }
  if(is.na(pan_check_select[1]) ){
    return(c(0,""))
  }
  
  
  if(grepl(pan_first_char, pan_check_select[1])){
    return(c(0,""))
  }else{
    return(c(1,"High Risk PAN Series"))
  }
  },error=function(e){})
  return(c(0,""))
}