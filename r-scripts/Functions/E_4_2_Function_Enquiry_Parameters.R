#################################################################################
################## NUMBER OF ENQUIRIES IN 60 DAYS & 12 MONTHS ###################
#################################################################################

#################### Number of Enquiries in Last 60 Days ########################
Get_Enquiries_In_60_Days <- function(cibil_analysis){
  count <- vector(mode = "integer", length = length(rownames(cibil_analysis)))
  for(i in 1:length(rownames(cibil_analysis))){
    if(!is.na(cibil_analysis$enquiries[i][1])){
      temp_json <- fromJSON(cibil_analysis$enquiries[i][[1]])
      if(length(temp_json)>0){
      for(j in 1:length(temp_json$enquiry_date)){
        if(as.Date(temp_json$enquiry_date[j], "%d-%m-%Y") >= (as.Date(cibil_analysis$date_pull[i])-60))
          count[i] <- count[i] + 1
      }
      }
    }
  }
  cibil_analysis$credit_enq_60d <- count
  return(cibil_analysis)
}

################### Number of Enquiries in Last 366 Days ########################
Get_Enquiries_In_366_Days <- function(cibil_analysis){
  count <- vector(mode = "integer", length = length(rownames(cibil_analysis)))
  for(i in 1:length(rownames(cibil_analysis))){
    if(!is.na(cibil_analysis$enquiries[i][1])){
      temp_json <- fromJSON(cibil_analysis$enquiries[i][[1]])
      for(j in 1:length(temp_json$enquiry_date)){
        if(as.Date(temp_json$enquiry_date[j], "%d-%m-%Y") >= (as.Date(cibil$date_pull[i])-366))
          count[i] <- count[i] + 1
      }
    }
  }
  cibil_analysis$credit_enq_366d <- count
  return(cibil_analysis)
}