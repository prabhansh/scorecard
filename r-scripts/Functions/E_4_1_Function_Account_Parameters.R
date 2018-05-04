#################################################################################
################ ACCOUNTS OPENED IN 12 MONTHS & ACTIVE ACCOUNTS #################
#################################################################################


##################### Accounts Opened in Last 12 Months #########################
Get_Accounts_Opened_In_Last_12_Months <- function(cibil_analysis){
  count <- vector(mode = "integer", length = length(rownames(cibil_analysis)))
  for(i in 1:length(rownames(cibil_analysis))){
    if(!is.na(cibil_analysis$accounts[i][1])){
      temp_json <- fromJSON(cibil_analysis$accounts[i][[1]])
      if("opened" %in% names(temp_json)){
      temp_json$opened <- as.Date(temp_json$opened, "%d-%m-%Y")
      for(j in 1:length(temp_json$opened)){
        if(!is.na(temp_json$opened[j]) &  as.Date(temp_json$opened[j]) >= (cibil_analysis$date_pull[i]-366) )
          count[i] <- count[i] + 1
      }
      }
    }
  }
  cibil_analysis$nbr_accts_open_l12m <- count
  return(cibil_analysis)
}
