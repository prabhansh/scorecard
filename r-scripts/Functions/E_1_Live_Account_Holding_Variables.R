#################################################################################
######################## LIVE ACCOUNTS HOLDING VARIABLES ########################
#################################################################################

########################### Live Home Loan Accounts #############################
Get_Live_Accounts <- function(cibil_analysis){
  count <- vector(mode = "integer", length = length(rownames(cibil_analysis)))
  count2 <- vector(mode = "integer", length = length(rownames(cibil_analysis)))
  
  for(i in 1:length(rownames(cibil_analysis))){
    #print(i)
    if(!is.na(cibil_analysis$accounts[i][1])){
      temp_json <- fromJSON(cibil_analysis$accounts[i][1])
      if("closed_date" %in% names(temp_json)){
      temp_json_2 <- subset(temp_json, is.na(temp_json$closed_date) == TRUE)
      }else{
        temp_json_2 <- temp_json
      }
      if(length(temp_json_2)>0){
      if( nrow(temp_json_2[grepl( 'hous' , tolower(temp_json_2$account_type)) | grepl( 'home' , tolower(temp_json_2$account_type))  | grepl( 'property' , tolower(temp_json_2$account_type) ) , ]) >0 ){
      temp_list <- tapply(temp_json_2$account_type, 
                         (grepl( 'hous' , tolower(temp_json_2$account_type)) | grepl( 'home' , tolower(temp_json_2$account_type))  | grepl( 'property' , tolower(temp_json_2$account_type) ) ),
                         length)
      count[i] <- ifelse(temp_list['TRUE'] > 0, 1, 0)
      rm(temp_list)
      }
      
      if( nrow(temp_json_2[grepl('credit card' , tolower(temp_json_2$account_type)),]) > 0 ){
      temp_list2 <- tapply(temp_json_2$account_type, 
                          grepl('credit card' , tolower(temp_json_2$account_type)),
                          length)
      count2[i] <- ifelse(temp_list2['TRUE'] > 0, 1, 0)
      rm(temp_list2)
      }
      
      }
        
      
    }
  }
  cibil_analysis$prod_live_hl <- count
  cibil_analysis$prod_live_hl[is.na(cibil_analysis$prod_live_hl)] <- 0
  cibil_analysis$prod_live_cc <- count2
  cibil_analysis$prod_live_cc[is.na(cibil_analysis$prod_live_cc)] <- 0
  
  return(cibil_analysis)
}

