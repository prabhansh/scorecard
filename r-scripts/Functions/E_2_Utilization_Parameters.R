#################################################################################
############################# UTILIZATION PARAMETERS ############################
################### Utilization across all active trade lines ###################
######################## Number of Total Active Accounts ########################
#################################################################################


Get_Utilization_Across_Trade_Lines <- function(cibil_analysis){
  utilization_ratio <- vector(mode = "numeric", length = length(rownames(cibil_analysis)))
  cibil_analysis$nbr_tot_active_accts <- 0
  for(i in 1:length(rownames(cibil_analysis))){
    #print(i)
    if(!is.na(cibil_analysis$accounts[i][1])){
      accounts <- fromJSON(cibil_analysis$accounts[i])
      if(!is.data.frame(t)){
        t <- fromJSON(paste0("[",cibil_analysis$accounts[i],"]"))
      }
    
    ## Sum of balances of selected trades
    if("current_balance_accounts" %in% names(accounts)){
    accounts$current_balance_accounts[is.na(accounts$current_balance_accounts)] <- 0
    accounts$current_balance_accounts <- gsub('[^0-9.]','',accounts$current_balance_accounts)
    }else{
      accounts$current_balance_accounts <- 0
    }
    #total_balance <- sum(as.numeric(active_accounts$current_balance_accounts))
    
    ## Sum of utilization
    if("sanctioned_amount" %in% names(accounts)){
    accounts$sanctioned_amount[is.na(accounts$sanctioned_amount)] <- 0
    accounts$sanctioned_amount <- as.numeric( gsub(  '[^0-9.]' , ''   ,  accounts$sanctioned_amount))
    }else{
      accounts$sanctioned_amount <- 0
    }
    accounts$latest_dpd_month <- 0
    accounts$pull_date <- cibil_analysis$date_pull[i]
    
    if("dpd_month" %in% names(accounts)  ){
    
    for(j in 1:nrow(accounts)){
      accounts$latest_dpd_month[j] <- accounts$dpd_month[[j]][1]
      
      accounts$latest_dpd_month[j] <- paste0("20",substr(accounts$latest_dpd_month[j],4,5) ,"-", substr(accounts$latest_dpd_month[j],1,2)  , "-01")
      
    }
      flag = 1
   tryCatch({
    accounts$days <- as.numeric(as.Date(accounts$pull_date) - as.Date(accounts$latest_dpd_month))
    flag = 0
   },error=function(e){})
      if(flag == 1){
        accounts$days  <- NA
      }
   active_accounts <- accounts[ accounts$days < 366 , ]
   if(length(rownames(active_accounts))==0){
     utilization_ratio[i] <- -1
   }
   cibil_analysis$nbr_tot_active_accts[i] <- nrow(active_accounts)
   
   active_accounts$sanctioned_amount[is.na( active_accounts$sanctioned_amount)] <- 0
   active_accounts$current_balance_accounts[is.na(active_accounts$current_balance_accounts)] <- 0
   active_accounts$sanctioned_amount[as.numeric(active_accounts$sanctioned_amount) < as.numeric(active_accounts$current_balance_accounts) ] <- active_accounts$current_balance_accounts[as.numeric(active_accounts$sanctioned_amount) < as.numeric(active_accounts$current_balance_accounts) ]
     if(sum(as.numeric(active_accounts$sanctioned_amount)) >0){
    utilization_ratio[i] <- sum(as.numeric(active_accounts$current_balance_accounts))/sum(as.numeric(active_accounts$sanctioned_amount))
     }
    }
    }
    }
  cibil_analysis$util_curr_all_tot <- utilization_ratio
  return(cibil_analysis)
}