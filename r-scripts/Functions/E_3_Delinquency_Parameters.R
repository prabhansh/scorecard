#################################################################################
############################# DELINQUENCY PARAMETERS ############################
########### All 4 delinquency parameters have been returned from ################
###########                 this single function only            ################
#################################################################################

risk_weight <- read.csv("DPD Weights.csv")
risk_weight$dpd <- tolower(risk_weight$dpd)

library(plyr)

############### Max Delinquency 12 months on active accounts ####################
Get_Delinquency_Parameters <- function(cibil_analysis){
  
  cibil_analysis$max_dpd_12m_act <- 0
  cibil_analysis$nbr_prod_0_12m_pl <- 0
  cibil_analysis$mon_since0_all <- 0
  cibil_analysis$nbr_30_3m_act <- 0
  
  
  
  for(i in 1:length(rownames(cibil_analysis))){
    #print(i)
    if(!is.na(cibil_analysis$accounts[i])){
      t <- fromJSON(cibil_analysis$accounts[i])
      if(!is.data.frame(t)){
         t <- fromJSON(paste0("[",cibil_analysis$accounts[i],"]"))
      }
      t_len <- nrow(t)
      if(!is.null(t_len)){
        if(!is.na(t_len)){
        if(t_len >0 ){
      tryCatch({
      if("last_payment" %in% names(t)){
        t$last_payment <- as.character(as.Date(t$last_payment , format = "%d-%m-%Y") ) }
      },error=function(e){})
      
      tryCatch({
      if("current_balance_accounts" %in% names(t)){
        t$current_balance_accounts <- as.numeric(gsub("[^0-9.]","",t$current_balance_accounts ))
      }else{
        t$current_balance_accounts <- 0
      }
      },error=function(e){})
      
      
      dpd_table <- data.frame()
      n_3<- 0
      nbr_prod_0_12m_pl <- 0
      
      for(j in 1: t_len){
        
        
        
        #dpd <- as.data.frame(t$dpd1[[j]] , stringsAsFactors = F)
        dpd <- as.data.frame(c(t$dpd1[[j]],t$dpd2[[j]]), stringsAsFactors = F)
        if(nrow(dpd) > 0){
        
        colnames(dpd)<-"dpd"
        
        month <- as.data.frame(t$dpd_month[[j]] , stringsAsFactors = F)
        dpd_len <- length(rownames(dpd))
        month_len <- length(rownames(month))
        dpd_len <- min(dpd_len, month_len)
        if(dpd_len > 0){
          dpd$month <- "-"
          dpd$month[1:dpd_len] <- month[1:dpd_len,]
          flag = 1
          tryCatch({
          dpd$month <- as.Date(paste0("20",substr(dpd$month,4,5) ,"-", substr(dpd$month,1,2)  , "-01"))
          flag = 0
          }, error= function(e){
          })
          if(flag==1){
            dpd$month <- NA
          }
          
          colnames(dpd) <- c("dpd", "month")
          dpd$pull_date <- as.Date(cibil_analysis$date_pull[i])
          dpd$days <- as.numeric( dpd$pull_date - dpd$month )
          dpd$dpd2 <- as.numeric(dpd$dpd)
          dpd$dpd[!is.na(dpd$dpd2)] <- dpd$dpd2[!is.na(dpd$dpd2)]
          dpd$dpd <- tolower(dpd$dpd)
          dpd$dpd2 <- NULL
          dpd <- merge(dpd ,risk_weight ,by = "dpd", all.x = T , all.y = F)
          dpd$risk_weight[is.na(dpd$risk_weight)] <- 10
          
          ############## Change DPD Month if DPD recurring & last payment is very far ###########
          
          tryCatch({
          if("last_payment" %in% names(t)){
            if(!is.na(t$last_payment[j])){
              latest_payment_month <- as.Date(paste0(substr(as.Date(t$last_payment[j])+365*3,1,7),"-01"))
              latest_dpd_month <- as.Date(dpd$month[1])
              days_diff <- as.numeric(latest_dpd_month - latest_payment_month)
              n_risk <- nrow(dpd[dpd$risk_weight >=10 ,])
              #print(days_diff)
              if(days_diff > 365*3 & n_risk > 5){
                dpd$month <- as.Date(paste0(substr(as.Date(dpd$month) - days_diff,1,7),"-01"))
              }
            }
          }
            }, error = function(e){})
          ######################################################################################
          
          dpd$dpd <- as.numeric(dpd$dpd)
          dpd$dpd[is.na(dpd$dpd) & dpd$risk_weight ==0 ] <- 0
          dpd$dpd[is.na(dpd$dpd) & dpd$risk_weight >0 ] <- 30
          dpd$dpd[is.na(dpd$dpd) & dpd$risk_weight > 4 ] <- 60
          dpd$dpd[is.na(dpd$dpd) & dpd$risk_weight > 6 ] <- 90
          dpd$dpd[is.na(dpd$dpd) & dpd$risk_weight > 8 ] <- 120
          
          
          dpd$dpd[dpd$dpd >= 180] <- 180 
          dpd$dpd[dpd$dpd >= 90 & dpd$dpd < 180] <- 90 
          dpd$dpd[dpd$dpd >= 60 & dpd$dpd < 90] <- 60
          dpd$dpd[dpd$dpd >= 30 & dpd$dpd < 60] <- 30
          dpd$dpd[dpd$dpd > 0 & dpd$dpd < 30] <- 29
          dpd <- dpd[order(dpd$days),]
          
          if("status" %in% names(t)){
            if(!is.na(t$status[j])){
              dpd$dpd[1] <- 180
            }
          }
          
          dpd$account_type <- t$account_type[j]
          if("reported" %in% names(t)){
          #dpd$reported_date <- as.Date(t$reported[j] , "%d-%m-%Y")
          dpd$reported <- max(dpd$month)
          }else{
          dpd$reported <- max(dpd$month)
          }
          dpd$current_balance <- t$current_balance_accounts[j]
          dpd$dpd2 <- dpd$dpd
          dpd$dpd2[dpd$current_balance <= 3000 & grepl('credit card', tolower(dpd$account_type) )] <- 0
          
          if(nrow(dpd[dpd$days <= 90 , ]) > 0){
          n_3 <- n_3 + nrow(dpd[dpd$days <= 90 & dpd$dpd2>=30 , ])/nrow(dpd[dpd$days <= 90 , ])
          #count_n3 <- count_n3 + 1
          }
          
          dpd_table_pl <- dpd[as.numeric(dpd$days) < 366 & grepl('personal loan',tolower(dpd$account_type) ) & dpd$dpd > 0  , ]
          
          if(nrow(dpd_table_pl)>0){
          nbr_prod_0_12m_pl <- nbr_prod_0_12m_pl + 1
          }
          
          dpd_table <- rbind.fill(dpd_table, dpd)
         
        }
      }
      }
    
      
      
      if(nrow(dpd_table)>0){
      dpd_table_12 <- subset(dpd_table, as.numeric(dpd_table$days) < 366)
      
      ############################### No Active Account ##########################
      if(nrow(dpd_table_12)<=0){
        n_3 <- -1
      }
      #################################################################################
      
      ###################################  No PL in Active Accounts ######################
      
      if("account_type" %in% names(dpd_table_12)){
        if(nrow(dpd_table_12[grepl('personal loan',tolower(dpd_table_12$account_type)) ,])<=0){
          nbr_prod_0_12m_pl <- -1
        }
      }else{
        nbr_prod_0_12m_pl <- -1
      }
      
      #####################################################################################
      
      dpd_table_12 <- subset(dpd_table_12, dpd_table_12$reported > (as.Date(cibil_analysis$date_pull[i]) -366) )
      
      if(nrow(dpd_table_12)>0){
      cibil_analysis$max_dpd_12m_act[i] <- max(dpd_table_12$dpd)
      }else{
        cibil_analysis$max_dpd_12m_act[i] <- -1
      }
      
      latest_dpd <- subset(dpd_table , dpd_table$dpd > 0)
      if(nrow(latest_dpd) > 0 ){
        temp <- latest_dpd$days[order(latest_dpd$month , decreasing = T)][1]/30.5
        cibil_analysis$mon_since0_all[i] <- 0
        if(!is.na(temp)){
        if(temp>=1){
          cibil_analysis$mon_since0_all[i] <- floor(temp)
        }else if(temp<1){
          cibil_analysis$mon_since0_all[i] <- ceiling(temp)
        }
      }
      }
      
      }else{
        n_3 <- -1
        nbr_prod_0_12m_pl <- -1
        cibil_analysis$max_dpd_12m_act[i] <- -1
      }
      
       cibil_analysis$nbr_prod_0_12m_pl[i] <- nbr_prod_0_12m_pl    
      
       
      cibil_analysis$nbr_30_3m_act[i] <- n_3
      
      
      
        }
      }
      }
    }
  }
  
  cibil_analysis$max_dpd_12m_act[is.na(cibil_analysis$max_dpd_12m_act)] <- 0
  cibil_analysis$nbr_prod_0_12m_pl[is.na(cibil_analysis$nbr_prod_0_12m_pl)] <- 0
  cibil_analysis$mon_since0_all[is.na(cibil_analysis$mon_since0_all)] <- 0
  cibil_analysis$nbr_30_3m_act[is.na(cibil_analysis$nbr_30_3m_act)] <- 0
  
  return(cibil_analysis)
}
