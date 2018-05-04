##########################  To Extract Enquiries from Json into Columns ########
##########   enquiry column needed from cibil_analysis Table
#cibil_analysis <- backup
emi_calc <- read.csv("EMI_Calculator.csv", stringsAsFactors = F)
emi_calc_len <- nrow(emi_calc)
Get_Delinquency_Index <- function( cibil_analysis){
library(jsonlite)
  payment_weight <- read.csv("Payment DPD Weights.csv")
  payment_weight$dpd <- tolower(payment_weight$dpd)
  payment_weight <- payment_weight[!duplicated(payment_weight$dpd),]
  
  risk_weight <- read.csv("DPD Weights.csv")
  risk_weight$dpd <- tolower(risk_weight$dpd)
  risk_weight <- risk_weight[!duplicated(risk_weight$dpd),]

    cibil_analysis$payment_index <- 0
    cibil_analysis$delinquency_index <- 0
    cibil_analysis$principal_paid_12 <- 0
    cibil_analysis$accounts_with_individual_ownership <-  "NA"
    cibil_analysis$accounts_with_other_ownership<- "NA"
    cibil_analysis$total_writeoff <- 0
    cibil_analysis$max_dpd_live <- 0
    cibil_analysis$number_of_writeoff <- 0
    cibil_analysis$delinquency_index_lifetime <- 0
    cibil_analysis$dpd_last12 <- 0
    cibil_analysis$dpd_last12_exceptcard <- 0
    cibil_analysis$dpd_last36 <- 0
    cibil_analysis$dpd_alllife <- 0
    cibil_analysis$emi_main <- 0
    cibil_analysis$dpd_last2 <- 0
    cibil_analysis$dpd_last6 <- 0
    len <- length(rownames(cibil_analysis))
  #i=4
  for( i in 1:len){
    #print(i)
    if(!is.na(cibil_analysis$accounts[i])){  
      t <- fromJSON(cibil_analysis$accounts[i])
      if(!is.data.frame(t)){
        t <- fromJSON(paste0("[",cibil_analysis$accounts[i],"]"))
      }
      
      t_len <- length(rownames(t))
      if(!is.null(t_len)){
        if(!is.na(t_len)){
      if(t_len > 0){
      tryCatch({
        if("last_payment" %in% names(t)){
          t$last_payment <- as.character(as.Date(t$last_payment , format = "%d-%m-%Y") ) }
        if("opened" %in% names(t)){
          t$opened <- as.character(as.Date(t$opened , format = "%d-%m-%Y") ) 
        }
        if("sanctioned_amount" %in% names(t)){
          t$sanctioned_amount[is.na(t$sanctioned_amount)] <- 0
          t$sanctioned_amount <-as.numeric(gsub("[^0-9.]" , "",t$sanctioned_amount))}
        if("current_balance_accounts" %in% names(t)){
          t$current_balance_accounts[is.na(t$current_balance_accounts)] <- 0
          t$current_balance_accounts <-as.numeric(gsub("[^0-9.]" , "",t$current_balance_accounts))}
        
        t$principal_paid <- 0
        tryCatch({
          t$principal_paid <- (t$sanctioned_amount - t$ current_balance_accounts )
        },error=function(e){})
        
        t$monthly_principal_paid <- 0
        if("last_payment" %in% names(t)){
          t$pay_days <- as.numeric(as.Date(t$last_payment) - as.Date(t$opened))
          t$monthly_principal_paid <- t$principal_paid / as.numeric(t$pay_days) * 365 / 12
        }
        
        
        cibil_analysis$accounts_with_individual_ownership[i] <- length(t$ownership[tolower(t$ownership) == "individual"])
        cibil_analysis$accounts_with_other_ownership[i] <- length(t$ownership[tolower(t$ownership) != "individual"])
      },error=function(e){
        
      })
      if("status" %in% names(t)){
        t$status <- tolower(t$status)
        temp <- subset( t , grepl('writ' , t$status) | grepl('sett' , t$status) | grepl('pur' , t$status)  )
        temp$current_balance_accounts <- as.numeric(gsub("[^0-9]","",temp$current_balance_accounts))
        cibil_analysis$total_writeoff[i] <- sum(temp$current_balance_accounts[!is.na(temp$current_balance_accounts)])
        
        #cibil_analysis$number_of_writeoff[i] <- length(rownames(temp))
        
        temp$years <- ceiling((as.Date(cibil_analysis$date_pull[i]) -  as.Date(temp$opened))/365)
        cibil_analysis$number_of_writeoff[i] <- sum(1/sqrt(as.numeric(temp$years[!is.na(temp$years)]))  )
        
      }
      t$emi <- 0
      principal_paid_12 <- 0 
      delinquency_index <- 0
      delinquency_index_lifetime <- 0
      payment_index <- 0
      dpd_max_weight <- 0
      dpd_last12_exceptcard <- 0
      dpd_last12 <- 0
      dpd_last36 <- 0
      dpd_alllife <- 0
      dpd_l2 <- 0
      dpd_l6 <- 0
      
      if("current_balance_accounts" %in% names(t)){
      t<- t[order(t$current_balance_accounts, decreasing = T),]
      }
      #j =1
      t$emi_estimated <- 0
      if("emi" %in% names(t)){
        t$emi_estimated[!is.na(t$emi) & t$current_balance_accounts > 100] <- t$emi[!is.na(t$emi) & t$current_balance_accounts > 100 ]}
      
      for(j in 1: t_len){
        if("account_type" %in% names(t) & "current_balance_accounts" %in% names(t) & "sanctioned_amount" %in% names(t)){
        if(!is.na(t$current_balance_accounts[j])  & !is.na(t$sanctioned_amount[j]) & as.numeric(t$current_balance_accounts[j]) > 100 & as.numeric(t$sanctioned_amount[j]) > 100 ){
          if(is.na(t$account_type[j])){
            t$emi[j] <- Get_EMI("other" , t$current_balance_accounts[j] , t$sanctioned_amount[j])
          }else{
          t$emi[j] <- Get_EMI(t$account_type[j] , t$current_balance_accounts[j] , t$sanctioned_amount[j])
          }
          
          t$emi[j] <- max(t$emi_estimated[j], t$emi[j], na.rm = T)
          if(t$sanctioned_amount[j]/t$emi[j] < 6 ){
            t$emi[j]  <- 0
          }
          if(t$current_balance_accounts[j]/t$emi[j] < 6 ){
            t$emi[j]  <- 0
          }
        }
          }
        
        
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
      
      
      
      if("overdue" %in% names(t)){
      
        overdue <-  as.numeric(gsub("[^0-9]" ,"",NA))
        overdue <-  as.numeric(gsub("[^0-9]" ,"",as.data.frame(t$overdue[[j]] , stringsAsFactors = F))[1])
        if(overdue<1000 | is.na(overdue)){
          dpd$`t$dpd`[1] <- 0
        }
      
      }
      
      colnames(dpd) <- c("dpd", "month")
      dpd$monthly_prinicipal <- t$monthly_principal_paid[j]
      dpd$pull_date <- as.Date(cibil_analysis$date_pull[i])
      dpd$dpd2 <- as.numeric(dpd$dpd)
      dpd$dpd[!is.na(dpd$dpd2)] <- dpd$dpd2[!is.na(dpd$dpd2)]
      dpd$dpd <- tolower(dpd$dpd)
      dpd$dpd2 <- NULL
      
      dpd <- merge(dpd ,risk_weight ,by = "dpd", all.x = T , all.y = F)
      dpd <- merge(dpd ,payment_weight ,by = "dpd", all.x = T , all.y = F)
      dpd$risk_weight[is.na(dpd$risk_weight)] <- 10
      dpd$payment_weight[is.na(dpd$payment_weight)] <- 0
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
      
      if('closed_date' %in% names(t)){
      if(is.na(t$closed_date[j])){
        ###########   live loans only ##############
        if(max(dpd$risk_weight) > dpd_max_weight){
          dpd_max_weight <- max(dpd$risk_weight)
        }
      }}
      
      dpd$days <- as.numeric( dpd$pull_date - dpd$month )
      dpd_last12 <- dpd_last12 + nrow(dpd[dpd$risk_weight > 2 & dpd$days <=366  , ])
      if(is.na(t$account_type[j])){t$account_type[j] <- ""}
      if(!grepl('credit card' , tolower(t$account_type[j]))){
         dpd_last12_exceptcard <- dpd_last12_exceptcard + nrow(dpd[dpd$risk_weight > 2 & dpd$days <=366  , ])
      }


      dpd_last36 <- dpd_last36 + nrow(dpd[dpd$risk_weight > 2 & dpd$days <=366*3  , ])
      dpd_alllife <- dpd_alllife + nrow(dpd[dpd$risk_weight > 2 & dpd$days <=366*30  , ])
    
      dpd_l2 <- dpd_l2 + nrow(dpd[ ( dpd$risk_weight >= 2 )  & dpd$days <=100  , ])
      dpd_l6 <- dpd_l6 + nrow(dpd[ ( dpd$risk_weight >= 2 )  & dpd$days <=200  , ])
      
      dpd$days[dpd$days <= 30 & !is.na(dpd$days)] <- 30
      dpd <-  dpd[ as.numeric(dpd$days) <= 365*30 , ]
      
      
      if(length(rownames(dpd)) > 0){
        dpd$delinquency_index_lifetime <- as.numeric(dpd$risk_weight / sqrt(dpd$days))
        delinquency_index_lifetime <- delinquency_index_lifetime + sum(dpd$delinquency_index_lifetime[!is.na(dpd$delinquency_index_lifetime)])
      }
      
      
     
      dpd <-  dpd[ as.numeric(dpd$days) <= 365*3 , ]
      if(length(rownames(dpd)) > 0){
      dpd$delinquency_index <- dpd$risk_weight / sqrt(dpd$days)
      delinquency_index <- delinquency_index + sum(dpd$delinquency_index[!is.na(dpd$delinquency_index)])
      dpd$payment_index <- dpd$payment_weight / sqrt(dpd$days)
      payment_index <- payment_index + sum(dpd$payment_index[!is.na(dpd$payment_index)])
      }
      dpd <-  dpd[ as.numeric(dpd$days) <= 365 , ]
      if(length(rownames(dpd)) > 0){
       principal_paid_12 <- principal_paid_12 + sum(dpd$monthly_prinicipal[!is.na(dpd$monthly_prinicipal) & dpd$risk_weight <=5 ])
      }
      }
      
      }
      }
      
      cibil_analysis$emi_main[i] <- sum(t$emi[!is.na(t$emi)])
      cibil_analysis$max_dpd_live[i] <- dpd_max_weight
      cibil_analysis$delinquency_index_lifetime[i] <- delinquency_index_lifetime
      cibil_analysis$delinquency_index[i] <- delinquency_index
      cibil_analysis$payment_index[i] <- payment_index
      cibil_analysis$principal_paid_12[i] <- principal_paid_12
      cibil_analysis$principal_paid_12[i] <- principal_paid_12
      cibil_analysis$dpd_last12[i] <- dpd_last12
      cibil_analysis$dpd_last12_exceptcard[i] <- dpd_last12_exceptcard
      cibil_analysis$dpd_last36[i] <- dpd_last36
      cibil_analysis$dpd_alllife[i] <- dpd_alllife
      cibil_analysis$dpd_last2[i] <- dpd_l2
      cibil_analysis$dpd_last6[i] <- dpd_l6
      #print(dpd_l2)
      }
    }
  }
      }
    }
  
return(cibil_analysis)
}



Get_EMI <- function( product , latest_balance , sanction){
  temp <- data.frame()
  if(emi_calc_len>0){
  for ( i in 1: emi_calc_len){
    if(grepl( tolower(emi_calc$Product[i]) , tolower(product) ))
    temp <- emi_calc[i , ]
    i <- emi_calc_len+1
  }
  }
  if(nrow(temp) > 0){
    emi1 <- latest_balance*temp$LatestBalance[1]
    emi2 <- sanction*temp$Sanction[1]/100000
  }else{
    temp <- emi_calc[grepl(tolower("other") , tolower(emi_calc$Product)) , ]
    emi1 <- latest_balance*temp$LatestBalance[1]
    emi2 <- sanction*temp$Sanction[1]/100000
  }
  return(max(emi1,emi2))
}
