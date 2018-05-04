##########################  To Extract Enquiries from Json into Columns ########
##########   enquiry column needed from cibil_analysis Table
#cibil_analysis <- backup
fast_loan_pre_line_parameters <- read.csv("fast_loan_pre_adjusted_line.csv", stringsAsFactors = F)

source("Functions/9_Function_Line_Calculation.R")
Get_Accounts <- function( cibil_analysis){
 
  cibil_analysis$number_of_accounts <- nchar(cibil_analysis$accounts) - nchar(gsub("}","",cibil_analysis$accounts))
  cibil_analysis$number_of_accounts[is.na( cibil_analysis$number_of_accounts)] <- 0
  
  max_accounts <- max(cibil_analysis$number_of_accounts[!is.na(cibil_analysis$number_of_accounts)])
  
  max_accounts <- min(1 , max_accounts)
  
  cibil_analysis$debt_as_of_today <- 0
  cibil_analysis$total_monthly_debt <- 0
  cibil_analysis$dpd_all <- ""
  cibil_analysis$account_opened_60  <- 0
  cibil_analysis$account_opened_90  <- 0
  cibil_analysis$account_opened_180 <- 0
  cibil_analysis$account_opened_366 <- 0
  cibil_analysis$account_closed_60  <- 0
  cibil_analysis$account_closed_90  <- 0
  cibil_analysis$account_closed_180 <- 0
  cibil_analysis$account_closed_366 <- 0
  cibil_analysis$invol_closed_60  <- 0
  cibil_analysis$invol_closed_90  <- 0
  cibil_analysis$invol_closed_180 <- 0
  cibil_analysis$invol_closed_366 <- 0
  cibil_analysis$life_time_sanction_amount <- 0
  cibil_analysis$highest_amount_borrowed <- 0
  cibil_analysis$highest_amount_borrowed2 <- 0
  cibil_analysis$highest_amount_borrowed_account_type <- ""
  cibil_analysis$highest_amount_borrowed_opened <- NA
  cibil_analysis$highest_amount_borrowed_account_ownership <- ""
  cibil_analysis$total_principal_paid <- 0
  cibil_analysis$leverage_ratio <- 0
  cibil_analysis$total_overdue <- 0
  cibil_analysis$max_cibil_line <- 0
  
  if(max_accounts <=0 ){
    return(cibil_analysis)
  }
  

 
  
  for( i in 1 : max_accounts){
    col <- paste0("ac_opened_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("ac_reported_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("ac_ownership",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("ac_closed_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("ac_payment_start_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("ac_payment_end_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("ac_collateral_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("ac_sanctioned_amt_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("ac_current_balance_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("ac_type_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("ac_status_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("overdue_",i)
    cibil_analysis[[col]] <- "-"
    
    col <- paste0("emi_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("f_start_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("f_report_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("last_payment_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("principal_paid_",i)
    cibil_analysis[[col]] <- "NA"
    col <- paste0("pay_days_",i)
    cibil_analysis[[col]] <- "NA"
    col <- paste0("principal_monthly_",i)
    cibil_analysis[[col]] <- "NA"
    
    
    month <- as.Date(paste0(substr(Sys.Date(),1,7),"-01"))
    for(k in 1:60){
      
      dpd_col <- paste0("dpd_", substr(month,6,7),"-", substr(month,3,4) ,"_",i)
      cibil_analysis[[dpd_col]] <- "NA"
      month <- as.Date(paste0(substr(month-1,1,7),"-01"))
    }
    
  }
  
  
  
  len <- length(rownames(cibil_analysis))
  i=1
  for( i in 1:len){
    #print(i)
    if(!is.na(cibil_analysis$accounts[i])){  
     
      t <- fromJSON(cibil_analysis$accounts[i])
      if(!is.data.frame(t)){
        t <- fromJSON(paste0("[",cibil_analysis$accounts[i],"]"))
      }
      
      t_len <- min(max_accounts,length(rownames(t)))
      pull_date <- as.Date(cibil_analysis$date_pull[i])
        
      
      if("reported" %in% names(t)){
      t$reported <- as.character(as.Date(t$reported , format = "%d-%m-%Y") ) }
      
      if("current_balance_accounts" %in% names(t)){
        if("account_type" %in% names(t)){
        t$current_balance_accounts[!grepl('credit' , tolower(t$account_type))] <- as.numeric(gsub("[^0-9]","",t$current_balance_accounts[!grepl('credit' , tolower(t$account_type))]))
        
        }else{
          t$current_balance_accounts<- as.numeric(gsub("[^0-9]","",t$current_balance_accounts))
        }
      }
      
      if("payment_end" %in% names(t)){
      t$payment_end <- as.character(as.Date(t$payment_end , format = "%d-%m-%Y")) }
      if("payment_start" %in% names(t)){
        t$payment_start <- as.character(as.Date(t$payment_start , format = "%d-%m-%Y")) }
      if("last_payment" %in% names(t)){
      t$last_payment <- as.character(as.Date(t$last_payment , format = "%d-%m-%Y") ) }else{
        if("payment_start" %in% names(t)){
        t$last_payment <- as.character(as.Date(t$payment_start  , format = "%d-%m-%Y"))}
      }
      if("opened" %in% names(t)){
        t$opened <- as.character(as.Date(t$opened , format = "%d-%m-%Y") ) 
       }
      if("sanctioned_amount" %in% names(t)){
        
        t$sanctioned_amount <-as.numeric(gsub("[^0-9.]" , "",t$sanctioned_amount))
        t$sanctioned_amount[is.na(t$sanctioned_amount)] <- 0
        cibil_analysis$life_time_sanction_amount[i] <- sum(t$sanctioned_amount[!is.na(t$sanctioned_amount) & t$sanctioned_amount >0 ])
        t$cb <- 0
        for(ii in 1 : nrow(t)){
          t$cb[ii] <- get_pre_adjusted_fast_loan_line(t$sanctioned_amount[ii],
                                         t$account_type[ii],
                                         t$opened[ii],
                                         cibil_analysis$date_pull[i],
                                         t$ownership[ii])
          
        }
        
        t <- t[order(t$cb, decreasing = T),]
        cibil_analysis$highest_amount_borrowed[i] <- t$sanctioned_amount[1]
        if("account_type" %in% names(t)){
          cibil_analysis$highest_amount_borrowed_account_type[i] <- t$account_type[1]
        }
        if("opened" %in% names(t)){
          cibil_analysis$highest_amount_borrowed_opened[i] <- t$opened[1]
        }
        if("ownership" %in% names(t)){
          cibil_analysis$highest_amount_borrowed_account_ownership[i] <- t$ownership[1]
        }
        
        
        
      }
     
      # if("sanctioned_amount" %in% names(t)){
      # t$sanctioned_amount <-as.numeric(gsub("[^0-9.]" , "",t$sanctioned_amount))
      # cibil_analysis$life_time_sanction_amount[i] <- sum(t$sanctioned_amount[!is.na(t$sanctioned_amount) & t$sanctioned_amount >0 ])
      highest_amount_borrowed <- max(t$sanctioned_amount[!is.na(t$sanctioned_amount) & t$sanctioned_amount >0 ])
      cibil_analysis$highest_amount_borrowed2[i] <- highest_amount_borrowed
      # highest_amount_borrowed_account <- t[t$sanctioned_amount == highest_amount_borrowed, ][1,]
      # cibil_analysis$highest_amount_borrowed_account_type[i] <- highest_amount_borrowed_account$account_type
      # cibil_analysis$highest_amount_borrowed_opened[i] <- highest_amount_borrowed_account$opened
      # }
      
      if("formatted_start_date" %in% names(t)){
      t$formatted_start_date <- as.character(as.Date(substr(t$formatted_start_date,1,10))) }
      if("formatted_report_date" %in% names(t)){
      t$formatted_report_date <- as.character(as.Date(substr(t$formatted_report_date,1,10))) }
      if("formatted_opened_date" %in% names(t)){
      t$formatted_opened_date <- as.character(as.Date(substr(t$formatted_opened_date,1,10))) }
      if("emi" %in% names(t)){
        t$emi <-  as.numeric(gsub("[^0-9.]","",t$emi)) }
      if("actual_payment" %in% names(t)){
        t$actual_payment <-  as.numeric(gsub("[^0-9.]","",t$actual_payment)) }
      if("opened" %in% names(t)){
        cibil_analysis$account_opened_60[i] <- length(rownames( subset( t  , as.Date(t$opened) > pull_date - 60  ) ))
        cibil_analysis$account_opened_90[i] <- length(rownames( subset( t  , as.Date(t$opened) > pull_date - 90  ) ))
        cibil_analysis$account_opened_180[i] <- length(rownames( subset( t  , as.Date(t$opened) > pull_date - 180  ) ))
        cibil_analysis$account_opened_366[i] <- length(rownames( subset( t  , as.Date(t$opened) > pull_date - 366  ) ))
        
      }
      tryCatch({
      t$principal_paid <- (t$sanctioned_amount - t$ current_balance_accounts )
      t$pay_days <- as.numeric(as.Date(t$last_payment) - as.Date(t$opened))
      
      
      t$emi_estimated <- t$principal_paid / as.numeric(t$pay_days) * 365 / 12
      t$emi_estimated[is.na(t$emi_estimated)] <- 0
      t$emi_estimated[t$emi_estimated < 0 ] <- 0
     
      if("emi" %in% names(t)){
      t$emi_estimated[!is.na(t$emi) & t$current_balance_accounts > 100] <- t$emi[!is.na(t$emi) & t$current_balance_accounts > 100 ]}
      
      t$emi2 <- (t$current_balance_accounts*1.2)/12
      t$emi3 <- (t$current_balance_accounts*((1.09)^15))/360
      t$emi_estimated[t$emi_estimated > t$emi2 & !is.na(t$emi2)] <-  t$emi2[t$emi_estimated > t$emi2 & !is.na(t$emi2)]
      t$emi_estimated[t$emi_estimated < t$emi3 & !is.na(t$emi3)] <-  t$emi3[t$emi_estimated < t$emi3 & !is.na(t$emi3)]
      
      #t$emi_estimated[t$emi > 100 & !is.na(t$emi)] <- t$emi[t$emi > 100 & !is.na(t$emi)]
      cibil_analysis$total_principal_paid[i]  <- sum(t$principal_paid[!is.na(t$principal_paid) & t$principal_paid > 0 ])
      cibil_analysis$debt_as_of_today[i] <- sum(t$current_balance_accounts[!is.na(t$current_balance_accounts)])
      cibil_analysis$total_monthly_debt[i] <- sum(t$emi_estimated[!is.na(t$emi_estimated) & t$current_balance_accounts > 100 & !is.na(t$current_balance_accounts)])
      cibil_analysis$leverage_ratio[i] <- as.numeric(cibil_analysis$debt_as_of_today[i])/as.numeric(cibil_analysis$life_time_sanction_amount[i])
      #cibil_analysis$dpd_all[i] <- paste(unique(unlist(t$dpd1)),collapse = "|")
      
      cibil_analysis$debt_to_monthly_principal[i] <-  as.numeric(cibil_analysis$debt_as_of_today[i]) /  as.numeric(cibil_analysis$total_monthly_debt[i])
      
      
      t2 <- aggregate(principal_paid~account_type , t , sum )      
      for(z in 1:length(rownames(t2))){
       col <- paste0("principal_paid_",t2$account_type[z])
       if(!col %in% names(cibil_analysis)){
         cibil_analysis[[col]] <- 0
       }
       cibil_analysis[[col]][i] <- t2$principal_paid[z] 
      
      }
      
      
      
      },error = function(e){})
      

      if("closed_date" %in% names(t)){
        t$closed_date <- as.character(as.Date(t$closed_date , format = "%d-%m-%Y") )
        
        cibil_analysis$account_closed_60[i] <- length(rownames( subset( t  , as.Date(t$closed_date) > pull_date - 60  ) ))
        cibil_analysis$account_closed_90[i] <- length(rownames( subset( t  , as.Date(t$closed_date) > pull_date - 90  ) ))
        cibil_analysis$account_closed_180[i] <- length(rownames( subset( t  , as.Date(t$closed_date) > pull_date - 180  ) ))
        cibil_analysis$account_closed_366[i] <- length(rownames( subset( t  , as.Date(t$closed_date) > pull_date - 366  ) ))
        
        
        if("status" %in% names(t)){
          t$status <- tolower(t$status)
          temp <- subset( t , grepl('writ' , t$status) | grepl('restruct' , t$status) )
          
          cibil_analysis$invol_closed_60[i] <- length(rownames( subset( temp  , as.Date(temp$closed_date) > pull_date - 60 ) ))
          cibil_analysis$invol_closed_90[i] <- length(rownames( subset( temp  , as.Date(temp$closed_date) > pull_date - 90  ) ))
          cibil_analysis$invol_closed_180[i] <- length(rownames( subset( temp  , as.Date(temp$closed_date) > pull_date - 180  ) ))
          cibil_analysis$invol_closed_366[i] <- length(rownames( subset( temp  , as.Date(temp$closed_date) > pull_date - 366  ) ))
        }
      }
      
      if("overdue" %in% names(t)){
        cibil_analysis$total_overdue[i] <- sum(as.numeric( gsub("[^0-9]", "" , t$overdue[!is.na(t$overdue)])))
        }
             
      
    }
  }
  
#all_dpd <- unique(all_dpd)  
#write.csv(all_dpd , "dpd_all.csv" , row.names = F)  
  
return(cibil_analysis)
}





get_pre_adjusted_fast_loan_line <- function(highest_amount_borrowed, account_type, account_opened, cibil_pull_date, account_ownership){
  
  inflation_rate <- 0.08
  if(is.null(account_opened)){
    return(0)
  }
  if(is.na(account_opened)){
    return(0)
  }
  account_opened <- as.character(account_opened)
  
  cibil_pull_date <- as.character(cibil_pull_date)
  account_opened <- as.Date(account_opened, "%Y-%m-%d")
  years_from_today <- as.numeric((as.Date(cibil_pull_date, "%Y-%m-%d")-account_opened)/365)
  years_from_today[is.na(years_from_today) | years_from_today <0 ] <- 0
  inflation_factor <- (1+inflation_rate)^years_from_today
  
  inflation_factor[is.na(inflation_factor)] <- 1
  
  if(years_from_today < 0.5){
    inflation_factor <- 0
  } else if (years_from_today <= 1){
    inflation_factor <- inflation_factor*0.7
  }else if (years_from_today > 7){
    inflation_factor <- 0
  }
  
  highest_amount_borrowed <- highest_amount_borrowed*inflation_factor
  
  if(is.null(account_type)){
    account_type <- ""
  }
  
  account_type[is.na(account_type)] <- ""
  
  account_type <- as.character(account_type)
  
  final_account_type <- "Others"
  if(grepl("hous", account_type, ignore.case = T) | grepl("prop", account_type, ignore.case = T)){
    final_account_type <- "Housing Loan"
  } else if (grepl("auto", account_type, ignore.case = T) | grepl("wheeler", account_type, ignore.case = T)  | grepl("car ", account_type, ignore.case = T) | grepl("vehicle", account_type, ignore.case = T)){
    final_account_type <- "Auto"
  } else if (grepl("personal", account_type, ignore.case = T) | grepl("business", account_type, ignore.case = T) | grepl("consumer", account_type, ignore.case = T)){
    final_account_type <- "PL/BL"
  }else if (grepl("card", account_type, ignore.case = T)){
    final_account_type <- "Credit Card"
  }
  
  
  m_factor <- fast_loan_pre_line_parameters[
      fast_loan_pre_line_parameters$AccountType == final_account_type,2]
  
  if(is.null(account_ownership)){
    account_ownership <- ""
  }
  account_ownership[is.na(account_ownership)] <- ""
  account_ownership <- tolower(as.character(account_ownership))
  account_type <- tolower(as.character(account_type))
  
  if( grepl("gurantor",account_ownership)){
    m_factor <- 0
  }
  
  if(grepl("securities", account_type) | grepl("shares", account_type) | grepl("kisan credit card", account_type) | grepl("gold", account_type) | grepl("overdraft", account_type)){
    m_factor <- 0
  }
  
  return(highest_amount_borrowed*m_factor)
  
}  