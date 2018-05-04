
get_bureau_thickness <- function(df){
  x <- df[,CIBIL_SCORE]
  x <- as.numeric(as.character(x))
  result <- ifelse(is.na(x) | x==-1 | x == 0 | x == NULL_STRING,
                   BUREAU_THICKNESS_THIN,
                   BUREAU_THICKNESS_THICK)
  return(result)
}


get_highest_dpd_in_live_loan_weights <- function(df){
  x <- df[,HIGHEST_DPD_IN_LIVE_LOAN]
  y <- df[,BUREAU_THICKNESS_CAL]
  x <- as.numeric(as.character(x))
  result <- ifelse(is.na(x) | (x==0 & y==BUREAU_THICKNESS_THIN), 8,
                   ifelse(x==0, 1,
                          ifelse(x < 30,3,
                                 ifelse(x<60, 8,
                                        ifelse(x<90, 15, 15)))))
  return (result)
}


get_highest_dpd_in_loan_product_last_year_except_gold_weights <- function(df){
  x <- df[,HIGHEST_DPD_IN_LOAN_PRODUCT_LAST_YEAR_EXCEPT_GOLD]
  y <- df[,BUREAU_THICKNESS_CAL]
  x <- as.numeric(as.character(x))
  result <- ifelse(is.na(x) | (x==0 & y==BUREAU_THICKNESS_THIN), 10,
                   ifelse(x==0, 1,
                          ifelse(x < 30,3,
                                 ifelse(x<60, 8,
                                        ifelse(x<90, 15, 15)))))
  return (result)
}

get_highest_dpd_in_loan_product_six_months_weights <- function(df){
  x <- df[,HIGHEST_DPD_IN_LOAN_PRODUCT_SIX_MONTHS]
  y <- df[,BUREAU_THICKNESS_CAL]
  x <- as.numeric(as.character(x))
  result <- ifelse(is.na(x) | (x==0 & y==BUREAU_THICKNESS_THIN), 10,
                   ifelse(x==0, 1,
                          ifelse(x < 30,2,
                                 ifelse(x<60, 10,
                                        ifelse(x<90, 15, 15)))))
  return (result)
}

get_sixty_dpd_last_year_weights <- function(df){
  x <- df[,SIXTY_DPD_LAST_YEAR]
  y <- df[,BUREAU_THICKNESS_CAL]
  x <- as.numeric(as.character(x))
  result <- ifelse(is.na(x) | (x==0 & y==BUREAU_THICKNESS_THIN), 10,
                   ifelse(x==0, 1,
                          ifelse(x ==1,8,10)))
  return (result)
}

get_any_woff_substandard_suit_filed_weights <- function(df){
  x <- tolower(as.character(df[,ANY_WOFF_SUBSTANDARD_SUIT_FILED]))
  result <- ifelse(is.na(x) | x == NULL_STRING, 8,
                   ifelse(x=="no"| x=="false", 2, 10))
  return (result)
}

get_overdue_accounts_weights <- function(df){
  x <- df[,OVERDUE_ACCOUNTS]
  y <- df[,BUREAU_THICKNESS_CAL]
  x <- as.numeric(as.character(x))
  result <- ifelse(is.na(x) | (x==0 & y==BUREAU_THICKNESS_THIN), 6,
                   ifelse(x==0, 1,
                          ifelse(x ==1,6,10)))
  return (result)
}

get_cibil_score_weights <- function(df){
  x <- df[,CIBIL_SCORE]
  x <- as.numeric(as.character(x))
  result <- ifelse(is.na(x) | x==0 | x==-1, 4,
                   ifelse(x<=600, 15,
                          ifelse(x <= 625,10,
                                 ifelse(x<=650, 9,
                                        ifelse(x<=675, 8,
                                               ifelse(x<=700,7,
                                                      ifelse(x<=725, 6,
                                                             ifelse(x<=750, 4,
                                                                    ifelse(x<=800,2,1)))))))))
  return (result)
}

get_no_of_enquiries_last_6_months_weights <- function(df){
  x <- df[,NO_OF_ENQUIRIES_LAST_6_MONTHS]
  y <- df[,BUREAU_THICKNESS_CAL]
  x <- as.numeric(as.character(x))
  result <- ifelse(is.na(x) | (x==0 & y==BUREAU_THICKNESS_THIN), 2,
                   ifelse(x==0, 1,
                          ifelse(x ==1,2,
                                 ifelse(x==2,3,
                                        ifelse(x==3, 4,
                                               ifelse(x<=9, 5,7))))))
  return (result)
}

get_tenure_on_bureau_weights <- function(df){
  x <- as.numeric(df[,BUREAU_HISTORY_SINCE])*365
  result <- ifelse(is.na(x), 6,
                   ifelse(x<=90, 10,
                          ifelse(x <=365,8,
                                 ifelse(x<=730,6,
                                        ifelse(x<=1825, 5,
                                               ifelse(x<=2555, 4,
                                                      ifelse(x<=3650,3,2)))))))
  return (result)
}

get_location_risk_weights <- function(df){
  x <- df[,LOCATION_RISK]
  x <- as.numeric(as.character(x))
  result <- ifelse(is.na(x) | x==0, 6,
                   ifelse(x==1, 2,
                          ifelse(x ==2,3,
                                 ifelse(x==3,5,
                                        ifelse(x==4 | x==5, 6,
                                               ifelse(x==6, 7,
                                                      ifelse(x==7,8,
                                                             ifelse(x==8, 10, 
                                                                    ifelse(x==9,15,20)))))))))
  return (result)
}

get_email_domain_unique_weights <- function(df){
  x <- df[,EMAIL_EDIT_DISTANCE]
  x <- as.numeric(as.character(x))
  result <- ifelse(is.na(x) | x<1, 5,3)
  return (result)
}

get_promoter_age_weights <- function(df){
  x <- df[,PROMOTER_DOB]
  result <- ifelse(is.na(x), 6,
                   ifelse(x<=25, 6,
                          ifelse(x <=35,4,
                                 ifelse(x<=50,3,
                                        ifelse(x<=60, 4,6)))))
  return (result)
}

get_business_vinatge_weights <- function(df){
   x <- df[,VINTAGE_MONTHS]
   # Check whether to use anchor vintage here
  result <- ifelse(is.na(x), 7,
                   ifelse(x<=1, 10,
                          ifelse(x <=2,8,
                                 ifelse(x<=5,5,
                                        ifelse(x<=10, 3,
                                               ifelse(x<=15, 2,1))))))
  return (result)
}

get_own_address_weights <- function(df){
  x <- tolower(as.character(df[,RESIDENCE_OWNED]))
  x[is.na(x)] <- FALSE_STRING
  y <- tolower(as.character(df[,OFFICE_OWNED]))
  y[is.na(y)] <- FALSE_STRING
  result <- ifelse(x==TRUE_STRING & y==TRUE_STRING, 3,
                   ifelse(y==TRUE_STRING | x==TRUE_STRING, 5, 7))
  return (result)
}

get_score_against_weight <- function(df, var_name){
  x <- df[,var_name]
  y <- df[,BUREAU_THICKNESS_CAL]
  result <- ifelse(y==BUREAU_THICKNESS_THICK,
                   THICK_FILE_WEIGHTS_LIST[[var_name]]*x,
                   THIN_FILE_WEIGHTS_LIST[[var_name]]*x)
  return(result)
}

get_risk_band <- function(df){
  x <- df[,NEW_SCORE_CAL]
  y <- df[,BUREAU_THICKNESS_CAL]
  result <- ifelse(y==BUREAU_THICKNESS_THICK,
                   ifelse(x <= 2.45, 0,
                          ifelse(x <= 2.93, 1,
                                 ifelse(x <= 3.25, 2,
                                        ifelse(x <= 3.57, 3,
                                               ifelse(x <= 3.88, 4,
                                                      ifelse(x <= 4.21, 5,
                                                             ifelse(x<=4.6, 6,
                                                                    ifelse(x <= 5.08, 7,
                                                                           ifelse(x <= 5.64, 8,
                                                                                  ifelse(x <= 6.39, 9, 10)))))))))),
                   ifelse(x <= 3.8, 0,
                          ifelse(x <= 4.88, 1,
                                 ifelse(x <= 5.46, 2,
                                        ifelse(x <= 5.88, 3,
                                               ifelse(x <= 6.31, 4,
                                                      ifelse(x <= 6.59, 5,
                                                             ifelse(x<=6.73, 6,
                                                                    ifelse(x <= 6.97, 7,
                                                                           ifelse(x <= 7.57, 8,
                                                                                  ifelse(x <= 8.25, 9, 10)))))))))))
  return (result)
}

get_as_date <- function(x, format1="%Y-%m-%d", format2="%d-%m-%Y", format3="%m/%d/%Y"){
  y <- strptime(x, format3)
  y[is.na(y)] <- strptime(x[is.na(y)], format1)
  y[is.na(y)] <- strptime(x[is.na(y)], format2)
  y[is_year_wrong(y)] <- strptime(x[is_year_wrong(y)], format2)
  return(y)
}

is_year_wrong <- function(x){
  y <- year(x) > 2020 | year(x) < 1900
  y[is.na(y)] <- FALSE
  return(y)
}

get_anchor_monthly_ts_factor <- function(band, file_type){

  x <- ifelse(file_type==BUREAU_THICKNESS_THICK,
              ifelse(band==1, 2,
                     ifelse(band==2, 1.5,
                            ifelse(band==3, 1.0,
                                   ifelse(band==4, 0.8,
                                          ifelse(band==5, 0.7,
                                                 ifelse(band==6, 0.6,
                                                        ifelse(band==7|band==8,0.5,-1))))))),
              ifelse(band==1| band==0,1.5,
                     ifelse(band==2, 1.25,
                            ifelse(band==3, 0.8, 
                                   ifelse(band==4, 0.6,-1)))))
  return (x)
}

get_bank_balance_eod_factor <- function(band, file_type){
  x <- ifelse(file_type==BUREAU_THICKNESS_THICK,
              ifelse(band==1 | band==0, 12.5,
                     ifelse(band==2, 10.0,
                            ifelse(band==3, 9.0,
                                   ifelse(band==4, 8.0,
                                          ifelse(band==5, 6.0,
                                                 ifelse(band==6, 6.0,
                                                        ifelse(band==7|band==8,6.0,-1))))))),
              ifelse(band==1| band==0,10.0,
                     ifelse(band==2, 8.0,
                            ifelse(band==3, 8.0, 
                                   ifelse(band==4, 6.0, -1)))))
  return (x)
}

get_highest_amount_borrowed_factor <- function(band,file_type){
  
  x <- ifelse(file_type==BUREAU_THICKNESS_THICK,
              ifelse(band==1 | band==0, 7.0,
                     ifelse(band==2, 6.0,
                            ifelse(band==3, 5.0,
                                   ifelse(band==4, 4.0,
                                          ifelse(band==5, NA,
                                                 ifelse(band==6, NA,
                                                        ifelse(band==7|band==8,NA,-1))))))),
              ifelse(band==1| band==0,NA,
                     ifelse(band==2, NA,
                            ifelse(band==3, NA, 
                                   ifelse(band==4, NA,-1)))))
  return (x)
}

get_loan_cap <- function(df){
  bureau_thickness <- df[,BUREAU_THICKNESS_CAL]
  band <- df[,NEW_SCORE_BAND_CAL]
  x <- ifelse(bureau_thickness==BUREAU_THICKNESS_THICK,
              ifelse(band==1 | band==0, 5000000,
                     ifelse(band==2, 3500000,
                            ifelse(band==3, 3000000,
                                   ifelse(band==4, 2000000,
                                          ifelse(band==5, 500000,
                                                 ifelse(band==6, 200000,
                                                        ifelse(band==7|band==8,100000,-1))))))),
              ifelse(band==1| band==0,300000,
                     ifelse(band==2, 200000,
                            ifelse(band==3, 200000,
                                   ifelse(band==4, 100000, -1)))))
  return (x)
}

get_anchor_monthly_ts_line <- function(df){
  anchor_monthly_ts <- df[,ANCHOR_MONTHLY_TS]
  anchor_monthly_ts <- as.numeric(as.character(anchor_monthly_ts))
  bureau_thickness <- df[,BUREAU_THICKNESS_CAL]
  band <- df[,NEW_SCORE_BAND_CAL]
  return(anchor_monthly_ts*get_anchor_monthly_ts_factor(band, bureau_thickness))
}

get_bank_eod_balance_line <- function(df){
  bank_eod_balance <- df[,BANK_EOD_BALANCE]
  bank_eod_balance <- as.numeric(as.character(bank_eod_balance))
  bank_eod_balance[bank_eod_balance <0 ] <- NA
  #bank_eod_balance <- abs(bank_eod_balance)
  bureau_thickness <- df[,BUREAU_THICKNESS_CAL]
  band <- df[,NEW_SCORE_BAND_CAL]
  return(bank_eod_balance*get_bank_balance_eod_factor(band, bureau_thickness))
}

get_highest_amount_borrowed_line <- function(df){
  highest_amount_borrowed <- df[,HIGHEST_AMOUNT_BORROWED]
  highest_amount_borrowed <- as.numeric(as.character(highest_amount_borrowed))
  bureau_thickness <- df[,BUREAU_THICKNESS_CAL]
  band <- df[,NEW_SCORE_BAND_CAL]
  return(highest_amount_borrowed*get_highest_amount_borrowed_factor(band, bureau_thickness))
}

get_all_line_amounts <- function(df){
  
  factor_names_vector <- c("Anchor Monthly TS", "Bank EOD Balance", "Highest Amount Borrowed", "Loan Cap", "NA")
  
  anchor_monthly_ts_line <- df[,ANCHOR_MONTHLY_TS_LINE]
  bank_eod_balance_line <- df[,BANK_EOD_BALANCE_LINE]
  highest_amount_borrowed_line <- df[,HIGHEST_AMOUNT_BORROWED_LINE]
  loan_cap <- df[,LOAN_CAP_LINE]
  
  loan_cap[is.na(anchor_monthly_ts_line) &
           is.na(bank_eod_balance_line) &
           is.na(highest_amount_borrowed_line)] <- NA
  
  tempdf <- as.data.frame(matrix(c(anchor_monthly_ts_line, bank_eod_balance_line, highest_amount_borrowed_line, loan_cap), ncol = length(anchor_monthly_ts_line), byrow = TRUE))
  
  line <- sapply(tempdf, FUN=min, na.rm=TRUE)
  
  line_reason_index <- sapply(tempdf, FUN=which.min)
  line_reason_index_lengths <- sapply(line_reason_index, FUN=length)
  line_reason_index[line_reason_index_lengths==0] <- 5
  line_reason_index = unlist(line_reason_index)
  line_reason_index[is.na(line_reason_index)] <- 5
  line_reason_index[line_reason_index==0] <- 5
  line_reasons_vector <- factor_names_vector[line_reason_index]
  
  line[line < 0] <- -1
  line[line==Inf] <- -1
  
  return(list(line, line_reasons_vector))
}

get_model_decision <- function(df){
  bureau_thickness <- df[,BUREAU_THICKNESS_CAL]
  band <- df[,NEW_SCORE_BAND_CAL]
  result <- ifelse(bureau_thickness==BUREAU_THICKNESS_THICK,
                   ifelse(band %in% 0:5, "Approved",
                          ifelse(band %in% 6:8, "Manual Review", "Declined")),
                   ifelse(band %in% 0:2, "Approved",
                          ifelse(band %in% 3:4, "Manual Review", "Declined")))
  return(result)
}

get_interest_rates <- function(df){
  bureau_thickness <- df[,BUREAU_THICKNESS_CAL]
  band <- df[,NEW_SCORE_BAND_CAL]
  result <- ifelse(bureau_thickness==BUREAU_THICKNESS_THICK,
                   ifelse(band==0 | band ==1, 18,
                          ifelse(band==2, 20,
                                 ifelse(band==3, 21,
                                        ifelse(band==4, 22,
                                               ifelse(band==5, 24,
                                                      ifelse(band==6, 26,
                                                             ifelse(band==7, 28,
                                                                    ifelse(band==8, 30, NA)))))))),
                   ifelse(band==1, 26,
                          ifelse(band==2, 30,
                                 ifelse(band==3 | band == 4, 35, NA)))
                   )
  return(result)
}

get_var_name <- function(x, replacement="_same"){
  return (gsub("_score_cal", replacement, x))
}