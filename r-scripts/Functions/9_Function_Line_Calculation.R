

options(scipen = 999)

get_vertical_line <- function(data,
                              industry,
                              monthly_anchor_ts,
                              interest_rate,
                              tenor){
  
  file_type <- data$Bureau_thickness_gen2
  score_band <- data$mean_band
  eod_bank_balance <- data$average_eod_balance_6_months
  eod_od_bank_balance <- data$average_od_eod_balance_6_months
  highest_amount_borrowed <- data$highest_amount_borrowed
  pbt <- data$profit_before_tax
  
  line_logic <- read.csv("line_logic.csv", stringsAsFactors = F)
  
  vertical_line_parameters <- list()
  
  industry <- tolower(as.character(industry))
  vertical_line_parameters$industry_adjusted <- industry
  
  monthly_anchor_ts <- as.numeric(as.character(monthly_anchor_ts))
  eod_bank_balance <- as.numeric(as.character(eod_bank_balance))
  eod_od_bank_balance <- as.numeric(as.character(eod_od_bank_balance))
  
  
  
  highest_amount_borrowed <- as.numeric(as.character(highest_amount_borrowed))
  pbt <- as.numeric(as.character(pbt))
  
  eod_bank_balance <- max(eod_bank_balance, 0.2*abs(eod_od_bank_balance), na.rm = T)
  
  if(!is.na(data$created[1]) & data$created[1] >= as.Date("2018-02-15") ){
    line_logic <- read.csv("line_logic_2018_02.csv", stringsAsFactors = F)
    if(industry == "mmt" & tolower(file_type)=="thick" & score_band %in% c(1,2)){
      line_logic$MaxTenor <- 36
    }
  }
  
  vertical_line_parameters$monthly_anchor_ts_adjusted <- monthly_anchor_ts
  vertical_line_parameters$eod_bank_balance <- eod_bank_balance
  vertical_line_parameters$highest_amount_borrowed <- highest_amount_borrowed
  vertical_line_parameters$pbt <- pbt
  
  monthly_anchor_ts[is.na(monthly_anchor_ts) | monthly_anchor_ts==0] <- -1
  eod_bank_balance[is.na(eod_bank_balance) | eod_bank_balance==0] <- -1
  highest_amount_borrowed[is.na(highest_amount_borrowed) | highest_amount_borrowed==0] <- -1
  pbt[is.na(pbt) | pbt==0] <- -1
  
  line_logic$Industry <- tolower(as.character(line_logic$Industry))
  
  # replace all non numeric characters
  interest_rate <- as.numeric(as.character(gsub("%", "", interest_rate)))/100
  tenor <- as.numeric(as.character(gsub(" Months", "", tenor)))
  
  line_row <- line_logic[line_logic$Industry==industry &
                           line_logic$FileType==file_type &
                           line_logic$ScoreBand==score_band,]
  if(nrow(line_row)==0 | is.na(score_band)){
    return( list("Vertical amount not defined for anchor", "Not applicable", NA, NA,vertical_line_parameters))
  }
  
  line_logic$ScoreBand <- as.character(line_logic$ScoreBand)
  score_band <- as.character(score_band)
  
  anchor_ts_line <- line_row$AnchorTSFactor[1]*monthly_anchor_ts
  eod_bank_balance_line <- line_row$BankBalanceEODFactor[1]*eod_bank_balance*10
  highest_amount_borrowed_line <- line_row$HighestAmountBorrowedFactor[1]*highest_amount_borrowed
  pbt_line <- line_row$PBTFactor[1]*pbt
  loan_cap <- line_row$LoanCap[1]
  
  vertical_line_parameters$AnchorTSFactor <- line_row$AnchorTSFactor[1]
  vertical_line_parameters$BankBalanceEODFactor <- line_row$BankBalanceEODFactor[1]
  vertical_line_parameters$HighestAmountBorrowedFactor <- line_row$HighestAmountBorrowedFactor[1]
  vertical_line_parameters$PBTFactor <- line_row$PBTFactor[1]
  
  vertical_line_parameters$anchor_ts_line <- anchor_ts_line
  vertical_line_parameters$eod_bank_balance_line <- eod_bank_balance_line
  vertical_line_parameters$highest_amount_borrowed_line <- highest_amount_borrowed_line
  vertical_line_parameters$pbt_line <- pbt_line
  vertical_line_parameters$LoanCap <- line_row$LoanCap[1]
  
  pbt_line[pbt_line<0] <- NA
  
  line_reason <- ""
  line_cal <- min(anchor_ts_line, eod_bank_balance_line, highest_amount_borrowed_line, pbt_line, na.rm = T)
  # if(!is.null(line_cal) & !is.na(line_cal) & length(line_cal) >0 & line_cal >= 1000000 & (is.na(pbt)| pbt==-1)){
  #   line_cal <- NA
  #   line_reason <- "Profit before tax as per Income Tax returns not available"
  # }
  
  if(is.null(line_cal) | length(line_cal) ==0){
    line_cal <- -1
  }
  
  line_cal[is.na(line_cal) | line_cal==Inf |line_cal==-Inf | line_cal<=0] <- -1
  line_overall <- min(line_cal, loan_cap)
  
  if(length(line_overall) ==0 | is.null(line_overall)){
    line_overall <- NA
  }
  if(is.numeric(line_overall) & !is.na(line_overall) & line_overall>0){
    line_overall <- round(line_overall,0)
  }
  
  line_reason_index <- which.min(c(anchor_ts_line, eod_bank_balance_line, highest_amount_borrowed_line,pbt_line, loan_cap))
  
  anchor_ts_line <- round(anchor_ts_line,0)
  eod_bank_balance_line <- round(eod_bank_balance_line,0)
  highest_amount_borrowed_line <- round(highest_amount_borrowed_line,0)
  pbt_line <- round(pbt_line,0)
  
  line_reason_arr <- c("Monthly Anchor TS", "Avg EOD Bank Balance", "Highest Amount Borrowed on Bureau", "Profit Before Tax as per Income Tax Returns", "Loan Line Cap")
  
  if(is.null(line_reason_index) | length(line_reason_index) < 1){
    line_reason_index <- NA
  }
  if(is.null(line_overall)){
    line_overall <- NA
  }
  
  if(!is.na(line_reason_index) & !is.na(line_overall) & line_overall >0){
  line_reason <- ifelse(line_reason_index==1, 
                        "Monthly Anchor TS",
                        ifelse(line_reason_index==2, 
                               "Avg EOD Bank Balance",
                               ifelse(line_reason_index==3,
                                      "Highest Amount Borrowed on Bureau",
                                      ifelse(line_reason_index==4,
                                             "Profit Before Tax as per Income Tax Returns",
                                             paste0("Loan Line Cap; without cap line is Rs. ", ceiling(line_cal/1000)*1000, " using ",line_reason_arr[line_reason_index])))))
  line_reason <- paste("Line calculated using ", line_reason)
  }
  
  
  
  line_overall[!(line_overall==-1 | is.na(line_overall))] <- ceiling(line_overall[!(line_overall==-1 | is.na(line_overall))]/1000)*1000
  
  line_overall[line_overall==-1 | is.na(line_overall)] <- "Not available"
  
  if(line_overall=="Not available"){
  if(!is.na(line_row$AnchorTSFactor[1]) & (monthly_anchor_ts==-1 | is.na(monthly_anchor_ts))){
    line_reason <- paste("Anchor transactions not available", line_reason)
  }
  if(!is.na(line_row$BankBalanceEODFactor[1]) & (eod_bank_balance==-1 | is.na(eod_bank_balance))){
    line_reason <- paste("Bank statement eod balance not available", line_reason)
  }
  if(!is.na(line_row$HighestAmountBorrowedFactor[1]) & (highest_amount_borrowed==-1 | is.na(highest_amount_borrowed))){
    line_reason <- paste("Highest amount borrowed as per CIBIL not available", line_reason)
  }
  }
  line_overall[line_overall!="Not available"] <- paste("Rs.", line_overall[line_overall!="Not available"])
  vertical_line_parameters$line_overall <- line_overall
  result <- list(line_overall,
              line_reason,
              line_row$EligibleForOD[1],
              line_row$MaxTenor[1],
              vertical_line_parameters)

  return(result)
}

get_horizontal_line <- function(data,
                                existing_loan_emis,
                                interest_rate,
                                tenor,
                                loan_cap,
                                lender_name="horizontal"){
  business_nature <- data$business_nature
  margin_industry <- data$margin_industry
  margin_sub_industry <- data$margin_sub_industry
  total_credits <- data$total_credits
  total_months <- data$total_months
  avg_eod_bank_balance <- data$average_eod_balance_6_months
  avg_od_eod_bank_balance <- data$average_od_eod_balance_6_months
  total_od_limit <- data$total_od_limit
  
  business_nature_new_margin <- read.csv("horizontal_margins.csv", stringsAsFactors = F)
  
  business_nature_margin <- read.csv("business_nature_margin.csv", stringsAsFactors = F)
  horizontal_line_parameters <- list()
  
  if(is.null(business_nature)){
    business_nature <- ""
  }
  if(is.null(margin_industry)){
    margin_industry <- ""
  }
  if(is.null(margin_sub_industry)){
    margin_sub_industry <- ""
  }
  
  business_nature[is.na(business_nature) | business_nature==0] <- ""
  margin_industry[is.na(margin_industry)| margin_industry==0] <- ""
  margin_sub_industry[is.na(margin_sub_industry)| margin_sub_industry==0] <- ""
  
  business_nature <- tolower(as.character(business_nature))
  horizontal_line_parameters$business_nature <- business_nature
  
  margin_industry <- tolower(as.character(margin_industry))
  margin_sub_industry <- tolower(as.character(margin_sub_industry))
  
  temp_business_nature_new_margin <- business_nature_new_margin[
    business_nature_new_margin$BusinessNature==business_nature & 
      business_nature_new_margin$Industry==margin_industry & 
      business_nature_new_margin$SubIndustry==margin_sub_industry,]
  
  if(nrow(temp_business_nature_new_margin) ==0){
    temp_business_nature_new_margin <- business_nature_new_margin[
      business_nature_new_margin$BusinessNature==business_nature & 
        business_nature_new_margin$Industry==margin_industry & 
        business_nature_new_margin$SubIndustry=="",]
  }
  
  if(nrow(temp_business_nature_new_margin) ==0){
    temp_business_nature_new_margin <- business_nature_new_margin[
      business_nature_new_margin$BusinessNature==business_nature & 
        business_nature_new_margin$Industry=="" & 
        business_nature_new_margin$SubIndustry=="",]
  }
  
  
  if(is.null(business_nature) | is.na(business_nature)){
    business_nature <- 0
  }
  
  business_nature_margin <- business_nature_margin[business_nature_margin$BusinessNature==business_nature,]
  
  if(nrow(temp_business_nature_new_margin) >=1 & lender_name != "clix"){
    margin <- as.numeric(as.character(temp_business_nature_new_margin$Margin[1]))
  }else if(nrow(business_nature_margin) >=1){
    margin <- as.numeric(as.character(business_nature_margin$NetMargin))
  }else{
    margin <- 0.1
  }
  
  horizontal_line_parameters$margin <- margin
  
  monthly_banking_turnover <- as.numeric(as.character(total_credits))/as.numeric(as.character(total_months))
  horizontal_line_parameters$total_credits <- total_credits
  horizontal_line_parameters$total_months <- total_months
  horizontal_line_parameters$monthly_banking_turnover <- monthly_banking_turnover
  
  net_monthly_earning <- monthly_banking_turnover*margin
  horizontal_line_parameters$net_monthly_earning <- net_monthly_earning
  
  existing_loan_emis <- as.numeric(as.character(existing_loan_emis))
  horizontal_line_parameters$existing_loan_emis <- existing_loan_emis
  
  max_permissible_emi_by_earning <- net_monthly_earning - existing_loan_emis
  max_permissible_emi_by_earning[max_permissible_emi_by_earning<0] <- 0
  horizontal_line_parameters$max_permissible_emi_by_earning <- max_permissible_emi_by_earning
  
  avg_eod_bank_balance <- as.numeric(as.character(avg_eod_bank_balance))
  avg_od_eod_bank_balance <- as.numeric(as.character(avg_od_eod_bank_balance))
  horizontal_line_parameters$avg_eod_bank_balance <- avg_eod_bank_balance
  horizontal_line_parameters$avg_od_eod_bank_balance <- avg_od_eod_bank_balance
  
  avg_eod_bank_balance[avg_eod_bank_balance>0] <- avg_eod_bank_balance*0.8
  #avg_od_eod_bank_balance[avg_od_eod_bank_balance<0] <- -0.2*avg_od_eod_bank_balance
  horizontal_line_parameters$avg_eod_bank_balance_limit <- avg_eod_bank_balance
  horizontal_line_parameters$avg_od_eod_bank_balance_limit <- avg_od_eod_bank_balance
  
  if(is.null(total_od_limit)){
    total_od_limit <- NA
  }
  total_od_limit <- as.numeric(as.character(total_od_limit))
  
  if( !is.na(total_od_limit)  & total_od_limit < 1000 ){
    total_od_limit <- NA
  }
  
  od_emi_limit <- min(-0.2*avg_od_eod_bank_balance, 0.1*total_od_limit, na.rm = T)
  
  if(is.null(od_emi_limit) | is.na(od_emi_limit) | od_emi_limit==Inf | od_emi_limit==-Inf){od_emi_limit <- 0}
  
  # if(!is.na(od_emi_limit) & is.numeric(od_emi_limit) & od_emi_limit >0){
  #   avg_eod_bank_balance <- od_emi_limit
  # }
  avg_eod_bank_balance <- max(avg_eod_bank_balance, od_emi_limit, na.rm = T)
  
  ##monthly interest rate
  interest_rate <- as.numeric(as.character(gsub("%", "", interest_rate)))/100/12
  tenor <- as.numeric(as.character(gsub(" Months", "", tenor)))
  loan_cap <- as.numeric(as.character(loan_cap))
  horizontal_line_parameters$interest_rate <- interest_rate
  horizontal_line_parameters$tenor <- tenor
  horizontal_line_parameters$loan_cap <- loan_cap
  
  max_permissible_emi <- min(avg_eod_bank_balance, max_permissible_emi_by_earning)
  horizontal_line_parameters$max_permissible_emi <- max_permissible_emi
  
  horizontal_line_from_emi <- max_permissible_emi*((1+interest_rate)^(tenor)-1)/(interest_rate*((1+interest_rate)^(tenor)))
  horizontal_line_parameters$horizontal_line_from_emi <- horizontal_line_from_emi
  
  horizontal_line <- min(loan_cap, horizontal_line_from_emi)
  horizontal_line_parameters$horizontal_line <- horizontal_line
  
  horizontal_line_reason <- ""
  
  if(length(horizontal_line) >0 & !is.na(horizontal_line) & horizontal_line==loan_cap){
    horizontal_line_from_emi <- ceiling(horizontal_line_from_emi/1000)*1000
    horizontal_line_parameters$horizontal_line_from_emi <- ceiling(horizontal_line_from_emi/1000)*1000
    horizontal_line_parameters$horizontal_line <- ceiling(horizontal_line/1000)*1000
    horizontal_line_reason <- paste("Line capped at max line set for risk band; Without cap line is Rs.", horizontal_line_from_emi)
  } else if(length(horizontal_line) >0 & (is.na(total_credits) | total_credits==0 | 
            is.na(total_months) | total_months==0|
            is.na(avg_eod_bank_balance) | avg_eod_bank_balance==0)){
    horizontal_line_reason <- "Bank statement not available"
    horizontal_line <- ceiling(horizontal_line/1000)*1000
  } else if(length(horizontal_line) >0 & !is.na(horizontal_line) & horizontal_line > 0){
    horizontal_line <- ceiling(horizontal_line/1000)*1000
    horizontal_line_parameters$horizontal_line_from_emi <- ceiling(horizontal_line_from_emi/1000)*1000
    horizontal_line_parameters$horizontal_line <- ceiling(horizontal_line/1000)*1000
    horizontal_line_reason <- paste("Max permissible EMI as per banking turnover is", round(max_permissible_emi))
  } else if(!is.na(max_permissible_emi) & max_permissible_emi == 0) {
    horizontal_line <- "NA - Existing EMI > Banking income"
    horizontal_line_reason <- "Existing loan EMIs are greater than monthly banking turnover"
  }
  else {
    horizontal_line <- ceiling(horizontal_line/1000)*1000
    horizontal_line_reason <- "Not available"
  }
  
  
  horizontal_line[is.na(horizontal_line) | horizontal_line <=0] <- "Not available"
  horizontal_line[is.numeric(horizontal_line)] <- paste("Rs.", horizontal_line[is.numeric(horizontal_line)])
  
  return(list(horizontal_line, horizontal_line_reason, horizontal_line_parameters))
}

get_fast_loan_line <- function(data,
                               interest_rate,
                               tenure){
  
  highest_amount_borrowed <- data$highest_amount_borrowed
  highest_amount_borrowed_original <- data$highest_amount_borrowed
  account_type <- data$highest_amount_borrowed_account_type
  account_opened <- data$highest_amount_borrowed_opened
  file_type <- data$Bureau_thickness_gen2
  mean_band <- data$mean_band
  cibil_pull_date <- data$date_pull
  applied_amount <- data$loan_amount
  account_ownership <- data$highest_amount_borrowed_account_ownership
  dpd_last2 <- data$dpd_last6
  has_pan_issue <- data$has_pan_issue
  
  inflation_rate <- 0.08
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
  } else if (grepl("card", account_type, ignore.case = T)){
    final_account_type <- "Credit Card"
  }
  
  fast_loan_line_parameters <- read.csv("fast_loan_line_parameters.csv", stringsAsFactors = F)
  mean_band <- as.numeric(mean_band)
  file_type <- as.character(file_type)
  
  fast_loan_line_parameters <- fast_loan_line_parameters[
    fast_loan_line_parameters$BureauThickness==file_type &
      fast_loan_line_parameters$MeanBand == mean_band &
      fast_loan_line_parameters$AccountType == final_account_type,]
  
  if(nrow(fast_loan_line_parameters) == 0){
    return(list(NA, account_type, final_account_type, account_opened, years_from_today, NA, NA, highest_amount_borrowed,F, NA, NA))
  }
  
  if(is.null(dpd_last2)){
    dpd_last2 <- 0
  }
  
  dpd_last2 <- as.numeric(as.character(dpd_last2))
  dpd_last2[is.na(dpd_last2)] <- 0
  
  if(is.null(has_pan_issue)){
    has_pan_issue <- 0
  }
  has_pan_issue <- as.numeric(as.character(has_pan_issue))
  has_pan_issue[is.na(has_pan_issue)] <- 0
  
  if(dpd_last2 > 1 | has_pan_issue >0){
    return(list(NA, account_type, final_account_type, account_opened, years_from_today, NA, NA, highest_amount_borrowed,F, NA, NA))
  }
  
  fast_loan_line_parameters <- fast_loan_line_parameters[1,]
  
  multiplying_factor <- fast_loan_line_parameters$Factor
  
  if(is.null(account_ownership)){
    account_ownership <- ""
  }
  account_ownership[is.na(account_ownership)] <- ""
  account_ownership <- tolower(as.character(account_ownership))
  
  if( grepl("gurantor",account_ownership) | grepl("securities", account_ownership) | grepl("shares", account_ownership) | grepl("gold", account_type) | grepl("overdraft", account_type)){
    multiplying_factor <- 0
  }
  
  loan_cap <- fast_loan_line_parameters$LoanCap
  
  fast_loan_line <- highest_amount_borrowed*multiplying_factor
  
  fast_loan_line <- min(fast_loan_line, loan_cap, highest_amount_borrowed_original)
  
  fast_loan_line <- ceiling(fast_loan_line/10000)*10000
  
  applied_amount <- as.numeric(as.character(applied_amount))
  applied_amount[is.na(applied_amount)] <- 10000000
  
  is_amount_acceptable <- F
  if(applied_amount <= 300000){
    if(fast_loan_line >= max(0.6*applied_amount,30000,na.rm = T) ){
      is_amount_acceptable <- T
    }
  } else{
    if(fast_loan_line >= max(0.35*applied_amount,200000,na.rm = T)){
      is_amount_acceptable <- T
    }
  }
  fast_loan_line <- max(fast_loan_line, 25000)
  fast_loan_actual_line <- min(applied_amount, fast_loan_line, na.rm = T)
  fast_loan_actual_line[fast_loan_actual_line==Inf | fast_loan_actual_line==-Inf] <- 0
  
  monthly_interest_rate <- interest_rate/12/100
  
  fast_loan_emi <- fast_loan_actual_line*(monthly_interest_rate)*((1+monthly_interest_rate)^tenure)/(((1+monthly_interest_rate)^tenure)-1)
  
  return(list(fast_loan_actual_line, account_type, final_account_type, account_opened, years_from_today, multiplying_factor, loan_cap, highest_amount_borrowed, is_amount_acceptable, fast_loan_line, fast_loan_emi))
}

get_cibil_anchor_line <- function(data,
                                  industry,
                                  anchor_ts,
                                  interest_rate,
                                  cibil_line,
                                  model_decision){
  #### Get necessary parameters
  file_type <- tolower(as.character(data$Bureau_thickness_gen2))
  score_band <- as.numeric(as.character(data$mean_band))
  tier <- tolower(as.character(data$Tier))
  borrower_address_owned <- as.numeric(as.character(data$owned_borrower))
  highest_amount_borrowed <- as.numeric(as.character(data$highest_amount_borrowed))
  anchor_vintage <- as.numeric(as.character(data$anchor_vintage))
  anchor_name <- tolower(as.character(data$anchor_name))
  applied_amount <- as.numeric(as.character(data$loan_amount))
  industry <- tolower(as.character(industry))
  anchor_ts <- as.numeric(as.character(anchor_ts))
  anchor_ts_unadjusted <- as.numeric(data$average_anchor_monthly_txns)
  fast_loan_anchor_data <- as.numeric(data$fast_loan_anchor_data)
  interest_rate <- as.numeric(as.character(gsub("%", "", interest_rate)))
  borrower_address_owned[borrower_address_owned > 1] <- 1
  vertical_band <- as.numeric(data$vertical_risk_profile_band)
  
  if(is.null(anchor_vintage)){anchor_vintage <- 0}
  anchor_vintage[is.na(anchor_vintage)] <- 0
  
  if(is.null(anchor_ts_unadjusted)){anchor_ts_unadjusted <- 0}
  anchor_ts_unadjusted[is.na(anchor_ts_unadjusted)] <- 0
  
  if(is.null(fast_loan_anchor_data)){fast_loan_anchor_data <- 0}
  fast_loan_anchor_data[is.na(fast_loan_anchor_data)] <- 0
  
  if(is.null(anchor_ts)){anchor_ts <- 0}
  anchor_ts[is.na(anchor_ts)] <- 0
  
  return_list <- list(cibil_anchor_line = NA, cibil_anchor_line_tenure =NA, interest_rate=NA, cibil_anchor_line_without_cap=NA, split_payment_percent=NA, cibil_anchor_line_emi=NA,cibil_anchor_line_decision=NA, cibil_anchor_line_anc_txns=NA)
  
  
  if(grepl("paytm", anchor_name)){
    merchant_id <- as.character(data$merchant_id)
    merchant_id[is.na(merchant_id)]  <- "NA"
    paytmPreApprovedOffers <- read.csv("PaytmPreApprovedOffers.csv", stringsAsFactors = F)
    paytmPreApprovedOffersRow <- paytmPreApprovedOffers[as.character(paytmPreApprovedOffers$ID)==merchant_id,]
    if(nrow(paytmPreApprovedOffersRow)==1){
    if((file_type==tolower(THICKNESS_THICK) & score_band <= 5) | (file_type==tolower(THICKNESS_THIN) & score_band <= 3)){
        preapproved_amount <- as.numeric(paytmPreApprovedOffersRow$Line)
        loan_tenure <- 6
        loan_amount <- min(preapproved_amount, applied_amount)
        return_list$cibil_anchor_line <- loan_amount
        return_list$cibil_anchor_line_tenure <- loan_tenure
        return_list$interest_rate <- interest_rate
        return_list$cibil_anchor_line_emi <- get_emi(loan_amount, interest_rate, loan_tenure)
        return_list$cibil_anchor_line_decision <- MODEL_DECISION_APPROVE
    }else{
      return_list$cibil_anchor_line_decision <- MODEL_DECISION_DECLINE
    }
    } else{
      return_list$cibil_anchor_line_decision <- MODEL_DECISION_MANUAL_REVIEW
    }
    return(return_list)
  }
  
  cibil_anchor_line_mapping <- read.csv("cibil_anchor_line_mapping.csv", stringsAsFactors = F)
  vertical_line_assignment <- read.csv("VerticalLineAssignment.csv", stringsAsFactors = F)
  if(!is.na(data$created[1]) & data$created[1] >= as.Date("2018-02-15") ){
    vertical_line_assignment <- read.csv("VerticalLineAssignment_2018_02.csv", stringsAsFactors = F)
  }
  vertical_line_assignment_row <- vertical_line_assignment[vertical_line_assignment$Anchor==anchor_name &
                                                             vertical_line_assignment$FileType==file_type &
                                                             vertical_line_assignment$Tier==tier &
                                                             vertical_line_assignment$ScoreBand==score_band &
                                                             vertical_line_assignment$VerticalBand==vertical_band, ]
  
  if(nrow(vertical_line_assignment_row)==1){
    if(anchor_ts_unadjusted == 0){
      anchor_ts_unadjusted <- fast_loan_anchor_data
      anchor_vintage <- 7
    }
    return_list$cibil_anchor_line_decision <- MODEL_DECISION_MANUAL_REVIEW
    if(tolower(vertical_line_assignment_row$ModelDecision) == tolower(MODEL_DECISION_APPROVE) & 
       anchor_ts_unadjusted >= as.integer(vertical_line_assignment_row$MinAnchorSales) &
       anchor_vintage >= as.integer(vertical_line_assignment_row$MinAnchorVintage)
       ){
    vertical_line_without_cap <- vertical_line_assignment_row$AnchorSalesMultiplier*anchor_ts_unadjusted
    vertical_line <- ceiling(min(vertical_line_without_cap,vertical_line_assignment_row$LoanCap)/1000)*1000
    return_list$cibil_anchor_line <- vertical_line
    return_list$cibil_anchor_line_tenure <- vertical_line_assignment_row$LoanTenure
    return_list$interest_rate <- interest_rate
    return_list$cibil_anchor_line_without_cap <- vertical_line_without_cap
    return_list$cibil_anchor_line_emi <- get_emi(vertical_line, interest_rate, vertical_line_assignment_row$LoanTenure)
    return_list$cibil_anchor_line_decision <- vertical_line_assignment_row$ModelDecision
    }
    if(anchor_ts_unadjusted==0){cibil_anchor_line_anc_txns <- "Not available"
    } else {cibil_anchor_line_anc_txns <- paste0(round(anchor_ts_unadjusted/1000), " K")}
    return_list$cibil_anchor_line_anc_txns <- cibil_anchor_line_anc_txns
    return(return_list)
  }
  
  
  line_row <- cibil_anchor_line_mapping[cibil_anchor_line_mapping$Industry==industry &
                                          cibil_anchor_line_mapping$FileType==file_type &
                                          cibil_anchor_line_mapping$ScoreBand==score_band &
                                          cibil_anchor_line_mapping$IsBorrowerAddressOwned
==borrower_address_owned,]
  
  
  if(nrow(line_row)==0 | is.na(score_band)){
    return(return_list)
  }
  
  minimum_anchor_vintage <- line_row$MinimumAnchorVintage[1]
  if(!is.na(minimum_anchor_vintage)){
    if(is.na(anchor_vintage) | anchor_vintage < minimum_anchor_vintage){
      return(return_list)
    }
  }
  
  hab_line <- highest_amount_borrowed*line_row$HighestAmountBorrowedFactor[1]
  cibil_anchor_line_amount <- anchor_ts*line_row$MultiplyingFactor[1]
  cibil_anchor_line_tenure <- line_row$Tenure[1]
  split_payment_percent <- line_row$SplitPaymentPercent[1]
  
  cibil_anchor_line_without_cap <- min(cibil_anchor_line_amount, hab_line)
  cibil_anchor_line <- min(line_row$LoanCap[1], cibil_anchor_line_without_cap)
  
  cibil_anchor_line <- ceiling(cibil_anchor_line/50000)*50000
  cibil_anchor_line_without_cap <- ceiling(cibil_anchor_line_without_cap/50000)*50000
  
  cibil_anchor_line <- max(cibil_anchor_line, cibil_line, na.rm = T)
  cibil_anchor_line_without_cap <- max(cibil_anchor_line_without_cap, cibil_line, na.rm = T)
  cibil_anchor_line_without_cap <- min(cibil_anchor_line_without_cap, 5000000)
  
  interest_rate <- max(interest_rate - line_row$PricingReduction[1], 16)
  cibil_anchor_line_emi <- get_emi(cibil_anchor_line, interest_rate, cibil_anchor_line_tenure)
  
  cibil_anchor_line[cibil_anchor_line==Inf | cibil_anchor_line==-Inf] <- NA
  
  return_list$cibil_anchor_line <- cibil_anchor_line
  return_list$cibil_anchor_line_tenure <- cibil_anchor_line_tenure
  return_list$interest_rate <- interest_rate
  return_list$cibil_anchor_line_without_cap <- cibil_anchor_line_without_cap
  return_list$split_payment_percent <- split_payment_percent
  return_list$cibil_anchor_line_emi <- cibil_anchor_line_emi

  return(return_list)
  
}

get_emi <- function(line, interest_rate, tenure){
  monthly_interest_rate <- interest_rate/12/100
  emi <- line*(monthly_interest_rate)*((1+monthly_interest_rate)^tenure)/(((1+monthly_interest_rate)^tenure)-1)
  return(emi)
}

get_instant_credit_line_result <- function(data){
  
  file_type <- tolower(as.character(data$Bureau_thickness_gen2))
  score_band <- as.numeric(as.character(data$mean_band))
  tier <- tolower(as.character(data$Tier))
  anchor_name <- tolower(as.character(data$anchor_name))
  anchor_agent_type <- as.character(data$anchor_agent_type)
  if(is.null(anchor_agent_type)){anchor_agent_type <- "cash"}
  if(is.na(anchor_agent_type)){anchor_agent_type <- "cash"}
  anchor_agent_type <- tolower(anchor_agent_type)
  applied_amount <- as.numeric(as.character(data$loan_amount))
  line <- as.numeric(gsub( "[^0-9.-]","",as.character(data$instant_credit_approved_amount)))
  if(length(line) == 0){line <- 0}
  if(is.na(line)){line <- 0}
  
  return_list <- list(model_decision = "Decline", fast_loan_line = line, pricing = 0.99, MaxTenor= 7)
  
  line <- min(line, applied_amount)
  instant_credit_logic <- read.csv("InstantCreditLogic.csv", stringsAsFactors = F)
  
  instant_credit_logic_row <- instant_credit_logic[instant_credit_logic$Anchor == anchor_name &
                                                     instant_credit_logic$FileType == file_type &
                                                     instant_credit_logic$Tier == tier &
                                                     instant_credit_logic$ScoreBand == score_band &
                                                     instant_credit_logic$AgentType == anchor_agent_type, ]
  if(nrow(instant_credit_logic_row) == 1){
    return_list$model_decision <- instant_credit_logic_row$ModelDecision
  }
  
  if(return_list$model_decision==MODEL_DECISION_APPROVE){
    
    dpd_last12_exceptcard <- data$dpd_last12_exceptcard
    if(is.null(dpd_last12_exceptcard)){dpd_last12_exceptcard <- 0}
    if(is.na(dpd_last12_exceptcard)){dpd_last12_exceptcard <- 0}
    dpd_last12_exceptcard <- as.numeric(dpd_last12_exceptcard)
    
    cibil_score <- data$cibil_score
    if(is.null(cibil_score)){cibil_score <- 0}
    if(is.na(cibil_score)){cibil_score <- 0}
    cibil_score <- as.numeric(cibil_score)
    
    number_of_writeoff <- data$number_of_writeoff
    if(is.null(number_of_writeoff)){number_of_writeoff <- 0}
    if(is.na(number_of_writeoff)){number_of_writeoff <- 0}
    number_of_writeoff <- as.numeric(number_of_writeoff)
    
    total_overdue <- data$total_overdue
    if(is.null(total_overdue)){total_overdue <- 0}
    if(is.na(total_overdue)){total_overdue <- 0}
    total_overdue <- as.numeric(total_overdue)
    
    if(dpd_last12_exceptcard > 1 | number_of_writeoff > 0.8 | total_overdue > 5000 | cibil_score < 630){
      return_list$model_decision <-  MODEL_DECISION_DECLINE
    }
    
  }
  
  ## Experimental decision on 100 cases
  if(return_list$model_decision == MODEL_DECISION_DECLINE){
    if(line >= 1000 & line <= 50000){
      if(((file_type=="thick" | file_type=="thin") & (score_band < 8)) |(file_type=="no hit")){
        return_list$model_decision <- MODEL_DECISION_APPROVE
      }
    }
  }
  
  return_list$fast_loan_line <- line
  return(return_list)
}