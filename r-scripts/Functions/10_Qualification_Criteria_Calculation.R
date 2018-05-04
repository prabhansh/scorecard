vertical_qualifying_criteria <- read.csv("vertical_qualification_criteria.csv", stringsAsFactors = F)

get_qualification_criteria <- function(data,
                                       industry,
                                       approvedAmount,
                                       criteria_type="horizontal"){
  cibil_score <- data$cibil_score
  business_vintage <- data$business_vintage_in_years
  anchor_vintage <- data$anchor_vintage
  banking_turnover <- data$total_credits
  total_months <- data$total_months
  account_opened_180 <- data$account_opened_180
  inward_cheque_bounces <- data$inward_cheque_bounces
  outward_cheque_bounces <- data$outward_cheque_bounces
  total_credit_txns <- data$total_credit_txns
  tier <- data$Tier
  age <- data$Age
  bureau_thickness <- data$Bureau_thickness_gen2
  risk_band <- data$mean_band
  experian_band <- data$ExperianScoreBand
  gen2_band <- data$Gen2_RiskBand
  propertyOwned <- data$owned_all
  has_female_promoter <- data$has_female_promoter
  business_type <- tolower(as.character(data$business_type))
  anchor_name <- tolower(as.character(data$anchor_name))
  leverage_ratio <- as.numeric(data$leverage_ratio)
  delinquency_index_lifetime <- as.numeric(data$delinquency_index_lifetime)
  
  result <- c()
  result_mention <- c()
  industry <- as.character(industry)
  
  if(industry=="e-commerce" | industry=="ecommerce"){
    result_mention <- c("Eligibility to be net of other loans for e-commerce", result_mention)
  }
  
  if(!grepl("sole", business_type) & grepl('online|paisa', anchor_name)){
    result <- c("Company is not Sole Proprietorship", result)
  }
  
  qualification_row <- vertical_qualifying_criteria[vertical_qualifying_criteria$Industry==industry,]
  
  qualification_business_vintage <- 1
  qualification_anchor_vintage <- 0
  vintage_reason <- ""
  
  if(nrow(qualification_row) > 0){
    qualification_business_vintage <- qualification_row$BusinessVintage[1]
    qualification_anchor_vintage <- qualification_row$AnchorVintage[1]
    vintage_reason <- paste("as required by", qualification_row$Industry[1])
  }
  
  if(criteria_type=="clix"){qualification_business_vintage <- 2}
  if(criteria_type=="fast_loan"){qualification_business_vintage <- 1}
  
  if(is.null(anchor_vintage) | length(anchor_vintage)==0){anchor_vintage <- 0}
  if(is.null(business_vintage) | length(business_vintage)==0){business_vintage <- 0}
  business_vintage <- as.numeric(as.character(business_vintage))
  anchor_vintage <- as.numeric(as.character(anchor_vintage))/12
  
  if(!is.na(business_vintage) & business_vintage != 0 & business_vintage < qualification_business_vintage){
    result <- c(paste("Business vintage is less than",qualification_business_vintage  ,"years", vintage_reason), result)
  } else if (is.na(business_vintage) | business_vintage == 0){
    result <- c("Business vintage is not available", result)
  }
  
  if(!is.na(anchor_vintage) & anchor_vintage != 0 & anchor_vintage < qualification_anchor_vintage & criteria_type=="horizontal"){
    result <- c(paste("Anchor vintage is less than",qualification_anchor_vintage  ,"years", vintage_reason), result)
  }
  
  if(is.null(approvedAmount) | length(approvedAmount)==0){approvedAmount <- 0}
  
  approvedAmount <- as.numeric(as.character(approvedAmount))
  approvedAmount[is.na(approvedAmount)] <- 0
  
  if(approvedAmount>=1000000){
    result_mention <- c("Property Ownership mandatory for loans > 10 Lacs", result_mention)
  }
  
  cibil_score <- as.numeric(as.character(cibil_score))
  cibil_score[is.na(cibil_score)] <- 0
  
  qualification_cibil_score <- 600
  if(criteria_type=="clix"){qualification_cibil_score <- 650}
  
  if(cibil_score < qualification_cibil_score){
    result <- c(paste0("CIBIL Score is less than ", qualification_cibil_score), result)
  }
  
  if(is.null(banking_turnover) | length(banking_turnover)==0){banking_turnover <- 0}
  if(is.null(total_months) | length(total_months)==0){total_months <- 0}
  
  banking_turnover <- as.numeric(as.character(banking_turnover))
  total_months <- as.numeric(as.character(total_months))
  total_months[is.na(total_months)] <- 12
  banking_turnover <- banking_turnover/total_months*12
  
  qualification_banking_turnover <- 2000000
  if(criteria_type=="clix"){qualification_banking_turnover <- 2400000}
  if(criteria_type=="banking"){qualification_banking_turnover <- 1500000}
  
  qualification_banking_turnover_text <- paste0(qualification_banking_turnover/100000," L")
  
  if(!is.na(banking_turnover) & banking_turnover!=0 & banking_turnover < qualification_banking_turnover & criteria_type!="fast_loan"){
    result <- c(paste0("Banking turnover < ",qualification_banking_turnover_text," in last year"), result)
  }
  else if (is.na(banking_turnover) | banking_turnover == 0 & criteria_type!="fast_loan"){
    result_mention <- c("Banking turnover is not available",result_mention)
  }
  
  account_opened_180 <- as.numeric(as.character(account_opened_180))
  if(!is.na(account_opened_180) & account_opened_180!=0 & account_opened_180 > 3){
    result <- c("More than 3 accounts opened in last 6 months", result)
  }
  
  if(is.null(inward_cheque_bounces)){inward_cheque_bounces <- 0}
  if(is.null(outward_cheque_bounces)){outward_cheque_bounces <- 0}
  inward_cheque_bounces <- as.numeric(as.character(inward_cheque_bounces))
  outward_cheque_bounces <- as.numeric(as.character(outward_cheque_bounces))
  if(is.null(total_credit_txns)){total_credit_txns <- 1}
  total_credit_txns <- as.numeric(as.character(total_credit_txns))
  cheque_bounce_percent <- inward_cheque_bounces/total_credit_txns
  outward_cheque_bounce_percent <- outward_cheque_bounces/total_credit_txns
  inward_cheque_bounce_criteria <- 5
  outward_cheque_bounce_criteria <- 5
  if(criteria_type=="clix"){inward_cheque_bounce_criteria <- 3}
  
  if(!is.na(cheque_bounce_percent) & cheque_bounce_percent!=0 & cheque_bounce_percent > inward_cheque_bounce_criteria/100 & inward_cheque_bounces>3 & criteria_type!="fast_loan"){
    result <- c(paste0("Inward cheque return percent is greater than ",inward_cheque_bounce_criteria,"%"), result)
  }
  if(!is.na(outward_cheque_bounce_percent) & outward_cheque_bounce_percent!=0 & outward_cheque_bounce_percent > outward_cheque_bounce_criteria/100 & outward_cheque_bounces>3 & criteria_type=="clix" & criteria_type!="fast_loan"){
    result <- c(paste0("Outward cheque return percent is greater than ",outward_cheque_bounce_percent,"%"), result)
  }
 
  
  age <- as.numeric(as.character(age))
  if(!is.na(age) & age!=0 & age <= 23){
    result <- c("Promoter age is less than 23 years", result)
  } else if(!is.na(age) & age!=0 & age >= 60){
    result <- c("Promoter age is more than 60 years", result)
  }
  
  risk_band <- as.numeric(as.character(risk_band))
  if(bureau_thickness=="No Hit"){
    result_mention <- c("One co-applicant mandatory for no hit segment", result_mention)
  } else if(!is.na(risk_band) & risk_band>6){
    result_mention <- c("One co-applicant mandatory as risk band > 6", result_mention)
  }
  
  experian_band <- as.numeric(as.character(experian_band))
  gen2_band <- as.numeric(as.character(gen2_band))
  
  if(!is.na(gen2_band - experian_band) & abs(gen2_band - experian_band) > 4){
    result <- c(" ", result)
  }
  
  if((!is.na(has_female_promoter[1]) & has_female_promoter[1] == 1) & (is.na(bureau_thickness) | bureau_thickness !="Thick") ){
    result <- c("Female Promoter in Application (Co-Applicant Recommended)", result)
  }
  
  if(is.null(propertyOwned) | length(propertyOwned)==0){propertyOwned <- 0}
  if(is.na(propertyOwned)){propertyOwned <- 0}
  
  if(propertyOwned <1 & criteria_type=="clix"){
    result <- c("Neither business nor home property owned by application", result)
  }
  if(criteria_type=="clix"){
    result_mention <- c("Hunter check should be yes", "Standard criteria on commercial CIBIL", result_mention)
  }
  
  if(is.null(leverage_ratio) | length(leverage_ratio) == 0){leverage_ratio <- 0}
  if(is.na(leverage_ratio)){leverage_ratio <- 0}
  
  if(leverage_ratio >=0.9){
    result <- c(" ", result)
  }
  
  if(is.null(delinquency_index_lifetime) | length(delinquency_index_lifetime) == 0){delinquency_index_lifetime <- 0}
  if(is.na(delinquency_index_lifetime)){delinquency_index_lifetime <- 0}
  
  if(delinquency_index_lifetime >=3){
    result <- c(" ", result)
  }
  
  return(list(v1=result, v2=result_mention))
}