bs_tree_based <- read.csv("BS_TreeBasedRules.csv", stringsAsFactors = F)
bs_model_config <- read.csv("BS_Model_Config.csv", stringsAsFactors = F)
bs_riskbands <- read.csv("BS_RiskBands.csv", stringsAsFactors = F)
bs_decision_config <- read.csv("BS_Decisions.csv", stringsAsFactors = F)

bs_model <- function(is_od , avg_eod , eod_l3 , eod_l3_w , eod_l3_w_non_od_N , avg_credits , credits_l3 , credits_macro_growth , credits_micro_growth , bureau_thickness , stability_eod , stability_credits , applied_amt , penal_transactions , HS_decision , HS_probability  , delinquency_index_lifetime , leverage_ratio , business_vintage , inward_cheque_bounces , owned_all , Age , cibil_score ){
  #print("Running BS Function")
  
  ## Load banking model config
  config <- bs_model_config
  a <- data.frame()
  is_od <- as.numeric(is_od)
  if(length(is_od) ==0 | is.null(is_od) ){ is_od <- 0 }
  if(is.na(is_od)){ is_od <- 0 }
  a[1 , 'is_od'] <- is_od
  
  ## Cleaning parameters
  avg_eod <- as.numeric(avg_eod)
  if(length(avg_eod) ==0 | is.null(avg_eod) ){ avg_eod <- 0 }
  if(is.na(avg_eod)){ avg_eod <- 0 }
  a[1 , 'avg_eod'] <- avg_eod
  
  eod_l3 <- as.numeric(eod_l3)
  if(length(eod_l3) ==0 | is.null(eod_l3) ){ eod_l3 <- 0 }
  if(is.na(eod_l3)){ eod_l3 <- 0 }
  a[1 , 'eod_l3'] <- eod_l3
  
  eod_l3_w <- as.numeric(eod_l3_w)
  if(length(eod_l3_w) ==0 | is.null(eod_l3_w) ){ eod_l3_w <- 0 }
  if(is.na(eod_l3_w)){ eod_l3_w <- 0 }
  a[1 , 'eod_l3_w'] <- eod_l3_w
  
  eod_l3_w_non_od_N <- as.numeric(eod_l3_w_non_od_N)
  if(length(eod_l3_w_non_od_N) ==0 | is.null(eod_l3_w_non_od_N) ){ eod_l3_w_non_od_N <- 0 }
  if(is.na(eod_l3_w_non_od_N)){ eod_l3_w_non_od_N <- 0 }
  a[1 , 'eod_l3_w_non_od_N'] <- eod_l3_w_non_od_N
  
  avg_credits <- as.numeric(avg_credits)
  if(length(avg_credits) ==0 | is.null(avg_credits) ){ avg_credits <- 0 }
  if(is.na(avg_credits)){ avg_credits <- 0 }
  a[1 , 'avg_credits'] <- avg_credits
  
  credits_l3 <- as.numeric(credits_l3)
  if(length(credits_l3) ==0 | is.null(credits_l3) ){ credits_l3 <- 0 }
  if(is.na(is_od)){ is_od <- 0 }
  a[1 , 'credits_l3'] <- credits_l3
  
  credits_macro_growth <- as.numeric(credits_macro_growth)
  if(length(credits_macro_growth) ==0 | is.null(credits_macro_growth) ){ credits_macro_growth <- 0 }
  if(is.na(credits_macro_growth)){ credits_macro_growth <- 0 }
  a[1 , 'credits_macro_growth'] <- credits_macro_growth
  
  credits_micro_growth <- as.numeric(credits_micro_growth)
  if(length(credits_micro_growth) ==0 | is.null(credits_micro_growth) ){ credits_micro_growth <- 0 }
  if(is.na(credits_micro_growth)){ credits_micro_growth <- 0 }
  a[1 , 'credits_micro_growth'] <- credits_micro_growth
  
  stability_eod <- as.numeric(stability_eod)
  if(length(stability_eod) ==0 | is.null(stability_eod) ){ stability_eod <- 0 }
  if(is.na(stability_eod)){ stability_eod <- 0 }
  a[1 , 'stability_eod'] <- stability_eod
  
  stability_credits <- as.numeric(stability_credits)
  if(length(stability_credits) ==0 | is.null(stability_credits) ){ stability_credits <- 0 }
  if(is.na(stability_credits)){ stability_credits <- 0 }
  a[1 , 'stability_credits'] <- stability_credits
  
  penal_transactions <- as.numeric(penal_transactions)
  if(length(penal_transactions) ==0 | is.null(penal_transactions) ){ penal_transactions <- 0 }
  if(is.na(penal_transactions)){ penal_transactions <- 0 }
  a[1 , 'penal_transactions'] <- penal_transactions
  
  HS_probability <- as.numeric(HS_probability)
  if(length(HS_probability) ==0 | is.null(HS_probability) ){ HS_probability <- 0.11 }
  if(is.na(HS_probability)){ HS_probability <- 0 }
  a[1 , 'HS_probability'] <- HS_probability
  a[1 , 'HS_Decision'] <- HS_decision
  a[1 , 'bureau_thickness'] <- bureau_thickness
  
  applied_amt <- as.numeric(applied_amt)
  if(length(applied_amt) ==0 | is.null(applied_amt)){ applied_amt <- 0 }
  if(is.na(applied_amt)){ applied_amt <- 0 }
  a[1 , 'applied_amt'] <- applied_amt
  
  delinquency_index_lifetime <- as.numeric(delinquency_index_lifetime)
  if(length(delinquency_index_lifetime) ==0 | is.null(delinquency_index_lifetime) ){ delinquency_index_lifetime <- 0 }
  if(is.na(delinquency_index_lifetime)){ delinquency_index_lifetime <- 0 }
  
  leverage_ratio <- as.numeric(leverage_ratio)
  if(length(leverage_ratio) ==0 | is.null(leverage_ratio) ){ leverage_ratio <- 0 }
  if(is.na(leverage_ratio)){ leverage_ratio <- 0 }
  
  business_vintage <- as.numeric(business_vintage)
  if(length(business_vintage) ==0 | is.null(business_vintage) ){ business_vintage <- 0 }
  if(is.na(business_vintage)){ business_vintage <- 0 }
  
  owned_all <- as.numeric(owned_all)
  if(length(owned_all) ==0 | is.null(owned_all)){ owned_all <- 0 }
  if(is.na(owned_all)){ owned_all <- 0 }
  a[1 , 'owned_all'] <- owned_all
  
  
  inward_cheque_bounces <- as.numeric(inward_cheque_bounces)
  if(length(inward_cheque_bounces) ==0 | is.null(inward_cheque_bounces)){ inward_cheque_bounces <- 0 }
  if(is.na(inward_cheque_bounces)){ inward_cheque_bounces <- 0 }
  a[1 , 'inward_cheque_bounces'] <- inward_cheque_bounces
  
  Age <- as.numeric(Age)
  if(length(Age) ==0 | is.null(Age)){ Age <- 0 }
  if(is.na(Age)){ Age <- 0 }
  a[1 , 'Age'] <- Age
  
  
  cibil_score <- as.numeric(cibil_score)
  if(length(cibil_score) ==0 | is.null(cibil_score)){ cibil_score <- 0 }
  if(is.na(cibil_score)){ cibil_score <- 0 }
  a[1 , 'cibil_score'] <- cibil_score
  
  
  
  if( is.na(eod_l3) | is.na(credits_l3) | eod_l3 == 0 | credits_l3 == 0 ){
    bs_decision <- NA
    
    ############ If Horizontal Decision is Decline then the Final Decision is also Decline ###
    if(!is.na(HS_decision) & HS_decision == "Decline"){
      bs_decision <- "Decline"     
    }
    
    
    ### BS Not Available for last 3 Months ####
    a$bs_model_score <- "NA"
    a$bs_rb <- "NA"
    a$bs_decision <- bs_decision
    a$reason <- NA
    return( as.list(a) )
    
    #return(c(HS_decision , HS_probability))
  }
  
  ######## Credits Final Growth ######
  final_growth <- credits_macro_growth
  ######## For No Hit - Macro Growth ####
  ######## For OD - Macro Growth only if Avg Credits > 50k else limit to 0 ############
  ###### For Non OD - Min of Macro & Micro Growth only if Avg Credits > 50k else limit to 0 ###
  if(bureau_thickness!="No Hit"){
  if(is_od == 1){
    if( avg_credits < 50000 ){
      final_growth <- min(0 , credits_macro_growth )
    }
  }else{
    if( avg_credits < 50000 ){
      final_growth <- min(0 , credits_macro_growth , credits_micro_growth )
    }else{
      final_growth <- min( 3 , credits_macro_growth , credits_micro_growth )
    }
  }
  }
  
  a[1 , 'final_growth'] <- final_growth 
  ####### Growth Rate Less than -10% implies Declining Income ##########
  is_declining_income <- 0
  if(final_growth < -0.1){
    is_declining_income <- 1
   }
  a[1 , 'is_declining_income'] <- is_declining_income 
  
  ####### EOD Balance less than 15000 implies Low EOD ##########
  is_eod_low <- 0 
  if(min(avg_eod , eod_l3 , eod_l3_w)<= 15000){
    if(is_od==0){
    is_eod_low <- 1
    }
  }
  a[1 , 'is_eod_low'] <- is_eod_low 
  ###### Stability is average of EOD Stability & Credit Stability #######
  ###### If EOD Stability is negative then make it positive ####
  if(stability_eod<0){
    stability_eod <- -1*stability_eod
  }
  stability_eod <- min(3, stability_eod)
  stability_credits <- min(3, stability_credits)
  stability <- sum(stability_eod , stability_credits )*0.5
  ######## stability > 0.4 is low stability ########
  is_low_stability <- 0
  if(stability > 0.4){
   is_low_stability <- 1  
  }
  a[1 , 'is_low_stability'] <- is_low_stability 
  ########### ability_eod is applied_amount as a ratio of EOD_L3 ########
  ######## if applied amount < 10000 , then applied amount is set to default of 200000
  if(applied_amt < 10000){
    applied_amt <- 200000
  }
  
    ability_eod <- applied_amt/eod_l3
    
    ability_eod2 <- ability_eod
    
    
    
    
    
    
    
    if(ability_eod2<0){
    ability_eod2 <- ability_eod2*-1
    }
    ability_credits <- applied_amt/(credits_l3*0.15)
    ability <- min(ability_eod2, ability_credits)
    
    is_low_ability <- 0
    if(ability>3 & applied_amt > 200000){
      is_low_ability <- 1
    }
    a[1 , 'is_low_ability'] <- is_low_ability
  ########## If ask is > 10 times his last 3 Months EOD balance then sensitivity is 1 ##### 
  sensitivity <- 0
  if(ability_eod>10 & applied_amt > 200000 ){
    sensitivity <- 1
  }
  
    a[1 , 'sensitivity'] <- sensitivity
  ########## Tree Based Rules ###################
    tree_rule <- paste0(HS_decision, bureau_thickness ,is_declining_income, is_eod_low , is_low_stability, is_low_ability)
    bs_rule_select <- bs_tree_based[ bs_tree_based$Rule == tree_rule , ]
    if(nrow(bs_rule_select)> 0){
      tree_score <- as.numeric(bs_rule_select$Default)
    }else{
      tree_score <- 0
    }
    if(!is.na(HS_decision) & HS_decision=="Decline"){
      tree_score <- HS_probability
    }
    
      ########## Run the Banking Model #########
      
      if(is_od==1){
        config <- bs_model_config[ bs_model_config$parameter!='eod_l3_w_non_od_N' , ]
      }
      
      config$value <- 1
      
      config$value[config$parameter=='default_probability'] <- HS_probability
      config$value[config$parameter=='credit_final_growth_N'] <- final_growth
      config$value[config$parameter=='inward_cheque_bounces'] <- inward_cheque_bounces 
      config$value[config$parameter=='stability_N'] <- stability
      config$value[config$parameter=='eod_l3_w_non_od_N'] <- min(1, eod_l3_w_non_od_N/2000000)
      High_Penal_Transactions <- 0
        if(penal_transactions > 6 | inward_cheque_bounces > 6 ){
          High_Penal_Transactions <- 1  
        }
      config$value[config$parameter=='High_Penal_Transactions'] <- High_Penal_Transactions
      config$value[config$parameter=='Sensitivity'] <- sensitivity
      config$value[config$parameter=='has_od'] <- is_od*min(1, avg_credits/5000000)
      config$model <- as.numeric(config$weight)*as.numeric(config$value)
      config$Expected <- config$Expected*config$weight
      config$diff <- config$model-config$Expected
      config <- config[order(config$diff, decreasing = T),]
      config$is_bad <- 0
      config$is_bad[config$value > config$greater_issue ] <- 1
      config$is_bad[config$value < config$less_issue ] <- 1
      config$is_bad[config$diff >0.01 ] <- 1
      #print(config)
      reas <-  config$Note[config$is_bad==1 & nchar(config$Note)> 2]
      if(length(reas)==0){
        if(is_declining_income==1){
          a[1 , "reason"] <- "Declining Income"  
        }else{
        a[1 , "reason"] <- NA
        }
      }else{
        if(length(reas)>3){
          reas <- reas[1:3]
        }
        if(is_declining_income== 1){
        reas <- c("Declining Income", reas)
        reas <- reas[reas!="Low Credit Growth"]  
        }
      a[1 , "reason"] <- as.character(toJSON(reas))
      }
      
      bs_logistic_score <- sum(config$model)
      bs_model_score <- 1/(1+ exp(-1*bs_logistic_score))
      
      if(delinquency_index_lifetime>=25){
        bs_model_score <- max(bs_model_score , tree_score , 0.2)
      }else{
        bs_model_score <- max(bs_model_score , tree_score)
      }
      
      bs_rb <- bs_riskbands$BS_RiskBand[ bs_riskbands$Min <=bs_model_score & bs_riskbands$Max > bs_model_score   ]
      bs_decision <- bs_decision_config$Decision[ bs_decision_config$Thickness == bureau_thickness & bs_decision_config$Min_Prob <=  bs_model_score & bs_decision_config$Max_Prob > bs_model_score ]
      
      ############ If Horizontal Decision is Decline then the Final Decision is also Decline ###
      if(!is.na(HS_decision) & HS_decision == "Decline"){
        bs_decision <- "Decline"     
      }
      
      
      ############ If Qualifying Criteria then Approve to Manual #############
      
      if(bs_decision=="Approve"){
        if(delinquency_index_lifetime >= 3 | leverage_ratio >=0.9 | business_vintage < 1 | (Age <23 | Age >60) | (cibil_score <610 & cibil_score >300) | inward_cheque_bounces >= 10){
          bs_decision <- "Manual Review" 
        }
      }
      
      ############ Decline to Manual if both properties are owened ##############
      
      if(bs_decision=="Decline" & HS_decision != "Decline"){
        if(owned_all >=2){
          bs_decision <- "Manual Review" 
        }
      }
      
      if(bs_decision=="Approve" ){
        a$reason <- NA
      }
      
      a$bs_model_score <- bs_model_score
      a$bs_rb <- bs_rb
      a$bs_decision <- bs_decision
  return( as.list(a)  )
}

