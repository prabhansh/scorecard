HS_PreFeb18 <- function(application , request_id){
  
  tryCatch({
  
  ###################  Error Handling ###################
  
    #### Return error if request id not found
    
    if(length(rownames(application))<=0){
      return(get_standard_error_message("Incorrect request id for application", paste("Request id was ", request_id)))
    }
    
    
    #################### Default response object created ####################
    
    final_response <- list()
    final_response$result <- T
    final_response$requestId <- request_id
    
    final_response$score <- NA
    
    #final_response$model_decision_value <- MODEL_DECISION_MANUAL_REVIEW
    final_response$model_decision <- list(name = "Model Decision", value = MODEL_DECISION_MANUAL_REVIEW)
    
    #final_response$pricing_value <- NA
    final_response$pricing <- list(name= "Pricing" , value = NA)
    
    #final_response$MaxTenor_value <- NA
    final_response$MaxTenor <- list(name= "Max Tenure" , value = NA)
    
    final_response$file_type <- list(name = "File Type", value=NA)
    final_response$score_band <- list(name = "Score Band" , value = NA)
    
    final_response$bank_statement_required <- list(name = "Bank Statement Required", value="TRUE")
    final_response$tier <- "Tier4"
    
    
    instant_credit_parameters <- list(model_decision=MODEL_DECISION_DECLINE)
    fast_loan_parameters <- list(model_decision=MODEL_DECISION_DECLINE, pricing=NA, MaxTenor=NA, fast_loan_line=NA)
    
    #################### Hardcoded Response ####################
    
    tryCatch({
      if(grepl("tradeindia-purchasefinancing", application$anchor_name[1], ignore.case = T)){
        applied_amount <- as.numeric(as.character(application$loan_amount[1]))
        tradeindialine <- as.integer(applied_amount*1.1*0.8)
        tradeindiatenure <- 8
        tradeindiapricing <- 0
        
        final_response$model_decision <- MODEL_DECISION_APPROVE
        final_response$MaxTenor$value <- tradeindiatenure
        final_response$pricing$value <- tradeindiapricing 
        
        fast_loan_parameters$model_decision <- MODEL_DECISION_APPROVE
        fast_loan_parameters$cibil_anchor_line <- tradeindialine
        fast_loan_parameters$fast_loan_line <- tradeindialine
        fast_loan_parameters$cibil_anchor_line_tenure <- tradeindiatenure
        fast_loan_parameters$MaxTenor <- tradeindiatenure
        fast_loan_parameters$cibil_anchor_line_emi <- as.integer(tradeindialine/tradeindiatenure)
        fast_loan_parameters$pricing <- tradeindiapricing
        fast_loan_parameters$processing_fee <- 0
        
        final_response$fast_loan_parameters <- fast_loan_parameters
        final_response$instant_credit_parameters <- instant_credit_parameters
        final_response<- toJSON(as.list(final_response), auto_unbox = T, pretty = T)
        return(final_response)
      }}, error=function(e){
        return(get_standard_error_message("Score could not be computed", paste(unlist(e), collapse = " -> ")))
      })
    
    
    ############## No CIBIL Present Error Response ####################
    
    #### Return error if CIBIL report not found
    if(sum(!is.na(application$ownerId))<=0){
      return(get_standard_error_message("No CIBIL report found - Please pull CIBIL", "No entries in CIBIL"))
    }
    
    #### Send error if manually updated cibil report
    if(sum(!(is.na(application$accounts) & !(application$cibil_score %in% c(-1:5)))) <= 0){
      return(get_standard_error_message("Manually Updated Cibil Report Or No Accounts in Cibil Report", "No Accounts in Cibil Report"))
    }
    
    ############### Initial Adjustments in Application Data #################
    
    #### Set variable value against business for all rows on following columns
    business_variable_columns <- c("total_od_limit", "fast_loan_anchor_data", "fast_loan_anchor_name", "fast_loan_business_category", "vertical_risk_profile_band", "override_emi_obligation","instant_credit_approved_amount", "anchor_agent_type", "business_type")
    
    for (business_variable_column in business_variable_columns){
      application[,business_variable_column] <- application[application$business_entity_type=="business", business_variable_column][1]
    }
    
    test <- merge(application[,c("requestId", "segment_category_id")], segment_category_industry_mapping, by.x="segment_category_id", by.y="SegmentCategoryId", all.x = T, all.y = F)
    application$margin_industry <- test$IndustryName
    #### Adjusting Credits for Intra Bank Statements as per No of Bank Statements
    
    application$number_of_bank_account <- 0
    k = 0
    tryCatch({
      k <- nrow(application)
    },error= function(e){  })
    
    for( q in 1:k){
      tryCatch({
        application$number_of_bank_account[q] <- length(fromJSON(application$unique_bank_accounts[q]))
      },error= function(e){
        
      })
    }
    
    tryCatch({
      application <- merge(application , bs_reduction , by ="number_of_bank_account" , all.x = T , all.y = F )
      application$reduction[is.na(application$reduction)] <- 1
      application$total_credits <- as.numeric(gsub("[^0-9.]","",application$total_credits))
      application$total_credits <- as.numeric(application$total_credits)* as.numeric(application$reduction)
    },error= function(e){
      
    })
    
    #### Application details json converted to columns
    be_len <- length(rownames(application))
    for(i in 1:be_len){
      
      if(!is.na(application$details[i] )){  
        details <- fromJSON(application$details[i])
        details <- Filter(Negate(is.null), details)
        details <- Filter(Negate(is.list), details)
        details_col <- names(details)  
        details_len <- length(details_col)  
        if(details_len > 0){
          for(w in 1: details_len){
            col <- details_col[w]
            if(!col %in% colnames(application)){
              application[[col]] <- "-"
            }
          }
          application[ i, details_col ] <- details[details_col]
        }
      }
    }
    
    #### CIBIL pull date converted to date
    application$date_pull <- as.Date(substr(application$date_of_pull,1,10))
    application$date_pull[application$date_pull < as.Date("2000-01-01") & !is.na(application$date_pull)] <- as.Date(substr(application$date_of_pull[application$date_pull < as.Date("2000-01-01") & !is.na(application$date_pull)],1,10) , format = "%d-%m-%Y")
    
    application$date_pull[is.na(application$date_pull) | application$date_pull < as.Date("2000-01-01") ] <- as.Date(application$cibil_created[is.na(application$date_pull) | application$date_pull < as.Date("2000-01-01")])
    
    #### Date of birth converted to date and age calcualted
    if(!(c("date_of_birth") %in% names(application) )){
      application$date_of_birth <- "-"
    }
    application$date_of_birth <- gsub("/" , "-",  as.character(application$date_of_birth))
    application$date_of_birth <- gsub("\"" , "",  as.character(application$date_of_birth))
    application$date_of_birth_C <- as.Date(substr(as.character(application$date_of_birth),1,10),format ="%Y-%m-%d")
    application$date_of_birth_C[as.Date(application$date_of_birth_C)<as.Date("1900-01-01") |
                                  is.na(as.Date(application$date_of_birth_C)) ] <- 
      as.Date(application$date_of_birth[as.Date(application$date_of_birth_C)<as.Date("1900-01-01") | 
                                          is.na(as.Date(application$date_of_birth_C)) ] ,format ="%m-%d-%Y")
    application$date_of_birth_C[as.Date(application$date_of_birth_C)<as.Date("1900-01-01") |
                                  is.na(as.Date(application$date_of_birth_C)) ] <- 
      as.Date(application$date_of_birth[as.Date(application$date_of_birth_C)<as.Date("1900-01-01") | 
                                          is.na(as.Date(application$date_of_birth_C)) ] ,format ="%d-%m-%Y")
    application$entity_details <- NULL
    
    
    ap_br <- application
    ap_br$details <- NULL
    ap_br$created <- as.Date(substr(ap_br$created,1,10))
    ap_br$Age <- as.numeric(as.Date(ap_br$created) - as.Date(ap_br$date_of_birth_C))/365
    ap_br$Age[is.na(ap_br$Age)] <- 18
    ap_br$owned <- 0
    ap_br$pin <- ""
    
    #### Pincode extracted from JSON
    for(i in 1 : length(rownames(ap_br))){
      
      if(!is.na(ap_br$primary_address[i])){  
        t <- fromJSON(ap_br$primary_address[i])
        pin <- ""
        if("pinCode" %in% names(t)){
          pin <- t$pinCode
        }
        if("postal_code" %in% names(t)){
          pin <- t$postal_code
        }
        
        if("isOwned" %in% names(t)){
          if(length(t$isOwned) > 0){
            ap_br$owned[i] <- t$isOwned
          }
        }
        if(length(pin)>0){
          ap_br$pin[i] <- pin
        }
      }
    }
    
    #### Experian Pincode Risk Ranking Geocode assigned
    experian_geo <- read.csv("Experian_Geo.csv", stringsAsFactors = F)
    experian_geo <- experian_geo[!duplicated(experian_geo$Overall_Pincode), ] 
    colnames(experian_geo) <- c("Pincodes", "ranking2" )
    ap_br <- merge(ap_br , experian_geo , by.x = "pin" , by.y = "Pincodes" , all.x = T , all.y = F )
    ap_br$ranking2 <- as.numeric(gsub('[^0-9]','',ap_br$ranking2))
    ap_br$ranking2[is.na(ap_br$ranking2)] <- 0
    temp <- ap_br[ , c("requestId","ranking2")]
    temp2 <- aggregate(ranking2~requestId , temp , max)
    temp2<- temp2[1,]
    ap_br$ranking2 <- NULL
    ap_br <- merge(ap_br , temp2 , by.x = "requestId" , by.y = "requestId" , all.x = T , all.y = F )
    
    #### Indifi Pincode Risk Ranking Geocode assigned
    Geo_Risk <- read.csv("Geo_Risk.csv")[ , 1:3]
    Geo_Risk <- Geo_Risk[!duplicated(Geo_Risk$Pincodes), ] 
    colnames(Geo_Risk) <- c("Pincodes", "location_risk_self_declared" , "Loc_Band" )
    ap_br2 <- merge(ap_br , Geo_Risk , by.x = "pin" , by.y = "Pincodes" , all.x = T , all.y = F )
    ap_br2$Loc_Band[is.na(ap_br2$location_risk_self_declared)] <- 0
    ap_br2$location_risk_self_declared[is.na(ap_br2$location_risk_self_declared)] <- 6
    temp <- ap_br2[ , c("requestId","location_risk_self_declared","Loc_Band")]
    temp2 <- aggregate(location_risk_self_declared~requestId+Loc_Band , temp , max)
    temp2 <- temp2[order(temp2$location_risk_self_declared, decreasing = T),]
    temp2<- temp2[1,]
    ap_br2$location_risk <- NULL
    ap_br2$Loc_Band <- NULL
    ap_br2 <- merge(ap_br , temp2 , by.x = "requestId" , by.y = "requestId" , all.x = T , all.y = F )
    
    ####  Get Business Location & Overwrite Pin for Qualifying Criteria
    temp <- ap_br2[ tolower(ap_br2$relationship) == "borrower" & !is.na(ap_br2$pin) , c("requestId", "pin")]
    temp <- temp[!duplicated(temp$requestId), ]
    colnames(temp) <- c("requestId", "pin2")
    ap_br2 <- merge(ap_br2 , temp , by.x = "requestId" , by.y = "requestId" , all.x = T , all.y = F )
    ap_br2$pin[!is.na(ap_br2$pin2)] <- ap_br2$pin2[!is.na(ap_br2$pin2)]
    ap_br2$pin2 <- NULL
    
    #### Incorporation date converted to date and business vintage calculated
    if((c("incorporation_date") %in% names(ap_br2))){
      date_incorp <- ap_br2[ , c("requestId", "incorporation_date" )]
      date_incorp$incorporation_date <- gsub("/","-",as.character(date_incorp$incorporation_date))
      date_incorp$incorp_date <- "-"
      date_incorp$incorp_date <- substr(date_incorp$incorporation_date,1,10)
      date_incorp$incorp_date2 <- NA
      date_incorp$incorp_date2 <- as.Date(as.character(date_incorp$incorp_date) , format = "%Y-%m-%d")
      date_incorp$incorp_date2[is.na(date_incorp$incorp_date2) | date_incorp$incorp_date2 < as.Date("1900-01-01") ] <- as.Date(as.character(date_incorp$incorp_date[is.na(date_incorp$incorp_date2) | date_incorp$incorp_date2 < as.Date("1900-01-01") ] ) , format = "%m-%d-%Y")
      date_incorp$incorp_date2[is.na(date_incorp$incorp_date2) | date_incorp$incorp_date2 < as.Date("1900-01-01") ] <- as.Date(as.character(date_incorp$incorp_date[is.na(date_incorp$incorp_date2) | date_incorp$incorp_date2 < as.Date("1900-01-01") ] ) , format = "%d-%m-%Y")
      date_incorp <- subset(date_incorp , !is.na(date_incorp$incorp_date2) & date_incorp$incorp_date2 < Sys.Date() )
      date_incorp <- date_incorp[order(date_incorp$incorp_date2, decreasing = T), ]
      date_incorp <- date_incorp[!duplicated(date_incorp$requestId), c("requestId", "incorp_date2") ]
      ap_br2 <- merge(ap_br2 , date_incorp , by.x = "requestId"  , by.y = "requestId" , all.x = T , all.y = F)
      ap_br2$business_vintage <- as.numeric(as.Date(ap_br2$created) - as.Date(ap_br2$incorp_date2))/365
    }else{
      ap_br2$business_vintage <- 0
    }
    ap_br2$business_vintage[is.na(ap_br2$business_vintage) | 
                              ap_br2$business_vintage > 100] <- 0
    
    #### E-mail domain unique or not calculated
    email_domian <- read.csv("Common_Email_Domains.csv")
    ap_br2$unique_domain <- 1
    ap_br2$unique_domain[is.na(ap_br2$email)] <- 0
    ap_br2$unique_domain[!grepl('@',ap_br2$email)] <- 0
    for(i in 1:length(rownames(email_domian))){
      sub <- tolower(as.character(email_domian[i,1]))
      ap_br2$unique_domain[grepl(sub , tolower(ap_br2$email))] <- 0 
    }
    
    temp3 <- ap_br2[ ,  c("requestId", "email" ,"unique_domain")]
    colnames(temp3)<-  c("requestId", "email" ,"unique_domain_all")
    temp3 <- aggregate(unique_domain_all~requestId , temp3 , sum)
    temp3$unique_domain_all[temp3$unique_domain_all > 0 ] <- 1
    temp3$email <- NULL
    ap_br2 <- merge(ap_br2 , temp3 , by.x = "requestId"  , by.y = "requestId" , all.x = T , all.y = F)
    
    #### Property ownership fields calculated
    temp4 <- ap_br[ , c("requestId" , "relationship" , "owned")]
    temp5 <- ap_br[tolower(ap_br$relationship) == "borrower" , c("requestId" , "relationship" , "owned")]
    colnames(temp5) <- c("requestId" , "relationship" , "owned_borrower")
    colnames(temp4) <- c("requestId" , "relationship" , "owned_all")
    temp4 <- temp4[!duplicated(temp4), ]
    temp4 <- aggregate(owned_all~requestId , temp4 , sum)
    temp5 <- aggregate(owned_borrower~requestId , temp5 , sum)
    ap_br2 <- merge(ap_br2 , temp4 , by.x = "requestId"  , by.y = "requestId" , all.x = T , all.y = F)
    ap_br2 <- merge(ap_br2 , temp5 , by.x = "requestId"  , by.y = "requestId" , all.x = T , all.y = F)
    
    ####  Bureau History History calculated
    cibil <- ap_br2    
    cibil$bureau_history2 <- as.Date(substr(as.character(cibil$bureau_history),1,10),format ="%Y-%m-%d")
    cibil$bureau_history2[as.Date(cibil$bureau_history2)<as.Date("1900-01-01") | is.na(as.Date(cibil$bureau_history2)) ] <- as.Date(cibil$bureau_history[as.Date(cibil$bureau_history2)<as.Date("1900-01-01") | is.na(as.Date(cibil$bureau_history2)) ] ,format ="%m-%d-%Y")
    cibil$bureau_history_years <- as.numeric(as.Date(cibil$date_pull) - as.Date(cibil$bureau_history2))/365
    
    ###########################  Variable Computation  ####################
    
    #### PAN Issue as per Date of Birth Computed
    cibil$has_pan_issue <- 0
    cibil$pan_note <- ""
    for(z in 1:nrow(cibil)){
      temp <- check_pan(as.numeric(substr(cibil$date_of_birth_C[z],1,4)), cibil$pan_id[z])
      if(!is.null(temp[1]) & !is.na(temp[1])){
        cibil$has_pan_issue[z] <- temp[1]
        cibil$pan_note[z] <- temp[2]
      }
    }
    temp <- cibil[order(cibil$has_pan_issue, decreasing = T), c("has_pan_issue", "pan_note")] 
    cibil$has_pan_issue <- temp$has_pan_issue[1]
    cibil$pan_note <- temp$pan_note[1]
    
    ####  For Experian
    source("Functions/E_1_Live_Account_Holding_Variables.R")
    source("Functions/E_2_Utilization_Parameters.R")
    source("Functions/E_3_Delinquency_Parameters.R")
    source("Functions/E_4_1_Function_Account_Parameters.R")
    source("Functions/E_4_2_Function_Enquiry_Parameters.R")
    
    cibil <- Get_Live_Accounts(cibil)
    cibil <- Get_Utilization_Across_Trade_Lines(cibil)
    cibil <- suppressWarnings(Get_Delinquency_Parameters(cibil))
    cibil <- Get_Accounts_Opened_In_Last_12_Months(cibil)
    cibil <- Get_Enquiries_In_60_Days(cibil)
    
    ####  For Gen2
    #### Get Employment
    tryCatch({
      source("Functions/6_Function_Employment_Extract.R")
      
      cibil <- Get_Employment(cibil_analysis = cibil)
    }, error=function(e){
    })
    
    #### Enquiry Index
    tryCatch({
      source("Functions/1_Function_Enquiry_Index.R")
      
      cibil <- Enquiry_Index(cibil_analysis = cibil)
    }, error=function(e){
      return(get_standard_error_message("Issues while parsing CIBIL report",paste(unlist(e), collapse = " -> ")))
    })
    
    
    #### Accounts Parse
    tryCatch({
      source("Functions/4_Function_Accounts_Extract.R")
      cibil <- Get_Accounts(cibil_analysis = cibil)
    }, error=function(e){
      return(get_standard_error_message("Issues while parsing Cibil Report",paste(unlist(e), collapse = " -> ")))
    })
    ####   Deliquency Index
    tryCatch({
      source("Functions/4.2_Function_Delinquency_Index.R")
      cibil <- Get_Delinquency_Index(cibil_analysis = cibil)
    }, error=function(e){
      return(get_standard_error_message("Issues while parsing Cibil Report",paste(unlist(e), collapse = " -> ")))
    })
    
    #### EMI set as max of emi main, emi running and fixed expenses
    
    row.names(cibil) <- 1:length(row.names(cibil))
    cibil$emi_running <- as.numeric(gsub("[^0-9.]","", as.character(cibil$emi_running)))
    cibil$fixed_expenses <- as.numeric(gsub("[^0-9.]","", as.character(cibil$fixed_expenses)))
    cibil$emi_main <- as.numeric(cibil$emi_main)
    cibil$emi_running[is.na(cibil$emi_running)] <- 0
    cibil$fixed_expenses[is.na(cibil$fixed_expenses)] <- 0
    cibil$emi_main[is.na(cibil$emi_main)] <- 0
    
    if(!is.na(cibil$override_emi_obligation[1]) & nchar(cibil$override_emi_obligation[1]) > 3){
      cibil$emi_main <- cibil$fixed_expenses
    } else{
      cibil$emi_main <- pmax(cibil$emi_main, cibil$emi_running, cibil$fixed_expenses)
    }
    
    cibil$address <- NULL
    cibil$enquiries <- NULL
    cibil$identity <- NULL
    cibil$accounts<- NULL
    
    #### Bureau Computed Variables Adjustments
    cibil$delinquency_index[cibil$delinquency_index > 30] <- 30
    cibil$total_writeoff[is.na(cibil$total_writeoff)] <- 0
    cibil$account_opened_60[cibil$account_opened_60 > 2] <- 2
    cibil$enquiry_index[cibil$enquiry_index > 20] <- 20
    cibil$bureau_history_years[is.na(cibil$bureau_history_years)] <- 0
    cibil$bureau_history_years[cibil$bureau_history_years > 20] <- 20
    
    cibil$life_time_sanction_amount <- as.numeric(cibil$life_time_sanction_amount)
    cibil$life_time_sanction_amount[is.na(cibil$life_time_sanction_amount)] <- 0
    cibil$life_time_sanction_amount[cibil$life_time_sanction_amount > 10000000] <- 10000000
    cibil$life_time_sanction_amount_log <- 0
    cibil$life_time_sanction_amount_log[cibil$life_time_sanction_amount >1] <- log10(cibil$life_time_sanction_amount[cibil$life_time_sanction_amount >1])
    cibil2 <- subset(cibil  , !( is.na(cibil$date_of_pull) & cibil$cibil_score < 2 ))
    
    
    #### Merge for No Hit Model
    
    cibil <- cibil2
    cibil <- cibil[!is.na(cibil$requestId) , ]
    
    
    cibil <- data.frame(cibil2 , stringsAsFactors=FALSE)
    cibil[ , BUREAU_THICKNESS_CAL] <- "thin"
    cibil[!is.na(cibil$cibil_score) & cibil$cibil_score > 100 ,  BUREAU_THICKNESS_CAL ] <- "thick"
    
    cibil[ , HIGHEST_DPD_IN_LOAN_PRODUCT_LAST_YEAR_EXCEPT_GOLD ] <- cibil[ , 'highest_dpd_in_loan_product_last_year_except_gold_v1' ]
    cibil[ , HIGHEST_DPD_IN_LOAN_PRODUCT_SIX_MONTHS ] <- cibil[ , 'highest_dpd_in_loan_product_six_months_v2' ]
    cibil[ , SIXTY_DPD_LAST_YEAR ] <- cibil[ , 'sixty_dpd_last_year_v1' ]
    cibil[ , ANY_WOFF_SUBSTANDARD_SUIT_FILED ] <- cibil[ , 'any_woff_sub_standard_suit_filed_v1' ]
    cibil[ , OVERDUE_ACCOUNTS ] <- cibil[ , 'overdue_accounts_v1' ]
    cibil[ , CIBIL_SCORE ] <- cibil[ , 'cibil_score' ]
    cibil[ , NO_OF_ENQUIRIES_LAST_6_MONTHS ] <- cibil[ , 'number_of_enquiries_in_last_6_months_v1' ]
    cibil[ , BUREAU_HISTORY_SINCE ] <- cibil[ , 'bureau_history_years' ]
    
    ####   Number of promoters
    temp <- cibil[ tolower(cibil$relationship) == 'promoter', c("requestId" , "relationship")]
    temp <- temp[!is.na(temp$requestId), ]
    temp <- plyr::count(temp$requestId)
    colnames(temp) <- c("requestId","number_of_promotors")
    
    cibil <- merge(cibil , temp , by = "requestId" , all.x = T , all.y = F)
    
    #### Lifetime sanction amount adjustment
    cibil$life_time_sanction_amount <- as.numeric(cibil$life_time_sanction_amount)
    cibil$life_time_sanction_amount[is.na(cibil$life_time_sanction_amount)] <- 0
    cibil$life_time_sanction_amount2 <- cibil$life_time_sanction_amount
    cibil$life_time_sanction_amount[cibil$life_time_sanction_amount > 10000000] <- 10000000
    cibil$life_time_sanction_amount_log <- 0
    cibil$life_time_sanction_amount_log[cibil$life_time_sanction_amount >1] <- log10(cibil$life_time_sanction_amount[cibil$life_time_sanction_amount >1])
    
    cibil$leverage_ratio_modified <- cibil$life_time_sanction_amount_log * cibil$total_principal_paid/cibil$life_time_sanction_amount
    cibil$leverage_ratio[cibil$leverage_ratio > 1] <- 1
    cibil$leverage_ratio[cibil$leverage_ratio < 0] <- 0
    
    #### Taking all Accounts linked to an application
    #### One person can be linked to multiple applications but Business Request Id will be different
    
    application_pre_2 <- cibil[!is.na(cibil$requestId), ]
    application_pre_2$enquiries <- NULL
    
    ####  Missing Value to Male
    
    temp_gender <- -1
    
    if(c("gender") %in% names(application_pre_2)){
      application_pre_2$gender[is.na(application_pre_2$gender)] <- "male"
      application_pre_2$is_male <- 0
      application_pre_2$is_male[tolower(application_pre_2$gender) == "male"] <- 1
      application_pre_2$is_female <- 0
      application_pre_2$is_female[tolower(application_pre_2$gender) == "female"] <- 1
      temp_gender <- application_pre_2[ application_pre_2$is_key_promoter == TRUE, c("requestId", "is_female")]
    }
    
    if(length(rownames(temp_gender)) > 0 ){
      temp_gender <- temp_gender[!duplicated(temp_gender),]
      temp_gender <- aggregate(is_female ~ requestId , temp_gender , sum)
      colnames(temp_gender) <- c("requestId", "has_female_promoter")
      application_pre_2 <- merge(application_pre_2 , temp_gender , by = "requestId" , all.x = T , all.y = F)
    }else{
      application_pre_2$has_female_promoter <- 0  
    }
    ############################## Gen 1 Scorecard for No Hit - To be removed #############
    
    application_pre_2$date_pull <- as.Date(application_pre_2$date_pull)
    application_pre_2$created <- as.Date(application_pre_2$created)
    application_pre_2$days_to_cibil <- round(as.numeric(as.Date(application_pre_2$date_pull) - as.Date(application_pre_2$created)),0)
    
    application_pre_2 <- application_pre_2[order(application_pre_2$version, decreasing = T),]
    application_pre_2 <- application_pre_2[order(application_pre_2$days_to_cibil, decreasing = F),]
    application_pre_2 <- application_pre_2[order(application_pre_2$is_key_promoter, decreasing = T),]
    
    application_pre_2[ , EMAIL_EDIT_DISTANCE ] <- application_pre_2$unique_domain_all
    application_pre_2[ , VINTAGE_MONTHS ] <- application_pre_2$business_vintage
    application_pre_2[ , PROMOTER_DOB ] <- application_pre_2$Age
    application_pre_2[ , RESIDENCE_OWNED ] <- application_pre_2$owned_all > 0
    application_pre_2[ , OFFICE_OWNED ] <- application_pre_2$owned_all > 1
    
    application_pre_2$location_risk <- application_pre_2$location_risk_self_declared
    application_pre_2$location_risk[is.na(application_pre_2$location_risk)] <- 6
    
    #### Gen1 Score calculation for No Hit - To be removed
    
    application_pre_2$get_highest_dpd_in_live_loan_weights <- get_highest_dpd_in_live_loan_weights(application_pre_2)
    application_pre_2$get_highest_dpd_in_loan_product_last_year_except_gold_weights <- get_highest_dpd_in_loan_product_last_year_except_gold_weights(application_pre_2)
    application_pre_2$get_highest_dpd_in_loan_product_six_months_weights <- get_highest_dpd_in_loan_product_six_months_weights(application_pre_2)
    application_pre_2$get_sixty_dpd_last_year_weights <- get_sixty_dpd_last_year_weights(application_pre_2)
    application_pre_2$get_any_woff_substandard_suit_filed_weights <-  get_any_woff_substandard_suit_filed_weights(application_pre_2)
    application_pre_2$get_overdue_accounts_weights <-  get_overdue_accounts_weights(application_pre_2) 
    application_pre_2$get_cibil_score_weights <- get_cibil_score_weights(application_pre_2) 
    application_pre_2$get_no_of_enquiries_last_6_months_weights <- get_no_of_enquiries_last_6_months_weights(application_pre_2)
    application_pre_2$get_tenure_on_bureau_weights <- get_tenure_on_bureau_weights(application_pre_2)
    application_pre_2$location_risk_weight <- application_pre_2$location_risk 
    application_pre_2$get_email_domain_unique_weights <- get_email_domain_unique_weights(application_pre_2)
    application_pre_2$get_business_vinatge_weights <- get_business_vinatge_weights(application_pre_2) 
    application_pre_2$get_promoter_age_weights <- get_promoter_age_weights(application_pre_2) 
    application_pre_2$get_own_address_weights <- get_own_address_weights(application_pre_2) 
    
    application_pre_2$new_score_thick <- (get_highest_dpd_in_live_loan_weights(application_pre_2) * 0.05 
                                          + get_highest_dpd_in_loan_product_last_year_except_gold_weights(application_pre_2)* 0.05 
                                          + get_highest_dpd_in_loan_product_six_months_weights(application_pre_2) * 0.03
                                          + get_sixty_dpd_last_year_weights(application_pre_2) * 0.04
                                          +  get_any_woff_substandard_suit_filed_weights(application_pre_2)* 0.05
                                          +  get_overdue_accounts_weights(application_pre_2) * 0.04
                                          + get_cibil_score_weights(application_pre_2) * 0.12
                                          + get_no_of_enquiries_last_6_months_weights(application_pre_2)*0.1
                                          + get_tenure_on_bureau_weights(application_pre_2)*0.1
                                          + application_pre_2$location_risk * 0.1
                                          + get_email_domain_unique_weights(application_pre_2) * 0.01
                                          + get_business_vinatge_weights(application_pre_2) * 0.15
                                          + get_promoter_age_weights(application_pre_2) * 0.1
                                          + get_own_address_weights(application_pre_2) * 0.06
    )
    
    application_pre_2$new_score_thin <- (get_highest_dpd_in_live_loan_weights(application_pre_2) * 0.02 
                                         + get_highest_dpd_in_loan_product_last_year_except_gold_weights(application_pre_2)* 0.02 
                                         + get_highest_dpd_in_loan_product_six_months_weights(application_pre_2) * 0.02
                                         + get_sixty_dpd_last_year_weights(application_pre_2) * 0.02
                                         +  get_any_woff_substandard_suit_filed_weights(application_pre_2)* 0.03
                                         +  get_overdue_accounts_weights(application_pre_2) * 0.02
                                         + get_cibil_score_weights(application_pre_2) * 0
                                         + get_no_of_enquiries_last_6_months_weights(application_pre_2)*0.05
                                         + get_tenure_on_bureau_weights(application_pre_2)*0.02
                                         + application_pre_2$location_risk_self_declared * 0.30
                                         + get_email_domain_unique_weights(application_pre_2) * 0.03
                                         + get_business_vinatge_weights(application_pre_2) * 0.22
                                         + get_promoter_age_weights(application_pre_2) * 0.15
                                         + get_own_address_weights(application_pre_2) * 0.10
    )
    
    
    application_pre_2[ , NEW_SCORE_CAL] <- application_pre_2$new_score_thin
    application_pre_2[ is.na(application_pre_2$new_score_thin) , NEW_SCORE_CAL] <- application_pre_2$new_score_thick[ is.na(application_pre_2$new_score_thin)]
    application_pre_2[ application_pre_2$BureauThicknessCal == BUREAU_THICKNESS_THICK & !is.na(application_pre_2$BureauThicknessCal) , NEW_SCORE_CAL] <- application_pre_2$new_score_thick [ application_pre_2$BureauThicknessCal == BUREAU_THICKNESS_THICK & !is.na(application_pre_2$BureauThicknessCal) ]
    
    source("Score_Factor/Risk_Band_New-Score.R")
    application_pre_2$new_score_risk_band_thick <- get_risk_band_thick(application_pre_2$new_score_thick)
    application_pre_2$new_score_risk_band_thin <- get_risk_band_thin(application_pre_2$new_score_thin)
    
    
    ################################### CIBIL Report Selection Section ####################
    
    col <- colnames(application_pre_2)
    col <- subset(col , col != "business_entity_id" &  col !=  "id" & col!="application_id" & col!= "requestId")
    for( i in 1:length(col)){
      tryCatch({
        application_pre_2[!is.na(application_pre_2$business_entity_id) & !is.na(application_pre_2$id) & is.na(application_pre_2[ , col[i]]) , col[i]] <- 0
        application_pre_2[!is.na(application_pre_2$business_entity_id) & !is.na(application_pre_2$id) & application_pre_2[ , col[i]]=="-" , col[i]] <- 0
        
      },error= function(e){})
    }
    
    application_pre_2$version <- as.numeric(application_pre_2$version)
    application_pre_2$version[is.na(application_pre_2$version)] <- 0
    
    application_pre_2$cibil_score <- as.numeric(application_pre_2$cibil_score)
    application_pre_2$cibil_score[is.na(application_pre_2$cibil_score)] <- 0
    
    application_pre_2$sort <- 1
    application_pre_2$sort[is.na(application_pre_2$days_to_cibil)] <- 0
    application_pre_2$sort[application_pre_2$days_to_cibil < -30 & !is.na(application_pre_2$days_to_cibil) ] <- 0
    application_pre_2$sort[application_pre_2$days_to_cibil > 90 & !is.na(application_pre_2$days_to_cibil) ] <- 0
    application_pre_2$sort[!is.na(application_pre_2$version) & application_pre_2$version == 2 & application_pre_2$cibil_score > 100] <- 
      application_pre_2$sort[application_pre_2$version == 2 & !is.na(application_pre_2$version) & application_pre_2$cibil_score > 100] + 0.1
    
    temp_ap <- subset(application_pre_2, !is.na(application_pre_2$days_to_cibil ))
    
    
    if(length(rownames(temp_ap))<=0){
      return(get_standard_error_message("Issues while parsing Cibil report"))
    }
    
    
    application_pre_2$business_vintage_in_years <- application_pre_2$business_vintage
    application_pre_2$Bureau_thickness_gen2 <- THICKNESS_THICK
    application_pre_2$Bureau_thickness_gen2[ as.numeric(application_pre_2$bureau_history_years) <= 1 ] <- THICKNESS_THIN
    application_pre_2$Bureau_thickness_gen2[ is.na(application_pre_2$highest_amount_borrowed2) |  as.numeric(application_pre_2$highest_amount_borrowed2) < 100000 ] <- THICKNESS_THIN
    application_pre_2$Bureau_thickness_gen2[ is.na(application_pre_2$number_of_accounts) |  as.numeric(application_pre_2$number_of_accounts) <=3 ] <- THICKNESS_THIN
    application_pre_2$Bureau_thickness_gen2[ is.na(application_pre_2$cibil_score) |  as.numeric(application_pre_2$cibil_score) < 10 ] <- THICKNESS_NO_HIT
    application_pre_2$sort[application_pre_2$Bureau_thickness_gen2 == THICKNESS_THICK & !is.na(application_pre_2$Bureau_thickness_gen2)] <-
      application_pre_2$sort[application_pre_2$Bureau_thickness_gen2 == THICKNESS_THICK & !is.na(application_pre_2$Bureau_thickness_gen2)] + 0.5
    application_pre_2$sort[application_pre_2$Bureau_thickness_gen2 == THICKNESS_THIN & !is.na(application_pre_2$Bureau_thickness_gen2)] <-
      application_pre_2$sort[application_pre_2$Bureau_thickness_gen2 == THICKNESS_THIN & !is.na(application_pre_2$Bureau_thickness_gen2)] + 0.25
    
    application_pre_2$enquiries_purpose <- NULL
    application_pre_2$dpd_all <- NULL
    
    temp <- application_pre_2[ , c("requestId","days_to_cibil", "version","cibil_score","new_score_risk_band_thick" , "new_score_thick", "new_score_risk_band_thin"  , "new_score_thin" ) ]
    temp$cibil_score <- as.numeric(temp$cibil_score)
    temp$cibil_score[temp$cibil_score < 100 & !is.na(temp$cibil_score)] <- temp$cibil_score[temp$cibil_score < 100 & !is.na(temp$cibil_score)] + 1000
    temp <- temp[!is.na(temp$days_to_cibil), ]
    temp$days_to_cibil[temp$days_to_cibil < 0 ] <- -1* temp$days_to_cibil[temp$days_to_cibil < 0 ]
    temp <- temp[order(temp$cibil_score , decreasing = F), ]
    temp$cibil_score <- NULL
    temp$version <- NULL
    temp$days_to_cibil <- NULL
    temp <- temp[!duplicated(temp$requestId),]
    
    colnames(temp) <- c("requestId","Final_new_score_risk_band_thick" , "Final_new_score_thick" ,  "final_new_score_risk_band_thin"  , "final_new_score_thin")
    
    application_pre_2 <- merge(application_pre_2 , temp , by = "requestId" , all.x = T , all.y = F)
    
    #### Tier and pricing calculated
    application_pre_2 <- merge(application_pre_2, pincode, by.x="pin", by.y="pincode", all.x = T, all.y = F)
    application_pre_2$Tier[is.na(application_pre_2$Tier)] <- "Tier4"
    application_pre_2$ExtraPricing[is.na(application_pre_2$ExtraPricing)] <- 2
    
    
    #### More variable computation
    application_pre_2$monthly_debt2 <- application_pre_2$total_monthly_debt
    application_pre_2$total_monthly_debt <- log10(application_pre_2$total_monthly_debt)
    application_pre_2$total_monthly_debt[is.na(application_pre_2$total_monthly_debt) | application_pre_2$total_monthly_debt < 0 | is.nan(application_pre_2$total_monthly_debt) | is.infinite(application_pre_2$total_monthly_debt) ] <- 0
    
    
    application_pre_2$avrg_sanction_per_account <- application_pre_2$life_time_sanction_amount/application_pre_2$number_of_accounts
    application_pre_2$avg_sanction_per_account <- application_pre_2$life_time_sanction_amount/application_pre_2$number_of_accounts
    
    application_pre_2$has_age_18_to_25 <- 0
    application_pre_2$has_age_18_to_25[application_pre_2$Age <=25] <- 1
    application_pre_2$location_risk <- application_pre_2$location_risk_self_declared
    application_pre_2$unique_domain <- application_pre_2$unique_domain_all
    if(c("income_1") %in% names(application_pre_2)){
      application_pre_2$income_1 <- as.numeric(gsub("[^0-9]","",application_pre_2$income_1))
      application_pre_2$income_1[is.na(application_pre_2$income_1)] <- 0
      application_pre_2$income_1[grepl( 'annual' , tolower(application_pre_2$monthly_annual_indicator_1) )] <-  application_pre_2$income_1[grepl( 'annual' , tolower(application_pre_2$monthly_annual_indicator_1) )] /12
    }else{
      application_pre_2$income_1 <- 0
    }
    application_pre_2$Gen2Score <- 0
    application_pre_2$overdue_accounts <- application_pre_2$overdue_accounts_v1
    
    application_pre_2$cibil_score <- as.numeric(application_pre_2$cibil_score)
    
    
    
    
    ####################### No Hit Section ############################
    
    if(sum(application_pre_2$Bureau_thickness_gen2 != THICKNESS_NO_HIT) == 0){
      
      data <- application_pre_2[1, ]
      
    } else {
      
      ####################################  Gen 2 #####################
      
      
      
      #### Select appropriate CIBIL report for Thick and Thin
      application_pre_2$cibil_score[application_pre_2$cibil_score < 10 & !is.na(application_pre_2$cibil_score)] <- application_pre_2$cibil_score[application_pre_2$cibil_score < 10 & !is.na(application_pre_2$cibil_score)] + 1000
      data <- application_pre_2[application_pre_2$Bureau_thickness_gen2 != THICKNESS_NO_HIT , ]
      data <- data[!is.na(data$days_to_cibil), ]
      data$days_to_cibil[data$days_to_cibil < 0 ] <- -1* data$days_to_cibil[data$days_to_cibil < 0 ]
      data$cibil_score[is.na(data$cibil_score)] <- 9000
      
      data <- data[order(data$cibil_score , decreasing = F), ]
      data <- data[order(data$sort , decreasing = T), ]
      data <- data[1 , ]
      
      data$cibil_score[data$cibil_score > 2000] <- 0
      data$cibil_score[data$cibil_score > 900] <- data$cibil_score[data$cibil_score > 900] - 1000
    }
    
    ################### Calculate variable weights ##################
    
    if(data$Bureau_thickness_gen2 == THICKNESS_THICK){
      weight_dirs_name <- "Weights/"
      score_factors_file_name <- "Score_Factor/score_factor.csv"
      column_weight_name <- "_weight"
      score_column_name <- "Gen2Score"
      risk_band_column_name <- "Gen2_RiskBand"
      thickness_name <- tolower(THICKNESS_THICK)
      risk_band_gen2_file_name <- "Score_Factor/Risk_Band_Gen2.csv"
    } else if (data$Bureau_thickness_gen2 == THICKNESS_THIN){
      weight_dirs_name <- "Weights_Thin/"
      score_factors_file_name <- "Score_Factor_Thin/score_factor.csv"
      column_weight_name <- "_weight_thin"
      score_column_name <- "Gen2Score_thin"
      risk_band_column_name <- "Gen2_RiskBand_Thin"
      thickness_name <- tolower(THICKNESS_THIN)
      risk_band_gen2_file_name <- "Score_Factor_Thin/Risk_Band_Gen2.csv"
    }
    data$Gen2Score <- 0
    data$Gen2Score_thin <- 0
    data$Gen2_RiskBand <- 0
    data$Gen2_RiskBand_Thin <- 0
    reason <- data.frame()
    par <- ""
    
    if(data$Bureau_thickness_gen2 == THICKNESS_THICK | data$Bureau_thickness_gen2 == THICKNESS_THIN){
      weight_dirs <- list.files(weight_dirs_name,".csv", full.names = F)
      score_factors <- read.csv(score_factors_file_name)
      reason <- score_factors
      reason$value <- 0
      reason$contribution <- 0
      
      cols <- substr(weight_dirs, 0 , nchar(weight_dirs)-4 )
      col_len <- length(cols)
      
      for(col_len_i in 1: col_len){
        column <- cols[col_len_i]
        
        col_weight <- paste0(column,column_weight_name)
        weight_map <- read.csv(paste0(weight_dirs_name,column,".csv"))
        score_factor_i <- 0
        tryCatch({
          score_factor_i <- score_factors$score_factor[score_factors$variable == column]
        },error=function(e){})
        
        if( length(score_factor_i) ==0 | is.na(score_factor_i)){
          score_factor_i <- 0
        }
        
        len <- length(rownames(weight_map))
        weight_map$max[1:(len-1)] <- weight_map$min[2:len]
        weight_map$max[len] <- weight_map$max[len]+100000
        
        data[[col_weight]] <- 0
        data[is.na(data[ , column]), column] <- 0
        data[ , col_weight] <- 0
        for(i in 1:len){
          data[ data[ , column] >= weight_map$min[i] & data[ , column] < weight_map$max[i] , col_weight]    <- weight_map$weight[i]   
        }
        
        reason$value[reason$variable == column]  <- data[ 1 , col_weight]
        reason$contribution[reason$variable == column]  <- score_factor_i*data[ 1 , col_weight]
        data[,score_column_name] <- data[,score_column_name] + score_factor_i*data[ , col_weight]
        reason$par[reason$variable == column] <-  as.numeric(data[1 , column]) 
      }
      
      data[,score_column_name][tolower(data$Bureau_thickness_gen2) != thickness_name] <- -1
      
      risk_band <- read.csv(risk_band_gen2_file_name)
      risk_band <- risk_band[order(risk_band$min), ]
      len <- length(rownames(risk_band))
      risk_band$max[1:(len-1)] <- risk_band$min[2:len]
      risk_band$max[len] <- risk_band$max[len]+100000
      risk_band$min[1] <- risk_band$min[1]-100000
      
      data[,risk_band_column_name] <- "-1"
      
      for(i in 1:len){
        data[data[,score_column_name] >= risk_band$min[i] & data[,score_column_name] < risk_band$max[i], risk_band_column_name] <- risk_band$risk_band_gen2[i]   
      }
      
      if(data$Bureau_thickness_gen2 == THICKNESS_THIN){
        data$Gen2Score <- data[,score_column_name]
        data$Gen2_RiskBand <- data[,risk_band_column_name]
      }
      
      col <- c(paste0(score_factors$variable,"_weight"))
      col2 <- c(paste0(score_factors$variable))
      
    }
    
    r_backup <- reason
    
    
    ################ Experian Variable Weights ############
    
    tryCatch({
      data$ExperianScore <- 0
      experian_weight_dirs <- list.files("Experian_Hit_Weights/",".csv", full.names = F)
      experian_cols <- substr(experian_weight_dirs, 0 , nchar(experian_weight_dirs)-4 )
      experian_col_len <- length(experian_cols)
      
      tryCatch({
        for(col_len_i in 1: experian_col_len){
          column <- experian_cols[col_len_i]
          col_weight <- paste0(column,"_weight_experian")
          weight_map <- read.csv(paste0("Experian_Hit_Weights/",column,".csv"))
          
          len <- length(rownames(weight_map))
          weight_map$max[1:(len-1)] <- weight_map$min[2:len]
          weight_map$max[len] <- weight_map$max[len]+100000
          
          data[[col_weight]] <- 0
          data[is.na(data[ , column]), column] <- 0
          data[ , col_weight] <- 0
          for(i in 1:len){
            data[ data[ , column] >= weight_map$min[i] & data[ , column] < weight_map$max[i] , col_weight]    <- weight_map$weight[i]   
          }
          
          data$ExperianScore <- data$ExperianScore + data[ , col_weight]
          
        }
      }, error = function(e){})
      
    }, error = function(e){})
    
    data$ExperianScoreBand <- NA
    experian_risk_weights <- read.csv("ExperianBand/ExperianRiskBand.csv")
    for(z in 1:length(rownames(experian_risk_weights))){
      data$ExperianScoreBand[(data$ExperianScore>=experian_risk_weights$min[z] & data$ExperianScore<experian_risk_weights$max[z])] <- experian_risk_weights$ExperianRiskBand[z]
    }
    
    
    ################ Model Reasons #######################
    #### Only for thick and thin
    reason <- r_backup
    reason_bad <- list("")
    notes <- list("")
    existing_EMI_obligation <- data$emi_main[1]
    if(is.null(existing_EMI_obligation) | is.na(existing_EMI_obligation)){
      existing_EMI_obligation <- 0
    }
    existing_EMI_obligation <- round(existing_EMI_obligation)
    
    if(data$Bureau_thickness_gen2 == THICKNESS_THICK | data$Bureau_thickness_gen2 == THICKNESS_THIN){
      
      reason$par[grepl('location' , tolower(reason$variable)) ] <- data$Loc_Band[1]
      reason$par[ tolower(reason$variable) == 'delinquency_index' ] <- data$dpd_last36[1]
      reason$par[ tolower(reason$variable) == 'delinquency_index_lifetime' ] <- data$dpd_alllife[1]
      reason$par[ tolower(reason$variable) == 'enquiry_index' ] <- data$unique_enquiries_last6[1]
      reason$par[ tolower(reason$variable) == 'has_age_18_to_25' ] <- data$Age[1]
      reason$par[ tolower(reason$variable) == 'total_monthly_debt' ] <- existing_EMI_obligation
      reason$par[ grepl( 'sanction_amount' , tolower(reason$variable) ) ] <- data$life_time_sanction_amount2[1]
      
      reason <- data.frame(lapply(reason, as.character), stringsAsFactors=FALSE)
      par <- data[ , c(col2, paste0(experian_cols,"_weight_experian"))]
      reason$par <- as.character(round(as.numeric(reason$par),2))
      reason$par[reason$par > 100 & !is.na(reason$par) ] <- as.character(round(as.numeric(reason$par[reason$par > 100 & !is.na(reason$par) ]),0))
      reason$value <- round(as.numeric(reason$value),2)
      
      reason <- merge(reason, reason_map , by.x = "variable", by.y = "variable" , all.x = T , all.y = F)
      
      
      reason$par2 <- as.character(paste0(round(as.numeric(reason$par)* reason$Multiplier , reason$Decimals)," ",reason$units))
      
      reason$text <- ""
      
      reason$text[reason$value >= reason$Moderate.Point & !is.na(reason$Moderate.Point) & !grepl('female',tolower(reason$variable))  ] <- paste0(reason$Moderate[reason$value >= reason$Moderate.Point  & !is.na(reason$Moderate.Point) & !grepl('female',tolower(reason$variable)) ],"(",reason$var_name[reason$value >= reason$Moderate.Point & !is.na(reason$Moderate.Point) & !grepl('female',tolower(reason$variable)) ]," is ",reason$par2[reason$value >= reason$Moderate.Point & !is.na(reason$Moderate.Point) & !grepl('female',tolower(reason$variable)) ] ,")")
      
      reason$text[reason$value >= reason$High.Point & !is.na(reason$High.Point) & !grepl('female',tolower(reason$variable))] <- paste0(reason$High[reason$value >= reason$High.Point & !is.na(reason$High.Point) & !grepl('female',tolower(reason$variable))],"(",reason$var_name[reason$value >= reason$High.Point & !is.na(reason$High.Point)& !grepl('female',tolower(reason$variable))]," is ",reason$par2[reason$value >= reason$High.Point & !is.na(reason$High.Point)& !grepl('female',tolower(reason$variable))] ,")")
      
      reason$text[reason$value >= reason$High.Point & !is.na(reason$High.Point) & grepl('owned',tolower(reason$variable))] <- paste0(reason$High[reason$value >= reason$High.Point & !is.na(reason$High.Point) & grepl('owned',tolower(reason$variable))])
      reason <- reason[!is.na(reason$par),]
      reason$text[reason$par == 0 & nchar(reason$NotAvailable) > 1 & !is.na(reason$NotAvailable)] <- reason$NotAvailable[reason$par == 0 & nchar(reason$NotAvailable) > 1 & !is.na(reason$NotAvailable) ]
      
      reason$notes[reason$par == 0 & nchar(reason$NotAvailable) > 1 & !is.na(reason$NotAvailable)] <- reason$NotAvailable[reason$par == 0 & nchar(reason$NotAvailable) > 1 & !is.na(reason$NotAvailable)] 
      
      reason <- reason[!is.na(reason$var_name) , ] 
      reason$variable <- reason$var_name
      reason$var_name <-NULL
      
      
      reason_bad <- as.data.frame(reason[nchar(reason$text)>2, ] , stringsAsFactors=FALSE )
      reason_bad <- subset(reason_bad, !grepl('domain', tolower(reason_bad[ , 1])))
      reason_count <- 4
      if(data$unique_enquiries_last6[1] <=1){
        reason_bad <- subset(reason_bad, !grepl('enquiry', tolower(reason_bad[ , 1])))
        reason_count <- 3
      }
      
      if(length(rownames(reason_bad))>0){
        
        
        reason_bad$type <- "High"
        reason_bad$type[reason_bad$value <=0.6] <- "Moderate"
        reason_bad <- reason_bad[order(reason_bad$contribution, decreasing = T),]
        reason_bad <- reason_bad[order(reason_bad$type, decreasing = F), ]
        reason_bad <- reason_bad[!duplicated(reason_bad[,1]), ]
        
        reason_bad$reason <- paste0(reason_bad$text )
        
        n <- min(reason_count, length(rownames(reason_bad)))
        
        reason_bad <- reason_bad[1:n, ]
        
        reason_bad <- as.list(as.data.frame( reason_bad$reason[!is.na(reason_bad$variable)] , stringsAsFactors=FALSE  ))
        
        notes <- as.list(as.data.frame( reason$notes[!is.na(reason$notes)] , stringsAsFactors=FALSE  ))
        
        
      }
    }
    ############### Formatting final output ##############
    
    data$gen2ScoreWeight <- ifelse(data$Bureau_thickness_gen2 == THICKNESS_THIN, 0.85, 0.6)
    data$experianScoreWeight <- 1 - data$gen2ScoreWeight
    
    data$mean_band <- ifelse(data$Bureau_thickness_gen2 == THICKNESS_NO_HIT, data$final_new_score_risk_band_thin,round( as.numeric(data$ExperianScoreBand)*data$experianScoreWeight +  data$gen2ScoreWeight*as.numeric(data$Gen2_RiskBand)))
    
    if(data$Bureau_thickness_gen2 == THICKNESS_NO_HIT){
      data$Gen2Score <- round(data[ , c("final_new_score_thin")]/10,2)
      data$Gen2_RiskBand <- data[ , c("final_new_score_risk_band_thin")]
    }
    
    mean_band <- data$mean_band
    
    final <- data[1 , c("Bureau_thickness_gen2", "Gen2Score" , "Gen2_RiskBand" , "pin" , "ExperianScoreBand", "mean_band", "Tier", "ExtraPricing")]
    colnames(final) <- c("file_type" , "score" , "score_band" , "pin" ,"ExperianScoreBand", "mean_band", "Tier", "ExtraPricing")
    
    
    final2 <- final
    
    location_tier <- data[ 1 , "Tier"]
    is_tier4 <- F
    
    tryCatch({
      if(location_tier == "Tier4" & !is.na(data$anchor_name[1]) & !grepl("online", data$anchor_name[1], ignore.case = T) & !grepl("direct", data$anchor_name[1], ignore.case = T) & !grepl("paisabazaar", data$anchor_name[1], ignore.case = T)){
        location_tier <- "Tier3"
        is_tier4 <- T
      }
    }, error = function(e){})
    
    
    model_decision_with_tier_row <- model_decision_with_tier[model_decision_with_tier$ScoreBand == mean_band &
                                                               model_decision_with_tier$BureauThickness == data[ 1 , c("Bureau_thickness_gen2")] & 
                                                               model_decision_with_tier$Tier== location_tier, ]
    
    
    if(is_tier4){
      model_decision_with_tier_row$model_decision[model_decision_with_tier_row$model_decision==MODEL_DECISION_APPROVE] <- MODEL_DECISION_MANUAL_REVIEW
    }
    
    if(nrow(model_decision_with_tier_row) == 1){
      fast_loan_parameters$model_decision <- model_decision_with_tier_row$model_decision
      final$pricing <- as.numeric(model_decision_with_tier_row$pricing) + as.numeric(final$ExtraPricing)
      final$MaxTenor <- model_decision_with_tier_row$MaxTenor
      final$loan_cap <- model_decision_with_tier_row$loan_cap
      final$model_decision <- model_decision_with_tier_row$model_decision
    }else{
      final$pricing <- NA
      final$MaxTenor <- NA
      final$loan_cap <- NA
      final$model_decision <- "Decline"
    }
    final2 <- final
    
    final$FinalDecision <- final$model_decision
    final2$FinalDecision[1] <- final$model_decision[1]
    
    final_response$tier <- final2$Tier
    final_response$tier_key <- list(name = "Tier" , value = final2$Tier )
    final_response$Experian <- data$ExperianScore[1]
    final_response$Experian_Band <- data$ExperianScoreBand
    final_response$Gen2Score <- data$Gen2Score[1]
    final_response$Gen2RiskBand <- data$Gen2_RiskBand
    final_response$pincode <- final$pin
    
    final_response$score <- round(data[ 1 , c("Gen2Score")],2)
    final_response$score_band$value = mean_band
    final_response$file_type$value=data$Bureau_thickness_gen2[1]
    final_response$model_decision$value = final2$FinalDecision[1]
    if(final2$FinalDecision[1] != MODEL_DECISION_DECLINE){
      final_response$pricing$value = paste0(final2$pricing, "%")
      final_response$MaxTenor$value =final2$MaxTenor
      final_response$loan_cap <-  final2$loan_cap
    }
    
    #### Zomato Band Output added if present
    if(!is.null(data$vertical_risk_profile_band  )){
      data$vertical_risk_profile_band <- as.numeric(data$vertical_risk_profile_band)
      if(!is.na(data$vertical_risk_profile_band[1])){
        final_response$restaurant_mortality_band <- list(name = "Restaurant Mortality Band", value=data$vertical_risk_profile_band[1])
      }
    }
    
    
    if( final2$model_decision != MODEL_DECISION_APPROVE){
      final_response$model_reasons <- reason_bad
      final_response$model_reasons <- final_response$model_reasons[[1]]
      if(final_response$tier == "Tier4"){
        final_response$model_reasons[length(final_response$model_reasons)+1] <- "Application is from Tier 4"
      }
    }
    
    final_response$parameters <- par
    final_response$pincode <- data$pin
    final_response$statements <- as.list( c(paste0("Cibil Report used in score card calculation has a cibil score of ", data$cibil_score, " & was pulled on " , format.Date(data$date_pull , format = "%d-%b-%y"))   ) )
    
    
    final_response$notes <- list()
    if(length(notes) > 0){
      final_response$notes <- notes
      final_response$notes <- list(final_response$notes[[1]])
    }
    
    if(final2$FinalDecision[1] != "Decline"){
      final_response$notes <- c(final_response$notes, "Please decrease pricing by 2% for escrow accounts and by another 1% in case of end use control, subject to a floor of 16% pricing")
      
    }
    data$has_pan_issue[is.na(data$has_pan_issue)]<-0
    if(as.numeric(data$has_pan_issue[1])==1){
      final_response$notes <- c(final_response$notes, data$pan_note[1])
    }
    
    
    data$average_anchor_monthly_txns[is.na(data$average_anchor_monthly_txns) | 
                                       data$average_anchor_monthly_txns==0] <-
      data$average_anchor_monthly_txns_all_time[is.na(data$average_anchor_monthly_txns) | 
                                                  data$average_anchor_monthly_txns==0]
    data$total_months[is.na(data$total_months) | is.null(data$total_months)] <- 12
    
    ############### Horizontal and Vertical Line ###################
    tryCatch({
      if(final2$model_decision != "Decline" & data$Bureau_thickness_gen2 != THICKNESS_NO_HIT){
        source("Functions/9_Function_Line_Calculation.R")
        
        
        if((is.na(data$average_eod_balance_6_months) & is.na(data$average_od_eod_balance_6_months))|
           (data$average_eod_balance_6_months==0 & data$average_od_eod_balance_6_months==0)){
          data$average_eod_balance_6_months <- data$avg_eod_bank_balance
          data$average_od_eod_balance_6_months <- data$avg_eod_bank_balance
        }
        
        pricing <- final2$pricing
        maxTenor <- as.numeric(gsub("[^0-9.-]","",final2$MaxTenor))
        
        horizontal_line_result <- get_horizontal_line(data,
                                                      existing_EMI_obligation,
                                                      pricing,
                                                      maxTenor,
                                                      final_response$loan_cap)
        
        horizontal_line_parameters <- horizontal_line_result[[3]]
        horizontal_line_parameters$total_od_limit <- data$total_od_limit
        final_response$horizontal_line_parameters <- horizontal_line_parameters
        
        industry_details <- get_industry(data, F)
        
        vertical_line_result <- get_vertical_line(data,
                                                  industry_details[1],
                                                  industry_details[2],
                                                  pricing,
                                                  final_response$MaxTenor$value)
        
        fast_loan_line_result <- get_fast_loan_line(data,
                                                    pricing,
                                                    maxTenor)
        
        industry_details_cibil_anchor <- get_industry(data, T)
        
        cibil_anchor_line_result <- get_cibil_anchor_line(data,
                                                          industry_details_cibil_anchor[1],
                                                          industry_details_cibil_anchor[2],
                                                          pricing,
                                                          fast_loan_line_result[[1]])
        
        instant_credit_line_result <- get_instant_credit_line_result(data)
        
        instant_credit_parameters <- modifyList(instant_credit_parameters, instant_credit_line_result, keep.null = T)
        #instant_credit_parameters$model_decision <- instant_credit_line_result[[1]]
        #instant_credit_parameters$instant_credit_line <- instant_credit_line_result[[2]]
        
        fast_loan_parameters$fast_loan_line <- fast_loan_line_result[[1]]
        fast_loan_parameters$account_type <- fast_loan_line_result[[2]]
        fast_loan_parameters$final_account_type <- fast_loan_line_result[[3]]
        fast_loan_parameters$account_opened <- fast_loan_line_result[[4]]
        fast_loan_parameters$years_from_today <- fast_loan_line_result[[5]]
        fast_loan_parameters$multiplying_factor <- fast_loan_line_result[[6]]
        fast_loan_parameters$loan_cap <- fast_loan_line_result[[7]]
        fast_loan_parameters$adjusted_highest_amount_borrowed <- fast_loan_line_result[[8]]
        fast_loan_parameters$is_amount_acceptable <- fast_loan_line_result[[9]]
        fast_loan_parameters$fast_loan_line_without_applied_cap <- fast_loan_line_result[[10]]
        fast_loan_parameters$fast_loan_emi <- fast_loan_line_result[[11]]
        fast_loan_parameters$cibil_anchor_line_amount <- cibil_anchor_line_result[[1]]
        fast_loan_parameters$cibil_anchor_line_tenure <- cibil_anchor_line_result[[2]]
        fast_loan_parameters$cibil_anchor_line_pricing <- cibil_anchor_line_result[[3]]
        fast_loan_parameters$cibil_anchor_line_amount_without_cap <- cibil_anchor_line_result[[4]]
        fast_loan_parameters$cibil_anchor_line_split_payment_percent <- cibil_anchor_line_result[[5]]
        fast_loan_parameters$cibil_anchor_line_emi <- cibil_anchor_line_result[[6]]
        fast_loan_parameters$applied_amount <- data$loan_amount
        fast_loan_pricing <- max(pricing, 24, na.rm = T)
        fast_loan_max_tenor <- maxTenor
        fast_loan_line <- as.numeric(fast_loan_line_result[[1]])
        if(!is.na(fast_loan_line) & fast_loan_line <= 50000){
          fast_loan_pricing <- max(fast_loan_pricing, 26, na.rm = T)
          fast_loan_max_tenor <- 6
        }else if(!is.na(fast_loan_line) & fast_loan_line <= 100000){
          fast_loan_pricing <- max(fast_loan_pricing, 25, na.rm = T)
          fast_loan_max_tenor <- 12
        }
        fast_loan_parameters$pricing <- fast_loan_pricing
        fast_loan_parameters$MaxTenor <- fast_loan_max_tenor
        
        
        
        vertical_line_parameters <- vertical_line_result[[5]]
        vertical_line_parameters$anchor_name <- data$anchor_name
        vertical_line_parameters$industry <- data$industry
        vertical_line_parameters$average_anchor_monthly_txns <- data$average_anchor_monthly_txns
        vertical_line_parameters$segment_category_id <- data$segment_category_id
        vertical_line_parameters$account_opened_180 <- data$account_opened_180
        #vertical_line_parameters$account_opened_180 <- data$account_opened_180
        final_response$vertical_line_parameters <- vertical_line_parameters
        
        existing_EMI_obligation <- round(existing_EMI_obligation)
        existing_EMI_obligation[is.na(existing_EMI_obligation)] <- "Not available"
        existing_EMI_obligation[existing_EMI_obligation!="Not available"] <- paste("Rs.",existing_EMI_obligation[existing_EMI_obligation!="Not available"])
        
        
        line_result <- c(horizontal_line_result[[1]], 
                         vertical_line_result[[1]],
                         vertical_line_result[[3]],
                         vertical_line_result[[4]],
                         fast_loan_line_result[[1]],
                         cibil_anchor_line_result[[1]],
                         cibil_anchor_line_result[[2]],
                         cibil_anchor_line_result[[3]],
                         existing_EMI_obligation)
        line_parameter_names <- c("Horizontal Line",
                                  "Vertical Line", "Eligible for OD", "Vertical Max Tenor",
                                  "CIBIL Line",
                                  "CIBIL Anchor Line", "CIBIL Anchor Tenure", "Cibil Anchor Pricing",
                                  "Existing EMI obligation")
        
        line_reasons <- c(paste("Horizontal Line Reason :" , horizontal_line_result[[2]]),
                          paste("Vertical Line Reason :", vertical_line_result[[2]]))
        
        for(i in 1:length(line_parameter_names)){
          line_parameter_value <- line_result[i]
          if(!(is.na(line_parameter_value) | line_parameter_value=="") ){
            line_parameter_name <- line_parameter_names[i]
            line_parameter_list <- list(name=line_parameter_name, value=line_parameter_value)
            final_response[[line_parameter_name]] <- line_parameter_list
          }
          
        }
        
        if(is.null(final_response$statements)){
          final_response$statements <- c()
        }
        
        final_response$statements <- c(final_response$statements, line_reasons)
      }
      
    }, error=function(e){})
    
    ############### Qualification Criteria ###################
    
    tryCatch({
      source("Functions/10_Qualification_Criteria_Calculation.R")
      if(is.null(final_response$notes)){
        final_response$notes <- c()
      }
      industry_details <- get_industry(data, F)
      qualification_criteria_result_list <- get_qualification_criteria(data,
                                                                       industry_details[1],
                                                                       final_response[["Vertical Line"]]$value)
      bad_criteria_count <- length(qualification_criteria_result_list$v1)
      qualification_criteria_result_list$v1 <- qualification_criteria_result_list$v1[qualification_criteria_result_list$v1 != ""]
      qualification_criteria_result <- c(qualification_criteria_result_list$v1, qualification_criteria_result_list$v2)
      
      fast_loan_qualification_criteria_result_list <- get_qualification_criteria_fast_loan(data)
      
      fast_loan_bad_criteria_count <- length(fast_loan_qualification_criteria_result_list$v1)
      if(fast_loan_bad_criteria_count > 0 & fast_loan_parameters$model_decision == MODEL_DECISION_APPROVE){
        fast_loan_parameters$model_decision <- MODEL_DECISION_MANUAL_REVIEW
      }
      
      final_response$qualifying_criteria <- list()
      
      if(length(qualification_criteria_result)==1){
        final_response$qualifying_criteria <- list(qualification_criteria_result)
      }else if(!is.na(qualification_criteria_result)){
        final_response$qualifying_criteria <- qualification_criteria_result
      }
      
      if(bad_criteria_count > 0 & final_response$model_decision$value == MODEL_DECISION_APPROVE){
        final_response$model_decision$value <- MODEL_DECISION_MANUAL_REVIEW
        final_response$model_reasons <- list(paste(qualification_criteria_result_list$v1, collapse = " , "))
      }
      
    }, error=function(e){})
    
    final_response$fast_loan_parameters <- fast_loan_parameters
    final_response$instant_credit_parameters <- instant_credit_parameters
    final_response<- toJSON(as.list(final_response), auto_unbox = T, pretty = T)
    
    return(final_response)
    
  },error=function(e){
    return(get_standard_error_message("Score could not be computed", paste(unlist(e), collapse = " -> ")))
  })
    
    
    
}

