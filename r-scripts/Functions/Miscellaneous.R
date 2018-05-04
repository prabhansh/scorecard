get_standard_error_message <- function(error_message, error_details=""){
  return(toJSON(list(result = FALSE , error_message = c(error_message), error_details = error_details), auto_unbox = T , pretty = T))
}

get_industry <- function(data, is_fast_loan){
  
  business_nature <- data$business_nature
  margin_industry <- data$margin_industry
  margin_sub_industry <- data$margin_sub_industry
  anchor_name <- data$anchor_name
  industry <- data$industry
  if(is_fast_loan){
    monthly_anchor_ts <- data$fast_loan_anchor_data
    business_category <- data$fast_loan_business_category
  } else{
    monthly_anchor_ts <- data$average_anchor_monthly_txns
    business_category <- data$segment_category_id
  }
  
  if(is.null(anchor_name) | length(anchor_name)==0 | is.na(anchor_name)){
    anchor_name <- ""
  }
  if(is.null(industry) | length(industry)==0| is.na(industry)){
    industry <- ""
  }
  if(is.null(monthly_anchor_ts) | length(monthly_anchor_ts)==0){
    monthly_anchor_ts <- 0
  }
  if(is.null(business_category) | length(business_category)==0){
    business_category <- -1
  }
  anchor_name <- tolower(as.character(anchor_name))
  industry <- tolower(as.character(industry))
  monthly_anchor_ts <- as.numeric(as.character(monthly_anchor_ts))
  
  if (grepl("makemytrip",anchor_name)){
    industry <- "mmt"
  }
  if (grepl("peelworks",anchor_name)){
    industry <- "peelworks"
  }
  if (grepl("yatra",anchor_name)){
    industry <- "yatra"
  }
  if (grepl("firstdata", anchor_name) | grepl("pinelabs" , anchor_name)){
    
    mca_new_margin <- read.csv("mca_margins.csv", stringsAsFactors = F)
    if(is.null(business_nature)){
      business_nature <- ""
    }
    if(is.null(margin_industry)){
      margin_industry <- ""
    }
    if(is.null(margin_sub_industry)){
      margin_sub_industry <- ""
    }
    
    business_nature[is.na(business_nature)] <- ""
    margin_industry[is.na(margin_industry)] <- ""
    margin_sub_industry[is.na(margin_sub_industry)] <- ""
    
    business_nature <- tolower(as.character(business_nature))
    margin_industry <- tolower(as.character(margin_industry))
    margin_sub_industry <- tolower(as.character(margin_sub_industry))
    
    temp_mca_new_margin <- mca_new_margin[
      mca_new_margin$BusinessNature==business_nature & 
        mca_new_margin$Industry==margin_industry & 
        mca_new_margin$SubIndustry==margin_sub_industry,]
    
    if(nrow(temp_mca_new_margin) ==0){
      temp_mca_new_margin <- mca_new_margin[
        mca_new_margin$BusinessNature==business_nature & 
          mca_new_margin$Industry==margin_industry & 
          mca_new_margin$SubIndustry=="",]
    }
    
    if(nrow(temp_mca_new_margin) ==0){
      temp_mca_new_margin <- mca_new_margin[
        mca_new_margin$BusinessNature==business_nature & 
          mca_new_margin$Industry=="" & 
          mca_new_margin$SubIndustry=="",]
    }
    
    category_margin <- read.csv("category_margin.csv", stringsAsFactors = F)
    anchor_ts_margin <- 0.1
    business_category_id <- as.numeric(as.character(business_category))
    if(!is.na(business_category_id)){
    category_margin$SegmentID <- as.numeric(category_margin$SegmentID)
    category_margin <- category_margin[category_margin$SegmentID==business_category_id,]
    if (nrow(category_margin) > 0){
      anchor_ts_margin <- category_margin$Margin[1]
    }}
    else{
      business_category <- tolower(as.character(business_category))
      category_margin$Segment <- as.character(category_margin$Segment)
      category_margin <- category_margin[category_margin$Segment==business_category,]
      if (nrow(category_margin) > 0){
        anchor_ts_margin <- category_margin$Margin[1]
      }
    }
    
    if(nrow(temp_mca_new_margin) >=1){
      anchor_ts_margin <- as.numeric(as.character(temp_mca_new_margin$Margin[1]))
    }
    
    #print(c("for mca",business_nature , margin_industry, margin_sub_industry,anchor_ts_margin))
    
    monthly_anchor_ts <- monthly_anchor_ts*anchor_ts_margin
    
    industry <- "mca"
  }
  if (grepl("dtdc",anchor_name)){
    industry <- "dtdc"
  }
  if (grepl("firstcry",anchor_name)){
    industry <- "firstcry"
  }
  if (grepl("food",industry)){
    industry <- "restaurant"
  }
  if (grepl("e-commerce", industry)){
    industry <- "ecommerce"
  }
  return(c(industry,monthly_anchor_ts))
}