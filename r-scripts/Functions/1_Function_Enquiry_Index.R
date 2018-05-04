#############################  Inquiry Index ###################
##########   enquiries column needed from cibil_analysis Table
#cibil_analysis <- backup
library(plyr)
risk_weight <- read.csv("Enquiry Weight.csv")
risk_weight <- risk_weight[!duplicated(risk_weight$purpose),]

Enquiry_Index <- function( cibil_analysis){

len <- length(rownames(cibil_analysis))
cibil_analysis$enquiry_index  <- 0 
cibil_analysis$enquiries_purpose <- "-"
cibil_analysis$days_since_last_enquiry <- "-"
cibil_analysis$enquiries_last_12 <- "-"
cibil_analysis$enquiries_last_6 <- "-"
cibil_analysis$unique_enquiries_last6 <- 0

for( i in 1:len){
  #print(i)
  if(!is.na(cibil_analysis$enquiries[i] )){  
    t <- fromJSON(cibil_analysis$enquiries[i])
    if(length(t)>0){
      
    if(c("formatted_enquiry_date") %in% names(t)){
      t$formatted_enquiry_date <- as.Date(substr(t$formatted_enquiry_date,1,10)) 
    }else{
      t$formatted_enquiry_date <- as.Date(t$enquiry_date, format = "%d-%m-%Y")
    }
    t <-  t[order(t$formatted_enquiry_date, decreasing = T),]
    t$month <- as.Date(paste0(substr(t$formatted_enquiry_date,1,7),"-01"))
    
    d12 <- as.Date(cibil_analysis$date_pull[i]) - 365
    t12 <- subset(t , t$formatted_enquiry_date > d12 )
    d6 <- as.Date(cibil_analysis$date_pull[i]) - 182
    t6 <- subset(t , t$formatted_enquiry_date > d6 )
    cibil_analysis$days_since_last_enquiry[i] <- as.numeric( as.Date(cibil_analysis$date_pull[i]) - as.Date(t$formatted_enquiry_date[1])  )
    
    cibil_analysis$enquiries_last_12[i] <- length(rownames(t12))
    
    cibil_analysis$enquiries_last_6[i] <-  length(rownames(t6))
        
    if(c("purpose") %in% names(t)){
      cibil_analysis$unique_enquiries_last6[i] <- nrow(unique(t[t$month >  d6 , c("purpose","month") ]))
      t <- merge(t , risk_weight , by.x ="purpose" , by.y = "purpose" , all.x = T , all.y = F)
      t <- t[!duplicated(t[ , c( "purpose" ,"month")]),]
      
      }else{
      t$risk_weight <- 10
    }
    
    date <- as.Date(substr(cibil_analysis$date_of_pull[i],1,10))
    if(is.na(date)){
      date <- as.Date(cibil_analysis$cibil_created[i])
    }
    t$days_last_enquiry <- as.numeric(date - t$formatted_enquiry_date+1)
    t$days_last_enquiry[ t$days_last_enquiry <30] <- 30
    
    t$index <- 0
    t$index[!is.na(t$days_last_enquiry)] <- ( t$risk_weight[!is.na(t$days_last_enquiry)] / sqrt(t$days_last_enquiry[!is.na(t$days_last_enquiry)]) )
    cibil_analysis$enquiry_index[i] <- sum(t$index[!is.na(t$index)])
    cibil_analysis$enquiries_purpose[i] <- paste(paste0(t$purpose,"_",t$formatted_enquiry_date), collapse = "|")
    }else{
      cibil_analysis$enquiry_index[i] <- 0
      cibil_analysis$enquiries_purpose[i] <- "NA"   
    }
  }else{
    cibil_analysis$enquiry_index[i] <- 0
    cibil_analysis$enquiries_purpose[i] <- "NA"
  }
}
return(cibil_analysis)
}