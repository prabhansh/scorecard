reason_map <- read.csv("ReasonsV2.csv", stringsAsFactors = F)
reason_map<- reason_map[!duplicated(reason_map$variable),]

pincode <- read.csv("PincodeTierMapping.csv" , stringsAsFactors = F)
pincode<- pincode[!duplicated(pincode$pincode), c("pincode", "Tier" ,"ExtraPricing")]

model_decision_with_tier <- read.csv("ModelDecisionMappingWithTier.csv", stringsAsFactors = F)
model_decision_with_tier<- model_decision_with_tier[!duplicated(model_decision_with_tier[, c("ScoreBand", "BureauThickness", "Tier")]),]

final_decision <- read.csv("ExperianBand/BandMix.csv", stringsAsFactors = F)
final_decision<- final_decision[!duplicated(final_decision[, c("Gen2", "Experian")]),]

bs_reduction <- read.csv("bank_statement_reduction.csv", stringsAsFactors = F)
bs_reduction <- bs_reduction[!duplicated(bs_reduction$number_of_bank_account),]

segment_category_industry_mapping <- read.csv("segment_category_industry_mapping.csv", stringsAsFactors = F)

source("1b_Creds.R")
source("Functions/functions.R")
source("Functions/variables.R")
source("Functions/Miscellaneous.R")
source("Functions/SQL_Queries.R")
source("Functions/11_PAN_Check.R")
source("variables.R")
source("2.1_HS.R")
source("2.1_HS_PreFeb18.R")

library("RPostgreSQL")
library("jsonlite")
library("plyr")

drv <- dbDriver("PostgreSQL")
db_postgres <- "indifi"


Gen2 <- function(args){
  
  tryCatch({
  
  ###################  Error Handling ###################
  
  #### Number of arguments should be 2, request id and server host string
  if(length(args)!=2){
    return(get_standard_error_message("Invalid number of parameters sent in request"))
  }
  
  request_id <- as.numeric(args[1])
  
  if(is.null(request_id) | length(request_id)==0){
    return(get_standard_error_message("Incorrect request id for application", "Request id was null"))
  }
  
  if(is.na(request_id) | request_id < 0){
    return(get_standard_error_message("Incorrect request id for application", paste("Request id was ", request_id)))
  }
  
  ###################  DB Queries ###################
  
  host_string <- args[2]
  creds_posgres <- get_creds_from_host_string(host_string)
  con <- dbConnect(drv, dbname = db_postgres , host = creds_posgres$host , port = creds_posgres$port,  user = creds_posgres$username, password = creds_posgres$password)
  
  application <- dbGetQuery(con,  get_application_query(request_id))
  
  dbDisconnect(con)
  application$created <- as.Date(substr(application$created,1,10))
  #print(application$created[1])
    if( !is.na(application$created[1]) & as.Date(application$created[1]) >= as.Date('2018-02-15') ){ 
     # print("1")
    final_response <- HS(application, request_id)
    }else{
     # print("2")
    final_response <- HS_PreFeb18(application, request_id)
    }
    return(final_response)
    
  },error=function(e){
    return(get_standard_error_message("Score could not be computed", paste(unlist(e), collapse = " -> ")))
  })
}

