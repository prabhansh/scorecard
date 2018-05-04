interest_map <- read.csv("Interest_Rate_2018_02.csv", stringsAsFactors = F, check.names = F)
escrow_adjust <- read.csv("Interest_Rate_2018_02_Escrow_Adjustment.csv", stringsAsFactors = F, check.names = F)

colnames(interest_map) <- tolower(colnames(interest_map))
#risk_band <- 1
#tier <- "Tier1"
#amount <- 43378
#tenure <- 12
#industry <- "Retail"
#thickness <- "No Hit"

get_interest_rate <- function(risk_band , thickness , tier , amount , tenure , industry ){
#print(c(risk_band , thickness , tier , amount , tenure , industry))
interest_map_select <- interest_map[ tolower(interest_map$industry) == tolower(industry)  ,  ]
if(nrow(interest_map_select)<=0){
  interest_map_select <- interest_map[ tolower(interest_map$industry) == tolower("default")  ,  ]
  
}

min_select <- interest_map_select[ interest_map_select$risk_band == "Min Interest Rate"  & !is.na(interest_map_select$risk_band)  , ] 
min_select[is.na(min_select)] <- 0

if(tolower(thickness) == "no hit"){
  risk_band <- NA
  interest_rb <-  interest_map_select[ is.na(interest_map_select$risk_band) , "no_hit"] 
  interest_map_select <-  interest_map_select[ is.na(interest_map_select$risk_band) , ]
  interest_col <-  "no_hit"
  
  }

if(!is.na(risk_band)){
interest_rb <-  interest_map_select[ interest_map_select$risk_band==risk_band & !is.na(interest_map_select$risk_band) , tolower(thickness)]
interest_map_select <-  interest_map_select[ interest_map_select$risk_band==risk_band & !is.na(interest_map_select$risk_band) , ]
interest_col <-  tolower(thickness)
}

if(is.null(interest_rb)){
  return("NA")
}

if(is.na(interest_rb)){
  return("NA")
}

amt <- "3-5l"
amt[amount <= 300000 ] <- "0-3l"
amt[amount > 500000 & amount <= 1000000 ] <- "5-10l"
amt[amount > 1000000 & amount <= 2000000 ] <- "10-20l"
amt[amount > 2000000 ] <- "20l+"

tnre <- "0-6m"
tnre[tenure>6 & tenure<=12] <- "6-12m"
tnre[tenure>12 & tenure<=18] <- "12-18m"
tnre[tenure>18 & tenure<=24] <- "18-24m"
tnre[tenure>24] <- "24m+"

escrow_adjust <- escrow_adjust[tolower(escrow_adjust$Industry)== tolower(industry) , ]
if(nrow(escrow_adjust) > 0 ){
  reduce <- escrow_adjust$Reduce[1]
  escrow_min <- escrow_adjust$Min_Interest[1]
  escrow_max <- escrow_adjust$Max_Interest[1]
}else{
  reduce <-0
  escrow_min <- 16
  escrow_max <- 29
}

interest_col <- c(interest_col, tolower(tier), amt , tnre)
min_select <- min_select[ interest_col]
min_interest <- max(min_select)
interest_map_select <- interest_map_select[ , interest_col]
interest_final <- sum(interest_map_select) - reduce
interest_final <- max( interest_final ,min_interest)
interest_final <- max( interest_final ,escrow_min)
interest_final <- min( interest_final ,escrow_max)

return(interest_final)

}