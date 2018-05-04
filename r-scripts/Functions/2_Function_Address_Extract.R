#############################  To Extract Address from Json into Columns ########
##########   address column needed from cibil_analysis Table

Get_Address <- function( cibil_analysis){
  Geo_Risk <- read.csv("Geo_Risk.csv")
  Geo_Risk <- Geo_Risk[!duplicated(Geo_Risk$Pincodes), ]
  
  cibil_analysis$number_of_address <- nchar(cibil_analysis$address) - nchar(gsub("}","",cibil_analysis$address))
  cibil_analysis$location_risk_cibil <- 0
  cibil_analysis$number_of_owned_property <- 0
  max_address <- max(cibil_analysis$number_of_address[!is.na(cibil_analysis$number_of_address)])
  
  max_address <- min(5, max_address)
  cibil_analysis$cibil_owned <-0
  
  for( i in 1 : max_address){
    col <- paste0("add_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("add_cat_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("add_report_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("add_code_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("add_pin_",i)
    cibil_analysis[[col]] <- "-"
  }
  
  
  len <- length(rownames(cibil_analysis))
  for( i in 1:len){
    #print(i)
    if(!is.na(cibil_analysis$address[i])){  
      t <- fromJSON(cibil_analysis$address[i])
      t$pin <- "-"
      for(k in 1:length(rownames(t))){
        temp <- strsplit(t$address[k] , " ")
        t$pin[k] <- temp[[1]][[length(temp[[1]])]]
      }
      
     
      
      
      t <- merge(t , Geo_Risk , by.x = "pin" , by.y = "Pincodes" , all.x = T , all.y = F )
      cibil_analysis$location_risk_cibil[i] <- max(t$GEO_Portfolio_PL_Index[!is.na(t$GEO_Portfolio_PL_Index)])
      if("residence_code" %in% names(t)){
      cibil_analysis$number_of_owned_property[i] <- length(rownames(t[grepl( 'own' , tolower(t$residence_code)) & ! is.na(t$residence_code) , ]))
      if(length(rownames(t[grepl( 'own' , tolower(t$residence_code)) & ! is.na(t$residence_code) , ])) > 0){
      cibil_analysis$cibil_owned[i] <-1
      }
      }
      
      
      t_len <- min(max_address, length(rownames(t)))
      
      for( j in 1:t_len){
        tryCatch({
          col <- paste0("add_",j)
          cibil_analysis[[col]][i] <- t$address[j]
          col <- paste0("add_pin_",j)
          temp <- strsplit(t$address[j] , " ")
          cibil_analysis[[col]][i] <- temp[[1]][[length(temp[[1]])]]
          
          
          
        },error = function(e){})
        
        tryCatch({
          col <- paste0("add_cat_",j)
          cibil_analysis[[col]][i] <- t$category[j]
        },error = function(e){})
        
        tryCatch({
          col <- paste0("add_report_",j)
          cibil_analysis[[col]][i] <- t$date_reported[j]
        },error = function(e){})
        
        tryCatch({
          col <- paste0("add_code_",j)
          cibil_analysis[[col]][i] <- t$residence_code[j]
          
        },error = function(e){})
        
      }
    }
    
  }
  
  for( i in 1 : max_address){
    col <- paste0("add_report_",i)
    cibil_analysis[ , col] <-  as.Date(cibil_analysis[ , col ] , format = "%d-%m-%Y")
  }
  
  
return(cibil_analysis)
}