##########################  To Extract Enquiries from Json into Columns ########
##########   enquiry column needed from cibil_analysis Table

Get_Enquiries <- function( cibil_analysis){
 
  cibil_analysis$number_of_enquiries <- nchar(cibil_analysis$enquiries) - nchar(gsub("}","",cibil_analysis$enquiries))
  
  max_enq <- max(cibil_analysis$number_of_enquiries[!is.na(cibil_analysis$number_of_enquiries)])
  
  max_enq <- min(5 , max_enq)
  
  for( i in 1 : max_enq){
    col <- paste0("enq_amt_",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("enq_purp",i)
    cibil_analysis[[col]] <- "-"
    col <- paste0("enq_date",i)
    cibil_analysis[[col]] <- "-"
  }
  
  len <- length(rownames(cibil_analysis))
  i=1
  for( i in 1:len){
    #print(i)
    if(!is.na(cibil_analysis$enquiries[i])){  
      t <- fromJSON(cibil_analysis$enquiries[i])
      t_len <- min(max_enq,length(rownames(t)))
      
      for( j in 1:t_len){
        
        tryCatch({
          col <- paste0("enq_amt_",j)
          cibil_analysis[[col]][i] <- t$amount[j]
        },error = function(e){})
        
        tryCatch({
          col <- paste0("enq_purp",j)
          cibil_analysis[[col]][i] <- t$purpose[j]
        },error = function(e){})
        
        tryCatch({
          col <- paste0("enq_date",j)
          cibil_analysis[[col]][i] <- t$enquiry_date[j]
        },error = function(e){})
        
        
      }
    }
    
  }
  
  for( i in 1 : max_enq){
    col <- paste0("enq_date",i)
    cibil_analysis[ , col] <-  as.Date(cibil_analysis[ , col ] , format = "%d-%m-%Y")
  }
  
  
  
  
return(cibil_analysis)
}