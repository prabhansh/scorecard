##########################  To Extract Enquiries from Json into Columns ########
##########   enquiry column needed from cibil_analysis Table

email_domain <- function( cibil_analysis){
 
  
  len <- length(rownames(cibil_analysis))
  
  for( i in 1:len){
    #print(i)
    if(!is.na(cibil_analysis$email[i])){  
      t <- fromJSON(cibil_analysis$email[i])
      t$domain <- "gmail"
      for( j in 1: length(rownames(t))){
        temp <- strsplit( t$email[j] , '@' )
        t$domain[j] <- temp[[1]][length(temp[[1]])]
        
        
      }
      
      
      
    }
    
  }
  
  

  
return(cibil_analysis)
}