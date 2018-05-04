##########################  To Extract Enquiries from Json into Columns ########
##########   enquiry column needed from cibil_analysis Table

Get_Identity <- function( cibil_analysis){
 
  
  len <- length(rownames(cibil_analysis))
  
  for( i in 1:len){
    #print(i)
    if(!is.na(cibil_analysis$identity[i])){  
      t <- fromJSON(cibil_analysis$identity[i])
      
      for(k in 1: length(rownames(t))){
        if(t$id_type[k] %in% colnames(cibil_analysis)){
          cibil_analysis[[as.character(t$id_type[k])]][i] <- t$id_number[k]
        }else{
          cibil_analysis[[as.character(t$id_type[k])]] <- "-"
          cibil_analysis[[as.character(t$id_type[k])]][i] <- t$id_number[k]
        }
      }
      
    }
    
  }
  
  

  
return(cibil_analysis)
}