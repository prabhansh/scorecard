##########################  To Extract Enquiries from Json into Columns ########
##########   enquiry column needed from cibil_analysis Table

Get_Employment <- function( cibil_analysis){
 
  cibil_analysis$number_of_employment <- nchar(cibil_analysis$employment) - nchar(gsub("}","",cibil_analysis$employment))
  
  len <- length(rownames(cibil_analysis))
  
  for( i in 1:len){
    #print(i)
    if(!is.na(cibil_analysis$employment[i])){  
      
      t <- fromJSON(cibil_analysis$employment[i])
      if(!is.data.frame(t)){
      t <- fromJSON(paste0("[",cibil_analysis$employment[i],"]"))
      }
      for( l in 1 : length(colnames(t))){
      for(k in 1: length(rownames(t))){
        
        col <- paste0(as.character(colnames(t)[l]),"_",k)
        if(col %in% colnames(cibil_analysis)){
          
          cibil_analysis[[col]][i] <- t[k,l]
        }else{
          cibil_analysis[[col]] <- "-"
          cibil_analysis[[col]][i] <- t[k,l]
        }
      }
      }
      
    }
    
  }
  
  
return(cibil_analysis)
}