get_risk_band_thick <- function(x){
  #x <- df[,NEW_SCORE_CAL]
  
  result <-      ifelse(x <= 2.45, 0,
                          ifelse(x <= 2.93, 1,
                                 ifelse(x <= 3.25, 2,
                                        ifelse(x <= 3.57, 3,
                                               ifelse(x <= 3.88, 4,
                                                      ifelse(x <= 4.21, 5,
                                                             ifelse(x<=4.6, 6,
                                                                    ifelse(x <= 5.08, 7,
                                                                           ifelse(x <= 5.64, 8,
                                                                                  ifelse(x <= 6.39, 9, 10))))))))))
                   
  
  return (result)
}
  
  
  
  
get_risk_band_thin <- function(x){
  
  result <-   ifelse(x <= 3.8, 0,
                          ifelse(x <= 4.88, 1,
                                 ifelse(x <= 5.46, 2,
                                        ifelse(x <= 5.88, 3,
                                               ifelse(x <= 6.31, 4,
                                                      ifelse(x <= 6.59, 5,
                                                             ifelse(x<=6.73, 6,
                                                                    ifelse(x <= 6.97, 7,
                                                                           ifelse(x <= 7.57, 8,
                                                                                  ifelse(x <= 8.25, 9, 10))))))))))
  return (result)
}
