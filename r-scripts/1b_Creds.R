get_creds_from_host_string <- function(x){

	db_creds <- list()
	db_creds$port <- 5432
	x <- substr(x, 12, nchar(x))
	x1 <- unlist(strsplit(x, ":"))
	x2 <- unlist(strsplit(x1[2], "@"))
	db_creds$host <- x2[2]
	db_creds$username <-  x1[1]
	db_creds$password <- x2[1]

	return(db_creds)
}