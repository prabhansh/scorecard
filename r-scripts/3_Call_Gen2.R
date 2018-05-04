get_result <- function(){
    tryCatch({
        args = commandArgs(trailingOnly = TRUE)
        suppressMessages(library("jsonlite"))
        RsciptFilePath <- Sys.getenv("RSCRIPTFILEPATH")
        if (Sys.getenv("ENVTYPE") == "local") {
            RsciptFilePath <- "."
        }
        if (RsciptFilePath == "") {
            ## As per production server
            RsciptFilePath <- "/deployments/automated_deployment/indifi_source/arya/r-scripts"
        }
        suppressMessages(setwd(RsciptFilePath))
        suppressMessages(source("2_Gen2Score_Function.R"))
        if (length(args) == 2) {
            writeLines(suppressWarnings(Gen2(args)))
        }
    }, error = function(e){

        final <- toJSON(list(result = FALSE , error_message = "Score could not be computed", error_details = paste(unlist(e), collapse = " -> ")), auto_unbox = T , pretty = T)
        writeLines(final)
    }
    )
}

suppressMessages(get_result())
