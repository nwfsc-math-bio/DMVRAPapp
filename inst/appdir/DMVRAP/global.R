EXAMPLES <- list("Bev-Holt, no covariates, ER"="exampleB2ER.rav", 
                 "Bev-Holt, with covariates, Pop"="exampleB4Pop.rav")
EXAMPLENAMES <- list("exampleB2ER.rav"="BevHolt_nocovar_ER.rav", 
                     "exampleB4Pop.rav"="BevHolt_covar_Pop.rav")
source("fileupload.R")

onServer <- function() {
  serverInfo()$shinyServer
}
