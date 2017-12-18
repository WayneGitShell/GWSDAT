
# This will cause trouble inside the package (move to inst/application )
library(GWSDAT)


vargs <- commandArgs(TRUE)

if (length(vargs) == 3) {

  # load the data from temp file
  
  n_spline_knots <- as.integer(vargs[1])
  infile <- vargs[2]
  outfile <- vargs[3]
  cat("n_spline_knots: ", n_spline_knots, "\n")
  csite <- readRDS(infile)
  csite$GWSDAT_Options[['PSplineVars']][['nseg']] <- n_spline_knots


# For testing (comment out above code):
#default_session_file <- 'GWSDAT_Examples.rds'
#infile <- system.file("extdata", default_session_file, package = "GWSDAT")
#csite_list <- readRDS(infile)
#csite <- csite_list[[1]]
#csite$GWSDAT_Options[['PSplineVars']][['nseg']] <- 10

  fitdat <- fitData(csite$All.Data, csite$GWSDAT_Options, showProgress = FALSE)

  saveRDS(fitdat, file = outfile)
}
