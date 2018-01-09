
# This will cause trouble inside the package (move to inst/application )
library(GWSDAT)


vargs <- commandArgs(TRUE)

if (length(vargs) == 4) {

  # load the data from temp file
  
  n_spline_segs <- as.integer(vargs[1])
  data_id <- as.integer(vargs[2])
  infile <- vargs[3]
  outfile <- vargs[4]
  
  cat("n_spline_segments: ", n_spline_segs, "\n")
  
  csite <- readRDS(infile)
  csite$GWSDAT_Options[['PSplineVars']][['nseg']] <- n_spline_segs

  # Do the fitting..  
  fitdat <- fitData(csite$All.Data, csite$GWSDAT_Options, showProgress = FALSE)

  # Construct parameter set to be read by the server.
  params = list(PSplineVars = list(nseg = n_spline_segs))
  
  results <- list(fitdat = fitdat, data_id = data_id, params = params)
  
  saveRDS(results, file = outfile)
}
