
# This will cause trouble inside the package (move to inst/application )
library(GWSDAT)

if (!require(DBI))
  stop("Missing the DBI package. Please install it.")

if (!require(RSQLite))
  stop("Missing the RSQLite package. Please install it.")

jobtype = 'pspline_fit.R' # Necessary to identify the result evaluation procedure 
                          # inside the main app. 
                          #FIXME: Would be better, though, to pass this information from the main app (FIXME). 


vargs <- commandArgs(TRUE)

if (length(vargs) == 5) {

  # load the data from temp file
  job_id <- as.integer(vargs[1])
  data_id <- as.integer(vargs[2])
  infile <- vargs[3]
  outfile <- vargs[4]
  dbPath <- vargs[5]
  
  
  cat("infile:  ", infile, "\n")
  cat("outfile: ", outfile, "\n")
  
  csite <- readRDS(infile)

  cat("n_spline_knots: ", csite$GWSDAT_Options[['PSplineVars']][['nseg']], "\n")
  
  fitdat <- fitData(csite$All.Data, csite$GWSDAT_Options, showProgress = FALSE)

  saveRDS(fitdat, file = outfile)
  
  # Write to DB: Enter job into 'done' table and remove from 'running' table.
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbPath)
  
  # Create new record for 'done' table.
  dt <- data.frame('job_id' = job_id, 'data_id' = data_id,'job_type' = jobtype, 'evaluated' = 0, 
                   'outputfile' = outfile, stringsAsFactors = FALSE)
  
  print.table(dt)
  
  dbWriteTable(con, 'done', dt, append = TRUE)
  
  # Delete job from 'running' table using the 'job_id'.
  SQLcmd <- paste0('DELETE FROM running WHERE job_id=\'', job_id, '\';')
  rs <- dbSendQuery(con, SQLcmd)
  dbClearResult(rs)  
  
  dbDisconnect(con)
  
} else {
  
  stop("Wrong number of arguments specified.")
}
