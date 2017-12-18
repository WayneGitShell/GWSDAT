
library(DBI)
library(RSQLite)

# This will cause trouble inside the package (move to inst/application )
# library(GWSDAT)

jobtype = 'modelfit_pspline'

vargs <- commandArgs(TRUE)

if (length(vargs) == 4) {
  
  # load the data from temp file
  
  max_numbers <- 20 # as.integer(vargs[1])
  jobid   <- as.integer(vargs[1])  # need this to access record in 'running' table.
  infile  <- vargs[2]
  outfile <- vargs[3]
  dbPath  <- vargs[4]  # need this to access 'running' and 'done' table
  
  cat('infile : ', infile, '\n')
  cat('outfile: ', outfile, '\n')
  
  #cat("n_spline_knots: ", n_spline_knots, "\n")
  #max_numbers <- readRDS(infile)
  #max_numbers <- 10
  #csite$GWSDAT_Options[['PSplineVars']][['nseg']] <- n_spline_knots
  
  
  # For testing (comment out above code):
  #default_session_file <- 'GWSDAT_Examples.rds'
  #infile <- system.file("extdata", default_session_file, package = "GWSDAT")
  #csite_list <- readRDS(infile)
  #csite <- csite_list[[1]]
  #csite$GWSDAT_Options[['PSplineVars']][['nseg']] <- 10
  
  #fitdat <- fitData(csite$All.Data, csite$GWSDAT_Options, showProgress = FALSE)
  res <- 0
  for (i in 1:max_numbers) {
    
    a <- runif(10000000)
    res <- a[1] + res
  }
  
  # Save the results to the output file.
  #saveRDS(fitdat, file = outfile)
  saveRDS(res, file = outfile)
  
  # Enter job into 'done' table.
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbPath)
  
  dt <- data.frame('jobid' = jobid, 'job_type' = jobtype, 'outputfile' = outfile, stringsAsFactors = FALSE)
  dbWriteTable(con, 'done', dt, append = TRUE)
  
  # Delete job from 'running' table.
  SQLcmd <- paste0('DELETE FROM running WHERE jobid=\'', jobid, '\';')
  rs <- dbSendQuery(con, SQLcmd)
  dbClearResult(rs)  
  
  
  dbDisconnect(con)
  
  # delete infile from temporary folder
  if (file.exists(infile))
      file.remove(infile)
  
}
