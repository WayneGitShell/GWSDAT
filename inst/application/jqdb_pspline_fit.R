

#library(GWSDAT) #Need to find libPath first before loading GWSDAT library. 



vargs <- commandArgs(TRUE)

cat('Number of args: ', length(vargs), '\n')

if (length(vargs) == 3) {

  # load the data from temp file
  infile <- vargs[1]
  outfile <- vargs[2]
  dbPath <- vargs[3]

  cat("infile:  ", infile, "\n")
  cat("outfile: ", outfile, "\n")
  cat("dbPath:  ", dbPath,  "\n")
    
  fcontent <- readRDS(infile)
  params   <- fcontent$params
  csite    <- fcontent$data
  job_id   <- fcontent$job_id 
  
  cat("job_id: ", job_id, ", refitting pslines with ", params$PSplineVars$nseg, " segments.\n")
  
  .libPaths(c(params$SavedlibPaths,.libPaths()))
  
  if (!require(DBI))
    stop("Missing the DBI package. Please install it.")
  
  if (!require(RSQLite))
    stop("Missing the RSQLite package. Please install it.")
  
  
  library(GWSDAT)
  fitdat <- GWSDAT:::fitData(csite$All.Data, params, showProgress = FALSE)

  # Pass the used parameters back, so they can update the data set parameters 
  # as soon as the modifications take effect.
  results <- list(fitdat = fitdat, params = params)
  
  saveRDS(results, file = outfile)
  
  #
  #FIXME: Consider moving the DB code below to jobqueue.R to make it cleaner.
  #
  
  
  # Modify DB: Enter job into 'done' table and remove from 'running' table.
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbPath)
  
  
  # Get record from 'running' table.
  res <- dbGetQuery(con, paste0("SELECT * FROM running where job_id = ", job_id, ";"))
  
  # If not exactly one row could be extracted, something is wrong with the table.
  # One possibility is that the main app deleted the job from running after it 
  # exceeded a time-out.
  if (nrow(res) == 1) {
    
    # Create new record for 'done' table.
    dt <- data.frame('info' = res$info, 'job_id' = job_id, 'script' = res$script,
                     'data_set' = res$data_set, 'data_id' = res$data_id, 'evaluated' = 0, 
                     'outputfile' = outfile, stringsAsFactors = FALSE)
  
    print.table(dt)
    
    dbWriteTable(con, 'done', dt, append = TRUE)
    
    # Delete job from 'running' table using the 'job_id'.
    SQLcmd <- paste0('DELETE FROM running WHERE job_id=\'', job_id, '\';')
    rs <- dbSendQuery(con, SQLcmd)
    dbClearResult(rs)  
  }  
  
  dbDisconnect(con)
  
} else {
  stop("Wrong number of arguments specified.")
}
