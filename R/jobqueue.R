

infoQueue <- function(dbPath = NULL, con = NULL) {
    
    if (!is.null(dbPath)) { 
        drv <- DBI::dbDriver("SQLite")
        con <- DBI::dbConnect(drv, dbPath)
    }
    
    jq <- DBI::dbReadTable(con, 'jobqueue')
    rq <- DBI::dbReadTable(con, 'running')
    dq <- DBI::dbReadTable(con, 'done')
    
    cat("  Info: jobqueue (", nrow(jq), " records), running (", nrow(rq), " records), done (", nrow(dq), ").\n")
    
    # Only disconnect if dbPath was specified.
    if (!is.null(dbPath)) { 
        DBI::dbDisconnect(con)
    }
    
    return(list(jq = jq, rq = rq, dq = dq))
}


#FIXME (minor): Instead of calling evalQueue() and after that infoQueue (2x read all tables from DB),
#       use info from evalQueue() to pass back to main process.
evalQueue <- function(jq_db, max_workers = 2) {
    
    con <- jq_db$dbConn
    dbPath <- jq_db$dbPath
      
    # Read all tables. 
    jq <- DBI::dbReadTable(con, "jobqueue")
    rq <- DBI::dbReadTable(con, "running")
    dq <- DBI::dbReadTable(con, "done")
    
    
    # Check number of running jobs
    active_workers <- nrow(rq)
    
    # Execute new job from jobqueue if some job is in queue and the number of 
    # running jobs does not exceed the maximum of allowed workers (max_workers).
    if (nrow(jq) != 0 && active_workers < max_workers) {
        
        # Retrieve first record.
        newjob <- jq[1,]
        
        # Remove job by id. 
        SQLcmd <- paste0('DELETE FROM jobqueue WHERE jobid=\'', newjob$jobid, '\';')
        rs <- DBI::dbSendQuery(con, SQLcmd)
        DBI::dbClearResult(rs)        # without this, dbWriteTable will complain.
        
        # Start external process.
        Rcmd <- paste0(newjob$Rcmd, ' ', newjob$jobid, ' ', newjob$inputfile, ' ', newjob$outputfile, ' ',  dbPath)
        cat("start process: ", Rcmd, "\n")
        shell(cmd = Rcmd, wait = FALSE)
        
        # Add a progress field to the job record and append to 'running' table.
        newjob$progress <- 0
        DBI::dbWriteTable(con, "running", newjob, append = TRUE)
    }
    
    # Check if any 'done' jobs have to be evaluated
    if (nrow(dq) > 0) {
      
      # Extract done jobs that have not been evaluated yet.
      not_eval <- dq[which(dq$evaluated == 0),]
      
      # If any jobs have been found.. 
      if (nrow(not_eval) > 0) {

        cat("--> found ", nrow(not_eval), ' done jobs that require evaluation.\n')
        
        # .. go through each one and evaluate result in the 'output' file field.
        for (row in 1:nrow(not_eval)) {
          
          jobid <- not_eval[row, "jobid"]
          
          cat('  * evaluating jobid ', jobid, '.....\n')
          
          # Do job evaluation. 
          eval_job <- list(not_eval[1, ])
          
          
          # Flag the 'evaluated' field as TRUE (1). This can be later deleted.
          SQLcmd = paste0('UPDATE done SET evaluated = 1 WHERE jobid = ', jobid, ';')
          rs <- DBI::dbSendQuery(con, SQLcmd)
          DBI::dbClearResult(rs)
        }
      }
    }
   
    
}


# #' @import DBI
# #' @import RSQLite
createJobQueue <- function() {
    
    dbPath <- tempfile(pattern = "jobqueue_", tmpdir = tempdir(), fileext = ".db")
    
    drv <- DBI::dbDriver("SQLite")
    con <- DBI::dbConnect(drv, dbPath)
    
    tables <- DBI::dbListTables(con)
    
    # Setup the jobqueue table.
    jobqueue <- data.frame('jobid' = integer(), 'Rcmd' = character(), 'inputfile' = character(), 
                           'outputfile' = character(), stringsAsFactors = FALSE)
    
    running <- data.frame('jobid' = integer(), 'Rcmd' = character(), 'inputfile' = character(), 
                          'outputfile' = character(), 'progress' = integer(), stringsAsFactors = FALSE)
    
    done <- data.frame('jobid' = integer(), 'job_type' = character(), 'evaluated' = integer(),'outputfile' = character(), stringsAsFactors = FALSE)
    
    if (!("jobqueue" %in% tables)) {
        
      cat("Create tables: jobqueue and running\n")
      
      DBI::dbWriteTable(con, "jobqueue", jobqueue)
      DBI::dbWriteTable(con, "running", running)
      DBI::dbWriteTable(con, "done", done)
    
    } else {
      cat("Not creating fresh tables, found jobqueue table in database.\n")
      return(NULL)    
    }
   
    #dbDisconnect(con)
    
    return(list(dbPath = dbPath, dbConn = con))
}


addQueueJob <- function(jq, scriptPath, pdata) {

  finput  <- tempfile(pattern = "filein_", tmpdir = tempdir(), fileext = ".rds")
  foutput <- tempfile(pattern = "fileout_", tmpdir = tempdir(), fileext = ".rds")

  saveRDS(pdata, file = finput)
  
  
  #FIXME: Generate unique job id inside addQueueJob()
  newjob <- data.frame('jobid' = floor(runif(1,1,100000)), 'Rcmd' = paste0('Rscript ', scriptPath), 
                     'inputfile' = finput, 'outputfile' = foutput, stringsAsFactors = FALSE)
  
  DBI::dbWriteTable(jq$dbConn, "jobqueue", newjob, append = TRUE)
  
}