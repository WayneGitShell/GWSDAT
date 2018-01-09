

infoQueue <- function(dbPath = NULL, con = NULL, verbose = FALSE) {
    
    if (!is.null(dbPath)) { 
        drv <- DBI::dbDriver("SQLite")
        con <- DBI::dbConnect(drv, dbPath)
    }
    
    jq <- DBI::dbReadTable(con, 'jobqueue')
    rq <- DBI::dbReadTable(con, 'running')
    dq <- DBI::dbReadTable(con, 'done')
    
    if (verbose) cat("  Info: jobqueue (", nrow(jq), " records), running (", nrow(rq), " records), done (", nrow(dq), ").\n")
    
    # Only disconnect if dbPath was specified.
    if (!is.null(dbPath)) { 
        DBI::dbDisconnect(con)
    }
    
    return(list(jq = jq, rq = rq, dq = dq))
}


#FIXME (minor): Instead of calling evalQueue() and after that infoQueue (2x read all tables from DB),
#       use info from evalQueue() to pass back to main process.
evalQueue <- function(jq_db, max_workers = 2, max_done_jobs = 1e10) {
    
  con <- jq_db$dbConn
  dbPath <- jq_db$dbPath
    
  # Read all tables. 
  jq <- DBI::dbReadTable(con, "jobqueue")
  rq <- DBI::dbReadTable(con, "running")
  dq <- DBI::dbReadTable(con, "done")
  
  # Check number of running jobs
  active_workers <- nrow(rq)
  
  # jobs that are done and need evaluation are saved in this list will be returned.
  done_jobs <- list()
  
  # Execute new job from jobqueue if some job is in queue and the number of 
  # running jobs does not exceed the maximum of allowed workers (max_workers).
  if (nrow(jq) != 0 && active_workers < max_workers) {
      
      # Retrieve first record.
      newjob <- jq[1,]
      
      # Remove job by id. 
      SQLcmd <- paste0('DELETE FROM jobqueue WHERE job_id=\'', newjob$job_id, '\';')
      rs <- DBI::dbSendQuery(con, SQLcmd)
      DBI::dbClearResult(rs)        # without this, dbWriteTable will complain.
    
      # Note: file newjob$inputfile includes all necessary information to identify
      # the job and data.
      Rcmd <- paste0(newjob$Rcmd, ' ', newjob$inputfile, ' ', newjob$outputfile, ' ',  dbPath)
      
      
      cat("start process: ", Rcmd, "\n")
      #shell(cmd = Rcmd, wait = FALSE)
      
      system(Rcmd, wait = FALSE, invisible = TRUE)

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
        
        # Abort extracting 'done' jobs if the threshold 'max_done_jobs' is reached
        if (row > max_done_jobs)
          break
        
        job_id <- not_eval[row, "job_id"]
        
        cat('  * evaluating job_id ', job_id, '.....\n')
        
        # Do job evaluation. 
        done_jobs[[length(done_jobs) + 1]] <- list(job_id = job_id, data_id = not_eval[row, "data_id"], 
                                                   job_type = not_eval[row, "job_type"], 
                                                   outputfile = not_eval[row, "outputfile"])
        
        # Flag the 'evaluated' field as TRUE (1). This can be later deleted.
        SQLcmd = paste0('UPDATE done SET evaluated = 1 WHERE job_id = ', job_id, ';')
        rs <- DBI::dbSendQuery(con, SQLcmd)
        DBI::dbClearResult(rs)
      }
    }
  }
    
  # Return the list of done jobs that require evaluation 
  if (length(done_jobs) > 0)
    return(done_jobs)
    
  return(NULL)
}


# #' @import DBI
# #' @import RSQLite
createJobQueue <- function() {
    
    dbPath <- tempfile(pattern = "jobqueue_", tmpdir = tempdir(), fileext = ".db")
    
    drv <- DBI::dbDriver("SQLite")
    con <- DBI::dbConnect(drv, dbPath)
    
    tables <- DBI::dbListTables(con)
    
    # Setup the jobqueue table.
    jobqueue <- data.frame('info' = character(), 'job_id' = integer(), 'job_type' = character(), 
                           'data_set' = character(),'data_id' = integer(), 'Rcmd' = character(), 'inputfile' = character(), 
                           'outputfile' = character(), stringsAsFactors = FALSE)
    
    running <- data.frame('info' = character(), 'job_id' = integer(), 'job_type' = character(), 
                          'data_set' = character(), 'data_id' = integer(), 'Rcmd' = character(), 'inputfile' = character(), 
                          'outputfile' = character(), 'progress' = integer(), stringsAsFactors = FALSE)
    
    done <- data.frame('info' = character(), 'job_id' = integer(), 'job_type' = character(), 
                       'data_set' = character(), 'data_id' = integer(), 'evaluated' = integer(), 'outputfile' = character(), 
                       stringsAsFactors = FALSE)
    
    if (!("jobqueue" %in% tables)) {
      
      DBI::dbWriteTable(con, "jobqueue", jobqueue)
      DBI::dbWriteTable(con, "running", running)
      DBI::dbWriteTable(con, "done", done)
    
    } else {
      cat("Not creating fresh tables, found jobqueue table in database.\n")
      return(NULL)    
    }
   
    return(list(dbPath = dbPath, dbConn = con))
}


createUniqueJobID <- function(dbConn) {
  
  # Extract all 'job_id' from the the tables.
  tables <- infoQueue(con = dbConn)
  all_job_ids <- c(tables$jq$job_id, tables$rq$job_id, tables$dq$job_id)
  
  new_id <- 0
  
  # Loop as long as no unique data id can be found. 
  while (1) {
    new_id <- sample.int(100000, 1)
    
    if (!(new_id %in% all_job_ids))
      break
  }
  
  return(new_id)
  
}


addQueueJob <- function(jq, script_name, info = 'short job description', data_name, 
                        data_id, pdata, params) {

  scriptPath <- system.file("inst/application", script_name, package = "GWSDAT")
  
  finput  <- tempfile(pattern = "filein_", tmpdir = tempdir(), fileext = ".rds")
  foutput <- tempfile(pattern = "fileout_", tmpdir = tempdir(), fileext = ".rds")

  job_id <- createUniqueJobID(jq$dbConn)
  
  # Put all necessary parameters to run the method and identify the job and data 
  # into this list and save it to the .rds file that is read by the target script.
  input_data <- list(params = params, data = pdata, job_id = job_id)
  saveRDS(input_data, file = finput)
  
  # Create record for the 'jobqueue' table and append to table.
  newjob <- data.frame('info' = info, 'job_id' = job_id, 'job_type' = script_name, 
                       'data_set' = data_name, 'data_id' = data_id, 'Rcmd' = paste0('Rscript ', scriptPath), 
                       'inputfile' = finput, 'outputfile' = foutput, stringsAsFactors = FALSE)
  
  DBI::dbWriteTable(jq$dbConn, "jobqueue", newjob, append = TRUE)
  
}