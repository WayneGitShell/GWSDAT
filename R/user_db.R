
createUserDB <- function(dbPath) {
  # Open connection to user database, if not existing, it will be created.            
  #drv <- DBI::dbDriver("SQLite")
  con <- DBI::dbConnect(DBI::dbDriver("SQLite"), dbPath)
  
  tables <- DBI::dbListTables(con)
  
  if (!('users' %in% tables)) {  

    # Create table.
    user_head <- data.frame('id' = integer(), 'email' = character(), 
                            'password' = character(), 'data_path' = character(),
                            stringsAsFactors = FALSE)
    
    DBI::dbWriteTable(con, "users", user_head)
  } 
  
  return(con)
}


createUserID <- function(dbConn) {
  
  # Extract table with user information.
  table <- DBI::dbReadTable(dbConn, 'users')

  new_id <- 0
  
  # Loop as long as no unique user id can be found. 
  while (1) {
    new_id <- sample.int(100000, 1)
    
    if (!(new_id %in% table$id))
      break
  }
  
  return(new_id)
  
}

userExists <- function(dbPath, email) {
  
  if (!file.exists(dbPath)) 
    return(FALSE)
  
  con <- DBI::dbConnect(DBI::dbDriver("SQLite"), dbPath)
  
  table <- DBI::dbReadTable(con, 'users')
  
  if (is.null(table$email)) {
    DBI::dbDisconnect(con)
    return(FALSE)
  }
  
  if (!(email %in% table$email)) {
    DBI::dbDisconnect(con)
    return(FALSE)
  }
  
  DBI::dbDisconnect(con)
  return(TRUE)
}


getUser <- function(dbPath, email) {
  
  con <- DBI::dbConnect(DBI::dbDriver("SQLite"), dbPath)
  
  table <- DBI::dbReadTable(con, 'users')
  
  # Get record and check if it is exactly one.
  rec <- table[which(table$email == email),]
  
  if (nrow(rec) != 1)
    return(NULL)
  
  return(rec)
  
}

#' @importFrom digest digest
verifyUser <- function(dbPath, email, password) {
  
  mda_passwd <- digest::digest(password)
  
  # Check if user exists in data base.
  user_rec <- getUser(dbPath, email)
  
  if (is.null(user_rec))
    return(NULL)

  # Check the mda password.
  if (user_rec$password != mda_passwd)
    return(NULL)
  
  return(user_rec)
  
}


infoUsers <- function(dbPath) {
  
  con <- DBI::dbConnect(DBI::dbDriver("SQLite"), dbPath)
  
  table <- DBI::dbReadTable(con, 'users')
  
  print.table(table)
  
  DBI::dbDisconnect(con)
  
}