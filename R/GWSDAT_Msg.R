
GWSDAT_Error <- function(msg, code = 1) {
  
  ret = list()
  ret$msg = msg
  ret$code = code
  
  class(ret) = "GWSDAT_Error"
  
  return(ret)
}


GWSDAT_OK <- function() {
  
  ret = list()
  ret$msg = "Everything OK."
  
  class(ret) = "GWSDAT_OK"
  
  return(ret)
}


GWSDAT_Warning <- function(msg, code = 2) {
  
  ret = list()
  ret$msg = msg
  ret$code = code
  
  class(ret) = "GWSDAT_Warning"
  
  return(ret)
}

