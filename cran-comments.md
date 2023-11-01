Version 3.20 fixed warning in first release attempt by using underscore instead of spaces in file names. Also fixed undeclared S3method issue. 

I see 2 outstanding notes in the CRAN check. 
1) platform-specific call - use of win.metafile - this is only implemented on windows distributions to generate windows metafiles. 
2) "Found if() conditions comparing class() to string" - we use custom classes and there doesn't exist a function "is.Date" in R. Safer and easier to compare character in this circumstance. 


In response to Uwe's comments I have added version dependency for "officer" to description file. 


