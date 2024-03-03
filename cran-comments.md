Version 3.2.1
Bug fix - https://github.com/WayneGitShell/GWSDAT/commit/9e0e482281d968b85cc12a2461c974aeac93e79d

Version 3.2.0
Fixed warning in first release attempt by using underscore instead of spaces in file names. 
Fixed undeclared S3method issue. 
Fixed issue of comparing classes to strings. 

I see 1 outstanding notes in the CRAN check. 
1) platform-specific call - use of win.metafile - this is only implemented on windows distributions to generate windows metafiles. 
I believe this is a false positive as per here: https://stackoverflow.com/questions/70585796/unable-to-understand-1-note-in-devtoolscheck-caused-by-a-platform-specific-d

In response to Uwe's comments I have added version dependency for "officer" to description file. 


