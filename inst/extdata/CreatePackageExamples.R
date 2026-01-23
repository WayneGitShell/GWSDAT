
setwd("C:/Users/Wayne.W.Jones/GitHub/GWSDAT")
tr1<-readRDS("GWSDAT_Basic_Example.rds")
tr1$csite$DO_NOT_MODIFY<-TRUE
tr2<-readRDS("GWSDAT_Example.rds")
tr2$csite$DO_NOT_MODIFY<-TRUE

out<-list()
out[[1]]<-tr1$csite
out[[2]]<-tr2$csite

saveRDS(out,"New GWSDAT_Examples.rds")
