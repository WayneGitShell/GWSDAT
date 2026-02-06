
srcdir<-utils::getSrcDirectory(function(){})

tr1<-readRDS(file.path(srcdir,"GWSDAT_Basic_Example.rds"))
tr1$csite$DO_NOT_MODIFY<-TRUE
tr2<-readRDS(file.path(srcdir,"GWSDAT_Example.rds"))
tr2$csite$DO_NOT_MODIFY<-TRUE

out<-list()
out[[1]]<-tr1$csite
out[[2]]<-tr2$csite

saveRDS(out,file.path(srcdir,"GWSDAT_Examples.rds"))

