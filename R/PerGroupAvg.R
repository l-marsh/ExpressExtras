#' Computes per group average expression
#' @param eset : eset
#' @return Dataframe with per group average expression
#' @import (tidyr)
#' @export
#' @examples
#' PerGroupAvg()

PerGroupAvg <- function(eset) {
d = as.data.frame(exprs(eset.bk))
colnames(d) = eset.bk@phenoData$maineffect
d$probe =rownames(d)
d2=gather(as.data.frame(d),'probe')
d2=d2[,(ncol(d2)-2):ncol(d2)]
colnames(d2) = c('ID','grp','signal')
res  <- d2 %>% group_by(ID,grp) %>% summarise(avg=mean(signal))
head(res)
avg <- res %>% spread(grp,avg)
return(avg)
}
