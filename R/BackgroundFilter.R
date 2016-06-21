#'Filter an ExpressionSet for Genes above background Vale
#' @param eset An ExpressionSet
#' @param method can be "mean" which will take the mean expression value of all the controls per array and use the median as the cutoff
#' Quantile will use the .75 quatile instead of the mean
#' @export
<<<<<<< HEAD
#' @import genefilter

=======
<<<<<<< HEAD
#' @import genefilter
=======
#' @imnport genefilter
>>>>>>> 4d67ba44dfbf7d56149c8a2e862446f405a792a4
>>>>>>> 6d551a9a49c26627682d0fe10a405d6c40442b49

BackgrdFilter <- function(eset,method='mean'){

  pdinfo <- annotation(eset)
  con <- db(get(pdinfo))
  antigm <- dbGetQuery(con, "select meta_fsetid from core_mps inner join
featureSet on core_mps.fsetid=featureSet.fsetid where
featureSet.type='7';")
  # Find the 80% number of samples in the eset for the filter backgrod
  bkgrdval = switch(method,
                    mean = median(apply(exprs(eset)[as.character(antigm[,1]),], 2, mean)),
                    quantile = median(apply(exprs(eset)[as.character(antigm[,1]),], 2, quantile,prob=.8))
  )

  eset@experimentData@normControls <-list('method'=method,'ExpressCutoffValue'=bkgrdval)


  # bkgrdval <- max( apply(exprs(eset)[as.character(antigm[,1]),], 2, quantile,prob=.8))
  ind <- genefilter(eset, filterfun(pOverA(.8, bkgrdval)))
  return(eset[ind,])

}
