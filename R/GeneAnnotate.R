
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Return a data frame of gene annotations
#' Takes a dataframe having output from limma, cleans up to remove unwanted columns and annotates it
#' @param ids : List of ENSEMBL IDs
#' @param organism : Mm for Mus musculus (mouse) and Hs for Homo sapiens (humans)
#' @return Dataframe with limma data and the annotation
#' @import org.Mm.eg.db EnsDb.Mmusculus.v75 org.Hs.eg.db EnsDb.Hsapiens.v75 dplyr
#' @export


GeneAnnotate <- function(ids,organism="Mm") {
  if(organism=="Mm"){
  edb = EnsDb.Mmusculus.v75
  org = org.Mm.eg.db
  }
  else if(organism=="Hs"){
    edb = EnsDb.Hsapiens.v75
    org = org.Hs.eg.db
  }else{
    stop("Wrong organism")
  }

  pos <- ensembldb::genes(edb,columns=c("gene_id","gene_biotype","seq_name"),
                          return.type="data.frame")
  res <- AnnotationDbi::select(org, keys=ids, columns=c("ENSEMBL","SYMBOL",'ENTREZID','GENENAME'), keytype="ENSEMBL")
  genes <- inner_join(res,pos,by=c('ENSEMBL'='gene_id'))
  genes <-subset(genes,!duplicated(res$ENSEMBL))
  rownames(genes)=make.names(genes$ENSEMBL, unique=TRUE)
  return(genes)
}


