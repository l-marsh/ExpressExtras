
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




GeneAnnotate <- function(ids,organism) {
  if(organism=="Mm"){
  edb = EnsDb.Mmusculus.v75
  org = org.Mm.eg.db
  }
  else if(organism=="Hs"){
    edb = EnsDb.Hsapiens.v75
    org = org.Hs.eg.db
  }

  pos <- ensembldb::genes(edb,columns=c("gene_id","gene_biotype","seq_name"),
                          return.type="data.frame")
  res <- AnnotationDbi::select(org, keys=ids, columns=c("ENSEMBL","SYMBOL",'ENTREZID','GENENAME'), keytype="ENSEMBL")

  res <-subset(res,!duplicated(res$ENSEMBL))
  res <- inner_join(res,pos,by=c('ENSEMBL'='gene_id'))
  genes<- merge(limma,res, by.x=0, by.y="ENSEMBL")
  rownames(genes)=make.names(genes[,1], unique=TRUE)
  #genes=data.frame(ENSEMBL=genes[,1],genes[,ncol(genes)-4:ncol(genes)],genes[,2:7])
  return(genes)
}





