
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
    #load('~/dsdata/NGSshare/mm9_data/Rdata/Mus_musculus.NCBIM37.67.RData')
    geneannotation='Mus_musculus.NCBIM37.67.RData'
  }
  else if(organism=="Hs"){
    #load('~/dsdata/NGSshare/hg19_data/RData/gencode.v19.annotation.RData')
    data('gencode.v19.annotation.RData')
  }else{
    stop("Wrong organism")
  }

  # genes <- geneannotation %>% filter (gene_id %in% ids) %>%
  #   dplyr::rename(biotype=gene_biotype, SYMBOL=gene_name, ENSEMBL=gene_id) %>%
  #   mutate(geneloc=paste(chr,':',start,'-',end,sep=''),Length=abs(end-start)) %>%
  #   dplyr::select(SYMBOL,ENSEMBL,ENTREZID,biotype,geneloc,Length) %>% arrange(ENSEMBL)

  genes <- geneannotation %>% filter (gene_id %in% ids) %>%
    dplyr::rename(biotype=gene_biotype, SYMBOL=gene_name, ENSEMBL=gene_id) %>%
    mutate(geneloc=paste(chr,':',start,'-',end,sep='')) %>%
    dplyr::select(SYMBOL,ENSEMBL,ENTREZID,biotype,geneloc) %>% arrange(ENSEMBL)

  genes <-as.data.frame(genes)
  rownames(genes)=genes$ENSEMBL
  return(genes)
}


