#' Cleans up topTable results
#' converts logFC to FC
#' @param vals : output from topTable
#' @return Dataframe with limma output
#' @export
#' @examples
#' Cleanup()

Cleanup <-function(tt){
  res <- tt %>% mutate(fc = ifelse(logFC<0, -1*2^abs(logFC),2^logFC)) %>%
    rename(biotype=gene_biotype,Chrom=seq_name) %>%
    select(ENSEMBL,SYMBOL,ENTREZID,GENENAME,biotype,Chrom,logFC,fc,P.Value,adj.P.Val)
    rownames(res) <- rownames(tt)
    return(res)
}
