#' Return a data frame of gene annotations
#' Takes a dataframe having output from limma, cleans up to remove unwanted columns and annotates it
#' @param ids : List of ENSEMBL IDs
#' @param organism : Mm for Mus musculus (mouse) and Hs for Homo sapiens (humans)
#' @return Dataframe with limma data and the annotation
#' @import topGO
#' @export


runTopGO <- function(tt,pcutoff=.1){
  geneList <-as.vector(tt$adj.P.Val)
  names(geneList) <- as.character(tt$ENSEMBL)


  ##
  GetGeneList <- function(input){
    input < pcutoff
  }

  GOdata <- try(new("topGOdata",
                    ontology = "BP",
                    allGenes = geneList,
                    geneSel = GetGeneList,
                    nodeSize = 10,
                    # annot, tells topGO to map from GO terms to "genes"
                    annot = annFUN.org,
                    mapping="org.Mm.eg.db",
                    ID = "ensembl"
  ), silent=TRUE)

  if (class(GOdata) == "try-error") {
    return(NULL)
  }

  resultKS.elim <- runTest(GOdata, algorithm = "elim", statistic = "ks")
  resultFisher.elim <- runTest(GOdata, algorithm = "elim", statistic = "fisher")
  resultFisher.cl <- runTest(GOdata, algorithm = "classic", statistic = "fisher")
  resultKS.cl <- runTest(GOdata, algorithm = "classic", statistic = "ks")

  allRes <- GenTable(GOdata,
                     elimKS = resultKS.elim , elimfisher = resultFisher.elim,classicfisher=resultFisher.cl,classicKS=resultKS.cl,
                     orderBy = "classicfisher", topNodes = length(GOdata@graph@nodes))

  return(allRes)
}