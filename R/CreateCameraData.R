#' Runs camera
#' @param Limma : Limma data
#' @param voom : eset having expression data
#' @param dataset : Molecular signature database gene set ("Mm" for mouse and "Hs" for humans). Currently only Hallmark and Curated exist
#' @param design : model matrix
#' @return Camera data and indices
#' @import (limma)
#' @export
#' @examples
#' CreateCameraData(limma,voom,dataset,design)


CreateCameraData <- function(limma,voom,dataset,design) {
  if(dataset=="Mm"){
  load("mouse_H_v5p1.rdata")
  load("mouse_c2_v5p1.rdata")
  indices.H <- ids2indices(Mm.H, limma$ENTREZID)
  indices.c2 <- ids2indices(Mm.H, limma$ENTREZID)
  }
  else if(dataset=="Hs"){
    load("human_H_v5p1.rdata")
    load("human_c2_v5p1.rdata")
    indices.H <- ids2indices(Hs.H, limma$ENTREZID)
    indices.c2 <- ids2indices(Hs.H, limma$ENTREZID)
  }
  voom=exprs(voom)
camera.H=camera(voom,indices.H, design)
camera.c2=camera(voom,indices.c2, design)
res=list(indices.H=indices.H,indices.c2=indices.c2,camera.H=camera.H,camera.c2=camera.c2)
return(res)
}
