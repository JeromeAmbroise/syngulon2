#' Title
#'
#' @param species
#' @param collicin
#' @param annotationDir
#'
#' @return
#' @export
#'
#' @examples
amelioration.annotation <- function(species,collicin,annotationDir){
  library(dplyr)

  collicin <- collicin$genename
  collicin <- toupper(collicin)
  annotationfiles <- list.files(annotationDir,full.names = T,recursive = T)


  for(i in 1:length(annotationfiles))
  {
    currentfile <- read.csv(annotationfiles[i])
    currentfile$gene <- unlist(lapply(strsplit(currentfile$gene,split='_'),function(x) x[[1]]))
    write.csv(currentfile,annotationfiles[i])
    print(i)
  }
  for (i in 1:length(collicin)) {
    product <-c()
    for (j in 1:length(annotationfiles)) {
      currentfile <- read.csv(annotationfiles[j])
      currentfile$gene <- toupper(currentfile$gene)
      currentfile <- currentfile%>%filter(gene==collicin[i])
      product <- c(product,currentfile$product)
    }
    print(i)
    write.csv(table(product),paste0("99-results/","table.",collicin[i],".csv"))
  }

}
