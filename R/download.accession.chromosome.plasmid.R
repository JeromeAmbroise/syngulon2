#' Title Create accession csv file
#'
#' @param species a vector of character including the species that we want to analyze
#' @param bacteria.table The bacteria table produced by the create.bacteria.table function
#' @param outDir The output directory
#'
#' @return
#' @export
#'
#' @examples
download.accession.chromosome.plasmid <- function(species,bacteria.table,outDir)
{
  library(reutils)
  library(ape)
  library(seqinr)
  library(Biostrings)
  library(dplyr)
  library(WriteXLS)
  n.species <- length(species)
  for(i in 1:n.species)
  {
    data1species <- bacteria.table[bacteria.table$Organism==species[i],]
    data1species <- data1species[is.na(data1species$Replicons)==F,]
    Nstrains <- dim(data1species)[1]
    Replicon.vec <- NULL

    for(j in 1:Nstrains)
    {
      Replicon <- data1species$Replicons[j]
      Replicon <- strsplit(Replicon,split=';')[[1]]
      Replicon <- Replicon[grep(':',Replicon)]
      Replicon <- unlist(lapply(strsplit(Replicon,split=':'), function(x) x[[2]]))
      Replicon <- unlist(lapply(strsplit(Replicon,split='/'), function(x) x[[1]]))
      Replicon <- gsub(Replicon,pattern = ' ',replacement = '')
      Replicon <- paste(Replicon,collapse = " - ")
      Replicon.vec <- c(Replicon.vec,Replicon)
    }
    accession <- data.frame(accession=Replicon.vec)
    write.csv(accession,paste0(outDir,species[i],'.csv'),row.names = F)
  }
}
