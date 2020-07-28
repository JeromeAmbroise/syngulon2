#' extract a table of bacteria with accession numbers based on the genome package
#'
#' to compute later
#'
#' @param phylum a data.frame that can be produced by the function extract.phylum
#' @return a table with the one line for each genome available
#' @export
#'
extract.bacteria.table <- function(phylum)
{
  library(genomes)
  library(dplyr)
  proks <- reports("prokaryotes.txt")
  proks.selected <- proks%>%filter(Status=='Complete Genome'|Status=='Chromosome')
  bacteria.table <- proks.selected[grep(paste(phylum$taxonname,collapse='|'),proks.selected$SubGroup,ignore.case = T),]
  bacteria.table$Organism <- unlist(lapply(strsplit(bacteria.table$Organism,split=' '),function(x) paste(x[1],x[2],sep=' ')))
  bacteria.table$Organism <- gsub(bacteria.table$Organism,pattern='\\[',replacement = '')
  bacteria.table$Organism <- gsub(bacteria.table$Organism,pattern='\\]',replacement = '')
  bacteria.table$Organism <- gsub(bacteria.table$Organism,pattern='\'',replacement = '')
  bacteria.table$Organism <- bacteria.table$Organism %>% gsub(' ','_',.)
  bacteria.table$Organism <- bacteria.table$Organism %>% gsub('.','',.)
  return(bacteria.table)
}


