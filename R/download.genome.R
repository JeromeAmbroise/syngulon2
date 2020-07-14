#' Downlaod genomes of selected species
#'
#' @param species a vector of character including the species that we want to analyze
#' @param maxOrganism a value with the number of genome we want to download for each species
#' @param indextostart : the index to restart the analysis. for example, if you interupted the analysis after 20 species, you can specify indextorestart=21
#' @param accessionDir the directory where the accession files can be found
#' @param outDir The output directory
#' @return
#' @export



download.genome <- function(species,maxOrganism,indextostart,accessionDir,outDir)
{

  library(reutils)
  library(ape)
  library(seqinr)
  library(Biostrings)
  library(dplyr)
  library(WriteXLS)

  n.species <- length(species)
  accession.list <- list.files(accessionDir,full.names = T)

  for(i in indextostart:n.species)
  {
    dir.create(paste0(outDir,species[i]))

    accession <- read.csv(accession.list[grep(species[i],accession.list)],stringsAsFactors = F)
    accession  <- accession$accession
    N.accession <- length(accession)
    for(j in 1:min(c(maxOrganism,N.accession)))
    {
      current.accession <- accession[j]
      current.accession <- strsplit(current.accession,split='-')[[1]]
      current.accession <- gsub(current.accession,pattern = ' ',replacement = '')
      N.chromosomes <- length(current.accession)
      dir.create(paste0(outDir,species[i],'/',current.accession[1]))
      for(k in 1:min(5,N.chromosomes))
      {
        seq <- read.GenBank(current.accession[k])
        write.FASTA(seq,paste0(outDir,species[i],'/',current.accession[1],'/',current.accession[k],'.fasta'))
      }
      seq <- readDNAStringSet(list.files(paste0(outDir,species[i],'/',current.accession[1]),full.names = T))
      writeXStringSet(seq,paste0(outDir,gsub(species[i],pattern = ' ',replacement = '.'),'/',paste(current.accession[1:min(5,N.chromosomes)],collapse = '-'),'.fasta'))
      unlink(paste0(outDir,species[i],'/',current.accession[1]),recursive = T,force = T)
      }
  print(i)
  }
}
