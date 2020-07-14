#' Title
#'
#' @param species a vector of character including the species that we want to analyze
#' @param maxOrganism a value with the number of genome we want to download for each species
#' @param indextostart : the index to restart the analysis. for example, if you interupted the analysis after 20 species, you can specify indextorestart=21
#' @param accessionDir the directory where the accession list can be found
#' @param outDir The output directory
#'
#' @return
#' @export
#'
#' @examples
download.annotation <- function(species,maxOrganism=20,indextostart,accessionDir,outDir)
{
  library(reutils)
  library(ape)
  library(seqinr)
  library(Biostrings)
  library(dplyr)
  library(WriteXLS)
  accession.list <- list.files(accessionDir,full.names = T)

  n.species <- length(species)
  for(i in indextostart:n.species)
  {
    dir.create(paste0(outDir,species[i]))
    accession <- read.csv(accession.list[grep(species[i],accession.list)],stringsAsFactors = F)
    accession  <- accession$accession
    accession <- accession[substr(accession,1,2)!='LR']
    N.accession <- length(accession)
    for(j in 1:min(c(maxOrganism,N.accession)))
    {
      current.accession <- accession[j]
      current.accession <- strsplit(current.accession,split='-')[[1]]
      current.accession <- gsub(current.accession,pattern = ' ',replacement = '')
      N.chromosomes <- length(current.accession)
      full.annotation <- NULL
      for(k in 1:N.chromosomes)
      {
        current.annotation <- try(getAnnotationsGenBank(access.nb=current.accession[k], quiet = TRUE))
        if(class(current.annotation)=='data.frame')
        {
          if(all(is.element(c('start','end','type','gene','product'),colnames(current.annotation))))
          {
            current.annotation <- tibble(current.annotation)
            current.annotation <- current.annotation %>% select(start,end,type,gene,product)
            current.annotation <- data.frame(genome=current.accession[k],current.annotation)
            full.annotation <- rbind(full.annotation,current.annotation)
          }
        }
      }
      if(class(full.annotation)=='data.frame')
      {
        write.csv(full.annotation,paste0(outDir,species[i],'/',paste(current.accession[1:min(5,N.chromosomes)],collapse = '-'),'.csv'),row.names = F)
      }
    }
  print(i)
  }
}
