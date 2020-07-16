#' Title
#'
#' @param species
#' @param collicin
#' @param geneDir
#' @param outDir
#'
#' @return
#' @export
#'
#' @examples
otus <- function(species,collicin,geneDir,outDir)
{
  library(dplyr)
  library(Biostrings)
  collicin = collicin$genename

  for (i in 1:length(species)) {
    dir.create(paste0(outDir,species[i]))
    for (j in 1:length(collicin)) {
      myarg <- paste0('-derep_fulllength ',paste0(geneDir,species[i],"/",collicin[j],".fasta"),' -fastaout ',outDir,'derep1.fasta -sizeout')
      system2(command='./usearch8.1.1861_i86linux32',args=myarg,stdout = NULL,stderr = NULL)

      myarg <- paste0('-sortbysize ',outDir,'derep1.fasta -fastaout ',outDir,'sorted.fasta -minsize 1')
      system2(command='./usearch8.1.1861_i86linux32',args=myarg,stdout = NULL,stderr = NULL)

      myarg <- paste0('-cluster_otus ',outDir,'sorted.fasta -otus ',paste0(outDir,species[i],"/",collicin[j],".fasta"),' -otu_radius_pct 5.0')
      system2(command='./usearch8.1.1861_i86linux32',args=myarg,stdout = NULL,stderr = NULL)
      file.remove(paste0(outDir,"derep1.fasta"))
      file.remove(paste0(outDir,"sorted.fasta"))
    }
    print(i)
  }

  fasta.list=c()
  for (i in 1:length(species)) {
    fasta.list.newspecies <- list.files(paste0(outDir,species[i],'/'),full.names = T)
    fasta.list.newspecies <- fasta.list.newspecies[grep('fasta',fasta.list.newspecies)]
    fasta.list <- c(fasta.list,fasta.list.newspecies)
  }


  genename <- unique(basename(fasta.list))
  for(i in 1:length(genename))
  {
    current.sequence <- readDNAStringSet(fasta.list[grep(genename[i],fasta.list)])
    writeXStringSet(current.sequence,paste0(outDir,genename[i]))
  }



}
