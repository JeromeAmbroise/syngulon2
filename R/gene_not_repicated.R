#' Title A function to exclude replicated genes within a species
#'
#' @param species
#' @param geneDir
#' @param outDir
#' @param collicin
#'
#' @return
#' @export
#'
#' @examples
gene_not_duplicated = function(species,geneDir,outDir,collicin)
{

  library(Biostrings)
  collicin <- collicin$genename
  ngenes <- length(collicin)
  nspecies=length(species)
  for (i in 1:nspecies) {
    dir.create(paste0(outDir,species[i]))
    for (j in 1:ngenes) {
      fasta.list <- readDNAStringSet(paste0(geneDir,species[i],"/",collicin[j],".fasta"))
      writeXStringSet(unique(fasta.list),paste0(outDir,species[i],"/",collicin[j],".fasta"))
    }
  }


  fasta.list=c()
  for (i in 1:nspecies) {
    fasta.list.newspecies <- list.files(paste0(outDir,species[i],'/'),full.names = T)
    fasta.list.newspecies <- fasta.list.newspecies[grep('fasta',fasta.list.newspecies)]
    fasta.list <- c(fasta.list,fasta.list.newspecies)
  }


  fileinfo <- file.info(fasta.list)
  fasta.list <- fasta.list[fileinfo$size>0]

  genename <- unique(basename(fasta.list))
  for(i in 1:length(genename))
  {
    current.sequence <- readDNAStringSet(fasta.list[grep(genename[i],fasta.list)])
    writeXStringSet(current.sequence,paste0(outDir,genename[i]))
  }

}
