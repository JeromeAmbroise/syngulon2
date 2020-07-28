#' Extraire les séquences des gènes de collicin dans le génome des especes grâce aux annotations
#'
#' @param species les espèces consernées
#' @param collicin les gènes consernés
#' @param annotationDir la localisation des fichiers d'annotations
#' @param genomeDir la localisation des fichiers des génomes
#' @param outDir l'emplacement pour stocker les différents gènes
#' @return
#' @export

extract.all.genes.annotationbased <-  function(species,collicin,annotationDir,genomeDir,outDir)
  {
    library(reutils)
    library(ape)
    library(seqinr)
    library(Biostrings)
    library(dplyr)

    nspecies <- length(species)
    collicin <- collicin$genename
    ngenes <- length(collicin)


    for(i in 1:nspecies)
    {
      dir.create(paste0(outDir,species[i]))
      for(j in 1:ngenes)
      {
        extract.1.gene.annotationbased(selectedspecies=species[i],selectedgene=collicin[j],annotationDir=annotationDir,genomeDir=genomeDir,outDir=outDir)
      }
      print(i)
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
