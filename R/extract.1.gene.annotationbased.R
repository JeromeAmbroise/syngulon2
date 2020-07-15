#' Title
#'
#' @param selectedspecies
#' @param selectedgene
#' @param annotationDir
#' @param genomeDir
#' @param outDir
#'
#' @return
#' @export

extract.1.gene.annotationbased <- function(selectedspecies='ecoli',selectedgene='tolB',annotationDir,genomeDir,outDir)
{
  library(reutils)
  library(ape)
  library(seqinr)
  library(Biostrings)
  library(dplyr)

  unlink(paste0(outDir,selectedspecies,'/',selectedgene),recursive = T)
  dir.create(paste0(outDir,selectedspecies,'/',selectedgene))
  annotationfiles <- list.files(paste0(annotationDir,selectedspecies),full.names = T)
  genomefiles <- list.files(paste0(genomeDir,selectedspecies),full.names = T)
  genomefiles <- genomefiles[grep('.fasta',genomefiles)]

  annotationfilename <- gsub(basename(annotationfiles),pattern = '.csv',replacement = '')
  genomefilename <- gsub(basename(genomefiles),pattern = '.fasta',replacement = '')
  selectedgenomes <- intersect(annotationfilename,genomefilename)

  N.genome <- length(selectedgenomes)
  if(N.genome>0)
  {
    for(i in 1:N.genome)
    {
      currentgenome <- selectedgenomes[i]
      genome <- readDNAStringSet(genomefiles[grep(currentgenome,genomefiles)])
      annotation <- read.csv(annotationfiles[grep(currentgenome,annotationfiles)],stringsAsFactors = F)
      annotation <- annotation[is.na(annotation$gene)==F,]
      annotation1gene <- annotation[toupper(annotation$gene)==toupper(selectedgene),][1,]
      if(is.na(annotation1gene$gene)==F)
      {
        start <- min(annotation1gene$start[1],annotation1gene$end[1])
        end <- max(annotation1gene$start[1],annotation1gene$end[1])
        genome <- genome[names(genome)==annotation1gene$genome[1]]
        sequence <- subseq(genome,start =start[1],end=end[1])
        if(annotation1gene$start[1]>annotation1gene$end[1]){sequence <- reverseComplement(sequence)}
        names(sequence) <- paste(selectedspecies,selectedgene,names(sequence),sep=':')
        writeXStringSet(sequence,paste0(outDir,selectedspecies,'/',selectedgene,'/',selectedgene,'_',currentgenome,'.fasta'))
      }
    }
    allseq <- readDNAStringSet(list.files(paste0(outDir,selectedspecies,'/',selectedgene),full.names=T))
    writeXStringSet(allseq,paste0(outDir,selectedspecies,'/',selectedgene,'.fasta'))
  }
}
