#' Screen a sequence using Blast
#'
#' You provide a reference and a querry and the function compute the percentage of the reference which is covered by the querry
#'
#' @param reference the reference sequence that you want to screen. Fasta file in one or severa sequences
#' @param querry the querry sequence. Fasta file in one or severa sequences
#' @param dir.out the directory of the output
#' @param min.pc.ident
#'
#' @return numeric value of the percentage of the reference sequence which is covered by the querry
#' @import GenomicRanges IRanges Biostrings
#'
#' @export

screenBlast <- function (reference, querry,min.pc.ident,dir.out)
{
  library(Biostrings)
  library(GenomicRanges)
  try(unlink("temp", recursive = TRUE))
  dir.create("temp")
  dir.create("temp/dbblast")
  myarg <- paste0("-in ", reference, " -out temp/dbblast/db -dbtype nucl")
  system2(command = "makeblastdb", args = myarg, stdout = F)

  Sys.time()
  myarg <- paste0("-query ", querry, " -db temp/dbblast/db -out temp/blast.txt -num_threads 8 -num_alignments 1000 -outfmt \"7 sacc bitscore pident length slen \"")
  system2(command = "blastn", args = myarg)
  blast <- try(read.table("temp/blast.txt", comment.char = "#"), silent = T)

  if (class(blast) == "data.frame")
  {
    colnames(blast) <- c("subj.access", "bitscore", "pident", "align.length", "subj.len")
    blast <- blast[blast$pident>min.pc.ident,]
    if(dim(blast)[1]>0)
    {
      toreturn1 <- paste(unique(unlist(lapply(strsplit(blast$subj.access,split=':'),function(x) x[[1]]))),collapse='|')
      blast$pc.length <- round(100*(blast$align.length/blast$subj.len))
      blast$pc.length[blast$pc.length>100] <- 100
      blast <- blast[sort.list(blast$bitscore,decreasing=T),]
      blast <- blast[which.max(blast$pc.length),]
      toreturn2  <- paste0('pc.id = ',round(blast$pident),' ; pc.cov = ',blast$pc.length)
      toreturn <- paste(toreturn2,toreturn1,sep=';')
    }
    else{toreturn <- ''}
  }
  else{toreturn <- ''}
  unlink('temp',recursive = T)
  return(toreturn)
}
