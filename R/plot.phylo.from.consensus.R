#' A function to plot phylogenetic tree of all genes based on fasta file in the consensus directory
#'
#' @param consensusDir the directory with the consensus
#'
#' @return
#' @export
#'
#' @examples
phylo.from.consensus <- function(consensusDir,bacteria.table)
{
  library(dplyr)
  library(Biostrings)
  library(muscle)
  library(phangorn)
  library(ggtree)

  filelist <- list.files(consensusDir,full.names = T)

  correspondance.organism.subgroup <- bacteria.table%>%group_by(Organism) %>% dplyr::count(Organism, SubGroup) %>% dplyr::slice(which.max(n)) %>% dplyr::rename(species=Organism)



  genename <- gsub(basename(filelist),pattern = '.fasta',replacement = '')
  ngenes <- length(genename)
  for(i in 1:ngenes)
  {
    sequences <- readDNAStringSet(filelist[i])

    sequences <- sequences[sort.list(names(sequences))]
    currentname <- names(sequences)
    newname <- currentname
    newname.part1 <- paste(unlist(lapply(strsplit(newname,split=' '),function(x) x[[1]])),unlist(lapply(strsplit(newname,split=' '),function(x) x[[2]])),sep='_')

    newname.part2 <- unlist(lapply(strsplit(newname,split=' '),function(x) x[[3]]))
    newname.part2 <- data.frame(species=newname.part2)
    newname.part2 <- left_join(newname.part2,correspondance.organism.subgroup,by='species')
    newname.part2 <- paste(newname.part2$SubGroup,newname.part2$species,sep='_')
    newname <- paste(newname.part1,newname.part2,sep='_')

    names(sequences) <- newname

    align.muscle <- muscle::muscle(sequences)
    dist1 <- stringDist(as(align.muscle,"DNAStringSet"), method="hamming")
    mytree1 <- upgma(dist1)

    pdf(paste0('99-results/phylogenetic.consensus.',genename[i],'.pdf'),width=10,height = 10)
    p <- ggtree(mytree1)
    p <- p  + geom_tiplab(offset=0) + xlim(NA, 2000) +geom_treescale(0.05,-2.5,width=1000,fontsize = 2,linesize = 0.5)
    plot(p)
    dev.off()
  }
}
