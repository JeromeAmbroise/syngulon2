#' A function to summarize the presence of some genes based on annotation
#'
#' @param bacteria.table : the bacteria table
#' @param annotationDir the directory where the annotation list can be found
#' @param collicin the collicin data.frame
#'
#' @return
#' @export
#'
#' @examples

analyse.annotation <- function(bacteria.table,collicin,annotationDir)
{
  correspondance.organism.subgroup <- bacteria.table%>%group_by(Organism) %>% dplyr::count(Organism, SubGroup) %>% dplyr::slice(which.max(n)) %>% dplyr::rename(species=Organism)

  annotation.list <- list.files(annotationDir,full.names = T,recursive = T)
  annotation.list.name <- gsub(annotation.list,pattern = '.csv',replacement = '')
  annotation.list.name <- gsub(annotation.list.name,pattern = '03-annotation//',replacement = '')

  annotation.list <- lapply(annotation.list,function(x) read.csv(x,stringsAsFactors = F))

  presence <- lapply(annotation.list, function(x) as.numeric(is.element(toupper(collicin$genename),toupper(x$gene))))
  presence <- matrix(unlist(presence),ncol=dim(collicin)[1],byrow = T)
  colnames(presence) <-  collicin$genename
  rownames(presence) <- annotation.list.name
  presence <- presence[,sort.list(colnames(presence))]
  presence <- data.frame(presence)
  species <- unlist(lapply(strsplit(rownames(presence),split='/'),function(x) x[[1]]))
  presence <- data.frame(species,presence)

  ##### summary at bacteria species

  presence.summary.n <- presence %>% group_by(species) %>% dplyr::summarise(n=n())
  presence.summary.mean <- presence %>% group_by(species) %>% select(-species) %>% dplyr::summarise_all(.funs = list(MEAN = ~ round(mean(x = .,na.rm=T),2)))
  presence.summary <- full_join(presence.summary.n,presence.summary.mean,by='species')
  presence.summary <- presence.summary %>% rename_all(funs(gsub("_MEAN", "", .)))

  ##### summary at phylum level

  presence.summary <- correspondance.organism.subgroup %>% select(-n) %>% right_join(presence.summary,by='species')
  presence.summary.subgroup.n <- presence.summary %>% group_by(SubGroup) %>% dplyr::summarise(n=n())
  presence.summary.subgroup.mean <- presence.summary %>% group_by(SubGroup) %>% select(-c(n,species,SubGroup)) %>% dplyr::summarise_all(.funs = list(MEAN = ~ round(mean(x = .,na.rm=T),2)))
  presence.summary.subgroup <- full_join(presence.summary.subgroup.n,presence.summary.subgroup.mean,by="SubGroup")
  presence.summary.subgroup <- presence.summary.subgroup %>% rename_all(funs(gsub("_MEAN", "", .)))


  toreturn <- list(presence.summary,presence.summary.subgroup)
  names(toreturn) <- c('summmary at species level','summary at phylum level')

  return(toreturn)
}

