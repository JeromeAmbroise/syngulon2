#' A function to summarize the presence of some genes based on blast table
#'
#' @param bacteria.table : the bacteria table
#' @param blastresult the blast result data.frame
#'
#' @return
#' @export
#'
#' @examples

analyse.blast.table <- function(bacteria.table,blastresult)
{

  correspondance.organism.subgroup <- bacteria.table%>%group_by(Organism) %>% dplyr::count(Organism, SubGroup) %>% dplyr::slice(which.max(n)) %>% dplyr::rename(species=Organism)
  presence <- apply(blastresult,2,function(x) as.numeric(x!=''))
  species <- unlist(lapply(strsplit(rownames(blastresult),split='/'),function(x) x[[2]]))
  presence <- data.frame(species,presence)
  rownames(presence) <- rownames(blastresult)
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


  toreturn <- list(presence,presence.summary,presence.summary.subgroup)
  names(toreturn) <- c('presence at species level','summmary at species level','summary at phylum level')

  return(toreturn)
}
