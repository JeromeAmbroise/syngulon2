#' ajoute si le sous-groupe fait partie des bactéries gram + ou -
#'
#' @param presence.summary.subgroup
#'
#' @return
#' @export

Gram <- function(presence.summary.subgroup)
  {
  library(taxize)
  library(pracma)
  positive <- itis_downstream(id =956097 , downto="phylum")
  negative <- itis_downstream(id =956096 , downto="phylum")
  for (i in 1:length(presence.summary.subgroup$Phylum_n)) {
    if (isempty(grep(toupper(paste(positive$taxonname,collapse='|')),toupper(presence.summary.subgroup$Phylum_n[i])))) {
      presence.summary.subgroup$Phylum_n[i]=paste(presence.summary.subgroup$Phylum_n[i],"; Neg")
    }
    if (isempty(grep(toupper(paste(negative$taxonname,collapse='|')),toupper(presence.summary.subgroup$Phylum_n[i])))) {
      presence.summary.subgroup$Phylum_n[i]=paste(presence.summary.subgroup$Phylum_n[i],"; Pos")
    }
  }
  return(presence.summary.subgroup)
}
