% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.genome.R
\name{download.genome}
\alias{download.genome}
\title{Downlaod genomes of selected species}
\usage{
download.genome(species, maxOrganism, indextostart, accessionDir, outDir)
}
\arguments{
\item{species}{a vector of character including the species that we want to analyze}

\item{maxOrganism}{a value with the number of genome we want to download for each species}

\item{indextostart}{: the index to restart the analysis. for example, if you interupted the analysis after 20 species, you can specify indextorestart=21}

\item{accessionDir}{the directory where the accession files can be found}

\item{outDir}{The output directory}
}
\value{

}
\description{
Downlaod genomes of selected species
}
