#' Express genotype as an integer
#'
#' Convert a vector of raw LGC data to FAPS readable format.
#'
#' @param locus a vector of diploid genotypes in the format they are received
#' from LGC. Each element should be a string of two nucleotides with a separater
#' (e.g. "C:G"). Anything that is not in this format will be returned as NA.
#' @param sep a character string to separate the terms. Not NA_character_.
#'
#' @return A data.frame giving genotype IDs, and diploid genotypes as integers,
#' where 0 and 2 are homozygotes and 1 is the heterozygote. The 0 allele is
#' always whichever nucleotide comes first in the alphabet. For example, if a
#' locus has nucleotides C and G, diploid genotype C:C would be assigned as 0
#' and G:G as 2.
#'
#' @author Tom Ellis
#' @references Ellis T (2016), "*The role of pollinator-mediated selection in
#' the maintenance of a flower color polymorphism in an Antirrhinum majus hybrid
#' zone*", PhD thesis, IST Austria, available at
#' https://repository.ist.ac.at/526/
vectorise_locus <- function(locus, sep=":"){
  loclist <- strsplit(as.character(locus), split = sep)
  alleles <- sort(names(table(unlist(loclist)))) # assign major allele
  alleles <- alleles[alleles %in% c("A", "C", "G", "T")]

  loclist <- sapply(loclist, function(x) paste(x, collapse = ""))
  loclist[loclist == paste(c(alleles[1], alleles[1]), collapse="")] <- 0
  loclist[loclist == paste(c(alleles[1], alleles[2]), collapse="")] <- 1
  loclist[loclist == paste(c(alleles[2], alleles[1]), collapse="")] <- 1
  loclist[loclist == paste(c(alleles[2], alleles[2]), collapse="")] <- 2

  as.integer(loclist)
}
