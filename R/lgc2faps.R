#' Convert a dataframe from LGC to FAPS format.
#'
#' Convert a dataframe from the table of strings we get from LGC to a data.frame
#' of integers. Output format can be imported into FAPS directly for paternity
#' analysis.
#'
#' @param lgc_raw data.frame of raw SNP data in the format we receive from  LGC.
#' The first column should show genotype IDs. Subsequent columns should show
#' string data indicating diploid genotypes, such as "G:G", "C:A" etc. Anything
#' that is not a pair of nucleotides with a separator will be treated as missing
#' and returned as NA.
#' @param sep String indicating the separator between nucleotides. Defaults to
#' ":".
#' @return A data.frame giving genotype IDs, and diploid genotypes as integers,
#' where 0 and 2 are homozygotes and 1 is the heterozygote. The 0 allele is
#' always whichever nucleotide comes first in the alphabet. For example, if a
#' locus has nucleotides C and G, diploid genotype C:C would be assigned as 0
#' and G:G as 2.
#' @author Tom Ellis
#' @references Ellis T (2016), "*The role of pollinator-mediated selection in the
#' maintenance of a flower color polymorphism in an Antirrhinum majus hybrid
#' zone*", PhD thesis, IST Austria, available at https://repository.ist.ac.at/526/
#' @export
lgc2faps <- function(lgc_raw, sep=":"){
  faps <- apply(lgc_raw, 2, vectorise_locus, sep)
  faps <- data.frame(faps)
  faps[,1] <- lgc_raw[,1]
  return(faps)
}
