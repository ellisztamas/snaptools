#' Value matching for RametIDs
#'
#' `match_ramet` returns a vector of the positions of (first) matches of
#' individual Antspec IDs in in a vector of RametIDs.
#'
#' We often have data for a ste of plants from the field, but the Antspec IDs
#' might not match the unique IDs after duplicate genotypes have been filtered.
#' Instead, we get a table with a column "RametID" that gives names of likely
#' duplicates as a string, which is difficult to parse.
#'
#' `match_ramet` works like the base R function `match` in that it finds the
#' (first) position of your AntspecIDs in the column of RametIDs.
#'
#' @param input Vector of AntspecIDs for plants you would like to extract for.
#' @param plantIDs Vector of primary genet names.
#' @param rametIDs Vector of RametID strings.
#'
#' @return And vector of integers giving the position of each element in input
#' in the vector of rametIDs. Use this to subset the Antspec data.table.
#'
#' @author Tom Ellis
match_ramet <- function(input, plantIDs, rametIDs){
  if(any(input == -9)){
    input[input == -9] <- NA
    warning("input contains entries with the name -9, probably missing data
This can cause problems if there are multiple -9 values in the rametIDs.
These have been coerced to NA.")
  }
  split_ramet <- strsplit(rametIDs, ",")
  split_table <- data.frame(sapply(split_ramet, function(x) x[1]),
                            sapply(split_ramet, function(x) x[2]),
                            sapply(split_ramet, function(x) x[3]),
                            sapply(split_ramet, function(x) x[4]),
                            sapply(split_ramet, function(x) x[5]))
  m <- data.frame(
    match(input, plantIDs),
    match(input, split_table[,1]),
    match(input, split_table[,2]),
    match(input, split_table[,3]),
    match(input, split_table[,4]),
    match(input, split_table[,5]))
  m[is.na(m)] <- -9
  ix <- apply(m, 1, max)
  ix[ix == -9] <- NA
  return(ix)
}


