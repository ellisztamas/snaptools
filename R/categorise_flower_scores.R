#' Classify flower colour scores.
#'
#' Classify snapdragon flower colour and genotype using red and yellow colour scores.
#'
#' Hybrid zone \emph{Antirrhinum majus} plants segregare for the loci \emph{Rosea}
#' and \emph{Sulphurea} controlling anthocyanin (red or magenta) and aurone
#' (yellow) pigmentation. `categorise_flower_scores` classifies flower colour into
#' one of six phenotype classes and returns genotypes for pigmentation loci
#' based on the scoring scheme developed by Annabel Whibley in her thesis.
#'
#' @param red Vector of red scores between 0 and 5.
#' @param yellow Vector of yellow scores between 0 and 4
#'
#' @author Tom Ellis, David Field
#' @references Whibley, Annabel (2004), Molecular and genetic variation
#' underlying the evolution of flower colour in Antirrhinum. Ph.D. thesis,
#' University of East Anglia.
#'
#' @return Data frame listing flower colour phenotype (yellow, white, weak orange,
#' full orange, weak rea and full red), and genotypes for \emph{Rosea}
#' and /emph{Sulfurea}.
#'
categorise_flower_scores <- function(red, yellow){
    flower_colour <- rep(NA, length(red)) # empty vector to store values

    if(length(red) != length(yellow)){
      stop("Vectors of red and yellow scores are of unequal length.")
    }
    if(any(red > 5 | red<0, na.rm = T)){
      stop("One or more values of red scores are greater than 5 or less than 0.")
    }
    if(any(yellow > 4 | yellow<0, na.rm = T)){
      stop("One or more values of yellow scores are greater than 4 or less than 0.")
    }
    # Assign flower_colourotypes
    flower_colour[(red >= 3   & red <= 5) & (yellow >= 0 & yellow < 2)] <- "FR"
    flower_colour[(red >= 3   & red <= 5) & (yellow >= 2 & yellow <=4)] <- "FO"
    flower_colour[(red >= 1.5 & red <  3) & (yellow >= 0 & yellow < 2)] <- "WR"
    flower_colour[(red >= 1.5 & red <  3) & (yellow >= 2 & yellow <=4)] <- "WO"
    flower_colour[(red >= 0   & red <1.5) & (yellow >= 0 & yellow < 2)] <- "Wh"
    flower_colour[(red >= 0   & red <1.5) & (yellow >= 2 & yellow <=4)] <- "Ye"

    # Assign Ros genotypes
    rosea <- rep(NA, length(red))
    rosea[red >= 3   & red <= 5] <- "R/R"
    rosea[red >= 1.5 & red <  3] <- "R/r"
    rosea[red >= 0   & red <1.5] <- "r/r"

    # Assign Sulf genotypes.
    sulfurea <- rep(NA, length(yellow))
    sulfurea[yellow >= 0 & yellow < 2] <- "S/+"
    sulfurea[yellow >= 2 & yellow <=4] <- "s/s"

    return(data.frame(flower_colour, rosea, sulfurea, stringsAsFactors = F))
}
