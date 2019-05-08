#' Plant density and phenotypic frequency
#'
#' For a set of flocal plants you are interested in, calculate the density of
#' neighbouring plants, and (optionally) their local phenotypic frequency.
#'
#' Density can be calculated two ways. Most simply, we can count the number of
#' neighbours within a given radius, which is simple to interpret, but gives
#' coarse resolution. I have found that small changes to the radius can have
#' enormous effects on estimates of density.
#'
#' Alternatively, one can apply a Gaussian function to distances using the
#' standard deviation parameter as a scale. The density of plants around a
#' focal plant is then the sum of this function on distances to all neighbours
#' in the population. This has the advantage of smoothing effects with distance,
#' and ensuring that nearer neighbours have a greater influence than mor distant
#' neighbours. However, interpretation is not as straightforward as using a
#' fixed radius.
#'
#' Thirdly, the Gaussian approach can be extended by using a generalised Gaussian
#' distribution by specifying a `shape` parameter. This distribution is useful
#' for describing dispersal kernels, because it allows for rare long-distance
#' migration by modelling this as leptokurtosis.
#' See ?snaptools::d_generalised_gaussian for details.
#'
#' Phenotypic frequency is defined as the density of neighbours of the same
#' phenotype relative to the density of all neighbours. Thus, it is only defined
#' for categorical phenotypes.
#'
#' @param focal,population Positional information about a set of focal plants
#' and plants from the wider population of neighbours. This may be a vector of
#' positions, or (more likely) a data.frame of coordinates, with a column for
#' each axis. Arbitrary numbers of coordinate axes are allowed.
#' @param scale Float indicating the scale at which to look at neighbours.
#' If `density_function` is set to `gaussian`, this is the standard deviation of
#' the Gaussian function to use.
#' If `density_function` is set to `generalised`, this the `scale` paramater of the
#' generalised gaussian function. See ?snaptools::d_generalised_gaussian for
#' details.
#' If `density_function` is set to `radius`, this is the radius within which
#' plants are classified as neighbours.
#' @param shape Shape parameter for the generalised gaussian distribution. Only
#' functional if `density_function` is set to `generalised`.
#' See ?snaptools::d_generalised_gaussian for details.
#' @param focal_phenotypes,population_phenotypes Optional vectors of phenotype
#' data for each individual in focal and population.
#' @param density_function String indicating whether to calculate density using
#' a Gaussian function of distance, or counting the number of neighbours within
#' a certain radius. Must take the values 'gaussian', generalised or 'radius'.
#' See `Description` for details.
#'
#' @return A vector of densities for each plant in focal. If phenotypes are
#' supplied, a data.frame of densities and phenotypic frequencies are returned.
#'
#' @author Tom Ellis
#' @references Ellis T (2016), "*The role of pollinator-mediated selection in the
#' maintenance of a flower color polymorphism in an Antirrhinum majus hybrid
#' zone*", PhD thesis, IST Austria, available at https://repository.ist.ac.at/526/
#'
#' @export
density_frequency <- function(focal, population, scale, shape=2, focal_phenotypes = NULL, population_phenotypes= NULL, density_function='gaussian'){
  if(ncol(focal) != ncol(population)){
    stop("Number of columsn in focal does not match populations")
  }

  # Create a Euclidean distance matrix between all pairs of individuals.
  dist_mat <- matrix(0, nrow=nrow(focal), ncol=nrow(population))
  for(i in 1:ncol(focal)) dist_mat <- dist_mat + outer(focal[,i], population[,i], '-')^2 # squared distance for each axis
  dist_mat <- sqrt(dist_mat) # sqrt to get the hypotenuse.

  # Calculate a paiwise density score.
  if(density_function == "gaussian"){
    # If Gaussian weighting is used, this is the value of the Gaussian function for each pairwise distance
    count_matrix <- dnorm(dist_mat, sd = scale)
  }else if(density_function == "generalised"){
    # Use the generalised gaussian probability function.
    # If shape=2 (default), this should return the same as if `density_function == "gaussian`
    count_matrix <- dgennorm(dist_mat,  scale = scale, shape=shape)
  } else if(density_function == "radius"){
    # If using a discrete radius, count the number of neighbours with a set radius.
    count_matrix <- dist_mat < scale
  } else{
    stop("density_function should be either 'gaussian' or 'radius'.")
  }
  # Density is the sum of pairwise scores for each focal plant, regardless of how these are calculated.
  density <- rowSums(count_matrix, na.rm = TRUE)

  # If phenotypes are included, calculate phenotypic frequency as well.
  if(!is.null(focal_phenotypes) & !is.null(population_phenotypes)){
    if(length(focal_phenotypes) != nrow(focal)){
      stop("If phenotypes of focal plants are given, this vector should have the same length as the rows in the data.frame of GPS coordinates.")
    }

    if(length(population_phenotypes) != nrow(population)){
      stop("If phenotypes of population plants are given, this vector should have the same length as the rows in the data.frame of GPS coordinates.")
    }

    if(class(population_phenotypes) != class(focal_phenotypes)){
      stop(paste("focal_phenotypes is of class", class(focal_phenotypes), "but population phenotypes is of class", class(population_phenotypes)))
    }

    # Matrix of neighbours of the same phenotype as the focal plant.
    match_matrix  <- lapply(focal_phenotypes, function(x) x == population_phenotypes)
    match_matrix <- do.call('rbind', match_matrix)
    # Pairwise density scores for patching neighbours only.
    match_density <- rowSums(count_matrix * match_matrix, na.rm = TRUE)
    # Phenotypic frequency is the frequency of matching neighbours relative to total density
    frequency     <- match_density / density

    return(data.frame(density, frequency))
  } else{
    return(density)
  }
}
