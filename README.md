# snaptools

Tools for analysing data from the snapdragon hybrid zone in the collada de Toses.

snaptools provides tools for running common operations on
data from the Antirrhinum majus hybrid zone project. The aim is to provide
functions for some specific tasks that multiple people might want to implement,
to avoid replication of effort and creeping errors.

## Basic functions

* `categorise_flower_scores` uses scores for red and yellow pigmentation to infer
flower colour phenotype, and genotypes for *Rosea* and *Sulfurea*.

* `match_ramet`: David Field and Harald Ringbauer went through the Antspec and 
genotype data to check inconsistencies and identify plants that had been tagged
and genotypes more than once. Where duplicates are found, they were grouped as
a unique genotype (a genet) with a label `PlantID_Final`. Names of any duplicates
are given in the column `RametID`, which is a string of undetermined length that
is difficult to parse. `match_ramet` is an efficient way to look for PlantIDs
that could be in either `PlantID_Final` or `RametID`. Usage is the same as the
base R function `match`; see `?match` for details and caveats with this approach.

* `density_frequency` uses GPS information data to define the density of
plants around a set of focal plants.
See `?density_frequency` for details of how density is calculated.
If phenotype categories are given, it also returns the phenotypic frequency of
the focal plants as the density of neighbours of the same phenotype divided by
the density of all neighbours.

* `lgc2faps`: Convert a dataframe from the table of strings we get from LGC to
a data.frame of integers. Output format can be imported into FAPS directly for
paternity analysis.

* `dgennorm`: Calculate the PDF of the generalised normal distribution.

* `vectorise_locus`: Convert a vector of raw data from LGC into a vector of
inetegers.

## Installation

`snaptools` can be installed from with R straight from GitHub using `devtools`.

```
devtools::install_github("ellisztamas/snaptools")
```

If you don't have `devtools` installed, install it with `install.packages('devtools')`.

## Dependencies
None

## Author
Tom Ellis (thomas.ellis@gmi.oeaw.ac.at)
