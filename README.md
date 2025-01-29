# Measuring the Strength of Spatial Sorting

The bioRxiv version of the paper is available at https://doi.org/10.1101/2025.01.17.633577.

## Description of the data and file structure

1. The folder—/data/images—contains images used to create Figures 1 and 2 in the paper. These images were taken by Mattheau S. Comerford. These images are

   a) flooded.jpg—Image of the flooded site after the hurricane

   b) unflooded.jpg—Image of the flooded site before the hurricane.
2. The folder—/data—contains four CSV datasets

    a) genetic_crosses_Comerford.csv—morphological measurements and pedigree data of soapberry bugs obtained by crossing insects in the lab.

    b) Kingsolver_selection_gradients.csv—Metadata on the strength of spatial sorting from Kingsolver and Diamond (2010).

    c) Spatial_Sorting_Comerford_Soapberrybugs.csv—morphological data of soapberry bugs in Greater Houston collected during a monitoring experiment from 2016 to 2020.

    d) site_metadata.csv—metadata containing information about the location of survey sites in Greater Houston.
3. The folder—/Figures—contains two folders (Fig 1 and Fig 2), which contain R code to generate the two figures in the paper.
4. The folder—/code—contains R and Stan code files to estimate evolutionary parameters to quantify the strength of spatial sorting during a recolonization of habitats that were extripated by Hurricane Harvey.

## Code/software

To analyze data, we used R programming language and Stan.

1. R Core Team (2021). R: A language and environment for statistical ##   computing. R Foundation for Statistical Computing, Vienna, Austria. ##   URL [https://www.R-project.org/](https://www.R-project.org/).
2.  Carpenter, B., A. Gelman, M. D. Hoffman, D. Lee, B. Goodrich, M. Betancourt, M. A. Brubaker, J. Guo, P. Li, and A. Riddell. 2017. Stan: A probabilistic programming language. Journal of Statistical Software **76**:1-32.

## Access information

Other publicly accessible locations of the data:

* Kingsolver_selection_gradients.csv was obtained from [https://doi.org/10.5061/dryad.7996](https://doi.org/10.5061/dryad.7996)
* Spatial_Sorting_Comerford_Soapberrybugs.csv was modified from https://doi.org/10.5061/dryad.tht76hf4t

