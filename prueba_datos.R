#Setup
library(BIEN)
library(ape) #Package for working with phylogenies in R
library(maps) #Useful for making quick maps of occurrences
library(sf) # A package for spatial data

#Occurrence records for a species
#Quercus_rugosa <- BIEN_occurrence_species(species = "Quercus rugosa",
                                          cultivated = TRUE,
                                          all.taxonomy = TRUE,
                                          natives.only = TRUE, 
                                          observation.type = TRUE,
                                          political.boundaries = TRUE)
#str(Quercus_rugosa)
#head(Quercus_rugosa)



#Phylogenies
phylo <- BIEN_phylogeny_conservative()
plot.phylo(x = phylo, show.tip.label =  FALSE)
phylo_species <- phylo$tip.label


#Quercus_rugosa <- BIEN_occurrence_species(species = "Quercus rugosa")