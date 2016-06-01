## This is a script to launch the function clusterGBbyGenus() ('clusterGBbyGenus.r') on the GB data with some georeferencing (either 'original' or assigned..)

georef_data = read.csv(file = "georef_data.csv", head = T, stringsAsFactors = F)

# add separate columns for Linnean genus and species names
conv_tab = read.csv("iucn-ncbi_conversion.csv", head = T, stringsAsFactors = F)

georef_data$organism = gsub("[(]","",georef_data$organism)
georef_data$organism = gsub("[)]","",georef_data$organism)
georef_data$organism = gsub("  "," ",georef_data$organism)
georef_data$organism = gsub(","," ",georef_data$organism)

georef_data_conv = merge(georef_data, conv_tab, by.x = "organism", by.y = "ncbi.organism")
colnames(georef_data_conv)[which(colnames(georef_data_conv) == "iucn.spe")] = "species"
colnames(georef_data_conv)[which(colnames(georef_data_conv) == "iucn.gen")] = "genus"

georef_data_conv = georef_data_conv[-which(is.na(georef_data_conv$genus) == T),]
georef_data_conv$species[which(is.na(georef_data_conv$species) == T)] = "absent"
georef_data_conv$species = gsub(" ", "_", georef_data_conv$species)

# launch clustering function..
library(ape)

source("clusterGBbyGenus.r")

identity = 0.80
output_file = paste("seq_clusters", identity, "optimal.txt", sep = "_")
errors_file = paste("clustering_errors", identity, "optimal.txt", sep = "_")

clusterGBbyGenus(
  sumtab = georef_data_conv,
  output_file = output_file,
  errors_file = errors_file,
  superclust = F,
  path_to_uclust = "uclustq1.2.22_i86linux64",
  identity = 0.80,
  maxlen = 5000,
  minlen = 200,
  rev = "--rev",
  more_uclust_args = "--optimal"
  )

  
  
  
  
  
  
  
  
  
  
  
  