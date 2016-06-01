
clusts = read.table("seq_clusters_0.80_optimal.txt", sep = "\t", head = T, stringsAsFactors = F, quote = "", comment.char = "")
d = read.table("GB_all_vertebrates_GEOREF.txt", head = T, sep = "\t", quote = "", comment.char = "", stringsAsFactors = F)

## 1) thrash clusters with fewer than 20 sequences
clusts = clusts[which(clusts$n_seqs > 19),]

## 2) create and export a table with
# accession
# NCBI_organism.. (d$organism)
# cluster_ID (from 'clusts')
# genus
# species
# gene
# lon
# lat
# class (mamm, bird..)

# expand accession number for each cluster of each genus..
tab = data.frame(genus = rep(clusts$genus, clusts$n_seqs), species = rep(NA, sum(clusts$n_seqs)), clust_ID = rep(clusts$clust_id, clusts$n_seqs), accession = rep(NA, sum(clusts$n_seqs)),  stringsAsFactors = F)
for(i in 1:nrow(clusts)) {
  tab$species[tab$clust_ID == clusts$clust_id[i] & tab$genus == clusts$genus[i]] = unlist(strsplit(clusts$species[i], ","))
  tab$accession[tab$clust_ID == clusts$clust_id[i] & tab$genus == clusts$genus[i]] = unlist(strsplit(clusts$accession_ns[i], ","))
  # tab$matches[tab$clust_ID == clusts$clust_id[i] & tab$genus == clusts$genus[i]] = unlist(strsplit(clusts$matches[i], ","))
  if(i %% 10 == 0) {print(paste("cluster", i,"/", nrow(clusts), "processed.."))}
  }
tab$clust_fac_genus = paste(tab$genus, tab$clust_ID, sep = "_")
tab$clust_fac_species = paste(tab$species, tab$clust_ID, sep = "_")

# merge info from the big data table..
d = d[which(d$accession %in% tab$accession),]

# I am afraid of merge() with large tables.. so I will go the straight way..
d = d[order(d$accession),]
tab = tab[order(tab$accession),]
all(tab$accession == d$accession)
# [1] TRUE # (Pheeww..)

# tab$organism = d$organism
# tab$speciesID = d$speciesID
# tab$genusID = d$genusID
tab$lon = d$lon
tab$lat = d$lat
tab$organelle = d$organelle
tab$gene = d$gene
tab$CDS_product = d$CDS_product
tab$dloop = d$dloop
tab$class = d$class

tab = tab[order(tab$class, tab$clust_fac_genus, tab$clust_fac_species),]

## 3) count and filter clusters with at least 20 sequences per species..
tab_sp = tab
# tab_sp$clust_fac = gsub(" ", "_", paste(tab_sp$binomial_NCBI, tab_sp$clust_ID, sep = "_"))
ff = as.data.frame(table(tab_sp$clust_fac_species))
colnames(ff)[1] = "clust_fac_species"
ff = ff[-grep("absent", ff$clust_fac_species),]
tab_sp = tab_sp[which(tab_sp$clust_fac_species %in% ff$clust_fac_species[which(ff$Freq > 19)]),]
tab_sp = tab_sp[order(tab_sp$class, tab_sp$clust_fac_species),]

write.csv(tab, file = "genus_clusters_GEOREF.csv", row.names = F)
write.csv(tab_sp, file = "species_clusters_GEOREF.csv", row.names = F)








