# import metadata table from GB search..
d = read.table("GB_all_vertebrates.txt", head = T, sep = "\t", comment.char = "", quote = "", stringsAsFactors = F)

d_SEL = d[,which(colnames(d)  %in% c("accession", "organism", "speciesID", "haplotype", "isolate", "pop_variant", "country", "lat_lon", "class"))]

d_SEL = d_SEL[which(is.na(d_SEL$country) == F),]

write.csv(d_SEL, file = "GB_metadata_SEL.csv")
