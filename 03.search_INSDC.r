# This short script is a wrapper to launch the pyhton script 'search_GB.py' and collect results in a single file
system("python search_GB.py all_mammals.taxlist.NCBI.txt all_mammalsGB.csv")
system("python search_GB.py all_reptiles.taxlist.NCBI.txt all_reptilesGB.csv")
system("python search_GB.py all_birds.taxlist.NCBI.txt all_birdsGB.csv")
system("python search_GB.py all_amphibians.taxlist.NCBI.txt all_amphibiansGB.csv")

mamm = read.table("all_mammalsGB.csv", stringsAsFactors = F, head = T, quote = "", sep = "\t", comment.char = "")
rept = read.table("all_reptilesGB.csv", stringsAsFactors = F, head = T, quote = "", sep = "\t", comment.char = "")
bird = read.table("all_birdsGB.csv", stringsAsFactors = F, head = T, quote = "", sep = "\t", comment.char = "")
amph = read.table("all_amphibiansGB.csv", stringsAsFactors = F, head = T, quote = "", sep = "\t", comment.char = "")

all = rbind(mamm, rept, bird, amph)
# remove duplicated accessions
all = all[!duplicated(all$accession),]

write.table(all, file = "GB_all_vertebrates.txt", row.names = F, sep = "\t")
