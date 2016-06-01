## MAMMALS
# go to http://www.departments.bucknell.edu/biology/resources/msw3/
# download taxonomy file as 'msw3-all.csv' by clicking on "EXPORT ENTIRE TAXONOMY AS CSV"
# load file in R
mammals_tax = read.table("msw3-all.csv", sep = ",", quote ='"', header = T, stringsAsFactors = F)
# make a small correction (note: is not a very common species "The entirety of known material for this species consists of a single, poor-quality dry skin, a fluid-preserved animal, and a cranium and mandible")...
mammals_tax$Species[5323] = "buettneri"
specieslist = paste(mammals_tax$Genus, mammals_tax$Species)[which(mammals_tax$TaxonLevel == "SPECIES")]
write.table(specieslist, file = "all_mammals_WR.txt", row.names = F, col.names = F, quote = F)

## REPTILES
# go to http://www.reptile-database.org/data/
# download taxonomy file 'reptile_checklist_2014_12.xls' by clicking on "Release 1 Dec 2014 (most recent)"
# brutally copy and paste the first column into a text file, and save it as 'all_reptiles_TIGR_JCVI.txt'

## BIRDS
# go to http://www.worldbirdnames.org/ioc-lists/
# download taxonomy file 'IOC_Names_File_Plus-4.4c.xls' by clicking on "IOC Lists >> Master Lists >> Life List+"
# save file as 'IOC_Names_File_Plus-4.4c.csv'
# delete second line:
system("sed 2d IOC_Names_File_Plus-4.4c.csv > IOC_Names_File_Plus-4.4c.txt")
# load file in R
birds_tax = read.table("IOC_Names_File_Plus-4.4c.txt", sep = "\t", quote ='"', header = T, stringsAsFactors = F)
specieslist = birds_tax$Scientific.name[birds_tax$Rank == "Species"]
write.table(specieslist, file = "all_birds_IOC.txt", row.names = F, col.names = F, quote = F)

## AMPHIBIANS
# go to http://www.iucnredlist.org/technical-documents/spatial-data#amphibians
# download archive file 'AMPHIBIANS.zip' (222MB)
# extract it to a directory 'amphibians_shp' and export list of binomials from AMPHIBIANS.dbf database to all_amphibians_IUCN.txt from R...
system("dbview ./amphibians_shp/AMPHIBIANS.dbf | grep binomial | sort | uniq | cut -f 2 -d ':' | sed 's/^ //g' > all_amphibians_IUCN.txt")
