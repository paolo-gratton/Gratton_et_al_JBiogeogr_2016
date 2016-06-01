# download 'taxdump.tar.gz' from ftp://ftp.cbi.edu.cn/pub/biomirror/taxonomy/ncbi/ and extract it to the directory 'NCBI_taxonomy'

# load library CHNOSZ (contains functions to read NCBI taxonomy files)
library(CHNOSZ)

## MAMMALS
# go to http://www.ncbi.nlm.nih.gov/Taxonomy/TaxIdentifier/tax_identifier.cgi
# click on 'choose file' and select the file 'all_mammals_WR.txt'
# download taxonomy report to the local directory as 'tax_report.txt'

# format it for R, e.g.:
system("sed 's/\t \t/\tNA\t/g; s/\t $/\tNA/g; s/\t|//g' tax_report.txt > all_mammals_WR_NCBI_taxID.txt")

taxlist = read.table('all_mammals_WR_NCBI_taxID.txt', head = T, sep = '\t')

# get the genera. families, etc..
genera = parent(id = taxlist$taxid, taxdir = './NCBI_taxonomy/taxdump', rank='genus', nodes=NULL)
families = parent(id = taxlist$taxid, taxdir = './NCBI_taxonomy/taxdump', rank='family', nodes=NULL)
orders = parent(id = taxlist$taxid, taxdir = './NCBI_taxonomy/taxdump', rank='order', nodes=NULL)

taxlist$genus = genera
taxlist$family = families
taxlist$order = orders

# export data to table
write.table(taxlist, file = 'all_mammals.taxlist.NCBI.txt', sep = '\t', quote = FALSE, row.names = F)

## we are now ready to launch 'search_GB.py'


## REPTILES
# go to http://www.ncbi.nlm.nih.gov/Taxonomy/TaxIdentifier/tax_identifier.cgi
# click on 'choose file' and select the file 'all_reptiles_TIGR_JCVI.txt'
# download taxonomy report to the local directory as 'tax_report.txt'

# format it for R, e.g.:
system("sed 's/\t \t/\tNA\t/g; s/\t $/\tNA/g; s/\t|//g' tax_report.txt > all_reptiles_TIGR_JCVI_NCBI_taxID.txt")

taxlist = read.table('all_reptiles_TIGR_JCVI_NCBI_taxID.txt', head = T, sep = '\t')

# get the genera. families, etc..
genera = parent(id = taxlist$taxid, taxdir = './NCBI_taxonomy/taxdump', rank='genus', nodes=NULL)
families = parent(id = taxlist$taxid, taxdir = './NCBI_taxonomy/taxdump', rank='family', nodes=NULL)
orders = parent(id = taxlist$taxid, taxdir = './NCBI_taxonomy/taxdump', rank='order', nodes=NULL)

taxlist$genus = genera
taxlist$family = families
taxlist$order = orders

# export data to table
write.table(taxlist, file = 'all_reptiles.taxlist.NCBI.txt', sep = '\t', quote = FALSE, row.names = F)

## we are now ready to launch 'search_GB.py'


## BIRDS
# go to http://www.ncbi.nlm.nih.gov/Taxonomy/TaxIdentifier/tax_identifier.cgi
# click on 'choose file' and select the file 'all_birds_IOC.txt'
# dowload taxonomy report to the local directory as 'tax_report.txt'

# format it for R, e.g.:
system("sed 's/\t \t/\tNA\t/g; s/\t $/\tNA/g; s/\t|//g' tax_report.txt > all_birds_IOC_NCBI_taxID.txt")

taxlist = read.table("all_birds_IOC_NCBI_taxID.txt", head = T, sep = '\t')

# get the genera. families, etc..
genera = parent(id = taxlist$taxid, taxdir = './NCBI_taxonomy/taxdump', rank='genus', nodes=NULL)
families = parent(id = taxlist$taxid, taxdir = './NCBI_taxonomy/taxdump', rank='family', nodes=NULL)
orders = parent(id = taxlist$taxid, taxdir = './NCBI_taxonomy/taxdump', rank='order', nodes=NULL)

taxlist$genus = genera
taxlist$family = families
taxlist$order = orders

# export data to table
write.table(taxlist, file = 'all_birds.taxlist.NCBI.txt', sep = '\t', quote = FALSE, row.names = F)

## we are now ready to launch 'search_GB.py'


## AMPHIBIANS
# go to http://www.ncbi.nlm.nih.gov/Taxonomy/TaxIdentifier/tax_identifier.cgi
# click on 'choose file' and select the file 'all_mammals_WR.txt'
# download taxonomy report to the local directory as 'tax_report.txt'

# format it for R, e.g.:
system("sed 's/\t \t/\tNA\t/g; s/\t $/\tNA/g; s/\t|//g' tax_report.txt > all_amphibians_IUCN_NCBI_taxID.txt")
 
taxlist = read.table('all_amphibians_IUCN_NCBI_taxID.txt', head = T, sep = '\t')

# get the genera. families, etc..
genera = parent(id = taxlist$taxid, taxdir = './NCBI_taxonomy/taxdump', rank='genus', nodes=NULL)
families = parent(id = taxlist$taxid, taxdir = './NCBI_taxonomy/taxdump', rank='family', nodes=NULL)
orders = parent(id = taxlist$taxid, taxdir = './NCBI_taxonomy/taxdump', rank='order', nodes=NULL)

taxlist$genus = genera
taxlist$family = families
taxlist$order = orders

# export data to table
write.table(taxlist, file = 'all_amphibians.taxlist.NCBI.txt', sep = '\t', quote = FALSE, row.names = F)

## ready to launch '3.search_INSDC.r'
