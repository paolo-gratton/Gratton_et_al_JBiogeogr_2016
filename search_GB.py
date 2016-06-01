### search_GB.py ##

### This is a python script to make a table of metadata associated with GenBank sequences
### It takes as input a 'taxlist.NCBI' file (see '2.get_NCBI_taxonomy.r') with a table of species associated with NCBI taxonIDs for (at least) species and genus.
### Working genus-wise, it will build a table containing the following fields, mostly extracted from xml-formatted GB files:
# accession
# definition
# organism (latin binomial)
# speciesID (NCBI taxonID for the species)
# genusID (NCBI taxonID for the species)
# gene (comma separated list of gene names in the GENE sections of the GB file)
# CDS_product (comma separated list of names of CDS product in the CDS sections of the GB file)
# dloop (it will be 'dloop_section' if the GB file contains a D-loop section, 'dloop_misc' if the words "D-loop" or "Control Region" appear in the misc_feature section or 'dloop_def' i the same words appear in the GBSeq_definition; some flexibility in the spelling and hyphenation is allowed by regex.. see below)
# tRNA (comma separated list of tRNA products in the tRNA sections of the GB file)
# rRNA (comma separated list of rRNA products in the rRNA sections of the GB file)
# haplotype
# isolate
# pop_variant
# country (often there is more information than the country alone.. Unfortunately the format is a bit complicated.. where there is more than the country, it is normal to have it after a ':' with further levels separated by ',' or included within brackets..)
# lat_lon (the usual format is decimal degreees: X.XXX N X.XXX E.. though not entirely consistent!)
# GBReference_pubmed
# GBReference_authors
# GBReference_title
# GBReference_journal
### For each of the genera listed in the input file, it also returns a fasta file with all processed sequences

### ARGUMENTS to be passed in the command line IN THIS ORDER!!
# inputfile (mandatory)
# outputfile (optional, default = 'GBoutput_table.csv')
# removed_species (optional, comma separated list of latin binomials or NCBI taxonIDs, see below for default.. NOTE!! Custom definition of 'removed_species' will not work if 'outputfile' is not provided as well!!)

### e.g.: python search.GB.py mammals.taxlist.NCBI.txt GBmammals.csv 'Mus musculus, Homo sapiens'

from Bio import Entrez
import os
from pandas import DataFrame
import re
import sys

inputfile = sys.argv[1]

# check if output file name was given as argument, otherwise use the default name 'GBoutput_table.csv'
if len(sys.argv) > 2 :
    outputfile = sys.argv[2]
else :
    outputfile = "GBoutput_table.csv"

if len(sys.argv) > 3 :
    removed_species = sys.argv[3].split(',')
else :
    removed_species = ["Mus musculus", "Homo sapiens", "Felis silvestris", "Rattus rattus", "Rattus norvegicus", "Chrysochloris asiatica", "Oryctolagus cuniculus", "Felis catus", "Canis lupus", "Mustela putorius", "Sus scrofa", "Bos taurus", "Ovis aries", "Capra hircus"]

## define a very useful function to search into dictionaries  (returns a LIST of dictionaries - though possibly of len 1 - not a dictionary)
def search_features(name, key, table) :
  return [element for element in table if element[key] == name]
## define this much less elegant and more specialized function to search for 'taxonID'.. since it has not a field of its own, but is 'confused' with other identifiers..
def search_taxonID(table):
  return [element for element in table if element["GBQualifier_name"] == "db_xref" and element["GBQualifier_value"].split(":")[0] == 'taxon']  
## define a rather stupid function to rearrange columns in a data frame
def set_column_sequence(dataframe, seq):
    '''Takes a dataframe and a subsequence of its columns, returns dataframe with seq as first columns'''
    cols = seq[:] # copy so we don't mutate seq
    for x in dataframe.columns:
	if x not in cols:
	    cols.append(x)
    return dataframe[cols]

# create a csv file to store data
# create empty lists
genera = []
accessions = []
definitions = []
organisms = []
taxonIDs = []
countries = []
lat_lons = []
genes = []
rRNAs = []
tRNAs = []
genera = []
isolates = []
haplotypes = []
pop_variants = []
dloop = []
CDSs = []
organelles = []
ref_pubmed = []
ref_authors = []
ref_journal = []
ref_titles = []

# write empty fields to csv file
restab = DataFrame({'genusID': genera, 'accession': accessions, 'organism': organisms, 'speciesID': taxonIDs, 'country' : countries, 'lat_lon': lat_lons, 'gene' : genes, 'rRNA' : rRNAs, 'tRNA' : tRNAs,'isolate': isolates, 'definition': definitions, 'haplotype': haplotypes, 'pop_variant': pop_variants, 'dloop' : dloop, 'CDS_product' : CDSs, 'organelle' : organelles, 'ref_pubmed': ref_pubmed, 'ref_authors': ref_authors, 'ref_journal': ref_journal, 'ref_titles': ref_titles})
restab = set_column_sequence(restab, ['accession', 'definition', 'organism','speciesID', 'genusID', 'organelle', 'gene', 'CDS_product', 'dloop', 'tRNA', 'rRNA', 'haplotype', 'isolate', 'pop_variant', 'country', 'lat_lon', 'ref_pubmed', 'ref_journal', 'ref_authors', 'ref_titles'])
restab.to_csv(outputfile, "\t", header = True)

## get the list of generaIDs from taxlist.NCBI file..n (exclude model species)
# create command to retrieve genus names after removing unwanted species

cmd = "sed '"
for species_to_remove in removed_species[:-1]:
  cmd = cmd + "/" + species_to_remove + "/d; "
cmd = cmd + "/" + removed_species[-1] + "/d' "
cmd = cmd + inputfile + " | cut -f 5 | sed  '/NA/d; /genus/d' | sort -n | uniq > temp.generaID.txt"

os.system(cmd)

temp_generaID = open('temp.generaID.txt', "r")
list_of_genera = []
for line in temp_generaID :
  list_of_genera.append(line.split('\n')[0])


counter = 1
for genus in list_of_genera :
  print ('working on genus ' + genus + '.. ' + str(counter) + ' out of ' + str(len(list_of_genera)) + ' genera')
  
  # reset lists for values to be stored
  accessions = []
  definitions = []
  organisms = []
  taxonIDs = []
  countries = []
  lat_lons = []
  genes = []
  rRNAs = []
  tRNAs = []
  genera = []
  isolates = []
  haplotypes = []
  pop_variants = []
  dloop = []
  CDSs = []
  organelles = []
  ref_pubmed = []
  ref_authors = []
  ref_journal = []
  ref_titles = []
  
  # open file for FASTA sequences storage
  #filename = genus + ".fasta"
  #out = open(filename, "w")
  
  # get the list of speciesID for the genus from taxlist.NCBI file..
  
  cmd = "sed '"
  for species_to_remove in removed_species[:-1]:
    cmd = cmd + "/" + species_to_remove + "/d; "
  cmd = cmd + "/" + removed_species[-1] + "/d' "
  cmd = cmd + inputfile + "| grep -w " + genus + " | cut -f 4 > temp.speciesID.txt"

  os.system(cmd)
  
  temp_speciesID = open('temp.speciesID.txt', "r")
  list_of_species = []
  for line in temp_speciesID :
    list_of_species.append(line.split('\n')[0])
  
  for species in list_of_species :
    trm = 'txid'
    trm += species
    trm += '[orgn] AND ddbj embl genbank with limits[filt] NOT transcriptome[All Fields] NOT mRNA[filt] NOT scaffold[All Fields]'
    # build genbank search term..
    
    # provide email
    Entrez.email = "paolo_gratton@eva.mpg.de"
    
    # download list of GIs for the species
    print ('downloading sequences for species txid:' + species + ' in genus ' + genus)
    handle = Entrez.esearch(db="Nucleotide", term= trm, RetMax= "10000")
    record = Entrez.read(handle, validate = False)
    list_of_ids = record['IdList']
    
    if len(list_of_ids) > 0 :
      Entrez.email = "paolo_gratton@eva.mpg.de" # provide email
      ### read entry in xml and extract features..
      # use Entrez.efetch to read data in .xml format
      handle = Entrez.efetch(db="nucleotide", id=list_of_ids, retmode="xml")
      record = Entrez.read(handle, validate = False)

      ## search and store data in the entry.. USING 'GENERATOR EXPRESSIONS' (whatever they are!!)

      print ('collecting data from ' + str(len(list_of_ids)) + ' sequences for species txid:' + species + ' in genus ' + genus)
      # loop through all records
      for i in range(len(record)) :
	if float(record[i]['GBSeq_length']) < 20001 : # I am ignoring all sequences longer than 20000 bp!! (they tend to be fucking genomic scaffolds..), but it will likely not be enough to let it go through everything!!!
	  genera.append(genus) # store the genus for this sequence
	  accessions.append(record[i]['GBSeq_primary-accession']) # store the sequence accession number
	  definitions.append(record[i]['GBSeq_definition']) # store sequence definition (description)
	  # name the 'feature table' section in the entry
	  feattab = record[i]['GBSeq_feature-table']
	  
	  refs = record[i]['GBSeq_references']  # I decided to take only the first reference, at the moment..
	  if len(re.findall("GBReference_pubmed",str(record[i]['GBSeq_references'][0].keys()))) > 0:
	    ref_pubmed.append(record[i]['GBSeq_references'][0]['GBReference_pubmed'])
	  else:
	    ref_pubmed.append('NA')
	  if len(re.findall("GBReference_authors",str(record[i]['GBSeq_references'][0].keys()))) > 0:
	    ref_authors.append(record[i]['GBSeq_references'][0]['GBReference_authors'])
	  else:
	    ref_authors.append('NA')
	  if len(re.findall("GBReference_title",str(record[i]['GBSeq_references'][0].keys()))) > 0: 
	    ref_titles.append(record[i]['GBSeq_references'][0]['GBReference_title'])
	  else:
	    ref_titles.append('NA')
	  if len(re.findall("GBReference_journal",str(record[i]['GBSeq_references'][0].keys()))) > 0: 
	    ref_journal.append(record[i]['GBSeq_references'][0]['GBReference_journal'])
	  else:
	    ref_journal.append('NA')
	    
	  # get and name the 'source' section (s)
	  sourcetab = search_features('source', 'GBFeature_key', feattab)
	  # output.write(record[i]['GBSeq_primary-accession'] + ": " + str(len(sourcetab))+ "\n") # this was just a check!
	  
	  # create temporary objects to collect values of the fields of interest.. note that we run a loop through 'sourcetab'.. It is probably not necessary, since there should be only one 'source' section.. but we do it this way because the 'gene' sections will be often multiple..
	  temp_organisms = []
	  temp_taxonIDs = []
	  temp_countries = []
	  temp_lat_lons = []
	  temp_isolates = []
	  temp_pop_variants = []
	  temp_haplotypes = []
	  temp_organelles = []
	  
	  if len(sourcetab) > 0 :
	    for j in range(len(sourcetab)) :
	      if len(search_features('organism', 'GBQualifier_name', sourcetab[j]['GBFeature_quals'])) > 0 : # store the organism name (Linnean binomial)	    
		temp_organisms.append(search_features('organism', 'GBQualifier_name', sourcetab[j]['GBFeature_quals'])[0]['GBQualifier_value']) # we assume that there is only one field 'organism' per each source field..
	      else :
		temp_organisms.append('NA')

	      # store organelle information
	      if len(search_features('organelle', 'GBQualifier_name', sourcetab[j]['GBFeature_quals'])) > 0 :
		temp_organelles.append(search_features('organelle', 'GBQualifier_name', sourcetab[j]['GBFeature_quals'])[0]['GBQualifier_value']) # we assume that there is only one field 'organelle' per each source field..
	      else :
		temp_organelles.append('NA') 

	      # store the isolate name
	      if len(search_features('isolate', 'GBQualifier_name', sourcetab[j]['GBFeature_quals'])) > 0 :
		temp_isolates.append(search_features('isolate', 'GBQualifier_name', sourcetab[j]['GBFeature_quals'])[0]['GBQualifier_value']) # we assume that there is only one field 'isolate' per each source field..
	      else :
		temp_isolates.append('NA')
		
	      # store the pop_variant
	      if len(search_features('pop_variant', 'GBQualifier_name', sourcetab[j]['GBFeature_quals'])) > 0 :
		temp_pop_variants.append(search_features('pop_variant', 'GBQualifier_name', sourcetab[j]['GBFeature_quals'])[0]['GBQualifier_value']) # we assume that there is only one field 'isolate' per each source field..
	      else :
		temp_pop_variants.append('NA')
		
	      # store the haplotype
	      if len(search_features('haplotype', 'GBQualifier_name', sourcetab[j]['GBFeature_quals'])) > 0 :
		temp_haplotypes.append(search_features('haplotype', 'GBQualifier_name', sourcetab[j]['GBFeature_quals'])[0]['GBQualifier_value']) # we assume that there is only one field 'isolate' per each source field..
	      else :
		temp_haplotypes.append('NA') 

	      # store the NCBI taxonID
	      if len(search_taxonID(sourcetab[0]['GBFeature_quals'])) > 0 :
		temp_taxonIDs.append(search_taxonID(sourcetab[0]['GBFeature_quals'])[0]['GBQualifier_value'].split(':')[1]) # we assume that there is only one sub-field 'taxon:' per each source field..
	      else :
		temp_taxonIDs.append('NA')    
	  
	      # store country information  
	      if len(search_features('country', 'GBQualifier_name', sourcetab[j]['GBFeature_quals'])) > 0 :
		temp_countries.append(search_features('country', 'GBQualifier_name', sourcetab[j]['GBFeature_quals'])[0]['GBQualifier_value']) # we assume that there is only one field 'organism' per each source field..
	      else :
		temp_countries.append('NA')     
		
	      # store latitude and longitude
	      if len(search_features('lat_lon', 'GBQualifier_name', sourcetab[j]['GBFeature_quals'])) > 0 :
		temp_lat_lons.append(search_features('lat_lon', 'GBQualifier_name', sourcetab[j]['GBFeature_quals'])[0]['GBQualifier_value']) # we assume that there is only one field 'lat_lon' per each source field..
	      else :
		temp_lat_lons.append('NA')
		
	    organisms.append(','.join(temp_organisms))
	    taxonIDs.append(','.join(temp_taxonIDs))
	    countries.append(','.join(temp_countries))
	    lat_lons.append(','.join(temp_lat_lons))
	    isolates.append(','.join(temp_isolates))
	    haplotypes.append(','.join(temp_haplotypes))
	    pop_variants.append(','.join(temp_pop_variants))
	    organelles.append(','.join(temp_organelles))
	  else :
	      organisms.append('NA')
	      taxonIDs.append('NA')
	      countries.append('NA')
	      lat_lons.append('NA')
	      isolates.append('NA')
	      haplotypes.append('NA')
	      pop_variants.append('NA')
	      organelles.append('NA')
	  
	  # get and name the 'gene' section(s)
	  genetab = search_features('gene', 'GBFeature_key', feattab) # this function is much more elegant, but, remeber, it returns a LIST of dictionaries, not a dictionary
	  
	  temp_genes = []
	  
	  if len(genetab) > 0 :
	    for j in range(len(genetab)) :
	      if len(search_features('gene', 'GBQualifier_name', genetab[j]['GBFeature_quals'])) > 0 :
		temp_genes.append(search_features('gene', 'GBQualifier_name', genetab[j]['GBFeature_quals'])[0]['GBQualifier_value']) # we assume that there is only one sub-field 'gene' per each gene field..
	      else :
		temp_genes.append('NA')

	    genes.append(','.join(temp_genes))
	  else :
	    genes.append('NA')


	  # get and name the 'rRNA' section(s)
	  rRNAtab = search_features('rRNA', 'GBFeature_key', feattab) # this function is much more elegant, but, remeber, it returns a LIST of dictionaries, not a dictionary
	  
	  temp_rRNAs = []
	  
	  if len(rRNAtab) > 0 :
	    for j in range(len(rRNAtab)) :
	      if len(re.findall("GBFeature_quals", str(rRNAtab[0].keys()))) > 0:
		if len(search_features('product', 'GBQualifier_name', rRNAtab[j]['GBFeature_quals'])) > 0 :
		  temp_rRNAs.append(search_features('product', 'GBQualifier_name', rRNAtab[j]['GBFeature_quals'])[0]['GBQualifier_value']) # we assume that there is only one sub-field 'product' per each rRNA field..
		else :
		  temp_rRNAs.append('NA')
	      else :
		temp_rRNAs.append('undefined_rRNA')
	    rRNAs.append(','.join(temp_rRNAs))
	  else :
	    rRNAs.append('NA')

	  # get and name the 'tRNA' section(s)
	  tRNAtab = search_features('tRNA', 'GBFeature_key', feattab) # this function is much more elegant, but, remeber, it returns a LIST of dictionaries, not a dictionary
	  
	  temp_tRNAs = []
	  
	  if len(tRNAtab) > 0 :
	    for j in range(len(tRNAtab)) :
	      if len(re.findall("GBFeature_quals", str(tRNAtab[0].keys()))) > 0:
		if len(search_features('product', 'GBQualifier_name', tRNAtab[j]['GBFeature_quals'])) > 0 :
		  temp_tRNAs.append(search_features('product', 'GBQualifier_name', tRNAtab[j]['GBFeature_quals'])[0]['GBQualifier_value']) # we assume that there is only one sub-field 'product' per each tRNA field..
		else :
		  temp_tRNAs.append('NA')
	      else :
		temp_tRNAs.append('undefined_tRNA')
	    tRNAs.append(','.join(temp_tRNAs))
	  else :
	    tRNAs.append('NA')

	  # get and name the 'CDS' section(s)
	  CDStab = search_features('CDS', 'GBFeature_key', feattab) # this function is much more elegant, but, remeber, it returns a LIST of dictionaries, not a dictionary
	  
	  temp_CDSs = []
	  
	  if len(CDStab) > 0 :
	    for j in range(len(CDStab)) :
	      if len(search_features('product', 'GBQualifier_name', CDStab[j]['GBFeature_quals'])) > 0 :
		temp_CDSs.append(search_features('product', 'GBQualifier_name', CDStab[j]['GBFeature_quals'])[0]['GBQualifier_value']) # we assume that there is only one sub-field 'product' per each CDS field..
	      else :
		temp_CDSs.append('NA')

	    CDSs.append(','.join(temp_CDSs))
	  else :
	    CDSs.append('NA')

	  # get and name the 'dloop' section (if it exists)
	  dlooptab = search_features('D-loop', 'GBFeature_key', feattab)
	  # get and name the 'misc_feature' section(s) (if it exists)
	  misctab = search_features('misc_feature', 'GBFeature_key', feattab)
	  
	  if len(dlooptab) > 0 : # if there is a 'dloop' section, then write the field down
	    dloop.append('dloop_section')
	  else:
	    if(len(re.findall("[Dd].{0,3}[Ll][Oo][Oo][Pp]", str(misctab))) > 0 or len(re.findall("[Cc][Oo][Nn][Tt].{0,5}[Rr][Ee][Gg]", str(misctab))) > 0):
	      dloop.append('dloop_misc')
	    else:
	      if(len(re.findall("[Dd].{0,3}[Ll][Oo][Oo][Pp]", record[i]['GBSeq_definition'])) > 0 or len(re.findall("[Cc][Oo][Nn][Tt].{0,5}[Rr][Ee][Gg]", record[i]['GBSeq_definition'])) > 0):
		dloop.append('dloop_def')
	      else:
		dloop.append('NA')

	  
	  # write FASTA file with nucleotide sequences
	  #out.write('>'+record[i]['GBSeq_primary-accession']+'\n')
	  #out.write(record[i]['GBSeq_sequence']+'\n')

  # store (append) results to a csv file
  # restab = DataFrame({'accession': accessions, 'organism': organisms, 'taxonID': taxonIDs, 'country' : countries, 'lat_lon': lat_lons, 'gene' : genes})
  restab = DataFrame({'genusID': genera, 'accession': accessions, 'organism': organisms, 'speciesID': taxonIDs, 'country' : countries, 'lat_lon': lat_lons, 'gene' : genes, 'rRNA' : rRNAs, 'tRNA' : tRNAs,'isolate': isolates, 'definition': definitions, 'haplotype': haplotypes, 'pop_variant': pop_variants, 'dloop' : dloop, 'CDS_product' : CDSs, 'organelle' : organelles, 'ref_pubmed': ref_pubmed, 'ref_authors': ref_authors, 'ref_journal': ref_journal, 'ref_titles': ref_titles})
  restab = set_column_sequence(restab, ['accession', 'definition', 'organism','speciesID', 'genusID', 'organelle', 'gene', 'CDS_product', 'dloop', 'tRNA', 'rRNA', 'haplotype', 'isolate', 'pop_variant', 'country', 'lat_lon', 'ref_pubmed', 'ref_journal', 'ref_authors', 'ref_titles'])
  restab.to_csv(outputfile, "\t", mode = 'a', header = False)
  ## data stored
  #out.close() # close FASTA file
  counter = counter + 1

