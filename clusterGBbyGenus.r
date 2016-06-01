## Description
# clusterGBbyGenus() takes a data frame containing descriptions and accession numbers of GenBank sequences (called by argument <sumtab>).
# The input data.frame must contain the following columns:
  # sumtab$genus (genus names for organisms)
  # sumtab$species (species names for organisms)
  # sumtab$description (descriptions of the sequences: e.g. the first line of the GB summary format)
  # sumtab$accession (accession numbers for the sequences)

# Clusters are built using 'uclust' (http://drive5.com/uclust/downloads1_2_22q.html).
# Sequences are downloaded from the GenBank before being passed to uclust. This is done through a custom modified version of the read.GenBank() function {ape}, included in this script

## Requirements
  # An appropriate executable for 'uclust' must be available at the specified path!
  # R package 'ape' must be loaded!

## Value
# clusterGBbyGenus() outputs results to two tab-separated files: <output_file> and <output_file>.sclust (the latter unless superclust = F).
# Each line of <output_file> contains informations about one cluster of similar sequences from the same genus.
# The output file contains the following columns:
  # genus
  # species (comma-separated list of species represented in the cluster; comma-separated vector of number of sequences per each species. E.g.: leo,pardus;12,134)
  # clust_id (a consecutive number per each cluster within the genus)
  # n_seq (total number of sequences in the cluster)
  # rep_desc (the description of the longest sequence in the cluster)
  # median_lenght (the median length in bp of the sequences in the cluster)
  # mean_length (mean.. same as above)
  # min_length (min.. same as above)
  # max_length (max.. same as above)
  # mean_identity (the mean percent identity of sequences in the cluster to the longest sequence in the cluster)
  # seed (the seed sequence of each cluster.. i.e. the longest sequence)
  # accession_ns (comma-separated list of accession numbers of all sequences in the cluster)
# Genera that could not be processed to to errors (e.g. incorrect or void accesion_ns) are listed in a file '	'

# <output_file>.sclust is similar to <output_file>, but additionally contains two columns:
  # sclust (an ID number for clusters of similar sequences belonging to different genera, also called 'superclusters')
  # sclust_n_seqs (how many sequences are included in the supercluster)
  

## Arguments
# sumtab:		name of input data.frame, required.
# from:			number of the first genus to be processed in unique(sumtab$genus), default = 1.
# to:			number of the last genus to be processed in unique(sumtab$genus), default = length(unique(sumtab$genus)).
# output_file:		name of output file, required.
# errors_file:		name of file to save errors, default = "errors.txt".
# superclust:		logical, whether or not to perform inter-genera 'superclustering' (default is "null")
# path_to_uclust: 	path to uclust executable, default is = "./uclustq1.2.22_i86linux64".

# 'uclust' arguments
# identity:		%identity threshold for clustering, default = 0.90.
# maxlen:		maximum length of sequences used by 'uclust0. Sequences longer than --maxlen will be IGNORED!! This may be a very imprtant parameter.. setting --maxlen < 5000 would likely result in more useful results (it would avoid creating large clusters connected by a single very long sequences.. e.g. complete mitochondria.. a higher --maxlen implies longer times for clustering.
# minlen:		maximum length of sequences used by 'uclust'. Sequences shorter than --minlen will be ignored.
# rev:			whether or not 'uclust' should try to match sequences on the reverse strand, default is yes: "--rev"; to disable this function, <rev> must be "".
# more_uclust_args:	a character vector of further argumens for 'uclust'.   

clusterGBbyGenus = function(sumtab, from = 1, to = "all", output_file, errors_file = "errors.txt", superclust = "null", path_to_uclust = "./uclustq1.2.22_i86linux64", identity = 0.90, maxlen = 5000, minlen = 200, rev = "--rev", more_uclust_args = "") {

  ## load a modified version of read.GenBank {ape} 

  read.GenBank = function (access.nb, seq.names = access.nb, species.names = TRUE, gene.names = FALSE, as.character = FALSE) {
      N <- length(access.nb)
      nrequest <- N%/%400 + as.logical(N%%400)
      X <- character(0)
      for (i in 1:nrequest) {
	  a <- (i - 1) * 400 + 1
	  b <- 400 * i
	  if (i == nrequest) 
	      b <- N
	  URL <- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&id=", 
	      paste(access.nb[a:b], collapse = ","), "&rettype=gb&retmode=text", 
	      sep = "")
	  X <- c(X, scan(file = URL, what = "", sep = "\n", quiet = TRUE))
      }
      # FI <- which(X == "ORIGIN      ") + 1 # THIS IS THE ORIGINAL LINE..
      FI <- which(grepl("^ORIGIN",X)) + 1 # THIS IS THE MODIFIED LINE
      # LA <- which(X == "//") - 1 # THIS IS THE ORIGINAL LINE..
      LA <- which(grepl("^//", X)) - 1 # THIS IS THE MODIFIED LINE
      obj <- vector("list", N)
      for (i in 1:N) {
	  tmp <- gsub("[[:digit:] ]", "", X[FI[i]:LA[i]])
	  obj[[i]] <- unlist(strsplit(tmp, NULL))
      }
      names(obj) <- seq.names
      if (!as.character) 
	  obj <- as.DNAbin(obj)
      if (species.names) {
	  tmp <- character(N)
	  sp <- grep("ORGANISM", X)
	  for (i in 1:N) tmp[i] <- unlist(strsplit(X[sp[i]], " +ORGANISM +"))[2]
	  attr(obj, "species") <- gsub(" ", "_", tmp)
      }
      if (gene.names) {
	  tmp <- character(N)
	  sp <- grep(" +gene +<", X)
	  for (i in 1:N) tmp[i] <- unlist(strsplit(X[sp[i + 1L]], 
	      " +/gene=\""))[2]
	  attr(obj, "gene") <- gsub("\"$", "", tmp)
      }
      obj
  }

  if(to == "all") {to = length(unique(sumtab$genus))}

  # write an empty table <output_file> to save results
  system(paste("echo 'genus\tspecies\tclust_id\tn_seqs\trep_desc\tmedian_length\tmean_length\tmin_length\tmax_length\tmedian_matches\tmean_matches\tmin_matches\tmax_matches\tmean_identity\tseed\taccession_ns' >", output_file ))

  # write an empty table <errors_file> to save errors
  system(paste("echo 'genus_with_errors' >", errors_file))


  for (genus in unique(sumtab$genus)[from:to]) {

    print(paste("downloading sequences for the genus",genus,"(",which(unique(sumtab$genus) == genus),"/",length(unique(sumtab$genus)),") .."))
    seqs = tryCatch(read.GenBank(sumtab$accession[sumtab$genus == genus]), error = function(x) x)
    #seqs = read.GenBank(sumtab$accession[sumtab$genus == genus])
    print(paste(length(seqs),"sequences for the genus",genus,"downloaded! .. Writing sequences to file 'seqs.fas'.."))
    write.dna(seqs, file = "seqs.fas", format = "fasta")
    print(paste(length(seqs),"sequences for the genus",genus,"written to file 'seqs.fas'!"))
    
    system(paste(normalizePath(path_to_uclust), "--sort seqs.fas --output seqs.sort.fas"))
    
    print(paste("sorted sequences for the genus",genus,"written to file 'seqs.sort.fas'!"))
      
    system(paste(normalizePath(path_to_uclust), "--input seqs.sort.fas --uc seqs.res.uc --id", identity, rev, "--minlen", minlen, "--maxlen", maxlen, more_uclust_args))
    
    clust = tryCatch(read.table("seqs.res.uc", head = F, stringsAsFactors = F), error = function(x) x)
    if (length(nrow(clust))>0) {
      colnames(clust) = c("Type", "Cluster", "Length", "PctId", "Strand", "QStart", "SStart", "Align", "QLab", "TLab")
      clust = clust[clust$Type == "S" | clust$Type == "H",] # keep only sequences and remove clusters.. we do this because we want to keep some summary data (mean length of sequences, etc..)
      
      # these lines attempt to interpret the clust$Align column and count the number of matches per each sequence..
      aligns = clust$Align
      aligns[which(clust$Type == "S")] = paste(clust$Length[which(clust$Type == "S")], "M", sep = "")
      str = gsub("^M","1M",(gsub("([ID])M","\\11M", aligns))) 
      spl = strsplit(str, "M")
      clust$Matches = unlist(lapply(spl, FUN = function(spl){sum(as.numeric(unlist(lapply(spl, FUN = gsub, pattern = "[0-9]{0,4}[ID]", replacement = ""))), na.rm = T)}))
      rm(aligns, str, spl)

      
      genera.clust = c()
      for(cluster in unique(clust$Cluster)) {
	genera.clust$genus = c(genera.clust$genus, genus)
	genera.clust$species = c(genera.clust$species, paste(sumtab$species[which(sumtab$accession %in% clust$QLab[clust$Cluster == cluster])], collapse = ","))
	genera.clust$clust_id = c(genera.clust$clust_id, cluster)
	genera.clust$n_seqs = c(genera.clust$n_seq, length(which(clust$Cluster == cluster)))
	genera.clust$rep_desc = c(genera.clust$rep_desc, sumtab$description[which(sumtab$accession == clust$QLab[clust$Cluster == cluster][1])])

	genera.clust$median_length = c(genera.clust$median_length, median(clust$Length[clust$Cluster == cluster]))
	genera.clust$mean_length = c(genera.clust$mean_length, mean(clust$Length[clust$Cluster == cluster]))
	genera.clust$min_length = c(genera.clust$min_length, min(clust$Length[clust$Cluster == cluster]))
	genera.clust$max_length = c(genera.clust$max_length, max(clust$Length[clust$Cluster == cluster]))
	
	genera.clust$median_matches = c(genera.clust$median_matches, median(clust$Matches[clust$Cluster == cluster]))
	genera.clust$mean_matches = c(genera.clust$mean_matches, mean(clust$Matches[clust$Cluster == cluster]))
	genera.clust$min_matches = c(genera.clust$min_matches, min(clust$Matches[clust$Cluster == cluster]))
	genera.clust$max_matches = c(genera.clust$max_matches, max(clust$Matches[clust$Cluster == cluster]))
	
	genera.clust$mean_identity = c(genera.clust$mean_identity, mean(as.numeric((as.character(clust$PctId[clust$Cluster == cluster])[clust$PctId[clust$Cluster == cluster] != "*"]), na.rm = T)))

	genera.clust$seed = c(genera.clust$seed, as.character(clust$QLab[clust$Cluster == cluster & clust$Type == "S"]))
	genera.clust$accesion_ns = c(genera.clust$accesion_ns, paste(sumtab$accession[which(sumtab$accession %in% clust$QLab[clust$Cluster == cluster])], collapse = ","))
	
	# genera.clust$matches = c(genera.clust$matches, paste(clust$Matches[clust$Cluster == cluster], collapse = ","))
	# As it is written, it would likely not return the matches in the same order as the accession numbers.. IT MUST BE REMOVED OR MODIFIED!!
	
	if (cluster%%10 == 0) {print(paste("cluster",cluster+1,"of",length(unique(clust$Cluster)),"for the genus",genus,"processed!"))}
	}
      write.table(as.data.frame(genera.clust), file = output_file, append = T, col.names = F, row.names = F, quote = F, sep ="\t")
      print(paste("processing of sequences for the genus",genus,"(",which(unique(sumtab$genus) == genus),"/",length(unique(sumtab$genus)),") completed!.."))
      print(paste("clustering results saved to file '", output_file, "'!", sep = ""))
      rm(seqs)
      rm(clust)
      rm(genera.clust)
      } else {
      print(paste("fucking genus",genus,"has some problems!!!"))
      write(genus, errors_file, append = T)
      rm(seqs)
      rm(clust)
      rm(genera.clust)
      }
    }

  # remove garbage
  #system("rm seqs.fas seqs.sort.fas seqs.res.uc")
  if (superclust == T) {
    print("genus-level clustering completed!.. starting supercluster analysis..")

    # copy the genera output table file to temp_output.txt
    system(paste("cp", output_file, "temp_output.txt"))

    clustab = read.table("temp_output.txt", head = T, sep = "\t", quote = "")

    print("downloading all seeds..")
    seeds = read.GenBank(clustab$seed)
    print("writing seeds to file 'seeds.fas' ..")
    write.dna(seeds, file = "seeds.fas", format = "fasta")
    print(paste(length(seeds),"seed sequences written to file 'seeds.fas'!"))
    system(paste(normalizePath(path_to_uclust), "--sort seeds.fas --output seeds.sort.fas"))
    print(paste("sorted seed sequences written to file 'seeds.sort.fas'!.. starting uclust from", path_to_uclust, ".."))
    system(paste(normalizePath(path_to_uclust), "--input seeds.sort.fas --uc seeds.res.uc --id", identity, rev, "--minlen", minlen, "--maxlen", maxlen, more_uclust_args))
    print("loading superclusters..")
    sclust = read.table("seeds.res.uc", quote = "", sep = "\t", head = F)

    colnames(sclust) = c("Type", "Cluster", "Length", "PctId", "Strand", "QStart", "SStart", "Align", "QLab", "TLab")

    sclust = sclust[sclust$Type == "S" | sclust$Type == "H",] # keep only sequences and remove clusters.. we do this because we want to keep some summary data (mean length of sequences, etc..)

    print(paste("writing new genera cluster table to file",paste(output_file,"sclust", sep = "."),".."))

    clustab$sclust = NA
    clustab$sclust_n_seqs = NA

    for (scluster in unique(sclust$Cluster)) {

      clustab$sclust[which(clustab$seed %in% as.character(sclust$QLab[sclust$Cluster == scluster]))] = scluster
      clustab$sclust_n_seqs[which(clustab$seed %in% as.character(sclust$QLab[sclust$Cluster == scluster]))] = sum(clustab$n_seqs[which(clustab$seed %in% as.character(sclust$QLab[sclust$Cluster == scluster]))])
      }

    write.table(clustab, file = paste(output_file,"sclust", sep = "."), append = F, col.names = T, row.names = F, quote = F, sep ="\t")

  print(paste("new genera cluster table written to file '",paste(output_file,"sclust", sep = "."),"'. Program complete!"))
    } else {
    print("genus-level clustering completed! No supercluster analysis was requested (superclust != T). Program complete!")
    }
  }





