# OPEN DATA
d = read.table("GB_all_vertebrates_GEOREF.txt", head = T, sep = "\t", quote = "", comment.char = "", stringsAsFactors = F)
iucn.data=read.csv("IUCN species names.csv",header=T,stringsAsFactors=F)

# create a table containing georeferenced data only..
to_keep = which(is.na(d$type) == F)
matching.data = d[to_keep, c("accession", "definition", "speciesID", "genusID", "organism", "gene", "lon", "lat", "type", "class")]
colnames(matching.data)[which(colnames(georef_data) == "definition")] = "description"

rm(d)

write.csv(matching.data, file = "georef_data.csv", row.names = F)

# TEST FOR DUPLCATE SPECIES NAMES BETWEEN CLASSES

spec.cla=character(nrow(iucn.data))
for(i in 1:length(spec.cla)){spec.cla[i]=paste(iucn.data$species[i],iucn.data$class[i],collapse="|")}

length(unique(iucn.data$species))==length(unique(spec.cla)) # unique(species)==unique(species|class)
# TRUE


# OBTAIN GENUS ONLY FROM IUCN DATA
iucn.genus=character(nrow(iucn.data))
for(i in 1:length(iucn.genus)){iucn.genus[i]=unlist(strsplit(iucn.data$species[i]," "))[1]}


# EXTRACT THE NUMBER OF WORDS IN THE FIELD matching.data$organism
names=unique(matching.data$organism)
names=gsub("[(]","",names)
names=gsub("[)]","",names)
names=gsub("  "," ",names)
names=gsub(","," ",names)

nwords=numeric(length(names))

for(i in 1:length(names)){nwords[i]=length(unlist(strsplit(names[i]," ")))}

# START SEARCH
res=as.data.frame(matrix(0,length(names),6))
names(res)=c("ncbi.organism","iucn.spe","iucn.gen","class","typol.gen","typol.spe")

res$ncbi.organism=names

for(i in 1:nrow(res)){
	
	if(i %in% seq(0,nrow(res),by=100)){print(paste(i," records on ",nrow(res),sep=""))}
	
	
	split=unlist(strsplit(res$ncbi.organism[i]," "))
	
	# GENUS
	sel.gen=which(split[1]==iucn.genus)
	
	if(length(sel.gen)==0){
	
		length.repl=nchar(split[1])-round(nchar(split[1])*0.9,0)
	
		match.g=character(0)
		for(j in 1:(50*length.repl)){
			pos=sort(sample(nchar(split[1]),round(nchar(split[1])*0.9,0)))		# retain approximately the 90% of letters
			spl.rec=unlist(strsplit(split[1],NULL))
			spl.rec[pos]=spl.rec[pos]
			spl.rec[-pos]=".*"
			pat=paste(spl.rec,collapse="")
			gr=iucn.genus[grep(pat,iucn.genus)]
			if(length(gr)>0){match.g=c(match.g,gr)}
		}
	
		if(length(unique(match.g))==0){

			res$iucn.spe[i]=NA
			res$iucn.gen[i]=NA
			res$class[i]=NA
			res$typol.gen[i]="genus not present"
			res$typol.spe[i]="no search"
		
		}else{
			if(length(unique(match.g))==1){
			
				split[1]=unique(match.g)
				sel.gen=grep(unique(match.g),iucn.data$species,fixed=T)
				
				res$typol.gen[i]="partial match"
				res$iucn.gen[i]=split[1]
				res$class[i]=unique(iucn.data$class[sel.gen])
				
			}else{

				res$iucn.spe[i]=NA
				res$iucn.gen[i]=NA
				res$class[i]=NA
				res$typol.gen[i]="more than one genus - partial"
				res$typol.spe[i]="no search"
	
			}
		}
	}else{
		res$typol.gen[i]="exact match"
		res$iucn.gen[i]=unique(iucn.genus[sel.gen])
		res$class[i]=unique(iucn.data$class[sel.gen])
	
	}

	if(is.na(res$iucn.gen[i])==F){


		# SPECIES (EXACT MATCH)
		
		spec=paste(split[1],split[2],collapse=" ")

		genus=iucn.data[sel.gen,]
		
		sel.spe=which(genus$species%in%spec==T)
		
		if(length(sel.spe)==1){
		
			res$iucn.spe[i]=genus$species[sel.spe]
			res$typol.spe[i]="exact match"
		
		}else{					# sel.spe==0 -> we can't find more than one match
			
			# SEARCH FOR ERRORS IN THE SPECIES NAME (PARTIAL MATCH) -> ("Actitis macularia" vs "Actitis macularius") or typos ("Acontias kgalagadi" vs"Acontias kgaladi")
		
			length.repl=nchar(split[2])-round(nchar(split[2])*0.9,0)

			match.s=character(0)
			for(j in 1:(50*length.repl)){
				pos=sort(sample(nchar(split[2]),round(nchar(split[2])*0.9,0)))		# retain approximately the 90% of letters
				spl.rec=unlist(strsplit(split[2],NULL))
				spl.rec[pos]=spl.rec[pos]
				spl.rec[-pos]=".*"
				pat=paste(spl.rec,collapse="")
				gr=genus$species[grep(pat,genus$species)]
				if(length(gr)>0){match.s=c(match.s,gr)}
			}

			if(length(unique(match.s))==1){
			
				res$iucn.spe[i]=unique(match.s)
				res$typol.spe[i]="partial match"
			
			}else{
				# COMBINE GENUS AND SUBSPECIES (WHEN PRESENT) 
				
				if(length(split)>2){
					
					subsp=paste(split[1],split[3],collapse=" ")
					sel.subsp=which(genus$species%in%subsp)
				
					if(length(sel.subsp)==1){
		
						res$iucn.spe[i]=genus$species[sel.subsp]
						res$typol.spe[i]="subsp to species"
		
					}else{		
						res$iucn.spe[i]=NA
						res$typol.spe[i]="species not present"
					}				
				}else{
					res$iucn.spe[i]=NA
					res$typol.spe[i]="species not present"
				}	
			}
		}
	}
}


table(res$typol.gen)
table(res$typol.spe)



####									####
####  CONTROL THIS LAST PART BEFORE TO USE WITH A NEW georef_data.csv	####
####									####

# CHECK PARTIAL MATCHES - GENUS

assign.gen=res[which(is.na(res$iucn.gen)==F),]

to.check.gen=assign.gen[which(assign.gen$typol.gen!="exact match"),]
to.check.gen

# 'MANUALLY' CORRECT FOR ERRORS IN PARTIAL MATCH - GENUS
res$iucn.gen[which(res$ncbi.organism=="Hoplopterus spinosus")]=res$class[which(res$ncbi.organism=="Hoplopterus spinosus")]=NA
res$iucn.gen[which(res$ncbi.organism=="Anurolimnas viridis")]=res$class[which(res$ncbi.organism=="Anurolimnas viridis")]=NA
res$iucn.gen[which(res$ncbi.organism=="Cinnyris pulchellus")]=res$class[which(res$ncbi.organism=="Cinnyris pulchellus")]=NA
res$iucn.gen[which(res$ncbi.organism=="Cinnyris mariquensis")]=res$class[which(res$ncbi.organism=="Cinnyris mariquensis")]=NA
res$iucn.gen[which(res$ncbi.organism=="Cinnyris jugularis aurora")]=res$class[which(res$ncbi.organism=="Cinnyris jugularis aurora")]=NA
res$iucn.gen[which(res$ncbi.organism=="Cinnyris jugularis jugularis")]=res$class[which(res$ncbi.organism=="Cinnyris jugularis jugularis")]=NA
res$iucn.gen[which(res$ncbi.organism=="Cinnyris jugularis")]=res$class[which(res$ncbi.organism=="Cinnyris jugularis")]=NA
res$iucn.gen[which(res$ncbi.organism=="Thryophilus pleurostictus nisorius")]=res$class[which(res$ncbi.organism=="Thryophilus pleurostictus nisorius")]=NA
res$iucn.gen[which(res$ncbi.organism=="Thryophilus rufalbus castanonotus")]=res$class[which(res$ncbi.organism=="Thryophilus rufalbus castanonotus")]=NA
res$iucn.gen[which(res$ncbi.organism=="Trionyx triunguis")]=res$class[which(res$ncbi.organism=="Trionyx triunguis")]=NA
res$iucn.gen[which(res$ncbi.organism=="Pelomedusa subrufa")]=res$class[which(res$ncbi.organism=="Pelomedusa subrufa")]=NA
res$iucn.gen[which(res$ncbi.organism=="Amblyrhynchus cristatus")]=res$class[which(res$ncbi.organism=="Amblyrhynchus cristatus")]=NA
res$iucn.gen[which(res$ncbi.organism=="Leiolepis belliana")]=res$class[which(res$ncbi.organism=="Leiolepis belliana")]=NA
res$iucn.gen[which(res$ncbi.organism=="Leiolepis belliana belliana")]=res$class[which(res$ncbi.organism=="Leiolepis belliana belliana")]=NA
res$iucn.gen[which(res$ncbi.organism=="Leiolepis boehmei")]=res$class[which(res$ncbi.organism=="Leiolepis boehmei")]=NA
res$iucn.gen[which(res$ncbi.organism=="Leiolepis guentherpetersi")]=res$class[which(res$ncbi.organism=="Leiolepis guentherpetersi")]=NA
res$iucn.gen[which(res$ncbi.organism=="Leiolepis reevesii")]=res$class[which(res$ncbi.organism=="Leiolepis reevesii")]=NA
res$iucn.gen[which(res$ncbi.organism=="Laudakia sacra")]=res$class[which(res$ncbi.organism=="Laudakia sacra")]=NA
res$iucn.gen[which(res$ncbi.organism=="Laudakia tuberculata")]=res$class[which(res$ncbi.organism=="Laudakia tuberculata")]=NA
res$iucn.gen[which(res$ncbi.organism=="Iphisa elegans")]=res$class[which(res$ncbi.organism=="Iphisa elegans")]=NA
res$iucn.gen[which(res$ncbi.organism=="Geocalamus acutus")]=res$class[which(res$ncbi.organism=="Geocalamus acutus")]=NA
res$iucn.gen[which(res$ncbi.organism=="Mitophis asbolepis")]=res$class[which(res$ncbi.organism=="Mitophis asbolepis")]=NA
res$iucn.gen[which(res$ncbi.organism=="Mitophis leptipileptus")]=res$class[which(res$ncbi.organism=="Mitophis leptipileptus")]=NA
res$iucn.gen[which(res$ncbi.organism=="Mitophis pyrites")]=res$class[which(res$ncbi.organism=="Mitophis pyrites")]=NA

# CHECK PARTIAL MATCHES - SPECIES

assign.sp=res[which(is.na(res$iucn.spe)==F),]

to.check.sp=assign.sp[which(assign.sp$typol.spe!="exact match"),]
to.check.sp


# CORRECT FOR ERRORS IN PARTIAL MATCH - SPECIES

res$iucn.spe[which(res$ncbi.organism=="Ceyx pictus")]=NA
res$iucn.spe[which(res$ncbi.organism=="Xiphorhynchus picus")]=NA
res$iucn.spe[which(res$ncbi.organism=="Xiphorhynchus picus extimus")]=NA
res$iucn.spe[which(res$ncbi.organism=="Tringa incanus")]=NA
res$iucn.spe[which(res$ncbi.organism=="Cardellina rubra")]=NA
res$iucn.spe[which(res$ncbi.organism=="Anolis cupreus")]=NA
res$iucn.spe[which(res$ncbi.organism=="Anolis microtus")]=NA
res$iucn.spe[which(res$ncbi.organism=="Emoia cyanura")]=NA
res$iucn.spe[which(res$ncbi.organism=="Lycodon fasciatus")]=NA
res$iucn.spe[which(res$ncbi.organism=="Phelsuma ornata ornata")]=NA
res$iucn.spe[which(res$ncbi.organism=="Liolaemus pictus")]=NA
res$iucn.spe[which(res$ncbi.organism=="Hemidactylus fasciatus")]=NA
res$iucn.spe[which(res$ncbi.organism=="Phrynocephalus guttatus")]=NA
res$iucn.spe[which(res$ncbi.organism=="Phrynocephalus guttatus kushakewitschi")]=NA
res$iucn.spe[which(res$ncbi.organism=="Phrynocephalus guttatus incerta")]=NA
res$iucn.spe[which(res$ncbi.organism=="Phrynocephalus guttatus guttatus")]=NA
res$iucn.spe[which(res$ncbi.organism=="Phrynocephalus guttatus kalmykus")]=NA
res$iucn.spe[which(res$ncbi.organism=="Phrynocephalus guttatus melanurus")]=NA
res$iucn.spe[which(res$ncbi.organism=="Sphenomorphus maculatus")]=NA
res$iucn.spe[which(res$ncbi.organism=="Darevskia caspica")]=NA
res$iucn.spe[which(res$ncbi.organism=="Plica plica")]=NA
res$iucn.spe[which(res$ncbi.organism=="Saproscincus rosei")]=NA
res$iucn.spe[which(res$ncbi.organism=="Rana maculata")]=NA


write.csv(res,"iucn-ncbi_conversion.csv",row.names=F)



# # ORIGINAL NUMBER OF GENERA AND "SPECIES"
# length(unique(unlist(lapply(matching.data$organism,FUN=function(x){unlist(strsplit(x,"\\s"))[1]}))))
# length(unique(unlist(lapply(matching.data$organism,FUN=function(x){paste(unlist(strsplit(x,"\\s"))[1],unlist(strsplit(x,"\\s"))[2],sep=" ")}))))
# 
# 
# # PERCENT OF GENERA AND SPECIES
# nrow(res[which(is.na(res$iucn.spe)==F),])/nrow(res)
# nrow(res[which(is.na(res$iucn.gen)==F),])/nrow(res)
# 

# ## PER CLASS COUNTS
# 
# # MAMMALS
# mamm=iucn.data[which(iucn.data$class=="Mammals"),]
# spe.mamm=unique(mamm$species)
# 
# gen.mamm=character(nrow(mamm))
# for(i in 1:length(gen.mamm)){gen.mamm[i]=unlist(strsplit(mamm$species[i]," "))[1]}
# gen.mamm=unique(gen.mamm)
# 
# sub=res[which(res$iucn.spe%in%spe.mamm==T),]
# length(unique(sub$iucn.spe))
# length(unique(sub$iucn.gen))
# length(spe.mamm)
# length(gen.mamm)
# 
# length(unique(sub$iucn.spe))/length(spe.mamm)
# length(unique(sub$iucn.gen))/length(gen.mamm)
# 
# 
# # BIRDS
# bird=iucn.data[which(iucn.data$class=="Birds"),]
# spe.bird=unique(bird$species)
# 
# gen.bird=character(nrow(bird))
# for(i in 1:length(gen.bird)){gen.bird[i]=unlist(strsplit(bird$species[i]," "))[1]}
# gen.bird=unique(gen.bird)
# 
# sub=res[which(res$iucn.spe%in%spe.bird==T),]
# length(unique(sub$iucn.spe))
# length(unique(sub$iucn.gen))
# length(spe.bird)
# length(gen.bird)
# 
# length(unique(sub$iucn.spe))/length(spe.bird)
# length(unique(sub$iucn.gen))/length(gen.bird)
# 
# 
# # AMPHIBIANS
# amph=iucn.data[which(iucn.data$class=="Amphibians"),]
# spe.amph=unique(amph$species)
# 
# gen.amph=character(nrow(amph))
# for(i in 1:length(gen.amph)){gen.amph[i]=unlist(strsplit(amph$species[i]," "))[1]}
# gen.amph=unique(gen.amph)
# 
# sub=res[which(res$iucn.spe%in%spe.amph==T),]
# length(unique(sub$iucn.spe))
# length(unique(sub$iucn.gen))
# length(spe.amph)
# length(gen.amph)
# 
# length(unique(sub$iucn.spe))/length(spe.amph)
# length(unique(sub$iucn.gen))/length(gen.amph)
# 
# 
# # REPTILES
# rept=iucn.data[which(iucn.data$class=="Reptiles"),]
# spe.rept=unique(rept$species)
# 
# gen.rept=character(nrow(rept))
# for(i in 1:length(gen.rept)){gen.rept[i]=unlist(strsplit(rept$species[i]," "))[1]}
# gen.rept=unique(gen.rept)
# 
# sub=res[which(res$iucn.spe%in%spe.rept==T),]
# length(unique(sub$iucn.spe))
# length(unique(sub$iucn.gen))
# length(spe.rept)
# length(gen.rept)
# 
# length(unique(sub$iucn.spe))/length(spe.rept)
# length(unique(sub$iucn.gen))/length(gen.rept)


