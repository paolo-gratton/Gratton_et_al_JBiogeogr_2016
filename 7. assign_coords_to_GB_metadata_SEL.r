# OBTAIN ISO CODES AND CREATE A COUNTRY:LOCALITY FIELD
geo.only=read.csv("geo.only_split.csv",header=T,row.names=1,stringsAsFactors=F)

geo.only=geo.only[geo.only$locality_new!="",]
geo.only=geo.only[!is.na(geo.only$locality_new),]
geo.only$code_ISO=gsub("\\s*$","",geo.only$code_ISO)

ISO.c=unique(geo.only$code_ISO[geo.only$code_ISO!="manualcheck"])

geo.only$key1=paste(geo.only$code_ISO,":",geo.only$original_locality,sep="")
geo.only$key2=paste(geo.only$country,":",geo.only$original_locality,sep="")


# IMPORT GEO.SEARCH RESULTS AND RBIND TABLES 
result=as.data.frame(matrix(0,0,19))
names(result)=c("locality","country","searched.topo","adm1","adm2","adm3","adm4","adm5","matching","total","position",
		"long","lat","area.km^2","precision","long.gaz","lat.gaz","matched.topo.gaz","feature.code")

for(i in 1:length(ISO.c)){
	cntry=ISO.c[i]
	
	data=read.csv(paste("./match_results/matching results_",cntry,".csv",sep=""),header=T,row.names=1,stringsAsFactors=F)

	result=rbind(result,data)
}

result$key=paste(result$country,":",result$locality,sep="")

# CHECK 1
nrow(result)==nrow(geo.only[geo.only$code_ISO!="manualcheck",])			# CHECK IS OK


# IMPORT GENBANK METADATA
gb.data=read.csv("GB_metadata_SEL.csv",header=T,stringsAsFactors=F)


# MERGE TABLES
merge.1=merge(geo.only,result,by.x="key1",by.y="key")

merge.2=merge(gb.data,merge.1,by.x="country",by.y="key2")


# A POSTERIORI:
#length(unique(gb.data$accession))==nrow(gb.data)			# FALSE

#dupl=gb.data[duplicated(gb.data$accession),]
#dim(dupl)
#length(unique(dupl$accession))



# CHECK 2 -> THIS IS TRUE IF AND ONLY IF WE DO NOT REMOVE ROWS IN THE LINES 4 AND 5
length(unique(geo.only$key2))==length(unique(gb.data$country[grep(":",gb.data$country)]))


write.csv(merge.2,"GB_geomatched.csv")	# -> 272970 records
