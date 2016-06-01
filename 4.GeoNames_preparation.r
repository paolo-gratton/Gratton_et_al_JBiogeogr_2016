# Download allCountries.zip from http://download.geonames.org/export/dump/ and extract it as '~/geonames_data/geonames.org.allCountries.txt'
# have file 'countries_and_country_codes_geonames.csv' in the working directory

# create directory for cleaning and splitting results
system("mkdir ~/geonames_data/split_simplified/")

# read geonames data
cntry=read.table("geonames.org.allCountries.txt", fill=T, header=T, sep="\t", quote="", stringsAsFactors=F)

## RENAME AND FILTER..
names(cntry)=c("geonameid","name","asciiname","alternatenames","latitude","longitude","feature_class","feature_code","country_code","cc2","admin1_code","admin2_code","admin3_code","admin4_code","population","elevation","dem","timezone","modification_date")
drops=c("geonameid","cc2","admin1_code","admin2_code","admin3_code","admin4_code","population","elevation","dem","timezone","modification_date")	
cntry=cntry[,!names(cntry)%in%drops]
cntry = cntry[which(cntry$country_code != ""),]
cntry = cntry[which(cntry$country_code != "YU"),]

## CREATE A COUNTRY INDEX AND ADD ISO3 CODES: THESE WILL ALLOW A DIRECT LINK WITH GADM db
conv=read.csv("countries_and_country_codes_geonames.csv",stringsAsFactors=F)
ISO2=unique(cntry$country_code)
# modify ISO2 for Namibia..
ISO2[is.na(ISO2)]="nam"
conv$ISO[is.na(conv$ISO)]="nam"
cntry$country_code[is.na(cntry$country_code)]="nam"

cntry$ISO3=NA
for(a in 1:length(ISO2)){iso3.tmp=conv$ISO3[which(conv$ISO==ISO2[a])]; cntry$ISO3[which(cntry$country_code==ISO2[a])]=iso3.tmp}

ISO3 = unique(cntry$ISO3)

## DATA CLEANING

for(a in 1:length(ISO3)){

	data=cntry[cntry$ISO3 == ISO3[a],]
	
	# REMOVE ADM1,ADM2 AND ADM3...	cf. featureCodes.txt  
	data=data[data$feature_class!="A",]
	
	### REMOVE DUPLICATED RECORDS (RECORDS SHARING SAME ASCIINAME, FEATURE_CODE AND APPROXIMATED COORDINATES)
	data$fact=paste(data$asciiname,data$feature_code,round(data$latitude,1),round(data$longitude,1),sep="_")
	data=data[!duplicated(data$fact),]
	
	data$fact=NULL
	
	### REMOVE EMPTY FIELDS
	sel=which(data$name=="" & data$asciiname=="" & data$alternatename=="")
	if(length(sel)>0){data=data[-sel,]}else{}
	
	### REMOVE NON-LATIN ALPHABET STRINGS: IN GEONAMES ARE NOT PRESENT COMPLETELY NON-LATIN STRINGS (FOR SOME COUNTRIES, ONLY THE FIELD alternatename COULD CONTAINS NON-LATIN STRINGS)

	write.table(data, sep = "\t", file = paste("~/geonames_data/split_simplified/geonames_simplified_", ISO3[a], ".txt", sep = ""))
}