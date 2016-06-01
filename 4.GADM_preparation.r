# Download 'gadm_v2_shp.zip' from www.gadm.org, extract it and split it by ISO code (using R, qGIS etc..)
# save files to '~/GADM_all_countries_split'

# results from this cleaning step are stored in directory "~/GADM_all_countries_split/plain"

setwd("~/GADM_all_countries_split")

# create directories for results..
system("mkdir ~/GADM_all_countries_split/semi.plain")
system("mkdir ~/GADM_all_countries_split/plain")

names=list.files(path = ".")
iso=substr(names,12,14)


# FIRST REPLACEMENT

for(i in 1:length(iso)){

	setwd("~/GADM_all_countries_split")

	data.shp=read.csv(paste("GADM_table_",iso[i],".csv",sep=""),header=T,stringsAsFactors=F)
	
	{## REMOVE SOME SPECIAL CHARACTERS FROM GADM DATA NAMES -> UPPERCASE????
	data.shp$NAME_1 = gsub("à", "a", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("á", "a", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("â", "a", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("ã", "a", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("ä", "a", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("è", "e", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("é", "e", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("ë", "e", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("ì", "i", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("í", "i", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("ï", "i", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("ò", "o", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("ó", "o", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("ô", "o", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("õ", "o", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("ö", "o", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("ù", "u", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("ú", "u", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("û", "u", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("ü", "u", data.shp$NAME_1)
	data.shp$NAME_1 = gsub("ñ", "n", data.shp$NAME_1)
	
	data.shp$NAME_2 = gsub("à", "a", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("á", "a", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("â", "a", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("ã", "a", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("ä", "a", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("è", "e", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("é", "e", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("ë", "e", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("ì", "i", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("í", "i", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("ï", "i", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("ò", "o", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("ó", "o", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("ô", "o", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("õ", "o", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("ö", "o", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("ù", "u", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("ú", "u", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("û", "u", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("ü", "u", data.shp$NAME_2)
	data.shp$NAME_2 = gsub("ñ", "n", data.shp$NAME_2)
	
	data.shp$NAME_3 = gsub("à", "a", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("á", "a", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("â", "a", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("ã", "a", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("ä", "a", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("è", "e", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("é", "e", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("ë", "e", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("ì", "i", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("í", "i", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("ï", "i", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("ò", "o", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("ó", "o", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("ô", "o", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("õ", "o", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("ö", "o", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("ù", "u", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("ú", "u", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("û", "u", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("ü", "u", data.shp$NAME_3)
	data.shp$NAME_3 = gsub("ñ", "n", data.shp$NAME_3)
	
	data.shp$NAME_4 = gsub("à", "a", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("á", "a", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("â", "a", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("ã", "a", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("ä", "a", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("è", "e", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("é", "e", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("ë", "e", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("ì", "i", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("í", "i", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("ï", "i", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("ò", "o", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("ó", "o", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("ô", "o", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("õ", "o", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("ö", "o", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("ù", "u", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("ú", "u", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("û", "u", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("ü", "u", data.shp$NAME_4)
	data.shp$NAME_4 = gsub("ñ", "n", data.shp$NAME_4)

	data.shp$NAME_5 = gsub("à", "a", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("á", "a", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("â", "a", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("ã", "a", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("ä", "a", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("è", "e", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("é", "e", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("ë", "e", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("ì", "i", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("í", "i", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("ï", "i", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("ò", "o", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("ó", "o", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("ô", "o", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("õ", "o", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("ö", "o", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("ù", "u", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("ú", "u", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("û", "u", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("ü", "u", data.shp$NAME_5)
	data.shp$NAME_5 = gsub("ñ", "n", data.shp$NAME_5)
	
	data.shp$VARNAME_1 = gsub("à", "a", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("á", "a", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("â", "a", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("ã", "a", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("ä", "a", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("è", "e", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("é", "e", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("ë", "e", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("ì", "i", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("í", "i", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("ï", "i", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("ò", "o", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("ó", "o", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("ô", "o", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("õ", "o", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("ö", "o", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("ù", "u", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("ú", "u", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("û", "u", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("ü", "u", data.shp$VARNAME_1)
	data.shp$VARNAME_1 = gsub("ñ", "n", data.shp$VARNAME_1)
	
	data.shp$VARNAME_2 = gsub("à", "a", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("á", "a", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("â", "a", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("ã", "a", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("ä", "a", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("è", "e", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("é", "e", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("ë", "e", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("ì", "i", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("í", "i", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("ï", "i", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("ò", "o", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("ó", "o", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("ô", "o", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("õ", "o", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("ö", "o", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("ù", "u", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("ú", "u", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("û", "u", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("ü", "u", data.shp$VARNAME_2)
	data.shp$VARNAME_2 = gsub("ñ", "n", data.shp$VARNAME_2)
	
	data.shp$VARNAME_3 = gsub("à", "a", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("á", "a", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("â", "a", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("ã", "a", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("ä", "a", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("è", "e", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("é", "e", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("ë", "e", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("ì", "i", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("í", "i", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("ï", "i", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("ò", "o", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("ó", "o", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("ô", "o", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("õ", "o", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("ö", "o", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("ù", "u", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("ú", "u", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("û", "u", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("ü", "u", data.shp$VARNAME_3)
	data.shp$VARNAME_3 = gsub("ñ", "n", data.shp$VARNAME_3)
	
	data.shp$VARNAME_4 = gsub("à", "a", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("á", "a", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("â", "a", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("ã", "a", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("ä", "a", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("è", "e", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("é", "e", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("ë", "e", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("ì", "i", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("í", "i", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("ï", "i", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("ò", "o", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("ó", "o", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("ô", "o", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("õ", "o", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("ö", "o", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("ù", "u", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("ú", "u", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("û", "u", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("ü", "u", data.shp$VARNAME_4)
	data.shp$VARNAME_4 = gsub("ñ", "n", data.shp$VARNAME_4)
	}
	
	setwd("~/GADM_all_countries_split/semi.plain")

	write.csv(data.shp,paste("GADM_table_",iso[i],".csv",sep=""),row.names=F)
}



# SEARCH FOR ADDITIONAL CHARACTERS NOT IN c(letters,LETTERS)

to.change=character(0)

for(i in 1:length(iso)){

	setwd("~/GADM_all_countries_split/semi.plain")

	data.shp=read.csv(paste("GADM_table_",iso[i],".csv",sep=""),header=T,stringsAsFactors=F)

	for(j in 1:nrow(data.shp)){
		
		lett=unlist(strsplit(paste(data.shp[j,-1],collapse=""),NULL))
		
		new=lett[which(lett%in%c(LETTERS,letters)==F)]
		
		if(length(new>0)){to.change=c(to.change,unique(new))}
		
		to.change=unique(to.change)
		
	}
}

to.change
to.change.sort=sort(to.change)
to.change.sort

conv=c("-","'","-","-"," "," "," "," "," "," "," "," ","(",")"," ",",",".","/",":",";"," "," "," "," "," "," ","|","i"," "," ","'",",",
	" "," "," "," "," "," "," "," "," "," "," "," "," "," ","0"," ","1"," ","2","3","4","5","6","7","8","9","A","A","A","A","A","a",
	"A","ae","Ae","c","C","d","D","E","E","e","E","E","I","I","i","I","N"," ","O","O","O","O","O","oe","Oe","oe","Oe","s","S","ss","th","Th",
	"U","U","U","y","Y","y","z","Z")


# SEARCH FOR: "\t","\n","\"","\\","#"

boh=character(0)

for(i in 1:length(iso)){

	setwd("~/GADM_all_countries_split/semi.plain")

	data.shp=read.csv(paste("GADM_table_",iso[i],".csv",sep=""),header=T,stringsAsFactors=F)

	for(j in 1:ncol(data.shp)){
		
		search1=grep("\t",data.shp[,j],fixed=T)
		search2=grep("\n",data.shp[,j],fixed=T)
		search3=grep("\"",data.shp[,j],fixed=T)
		search4=grep("\\",data.shp[,j],fixed=T)
		search5=grep("#",data.shp[,j],fixed=T)
		
		pos=unique(c(search1,search2,search3,search4,search5))
		
		new=data.shp[pos,j]
		
		if(length(new>0)){boh=c(boh,unique(new))}
		
		boh=unique(boh)
		
	}
}

boh 


# REPLACE: WE COULD NOT DIRECTLY SPECIFY THE PATTERN DUE TO THE IMPOSSIBILITY OF VISUALIZE SOME CHARACTERS IN txt

for(i in 1:length(iso)){

	setwd("~/GADM_all_countries_split/semi.plain")

	data.shp=read.csv(paste("GADM_table_",iso[i],".csv",sep=""),header=T,stringsAsFactors=F)
	
	for(j in 1:length(conv)){
	
		bad=to.change.sort[j]
		rep=conv[j]
	
	
		data.shp$NAME_1 = gsub(bad, rep, data.shp$NAME_1,fixed=T)
		data.shp$NAME_2 = gsub(bad, rep, data.shp$NAME_2,fixed=T)
		data.shp$NAME_3 = gsub(bad, rep, data.shp$NAME_3,fixed=T)
		data.shp$NAME_4 = gsub(bad, rep, data.shp$NAME_4,fixed=T)
		data.shp$NAME_5 = gsub(bad, rep, data.shp$NAME_5,fixed=T)

		data.shp$VARNAME_1 = gsub(bad, rep, data.shp$VARNAME_1,fixed=T)
		data.shp$VARNAME_2 = gsub(bad, rep, data.shp$VARNAME_2,fixed=T)
		data.shp$VARNAME_3 = gsub(bad, rep, data.shp$VARNAME_3,fixed=T)
		data.shp$VARNAME_4 = gsub(bad, rep, data.shp$VARNAME_4,fixed=T)
	}
	
	setwd("~/GADM_all_countries_split/plain")
	
	write.csv(data.shp,paste("GADM_table_",iso[i],".csv",sep=""),row.names=F)
}




# CHECK AGAIN FOR NON LETTER CHARACTERS

check=character(0)

for(i in 1:length(iso)){

	setwd("~/GADM_all_countries_split/plain")

	data.shp=read.csv(paste("GADM_table_",iso[i],".csv",sep=""),header=T,stringsAsFactors=F)

	for(j in 1:nrow(data.shp)){
		
		lett=unlist(strsplit(paste(data.shp[j,-1],collapse=""),NULL))
		
		new=lett[which(lett%in%c(LETTERS,letters)==F)]
		
		if(length(new>0)){check=c(check,unique(new))}
		
		check=unique(check)
		
	}
}

check