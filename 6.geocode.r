# Download 'gadm_v2_shp.zip' from www.gadm.org, extract it and split it by ISO code (using R, qGIS etc..)
# save files to '~/GADM_all_countries_split'
# results from 4.GADM_preparation.r are stored in "~/GADM_all_countries_split/plain"

# create directory to store country-wise matches..
system("mkdir ./match_results/")

library(raster)
library(rgeos)
library(rgdal)

date()

## OPEN DATA

# GENBANK DATA
data=read.csv("geo.only_split.csv",header=T,row.names=1,stringsAsFactors=F)
data=data[data$locality_new!="",]
data=data[!is.na(data$locality_new),]

# COUNTRY SELECTION
data$code_ISO=gsub("\\s*$","",data$code_ISO)

ISO=unique(data$code_ISO[data$code_ISO!="manualcheck"])

countries_with_errors = c()

while(i <= length(ISO)){
	cntry=ISO[i]
	
	data.sub=data[data$code_ISO==cntry,]
	data.i=data.sub$locality_new
	
	
	## GADM DATA ##
	##	     ##
	
	shp=shapefile(paste("~/GADM_all_countries_split/gadm2_ISO__",cntry,".shp",sep=""))
	data.shp=read.csv(paste("~/GADM_all_countries_split/plain/GADM_table_",cntry,".csv",sep=""),header=T,stringsAsFactors=F)
	
	
	# ORDER BOTH ACCORDING TO OBJECTID AND CHECK CONCORDANCE
	shp@data=shp@data[order(shp@data$OBJECTID),]
	data.shp=data.shp[order(data.shp$OBJECTID),]
	
	
	if(length(which(shp@data$OBJECTID!=data.shp$OBJECTID)) == 0){

	
		# SEARCH FOR EXTRA SPACES AND ADJUST DATA
		topo=cbind(data.shp$NAME_1,data.shp$VARNAME_1,data.shp$NAME_2,data.shp$VARNAME_2,data.shp$NAME_3,data.shp$VARNAME_3,data.shp$NAME_4,data.shp$VARNAME_4,data.shp$NAME_5)
		for(r in 1:nrow(topo)) {for(c in 1:ncol(topo)) {topo[r,c] = as.character(topo[r,c])}} ## this is needed to let the Seychelles in!! (It is stupid.. but does not hurt much for the rest..)
		topo=as.data.frame(toupper(topo),stringsAsFactors=F)
		
		# REMOVE DOUBLE AND EXTRA SPACING AT THE BEGINNING AND AT THE END OF THE STRING
		for(b in 1:ncol(topo)){
			topo[,b]=gsub("\\s+"," ",topo[,b])
			topo[,b]=gsub("^\\s*|\\s*$","",topo[,b])
			# topo[,b]=gsub("\\'\\s+","\\'",topo[,b])		REMOVED: this works for italian names but perhaps not for other countries
		}
		
		
		# CREATE A TOPO FACTOR
		topo$fact=character(nrow(topo))
		for(a in 1:nrow(topo)){topo$fact[a]=paste(topo[a,],collapse="|")}
		names(topo)=c("adm1","adm1.var","adm2","adm2.var","adm3","adm3.var","adm4","adm4.var","adm5","fact")
		
		
		## GEONAMES DATA ##
		##		 ##
		
		gaz=read.table(paste("~/geonames_data/split_simplified/geonames_simplified_",cntry, ".txt", sep = ""),header=T,stringsAsFactors=F)
		names(gaz)=c("name","asciiname","alternatenames","latitude","longitude","feature_class","feature_code","country_code","ISO3")
		gaz$name=toupper(gaz$name)
		gaz$asciiname=toupper(gaz$asciiname)
		gaz$alternatenames=toupper(gaz$alternatenames)


		## SAVE RESULTS
		data.adm=as.data.frame(matrix(0,length(data.i),5))
		data.coo=as.data.frame(matrix(0,length(data.i),3))
		data.rec=as.data.frame(matrix(0,length(data.i),3))
		data.pre=as.data.frame(matrix(0,length(data.i),1))
		data.fou=as.data.frame(matrix(0,length(data.i),3))
		data.gaz=as.data.frame(matrix(0,length(data.i),4))
		names(data.adm)=c("adm1","adm2","adm3","adm4","adm5")
		names(data.coo)=c("long","lat","area.km^2")
		names(data.rec)=c("locality","country","searched.topo")
		names(data.pre)=c("precision")
		names(data.fou)=c("matching","total","position")
		names(data.gaz)=c("long.gaz","lat.gaz","matched.topo.gaz","feature.code")
			


		## START SEARCH
		for(j in 1:length(data.i)){
						
			# COUNTER
			#par(mfrow=c(2,1))
# 	 		plot.new(); text(0.5,0.5,cex=0.8,paste("Analysed countries:",i," of ",length(ISO)))		
# 			plot.new(); text(0.5,0.5,cex=0.8,paste("Analysed records:",j," of ",length(data.i)))
			print(date())
			print(paste("Analysed countries: ",i," of ",length(ISO)))		
			print(paste("Analysed records: ",j," of ",length(data.i)))
			
			# RECORD PREPARATION
			rec=unlist(strsplit(as.character(data.i[j]),"[|]"))
			rec=toupper(rec)
		
			data.rec$locality[j]=data.sub$original_locality[j]
			data.rec$country[j]=ISO[i]
			data.rec$searched.topo[j]=paste(rec,collapse="|")
			
			
			####	       ####
			#### GADM DATA ####
			####	       ####
			
			
			# SEARCH FOR MATCHES - GREP (FIRST SELECTION)
			res.vec=numeric(0)
			for(k in 1:length(rec)){
				res.vec=c(res.vec,grep(paste("(^|\\s+|\\|)",rec[k],"($+|\\|)",sep=""),topo$fact))
			}
			
			res.vec=unique(res.vec[!is.na(res.vec)])
			sel.topo=topo[res.vec,]
			
			
			# REMOVE NON MATCHING RECORDS
			
			if(nrow(sel.topo)==0){									# no partial match
				data.adm[j,]="no match"
				data.pre[j,]="no match"
				data.coo[j,]="no match"
				data.fou[j,]=c(0,length(rec),0)							# 0 matching, length(rec) substrings, 0 position
			}else{
				# SEARCH FOR EXACT MATCHES - WHICH (SECOND SELECTION)
				
				res.mat=as.data.frame(matrix(0,nrow(sel.topo),8))
				names(res.mat)=c("pos","n.match","adm1","adm2","adm3","adm4","adm5","position")
				
				for(l in 1:nrow(sel.topo)){
					topo.adm1=c(sel.topo$adm1[l],unlist(strsplit(sel.topo$adm1.var[l],"[|]")))
					topo.adm2=c(sel.topo$adm2[l],unlist(strsplit(sel.topo$adm2.var[l],"[|]")))
					topo.adm3=c(sel.topo$adm3[l],unlist(strsplit(sel.topo$adm3.var[l],"[|]")))
					topo.adm4=c(sel.topo$adm4[l],unlist(strsplit(sel.topo$adm4.var[l],"[|]")))
					topo.adm5=sel.topo$adm5[l]
				
					topo.l=c(topo.adm1,topo.adm2,topo.adm3,topo.adm4,topo.adm5)
				
					res.mat$pos[l]=l
					res.mat$n.match[l]=length(which(rec%in%topo.l==T))
					res.mat$position[l]=paste(which(rec%in%topo.l),collapse="|")
								

					rec.adm1=which(rec%in%topo.adm1==T)
					rec.adm2=which(rec%in%topo.adm2==T)
					rec.adm3=which(rec%in%topo.adm3==T)
					rec.adm4=which(rec%in%topo.adm4==T)
					rec.adm5=which(rec%in%topo.adm5==T)
					
					if(length(rec.adm1)>=1){res.mat$adm1[l]=1}else{res.mat$adm1[l]=0}
					
					# CONSERVATIVE APPROACH: IF ADM2==ADM3 AND THE MATCHING RECORD IS THE SAME -> ADM3=0 (WE SUPPOSE THAT CAN EXIST AT MOST TWO ADMINISTRATIVE DIVISION WITH THE SAME NAME)
					if(length(rec.adm2)>=1){
						if(length(which(rec.adm2%in%rec.adm1==T))%in%c(0,2)){res.mat$adm2[l]=1}else{res.mat$adm2[l]=0}
					}
					if(length(rec.adm3)>=1){
						if(length(which(rec.adm3%in%c(rec.adm1,rec.adm2)==T))%in%c(0,2)){res.mat$adm3[l]=1}else{res.mat$adm3[l]=0}
					}
					if(length(rec.adm4)>=1){
						if(length(which(rec.adm4%in%c(rec.adm1,rec.adm2,rec.adm3)==T))%in%c(0,2)){res.mat$adm4[l]=1}else{res.mat$adm4[l]=0}
					}
					if(length(rec.adm5)>=1){
						if(length(which(rec.adm5%in%c(rec.adm1,rec.adm2,rec.adm3,rec.adm4)==T))%in%c(0,2)){res.mat$adm5[l]=1}else{res.mat$adm5[l]=0}
					}
				}
				
			
				data.fou$total[j]=length(rec)
				data.fou$matching[j]=max(res.mat$n.match)
				data.fou$position[j]=paste(unique(unlist(strsplit(res.mat$position[res.mat$n.match==max(res.mat$n.match)],"[|]"))),collapse="|")
				
				
				# REMOVE NON MATCHING RECORDS
				sel=which(res.mat$n.match!=0)

				if(length(sel)==0){			# no complete match (the script found only partial matches)
					data.adm[j,]="no match"
					data.pre[j,]="no match"
					data.coo[j,]="no match"
					data.fou[j,]=c(0,length(rec),0)	
				}else{}			


				# CLASSIFY EXACT MATCHES
				
				sel.topo=topo[res.vec,c(1,3,5,7,9)]											# ONLY ADM1 ADM2 ADM3 ADM4 and ADM5 FIELDS

				matching.adm=rowSums(res.mat[,3:7])											# IF ADM2==ADM3 AND THE RECORD IS IN THE FORM "ADM2 province, ADM3", WE HAVE n.match=2 AND matching.adm=2 -> WE RETAIN THIS RECORD AT THE ADM3 LEVEL

				sel=which(res.mat$n.match!=0 & res.mat$n.match==max(res.mat$n.match))							# we choose the topo with the highest number of element matching in rec
				if(length(sel)>0){sel=sel[which((res.mat$n.match[sel]-matching.adm[sel])==min(res.mat$n.match[sel]-matching.adm[sel]))]}	# we choose the rec with the highest number of elements matching in the different adm		
				
				
				if(length(sel)==1){													# only one match: we can store it
					data.adm[j,]=sel.topo[sel,]
					data.pre[j,]=paste(res.mat$adm1[sel],res.mat$adm2[sel],res.mat$adm3[sel],res.mat$adm4[sel],res.mat$adm5[sel],sep="|")
					
					poly.list=vector("list",0)
					pos.shp=res.vec[sel]
					
					for(m in 1:length(shp@polygons[[pos.shp]]@Polygons)){
						coords=shp@polygons[[pos.shp]]@Polygons[[m]]@coords
						r=Polygon(coords)
						poly.list[length(poly.list)+1]=Polygons(list(r),ID=length(poly.list)+1)
					}
					poly=SpatialPolygons(poly.list,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
					aggr=poly
					
					centr=gCentroid(aggr)@coords
					
					crs=CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")		# Behrmann cylindrical equal area
					transf=spTransform(aggr,crs)
					area=gArea(transf)/10^6
					
					data.coo[j,]=c(centr,area)

				}else{}
				
				
				if(length(sel)>1){
					
					fact.1=paste(res.mat$adm1,res.mat$adm2,res.mat$adm3,res.mat$adm4,res.mat$adm5,sep="|")[sel]				# factor (0|0|1|0|0) o (1|1|0|0|0) o ....
					
					if(length(unique(fact.1))==1){												# is the info region|0|0 or region|province|0 or ....constant?
						sel.col=which(unlist(strsplit(unique(fact.1),"[|]"))==1)
						fact.2=character(0)
						
						for(z in 1:length(sel)){fact.2=c(fact.2,paste(sel.topo[sel[z],(1:max(sel.col))],collapse="|"))}
						
						if(length(unique(fact.2))==1){											# are the names of region or region | province or... always the same?
							data.adm[j,1:max(sel.col)]=toupper(unique(sel.topo[sel,1:max(sel.col)]))
							data.adm[j,-(1:max(sel.col))]=NA
						
							data.pre[j,]=unique(fact.1)
							
							poly.list=vector("list",0)
							for(l in 1:length(sel)){
								pos.shp=res.vec[sel[l]]
								
								for(m in 1:length(shp@polygons[[pos.shp]]@Polygons)){
									coords=shp@polygons[[pos.shp]]@Polygons[[m]]@coords
									r=Polygon(coords)
									poly.list[length(poly.list)+1]=Polygons(list(r),ID=length(poly.list)+1)
								}
							}
							
							poly=SpatialPolygons(poly.list,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
							aggr=aggregate(poly)
							
							centr=gCentroid(aggr)@coords
							
							crs=CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")		# Behrmann cylindrical equal area
							transf=spTransform(aggr,crs)
							area=gArea(transf)/10^6
							
							data.coo[j,]=c(centr,area)
						}else{
							data.adm[j,]="multiple matches"
							data.pre[j,]="multiple matches"
							data.coo[j,]="multiple matches"
						}
					}else{
						data.adm[j,]="multiple matches"
						data.pre[j,]="multiple matches"
						data.coo[j,]="multiple matches"
					}
				}else{}
			}
		

			####	    	   ####
			#### GEONAMES DATA ####
			####	    	   ####
		
			if(data.fou$matching[j]==data.fou$total[j]){
				
				data.gaz[j,]="no search" # SEARCH COMPLETED IN GADM
			
			}else{
				if(data.fou$matching[j]>0 & data.pre[j,]!="multiple matches"){

					# SELECT UNMATCHING SUBSTRING WITHIN REC
					pos.fou=as.numeric(unlist(strsplit(data.fou$position[j],"[|]")))
					rec.sea=rec[-pos.fou]
					
					
					# SELECT EXTENT OF GADM-MATCHING SUBSTRINGS AND SUBSET GEOGRAPHICALLY GEONAMES DATA
					ext=extent(aggr)
					sub.gaz=gaz[gaz$longitude>=ext@xmin & gaz$longitude<=ext@xmax & gaz$latitude>=ext@ymin & gaz$latitude<=ext@ymax,]

					
					# SEARCH FOR MATCHES - GREP
					res.gaz=numeric(0)
					for(n in 1:length(rec.sea)){
						
						res.gaz=c(res.gaz,grep(paste("(^|\\s+)",rec.sea[n],"($+|\\s+)",sep=""),sub.gaz$name))
						res.gaz=c(res.gaz,grep(paste("(^|\\s+)",rec.sea[n],"($+|\\s+)",sep=""),sub.gaz$asciiname))
					
						pos.alternatename=numeric(0)
						for(o in 1:nrow(sub.gaz)){
							tmp.topo=unlist(strsplit(sub.gaz$alternatenames[o],"[,]"))
							grep=grep(paste("(^|\\s+|\\,)",rec.sea[n],"($+|\\s+|\\,)",sep=""),tmp.topo)
							
							if(length(grep)>=1){pos.alternatename=c(pos.alternatename,o)}
						}
						res.gaz=c(res.gaz,pos.alternatename)	
					}
					res.gaz=unique(res.gaz[!is.na(res.gaz)])

					
					# CLASSIFY MATCHES
					if(length(res.gaz)==0){
						
						data.gaz[j,]="no match"
					
					}else{
						if(length(res.gaz)==1){
							data.gaz$long.gaz[j]=sub.gaz$longitude[res.gaz]
							data.gaz$lat.gaz[j]=sub.gaz$latitude[res.gaz]
							data.gaz$matched.topo.gaz[j]=sub.gaz$asciiname[res.gaz]
							data.gaz$feature.code[j]=sub.gaz$feature_code[res.gaz]
						}else{

							# SEARCH FOR MATCHES - WHICH (WE USE THIS ONLY IF WE HAVE MORE THAN ONE MATCH)
							which.tab=sub.gaz[res.gaz,]
							
							res.gaz.wh=numeric(0)
							for(p in 1:nrow(which.tab)){
								tmp.topo=c(which.tab$name[p],which.tab$asciiname[p],unlist(strsplit(which.tab$alternatename[p],"[,]")))
								
								if(length(which(rec.sea%in%tmp.topo)==T)>0){res.gaz.wh=c(res.gaz.wh,p)}
							}
							
							if(length(res.gaz.wh)==0){
							
								# CALCULATE A DISTANCE MATRIX ON GREP DATA AND CHOOSE TO RETAIN POINTS CLOSER THAN 25km (BY CALCULATING THE CENTROID)
								coords=cbind(sub.gaz$longitude[res.gaz],sub.gaz$latitude[res.gaz])
								dist.mat=as.dist(pointDistance(coords,lonlat=T))/1000
							
								if(max(dist.mat)<=100){
									pts=SpatialPoints(coords,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
									centr=gCentroid(pts)@coords
								
									data.gaz$long.gaz[j]=centr[,1]
									data.gaz$lat.gaz[j]=centr[,2]
									data.gaz$matched.topo.gaz[j]="centroid"
									data.gaz$feature.code[j]=paste("maxdist = ",max(dist.mat)," km",sep="")
								}else{
								
									data.gaz[j,]="multiple matches"
								}	
							}else{
								if(length(res.gaz.wh)==1){
									data.gaz$long.gaz[j]=which.tab$longitude[res.gaz.wh]
									data.gaz$lat.gaz[j]=which.tab$latitude[res.gaz.wh]
									data.gaz$matched.topo.gaz[j]=which.tab$asciiname[res.gaz.wh]
									data.gaz$feature.code[j]=which.tab$feature_code[res.gaz.wh]
								}else{
								
									# CALCULATE A DISTANCE MATRIX ON WHICH DATA AND CHOOSE TO RETAIN POINTS CLOSER THAN 25km (BY CALCULATING THE CENTROID)
									coords=cbind(which.tab$longitude[res.gaz.wh],which.tab$latitude[res.gaz.wh])
									dist.mat=as.dist(pointDistance(coords,lonlat=T))/1000
							
									if(max(dist.mat)<=100){
										pts=SpatialPoints(coords,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
										centr=gCentroid(pts)@coords
									
										data.gaz$long.gaz[j]=centr[,1]
										data.gaz$lat.gaz[j]=centr[,2]
										data.gaz$matched.topo.gaz[j]="centroid"
										data.gaz$feature.code[j]=paste("maxdist = ",max(dist.mat)," km",sep="")
									}else{
										data.gaz[j,]="multiple matches"
									}
								}
							}
						}
					}				
				}else{
					# SEARCH WITHIN THE COMPOSITE EXTENT OF MULTIPLE MATCHES OR THE WHOLE GEONAMES TABLE OF THE i-th COUNTRY  
					
					if(data.fou$matching[j]>0 & data.pre[j,]=="multiple matches"){
					
						# SELECT UNMATCHING SUBSTRING WITHIN REC
						pos.fou=as.numeric(unlist(strsplit(data.fou$position[j],"[|]")))
						rec.sea=rec[-pos.fou]
					
						
						# IDENTIFY THE COMPOSITE EXTENT AND SUBSAMPLE gaz
						sub.gaz=as.data.frame(matrix(0,0,9))
					
						for(z in 1:length(sel)){
					
							tmp.shp=shp[res.vec[sel[z]],]
							tmp.ext=extent(tmp.shp)
							tmp.gaz=gaz[gaz$longitude>=tmp.ext@xmin & gaz$longitude<=tmp.ext@xmax & gaz$latitude>=tmp.ext@ymin & gaz$latitude<=tmp.ext@ymax,]
							sub.gaz=rbind(sub.gaz,tmp.gaz)
						}
						
						sub.gaz=sub.gaz[!duplicated(sub.gaz),]
						names(sub.gaz)=names(gaz)
					
						# SEARCH FOR MATCHES - GREP
						res.gaz=numeric(0)
						for(n in 1:length(rec.sea)){
						
							res.gaz=c(res.gaz,grep(paste("(^|\\s+)",rec.sea[n],"($+|\\s+)",sep=""),sub.gaz$name))
							res.gaz=c(res.gaz,grep(paste("(^|\\s+)",rec.sea[n],"($+|\\s+)",sep=""),sub.gaz$asciiname))
						
							pos.alternatename=numeric(0)
							for(o in 1:nrow(sub.gaz)){
								tmp.topo=unlist(strsplit(sub.gaz$alternatenames[o],"[,]"))
								grep=grep(paste("(^|\\s+|\\,)",rec.sea[n],"($+|\\s+|\\,)",sep=""),tmp.topo)
								
								if(length(grep)>=1){pos.alternatename=c(pos.alternatename,o)}
							}
							res.gaz=c(res.gaz,pos.alternatename)	
						}
						res.gaz=unique(res.gaz[!is.na(res.gaz)])
		
						
						# CLASSIFY MATCHES
						if(length(res.gaz)==0){
							
							data.gaz[j,]="no match"
						
						}else{
							if(length(res.gaz)==1){
								data.gaz$long.gaz[j]=sub.gaz$longitude[res.gaz]
								data.gaz$lat.gaz[j]=sub.gaz$latitude[res.gaz]
								data.gaz$matched.topo.gaz[j]=sub.gaz$asciiname[res.gaz]
								data.gaz$feature.code[j]=sub.gaz$feature_code[res.gaz]
							}else{

								# SEARCH FOR MATCHES - WHICH (WE USE THIS ONLY IF WE HAVE MORE THAN ONE MATCH)
								which.tab=sub.gaz[res.gaz,]
								
								res.gaz.wh=numeric(0)
								for(p in 1:nrow(which.tab)){
									tmp.topo=c(which.tab$name[p],which.tab$asciiname[p],unlist(strsplit(which.tab$alternatename[p],"[,]")))
									
									if(length(which(rec.sea%in%tmp.topo)==T)>0){res.gaz.wh=c(res.gaz.wh,p)}
								}
							
								if(length(res.gaz.wh)==0){
							
									# CALCULATE A DISTANCE MATRIX ON GREP DATA AND CHOOSE TO RETAIN POINTS CLOSER THAN 25km (BY CALCULATING THE CENTROID)
									coords=cbind(sub.gaz$longitude[res.gaz],sub.gaz$latitude[res.gaz])
									dist.mat=as.dist(pointDistance(coords,lonlat=T))/1000
							
									if(max(dist.mat)<=100){
										pts=SpatialPoints(coords,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
										centr=gCentroid(pts)@coords
								
										data.gaz$long.gaz[j]=centr[,1]
										data.gaz$lat.gaz[j]=centr[,2]
										data.gaz$matched.topo.gaz[j]="centroid"
										data.gaz$feature.code[j]=paste("maxdist = ",max(dist.mat)," km",sep="")
									}else{
								
										data.gaz[j,]="multiple matches"
									}	
								}else{
									if(length(res.gaz.wh)==1){
										data.gaz$long.gaz[j]=which.tab$longitude[res.gaz.wh]
										data.gaz$lat.gaz[j]=which.tab$latitude[res.gaz.wh]
										data.gaz$matched.topo.gaz[j]=which.tab$asciiname[res.gaz.wh]
										data.gaz$feature.code[j]=which.tab$feature_code[res.gaz.wh]
									}else{
								
										# CALCULATE A DISTANCE MATRIX ON WHICH DATA AND CHOOSE TO RETAIN POINTS CLOSER THAN 25km (BY CALCULATING THE CENTROID)
										coords=cbind(which.tab$longitude[res.gaz.wh],which.tab$latitude[res.gaz.wh])
										dist.mat=as.dist(pointDistance(coords,lonlat=T))/1000
							
										if(max(dist.mat)<=100){
											pts=SpatialPoints(coords,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
											centr=gCentroid(pts)@coords
									
											data.gaz$long.gaz[j]=centr[,1]
											data.gaz$lat.gaz[j]=centr[,2]
											data.gaz$matched.topo.gaz[j]="centroid"
											data.gaz$feature.code[j]=paste("maxdist = ",max(dist.mat)," km",sep="")
										}else{
											data.gaz[j,]="multiple matches"
										}
									}
								}
							}
						}				
					}else{
					
						rec.sea=rec
					
						# SEARCH FOR MATCHES - GREP
						res.gaz=numeric(0)
						for(n in 1:length(rec.sea)){
						
							res.gaz=c(res.gaz,grep(paste("(^|\\s+)",rec.sea[n],"($+|\\s+)",sep=""),gaz$name))
							res.gaz=c(res.gaz,grep(paste("(^|\\s+)",rec.sea[n],"($+|\\s+)",sep=""),gaz$asciiname))
					
							pos.alternatename=numeric(0)
							for(o in 1:nrow(gaz)){
								tmp.topo=unlist(strsplit(gaz$alternatenames[o],"[,]"))
								grep=grep(paste("(^|\\s+|\\,)",rec.sea[n],"($+|\\s+|\\,)",sep=""),tmp.topo)
							
								if(length(grep)>=1){pos.alternatename=c(pos.alternatename,o)}
							}
							res.gaz=c(res.gaz,pos.alternatename)	
						}

						res.gaz=unique(res.gaz[!is.na(res.gaz)])

					
						# CLASSIFY MATCHES
						if(length(res.gaz)==0){
						
							data.gaz[j,]="no match"
					
						}else{
							if(length(res.gaz)==1){
								data.gaz$long.gaz[j]=gaz$longitude[res.gaz]
								data.gaz$lat.gaz[j]=gaz$latitude[res.gaz]
								data.gaz$matched.topo.gaz[j]=gaz$asciiname[res.gaz]
								data.gaz$feature.code[j]=gaz$feature_code[res.gaz]
							}else{
							
								# SEARCH FOR MATCHES - WHICH (WE USE THIS ONLY IF WE HAVE MORE THAN ONE MATCH)
								which.tab=gaz[res.gaz,]
							
								res.gaz.wh=numeric(0)
								for(p in 1:nrow(which.tab)){
									tmp.topo=c(which.tab$name[p],which.tab$asciiname[p],unlist(strsplit(which.tab$alternatename[p],"[,]")))
									
									if(length(which(rec.sea%in%tmp.topo)==T)>0){res.gaz.wh=c(res.gaz.wh,p)}
								}
							
								if(length(res.gaz.wh)==0){
							
									# CALCULATE A DISTANCE MATRIX ON GREP DATA AND CHOOSE TO RETAIN POINTS CLOSER THAN 25km (BY CALCULATING THE CENTROID)
									coords=cbind(gaz$longitude[res.gaz],gaz$latitude[res.gaz])
									dist.mat=as.dist(pointDistance(coords,lonlat=T))/1000
							
									if(max(dist.mat)<=100){
										pts=SpatialPoints(coords,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
										centr=gCentroid(pts)@coords
								
										data.gaz$long.gaz[j]=centr[,1]
										data.gaz$lat.gaz[j]=centr[,2]
										data.gaz$matched.topo.gaz[j]="centroid"
										data.gaz$feature.code[j]=paste("maxdist = ",max(dist.mat)," km",sep="")
									}else{
								
										data.gaz[j,]="multiple matches"
									}	
								}else{
									if(length(res.gaz.wh)==1){
										data.gaz$long.gaz[j]=which.tab$longitude[res.gaz.wh]
										data.gaz$lat.gaz[j]=which.tab$latitude[res.gaz.wh]
										data.gaz$matched.topo.gaz[j]=which.tab$asciiname[res.gaz.wh]
										data.gaz$feature.code[j]=which.tab$feature_code[res.gaz.wh]
									}else{
								
										# CALCULATE A DISTANCE MATRIX ON WHICH DATA AND CHOOSE TO RETAIN POINTS CLOSER THAN 25km (BY CALCULATING THE CENTROID)
										coords=cbind(which.tab$longitude[res.gaz.wh],which.tab$latitude[res.gaz.wh])
										dist.mat=as.dist(pointDistance(coords,lonlat=T))/1000
							
										if(max(dist.mat)<=100){
											pts=SpatialPoints(coords,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
											centr=gCentroid(pts)@coords
										
											data.gaz$long.gaz[j]=centr[,1]
											data.gaz$lat.gaz[j]=centr[,2]
											data.gaz$matched.topo.gaz[j]="centroid"
											data.gaz$feature.code[j]=paste("maxdist = ",max(dist.mat)," km",sep="")
										}else{
											data.gaz[j,]="multiple matches"
										}
									}						
								}
							}
						}
					}
				}
			}
		}

	result=data.frame(data.rec,data.adm,data.fou,data.coo,data.pre,data.gaz)
	names(result)=c("locality","country","searched.topo","adm1","adm2","adm3","adm4","adm5","matching","total","position",
			"long","lat","area.km^2","precision","long.gaz","lat.gaz","matched.topo.gaz","feature.code")

	write.csv(result,paste("./match_results/matching results_",cntry,".csv",sep=""))
	print(date())
	print(paste("matches for country", i, "of", length(ISO), "written to file:"))
	print(paste("./match_results/matching results_",cntry,".csv",sep=""))
	i = i+1

	} else {

	print(paste("Ahó... la country",cntry, "c'ha qualcosa che non va!"))
	countries_with_errors = c(countries_with_errors, cntry)
	i = i+1
	}
}				

write.table(countries_with_errors,"./match_results/countries_with_errors.txt")
date()



