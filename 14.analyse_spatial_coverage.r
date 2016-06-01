## shapefiles for species ranges are assumed to be in the directory '~/iucn_shapefiles', with a special subdir '~/iucn_shapefiles/birds_shp' for birds..
## shapefiles for amphibians, reptiles and mammals must be renamed to match the 'class' field in 'genus_clusters_GEOREF.csv' (i.e. "Mammals.shp", etc..)

# NAs IN DISPERSION FIELDS IF THE NUMBER OF CELLS OF THE RANGE AND/OR POINTS IS <=1
# NAs IN K-MEANS FIELD IF THE NUMBER OF CELLS OF THE RANGE IS <=20

library(raster)
library(rgdal)
library(rgeos)
library(maptools)

# create directory to store results..
system("mkdir './spatial\ coverage'")

# OPEN DATA -> EXTRACT ALL SHAPEFILES TO THE WORKING DIRECTORY AND RENAME CLASS SHAPEFILE ACCORDING TO THE class FIELD IN genus_clusters_GEOREF.csv
wrld=shapefile("world.shp")

# list of marine mammals families to be EXCLUDED!!
marine_families = c("BALAENIDAE", "NEOBALAENIDAE", "BALAENOPTERIDAE", "ESCHRICHTIIDAE", "PHYSETERIDAE", "MONODONTIDAE", "ZIPHIIDAE", "DELPHINIDAE", "PHOCOENIDAE", "PLATANISTIDAE", "INIIDAE", "PONTOPORIIDAE", "TRICHECHIDAE", "DUGONGIDAE", "OTARIIDAE", "ODOBENIDAE", "PHOCIDAE")


# REPROJECT wrld AND CREATE A REFERENCE RASTER
crs=CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs") # Behrmann cylindrical equal area
wrld.behr=spTransform(wrld,crs)
tmp=raster(extent(wrld.behr),resolution=100000)

wrld.behr@data$field=1
rast=rasterize(wrld.behr,tmp,field=wrld.behr@data$field)
rast[!is.na(rast)]=1
rast[is.na(rast)]=0


###			###
###    SPECIES-WISE	###
###			###

data=read.csv("species_clusters_GEOREF.csv",header=T,stringsAsFactors=F)
data$species=gsub("_"," ",data$species)

# CONVERT data IN A SpatialPointsDataFrame AND REPROJECT
coordinates(data)=c("lon","lat")
crs(data)=crs(wrld)

crs=CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")		# Behrmann cylindrical equal area
pts=spTransform(data,crs)


# EXCLUDE UNCLASSIFIED RECORDS (SPECIES LEVEL)
#pts=pts[which(pts$species!="absent"),]


res.spe=as.data.frame(matrix(0,0,11))
names(res.spe)=c("spe.clust","class","percent.coverage","range.size","kmeans.coverage","range.disp.mean","range.disp.median","range.disp.sd","species.disp.mean","species.disp.median","species.disp.sd")

write.table(res.spe, "./spatial coverage/coverage-species.txt", row.names=F, append = F, col.names = T, sep = "\t", quote = F)

class=unique(pts$class)

for(i in 1:length(class)){
	
	pdf(paste("./spatial coverage/",class[i]," - species coverage.pdf",sep=""),width=14,height=7)
	
	if(class[i] %in% c("Mammals","Amphibians","Reptiles")){
	
		## MAMMALS - AMPHIBIAN - REPTILES ##
		
		# SUBSET DATA -> SINGLE CLASS
		print(paste(date(), ": loading shapefile ",paste("~/iucn_shapefiles/", class[i],".shp",sep=""), "...", sep = ""))
		shp.i=shapefile(paste("~/iucn_shapefiles/", class[i],".shp",sep=""))
		class.i=pts[which(pts$class==class[i]),]

		species=unique(class.i$species)
		
		for(j in 1:length(species)){
		
		# SUBSET DATA -> SINGLE SPECIES
		species.pts=class.i[which(class.i$species==species[j]),]
		
			if(length(which(shp.i$binomial==unique(species.pts$species))) > 0) {
				
				sub.range=shp.i[which(shp.i$binomial==unique(species.pts$species)),]
				
				if(unique(sub.range@data$family_nam) %in% marine_families) {
				
					print(paste(species[j], "belongs to a stupid swimming marine family.. we don't work with those guys.."))
				
				} else {

					sub.range=spTransform(sub.range,crs)

					#UNPREDICTABLE ERROR: Error: TopologyException: Input geom 1 is invalid: Ring Self-intersection at or near point ....
					#sub.range@data$field=1
					#sub.range=unionSpatialPolygons(sub.range,IDs=sub.range@data$field)
					#range.pts=spsample(sub.range,5000,type="random")
					#rast.range=rasterize(range.pts,rast)

					sub.range@data$field=1
					species.range=rasterize(sub.range,rast)
					species.range[!is.na(species.range)]=1
					species.range[is.na(species.range)]=0

					
					spe.clust=unique(species.pts$clust_fac_species)
					
					for(l in 1:length(spe.clust)){
			
						print(paste(date(), ": class ",i," of ",length(class)," - species ",j," of ",length(species)," (",spe.clust[l],")",sep=""))

						rast.range=species.range
						sub.pts=species.pts[which(species.pts$clust_fac_species==spe.clust[l]),]

						# RASTERIZE DATA AND CALCULATE COVERAGE 
						sub.pts@data$field=1		
						rast.pts=rasterize(sub.pts,rast,field=sub.pts@data$field)

						rast.pts[!is.na(rast.pts)]=1
						rast.pts[is.na(rast.pts)]=0
						
						# ADD ALL POINTS TO THE IUCN RANGE
						rast.range[rast.pts==1]=1
						
						prc.cov=cellStats(rast.pts,stat="sum")/cellStats(rast.range,stat="sum")
						
						
						# ESTIMATE SPATIAL DISPERSION -> TOO MUCH TIME CONSUMING - DELETED!
						
						if(cellStats(rast.pts,stat="sum")>1){

							pts.distance=NA
							
							#pts.disp=rasterToPoints(rast.pts,spatial=T)
							#pts.disp=pts.disp[which(pts.disp$layer==1),]
							#pts.disp=spTransform(pts.disp,crs(wrld))
							#pts.distance=as.dist(pointDistance(pts.disp,lonlat=T))
						
						}else{
						
							pts.distance=NA
							
						}


						if(cellStats(rast.range,stat="sum")>1){

							range.distance=NA
							
							#range.disp=rasterToPoints(rast.range,spatial=T)
							#range.disp=range.disp[which(range.disp$layer==1),]
							#range.disp=spTransform(range.disp,crs(wrld))
							#range.distance=as.dist(pointDistance(range.disp,lonlat=T))
						
						}else{
						
							range.distance=NA
							
						}
						
						
						# CALCULATE K-MEANS GROUPING -> REPRODUCIBILITY DEPENDS ON nstart and (maybe) iterations: IF nstart IS TOO LOW THE SAME DATA GIVE DIFFERENT PARTITIONS (LOCAL FIT)
						km.rast=rast.range
						km.rast[km.rast<1]=NA
						km.rast[km.rast>=1]=1
						
						if(cellStats(km.rast,stat="sum")>20){
						
							coord.km=as.data.frame(coordinates(rasterToPoints(km.rast,spatial=T)))
							km=kmeans(coord.km,20, iter.max=1000, nstart = 1000)
							km.range=rasterize(coord.km,rast,field=km$cluster)
						
							prc.cov.km=length(unique(extract(km.range,sub.pts)))/20
						
						}else{
							
							km.range=km.rast
							prc.cov.km=NA
						
						}
						
						
						# SAVE RESULTS
						tmp.res=as.data.frame(matrix(0,1,11))
						names(tmp.res)=names(res.spe)

						tmp.res$spe.clust=spe.clust[l]
						tmp.res$class=class[i]
						tmp.res$percent.coverage=prc.cov
						tmp.res$range.size=cellStats(rast.range,stat="sum")
						tmp.res$kmeans.coverage=prc.cov.km
						tmp.res$range.disp.mean=mean(range.distance)
						tmp.res$range.disp.median=median(range.distance)
						tmp.res$range.disp.sd=sd(range.distance)
						tmp.res$species.disp.mean=mean(pts.distance)
						tmp.res$species.disp.median=median(pts.distance)
						tmp.res$species.disp.sd=sd(pts.distance)
						
						
						write.table(tmp.res, "./spatial coverage/coverage-species.txt", row.names=F, append = T, col.names = F, sep = "\t", quote = F)
						
						print(paste("results for cluster",spe.clust[l] ,"saved to file ./spatial coverage/coverage-species.csv"))
			
						
						# CORRECT rast (THE WORLD) FOR IMPERFECTIONS DUE TO THE RASTERIZATION OF SMALL POLYGONS AND PLOT
						
						#gen.rast=rast
						#gen.rast[rast.range==1]=2
						#gen.rast[rast.pts==1]=3
						
						#plot(gen.rast,main=spe.clust[l],col=c("white","grey","dark green","red"),legend=F,axes=F)
						
						rast.pts[rast.pts!=1]=NA
						
						plot(rast,main=spe.clust[l],col=c("white","grey"),legend=F,axes=F)
						plot(km.range,legend=F,axes=F,add=T)
						plot(rast.pts,col="black",legend=F,axes=F,add=T)
					}
				}
			}
		}
	}else{
		## BIRDS ##
	
		list=list.files(path = "~/iucn_shapefiles/birds_shp/",pattern=".shp")
		list.spe=unlist(lapply(list,FUN=function(x){paste(unlist(strsplit(x,"_"))[1],unlist(strsplit(x,"_"))[2],sep=" ")}))

		class.i=pts[which(pts$class==class[i]),]
		
		species=unique(class.i$species)

		for(j in 1:length(species)){
		
			# SUBSET DATA -> SINGLE SPECIES
			species.pts=class.i[which(class.i$species==species[j]),]
			
						
			# OPEN SINGLE SPECIES SHAPEFILE
			sub.range=shapefile(paste("~/iucn_shapefiles/birds_shp/", list[which(list.spe == unique(species.pts$species))], sep = ""))
			
			sub.range=spTransform(sub.range,crs)

			#UNPREDICTABLE ERROR: Error: TopologyException: Input geom 1 is invalid: Ring Self-intersection at or near point ....
			#sub.range@data$field=1
			#sub.range=unionSpatialPolygons(sub.range,IDs=sub.range@data$field)
			#range.pts=spsample(sub.range,5000,type="random")
			#rast.range=rasterize(range.pts,rast)

			sub.range@data$field=1
			species.range=rasterize(sub.range,rast)
			species.range[!is.na(species.range)]=1
			species.range[is.na(species.range)]=0

				
			spe.clust=unique(species.pts$clust_fac_species)
			
			for(l in 1:length(spe.clust)){
	
				print(paste(date(), ": class ",i," of ",length(class)," - species ",j," of ",length(species)," (",spe.clust[l],")",sep=""))

				rast.range=species.range
				sub.pts=species.pts[which(species.pts$clust_fac_species==spe.clust[l]),]

				# RASTERIZE DATA AND CALCULATE COVERAGE -> RASTERIZING DIRECTLY COMPLEX POLYGONS USUALLY INVOLVES THE LOSS OF WIDE AREAS -> RASTERIZE RANDOM PTS GENERATED WITHIN THE RANGE
				sub.pts@data$field=1		
				rast.pts=rasterize(sub.pts,rast,field=sub.pts@data$field)

				rast.pts[!is.na(rast.pts)]=1
				rast.pts[is.na(rast.pts)]=0
				
				# ADD ALL POINTS TO THE IUCN RANGE
				rast.range[rast.pts==1]=1
				
				prc.cov=cellStats(rast.pts,stat="sum")/cellStats(rast.range,stat="sum")
				
				# ESTIMATE SPATIAL DISPERSION -> TOO MUCH TIME EXPENSIVE - DELETED!
				
				if(cellStats(rast.pts,stat="sum")>1){

					pts.distance=NA
					
					#pts.disp=rasterToPoints(rast.pts,spatial=T)
					#pts.disp=pts.disp[which(pts.disp$layer==1),]
					#pts.disp=spTransform(pts.disp,crs(wrld))
					#pts.distance=as.dist(pointDistance(pts.disp,lonlat=T))
				
				}else{
				
					pts.distance=NA
					
				}


				if(cellStats(rast.range,stat="sum")>1){

					range.distance=NA
					
					#range.disp=rasterToPoints(rast.range,spatial=T)
					#range.disp=range.disp[which(range.disp$layer==1),]
					#range.disp=spTransform(range.disp,crs(wrld))
					#range.distance=as.dist(pointDistance(range.disp,lonlat=T))
				
				}else{
				
					range.distance=NA
					
				}
				
				
				# CALCULATE K-MEANS GROUPING -> REPRODUCIBILITY DEPENDS ON nstart and (maybe) iterations: IF nstart IS TOO LOW THE SAME DATA GIVE DIFFERENT PARTITIONS (LOCAL FIT)
				km.rast=rast.range
				km.rast[km.rast<1]=NA
				km.rast[km.rast>=1]=1
				
				if(cellStats(km.rast,stat="sum")>20){
				
					coord.km=as.data.frame(coordinates(rasterToPoints(km.rast,spatial=T)))
					km=kmeans(coord.km,20, iter.max=1000, nstart = 1000)
					km.range=rasterize(coord.km,rast,field=km$cluster)
				
					prc.cov.km=length(unique(extract(km.range,sub.pts)))/20
				
				}else{
					
					km.range=km.rast
					prc.cov.km=NA
				
				}
				
				
				# SAVE RESULTS
				tmp.res=as.data.frame(matrix(0,1,11))
				names(tmp.res)=names(res.spe)

				tmp.res$spe.clust=spe.clust[l]
				tmp.res$class=class[i]
				tmp.res$percent.coverage=prc.cov
				tmp.res$range.size=cellStats(rast.range,stat="sum")
				tmp.res$kmeans.coverage=prc.cov.km
				tmp.res$range.disp.mean=mean(range.distance)
				tmp.res$range.disp.median=median(range.distance)
				tmp.res$range.disp.sd=sd(range.distance)
				tmp.res$species.disp.mean=mean(pts.distance)
				tmp.res$species.disp.median=median(pts.distance)
				tmp.res$species.disp.sd=sd(pts.distance)
				
				
				write.table(tmp.res, "./spatial coverage/coverage-species.txt", row.names=F, append = T, col.names = F, sep = "\t", quote = F)
					
				print(paste("results for cluster",spe.clust[l] ,"saved to file ./spatial coverage/coverage-species.csv"))
	
				
				# CORRECT rast (THE WORLD) FOR IMPERFECTIONS DUE TO THE RASTERIZATION OF SMALL POLYGONS AND PLOT
				
				#gen.rast=rast
				#gen.rast[rast.range==1]=2
				#gen.rast[rast.pts==1]=3
				
				#plot(gen.rast,main=spe.clust[l],col=c("white","grey","dark green","red"),legend=F,axes=F)
				
				rast.pts[rast.pts!=1]=NA
				
				plot(rast,main=spe.clust[l],col=c("white","grey"),legend=F,axes=F)
				plot(km.range,legend=F,axes=F,add=T)
				plot(rast.pts,col="black",legend=F,axes=F,add=T)
			}
		}
	}

	dev.off()
}
