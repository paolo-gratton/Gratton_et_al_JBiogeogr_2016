library(raster)
library(rgdal)

# OPEN DATA
data=read.csv("GB_geomatched.csv",header=T,row.names=1,stringsAsFactors=F)
coord=read.csv("country_fields_with_lat_lon.csv",header=T,stringsAsFactors=F)

NAs=which(is.na(coord$lon)==T | is.na(coord$lat)==T)
if(length(NAs) > 0) {coord=coord[-NAs,]}

# SUBSET GEOMATCHED
sub.data=data.frame(data$country,data$long,data$lat,data$code_ISO,data$area.km.2,data$long.gaz,data$lat.gaz,data$feature.code,stringsAsFactors=F)
names(sub.data)=c("country","long.adm","lat.adm","ISO","area","long.gaz","lat.gaz","feature.code")


# REMOVE DUPLICATED LOCALITIES IN coord AND sub.data + MERGE -> AVOID INFLUENCES OF THE NUMBER OF RECORDS SHARING THE SAME COORDS ON THE MATCHING EVALUATION (which could well have different coordinates.. we choose to retain only the unique text fields to have a sample of independent cases..)
coord.un=coord[!duplicated(coord$country),]					# 7798 unique records
sub.data.un=sub.data[!duplicated(sub.data$country),]

merge=merge(coord.un,sub.data.un, by = "country")
nrow(merge)==nrow(coord.un)							# FALSE merge=7640 coord.un=7797


for(i in 1:nrow(coord.un)){
	coord.un$country.only[i]=unlist(strsplit(coord.un$country[i],":"))[1]
	coord.un$locality.only[i]=paste(unlist(strsplit(coord.un$country[i],":"))[-1],collapse=":")
	}

coord.un=coord.un[!is.na(coord.un$locality.only),]
coord.un=coord.un[coord.un$locality.only!="",]
nrow(merge)==nrow(coord.un)							# FALSE merge=7640 coord.un=7682

# TAKE A LOOK TO THE REMAINING 42 RECORDS
coord.un$country[which(coord.un$country %in% sub.data.un$country==F)]


# CHECK FOR CONCORDANCE (AT LEAST BETWEEN DECLARED COUNTRY AND THE COUNTRY CONTAININGS COORDINATES!)
wrld=shapefile("world.shp")

pts=merge[,1:4]
coordinates(pts)=c("lon","lat")
crs(pts)=crs(wrld)

fun=function(x){unlist(strsplit(x,":"))[1]}
pts$country.only=unlist(lapply(pts$country,fun))

over=over(pts,wrld)$CNTRY_NAME

length(which(over==pts$country.only))
length(which(over!=pts$country.only))
length(which(is.na(over)==T))

# NAs WITHIN over: COASTAL/SMALL ISLAND RECORDS OR ERRORS??
plot(wrld)
plot(pts[which(is.na(over)==T),],add=T,cex=0.5,pch=19,col="red")

# DIFFERENCES IN COUNTRY: DIFFERENT CONVENTION IN WRITING COUNTRY NAME OR ERRORS?
cbind(pts$country.only[which(over!=pts$country.only)],over[which(over!=pts$country.only)])

# Mainly differences in coutry names (USA vs United States / Vietnam vs Viet Nam), but some errors occurs, e.g. :
# [8,] "Cambodia"                         "Nigeria"
# [68,] "Kazakhstan"                       "Jordan"

merge$over=over
merge$country.only=pts$country.only


# REMOVE NOT- (OR POORLY-) GEOREFERENCED RECORDS AND CHOOSE WICH COORDINATES TO COMPARE (PRIORITY TO GADM IF AREA <= 10000)

merge$long.adm=as.numeric(merge$long.adm)
merge$lat.adm=as.numeric(merge$lat.adm)
merge$long.gaz=as.numeric(merge$long.gaz)
merge$lat.gaz=as.numeric(merge$lat.gaz)
merge$area=as.numeric(merge$area)

NAs=which(is.na(merge$long.adm)==T & is.na(merge$long.gaz)==T)
if(length(NAs) > 0) {merge=merge[-NAs,]}

zeroes=which(merge$long.adm==0 & merge$lat.adm==0)
if(length(zeroes) > 0) {merge=merge[-zeroes,]}

area=which(merge$area>10000 & is.na(merge$long.gaz)==T)
merge=merge[-area,]

dist=numeric(nrow(merge))

for(i in 1:nrow(merge)){
	
	pt.gb=c(merge$lon[i],merge$lat[i])
	
	if(is.na(merge$long.adm[i])==F & merge$area[i]<=10000){pt.ref=c(merge$long.adm[i],merge$lat.adm[i])}else{pt.ref=c(merge$long.gaz[i],merge$lat.gaz[i])}

	dist[i]=pointDistance(pt.gb,pt.ref,lonlat=T)/1000
}

hist(log10(dist))			# ok, la follia


# CHECK AGAIN FOR CONCORDANCE (ONLY ON THE SUBSET OF PROBABLE ERRORS)

err=data.frame(dist,merge$over,merge$country.only,merge$ISO,merge$lon,merge$lat,merge$country,stringsAsFactors=F)
names(err)=c("dist","country.over","country.orig","ISO","gb.lon","gb.lat","country+loc")

err=err[order(err$dist,decreasing=T),]

## HAVE FUN ON GOOGLE EARTH!
nrow(err[err$dist>500,])
err[err$dist>500,]

write.csv(err,"referencing.errors.csv")



# PLOT DISTANCES
dist.sel=dist[dist<1000]
h=hist(dist.sel,main=paste("N records = ",length(dist),sep=""))

perc=round(h$count/length(dist),2)
text(h$mids,h$count+70,perc,cex=0.7)


# SAVE RESULTS
res=data.frame(merge,dist)

write.csv(res,"distance distribution.csv")













	