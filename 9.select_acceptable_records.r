library(raster)
library(rgdal)


# OPEN DATA
data=read.csv("GB_geomatched.csv",header=T,row.names=1,stringsAsFactors=F)
coord=read.csv("country_fields_with_lat_lon.csv",header=T,stringsAsFactors=F)


# SUBSET AND CLEAN GEOMATCHED
sub.data=data.frame(data$accession,data$country,data$long,data$lat,data$code_ISO,data$area.km.2,data$long.gaz,data$lat.gaz,data$feature.code,stringsAsFactors=F)
names(sub.data)=c("accession","country","long.adm","lat.adm","ISO","area","long.gaz","lat.gaz","feature.code")

sub.data$long.adm=as.numeric(sub.data$long.adm)
sub.data$lat.adm=as.numeric(sub.data$lat.adm)
sub.data$long.gaz=as.numeric(sub.data$long.gaz)
sub.data$lat.gaz=as.numeric(sub.data$lat.gaz)
sub.data$area=as.numeric(sub.data$area)


NAs=which(is.na(sub.data$long.adm)==T & is.na(sub.data$long.gaz)==T)
if(length(NAs) > 0) {sub.data=sub.data[-NAs,]}

zeroes=which(sub.data$long.adm==0 & sub.data$lat.adm==0)
if(length(zeroes) > 0) {sub.data=sub.data[-zeroes,]}

area=which(sub.data$area>10000 & is.na(sub.data$long.gaz)==T)
if(length(area) > 0) {sub.data=sub.data[-area,]}

# CLEAN COORDS
NAs=which(is.na(coord$lon)==T | is.na(coord$lat)==T)
if(length(NAs) > 0) {coord=coord[-NAs,]}

# UNIFORM TABLE FIELDS AND JOIN TABLES
gb.match=as.data.frame(matrix(NA,nrow(sub.data),4))
names(gb.match)= names(coord)

# PREFERENCES 1: IF WE HAVE adm AND gaz, WE CHOOSE gaz IF AND ONLY IF AREA > 10000
gb.match$accession=sub.data$accession
gb.match$country=sub.data$country

adm.pos=which(is.na(sub.data$long.adm)==F & sub.data$area<=10000)

gb.match$lon[adm.pos]=sub.data$long.adm[adm.pos]
gb.match$lat[adm.pos]=sub.data$lat.adm[adm.pos]

gb.match$lon[-adm.pos]=sub.data$long.gaz[-adm.pos]
gb.match$lat[-adm.pos]=sub.data$lat.gaz[-adm.pos]

# PREFERENCES 2: IF WE HAVE ORIGINAL AND ASSIGNED COORDINATES, WE CHOOSE ORIGINAL ONES

pos.dup=which(gb.match$country%in%coord$country==T)
gb.match=gb.match[-pos.dup,]

gb.match$type="assigned"
coord$type="original"

all.data=rbind(gb.match,coord)
		
# PLOT
wrld=shapefile("world.shp")

unique.pts=all.data[!duplicated(all.data$country),]
coordinates(unique.pts)=c("lon","lat")
crs(unique.pts)=crs(wrld)

pdf(file = "spatial_coverage.pdf", height = 5, width = 10)
par(mfrow=c(1,2))
plot(wrld,col="light grey",main=paste("Unique locations with original coords (N = ",nrow(unique.pts[unique.pts$type=="original",]),")",sep=""))
plot(unique.pts[unique.pts$type=="original",],pch=20,col=rgb(0,0,1,0.1),cex=0.3,add=T)

plot(wrld,col="light grey",main=paste("Unique locations with assigned coords (N = ",nrow(unique.pts[unique.pts$type=="assigned",]),")",sep=""))
plot(unique.pts[unique.pts$type=="assigned",],pch=20,,col=rgb(1,0,0,0.1),cex=0.3,add=T)
dev.off()

write.csv(all.data, "acceptable_records.csv")


