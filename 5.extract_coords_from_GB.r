d = read.table("GB_metadata_SEL.txt", sep = "\t", head = T, comment.char = "", quote = "")

## format latitude and longitude into separate vectors (and add a few vectors for ease of cross-referencing)
all_latlons = as.character(d$lat_lon[which(!is.na(d$lat_lon))])

## add a country vector only for those with lat and lon populated
country = as.character(d$country[which(!is.na(d$lat_lon))])
accession = as.character(d$accession[which(!is.na(d$lat_lon))])

## prepare output vectors
all_lats = rep(NA, length(all_latlons))
all_lons = rep(NA, length(all_latlons))

{## Write lat and lon for all coordinates with DECIMAL DEGREES with NSEW (with or without comma...)

## regex for DECIMAL DEGREES with NSEW (with or without comma...)
decimal_pattern_1 = "[0-9]{1,3}\\.[0-9]{0,18}\\s*?[NS],*?\\s[0-9]{1,3}\\.[0-9]{0,18}\\s*?[EW]"
length(grep(decimal_pattern_1, all_latlons))/length(all_latlons)
# [1] 0.975429
## It accounts for 97.5% of latlons

latlons = all_latlons[grep(decimal_pattern_1, all_latlons)]

ll_ll = strsplit(latlons, "\\sN|\\sS|\\sE|\\sW")

lat_sign = rep('+', length(latlons))
lon_sign = rep('+', length(latlons))
lat_sign[grep('S', latlons)] = '-'
lon_sign[grep('W', latlons)] = '-'

lat = as.numeric(sapply(ll_ll, "[[", 1))
lon = as.numeric(sapply(ll_ll, "[[", 2))

lat[lat_sign == '-'] = -lat[lat_sign == '-']
lon[lon_sign == '-'] = -lon[lon_sign == '-']

all_lats[grep(decimal_pattern_1, all_latlons)] = lat
all_lons[grep(decimal_pattern_1, all_latlons)] = lon

cbind(all_latlons, all_lats, all_lons)[(length(all_latlons)-10):length(all_latlons),]
}

{## Write lat and lon for all coordinates with DECIMAL DEGREES without NSEW
## regex for DECIMAL DEGREES without NSEW
decimal_pattern_2 = "-*?[0-9]{1,3}\\.[0-9]{0,6}[ ,]\\s*?-*?[0-9]{1,3}\\.[0-9]{0,6}"

## very interestingly, there is NEVER a format like "23.45, 33.20" WITHOUT NSEW!! Which is good, because it would be ambiguous if the format is unknown!!
all_latlons[setdiff(which(grepl("[NSEW]", all_latlons)==F), grep("-", all_latlons))]
# character(0)

latlons = all_latlons[grep(decimal_pattern_2, all_latlons)]

ll_ll = strsplit(latlons, ",|, | ")

lat = as.numeric(sapply(ll_ll, "[[", 1))
lon = as.numeric(sapply(ll_ll, "[[", 2))

all_lats[grep(decimal_pattern_2, all_latlons)] = lat
all_lons[grep(decimal_pattern_2, all_latlons)] = lon

cbind(all_latlons, all_lats, all_lons)[grep(decimal_pattern_2, all_latlons),]

}

{## Write lat and lon for all coordinates with degrees and DECIMAL MINUTES
## regex for degrees and DECIMAL MINUTES
degmin_pattern_1 = "[0-9]{1,3}\\s*?([Dd]eg|[Dd]egrees)\\s*?[0-9]{1,2}.[0-9]{1,6}'\\s*?[NS][,;]\\s*?[0-9]{1,3}\\s*?([Dd]eg|[Dd]egrees)\\s*?[0-9]{1,2}.[0-9]{1,6}'\\s*?[EW]"

latlons = all_latlons[grep(degmin_pattern_1, all_latlons)]

ll_lat = strsplit(latlons, "[Dd]eg|[Dd]egrees")
deg_lat = as.numeric(sapply(ll_lat, "[[", 1))

ll_lon = sapply(ll_lat, "[[", 2)
# eliminate all spaces not preceding longitudinal degrees
ll_lon = gsub("\\s$", "", ll_lon)
ll_lon = gsub("^\\s", "", ll_lon)
ll_lon = gsub("\\s[A-Za-z,;']", "", ll_lon)
ll_lon = strsplit(ll_lon, " ")
deg_lon = as.numeric(sapply(ll_lon, "[[", 2))
ll_min_lat = sapply(ll_lon, "[[", 1)
ll_min_lat = strsplit(ll_min_lat, "'|min|;| |,")
min_lat_as_degrees = as.numeric(sapply(ll_min_lat, "[[", 1))/60

ll_min_lon = sapply(ll_lat, "[[", 3)
# eliminate all spaces not preceding longitudinal degrees
ll_min_lon = gsub("\\s$", "", ll_min_lon)
ll_min_lon = gsub("^\\s", "", ll_min_lon)
ll_min_lon = gsub("\\s[A-Za-z,;']", "", ll_min_lon)
ll_min_lon = strsplit(ll_min_lon, "'|min")
min_lon_as_degrees = as.numeric(sapply(ll_min_lon, "[[", 1))/60

lat = deg_lat + min_lat_as_degrees
lon = deg_lon + min_lon_as_degrees

lat_sign = rep("+", length(latlons))
lon_sign = rep("+", length(latlons))
lat_sign[grep("S", latlons)] = "-"
lon_sign[grep("W", latlons)] = "-"

lat[lat_sign == "-"] = -lat[lat_sign == "-"]
lon[lon_sign == "-"] = -lon[lon_sign == "-"]

all_lats[grep(degmin_pattern_1, all_latlons)] = lat
all_lons[grep(degmin_pattern_1, all_latlons)] = lon
}

## There are a few records that are in fact NAs, even though they claim not to be..
length(grep("NA", all_latlons))
# [1] 165 !!

## We have recovered 99.8% of the coordinates
table(is.na(all_lats))
# FALSE  TRUE 
# 79392   336
table(is.na(all_lats))[1]/length(all_latlons[-grep("NA", all_latlons)])
# 0.9978508 

all_latlons[which(is.na(all_lats) == T & grepl("NA", all_latlons) == F)]

export = as.data.frame(cbind(accession, country, all_lons, all_lats), stringsAsFactors = F)
colnames(export) = c("accession", "country", "lon", "lat")
export$lon = as.numeric(export$lon)
export$lat = as.numeric(export$lat)

write.csv(export, "country_fields_with_lat_lon.csv", row.names = F)

{## NOTES
  ## apparently, the symbol '°' is never used to indicate degree (it is not available on most keyboard layouts, and probably it is forbidden!)
  length(grep("°", all_latlons))
  # character(0)
  ## the string "deg" or "degrees" is often used to indicate degrees, instead 
  length(grep("deg", all_latlons))
  # [1] 302
  ## apparently, the symbol '"' (double quote) is never use to indicate seconds of degree (is it forbidden?)
  all_latlons[grep('"', all_latlons)]
  # character(0)
  ## the symbol "'", though, is used in most cases when decimal degrees are not used
  length(grep("'", all_latlons))
  # [1] 305
  length(setdiff(grep("deg", all_latlons), grep("'", all_latlons)))
  # [1] 9
  unique(all_latlons[setdiff(grep("deg", all_latlons), grep("'", all_latlons))])
  # [1] "8 deg 45 min S, 63 deg 26 min W" "20-50 deg N, 86-54 deg W, 0 min"
  ## the second one is amazing!! WTF do they mean?!
  }
















