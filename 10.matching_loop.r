georef_table = read.csv("acceptable_records.csv", stringsAsFactors = F, row.names = 1) # reads table with mathced locations that fulfill criteria for acceptance

## Load GenBank data
d = read.table("GB_all_vertebrates.txt", head = T, sep = "\t", comment.char = "", quote = "", stringsAsFactors = F)

basic_d = d[,c(2,16)]

basic_d$lon = NA
basic_d$lat = NA
basic_d$type = NA

plot(0,1, type = "n", main = "Loop progress..", xlim = c(0, length(georef_table$accession)),ylim = c(-1, 2), bty = "n", yaxt = "n", ylab = "", xlab = paste("loop started at", date()))
rect(0, 0, length(georef_table$accession),1)

for (i in 1:length(georef_table$accession)) {
  pos = which(basic_d$accession == georef_table$accession[i])
  basic_d$lon[pos] = georef_table$lon[i]
  basic_d$lat[pos] = georef_table$lat[i]
  basic_d$type[pos] = georef_table$type[i]
  if(i %% 100 == 0) {
    print(paste("accession", i, "of", length(georef_table$accession), "processed at", date()))
    points(i, 0.5, pch = "|", col = "royalblue2")
    }
  }

# OLD NAME = 7_bis.create_GEOREF.r

# rename the matched table with georef assigned..
georef_match = basic_d

# check concordance between the two tables
if(all(d$accession == georef_match$accession) == TRUE) {
  
  print("Pheeww!!")

  {## SEPARATE COUNTRY FROM LOCALITY and add ISO COUNTRY CODES

  d_countries = unique(d$country[!is.na(d$country)])
  countries = rep(NA, length(d_countries))

  countries = unlist(lapply(strsplit(d_countries, ":"), FUN = "[[", 1))

  # create COUNTRY CODES
  refer=read.csv("countries_and_country_codes_ISO.csv",header=T)

  code_ISO=character(length(countries))

  for(i in 1:length(countries)){
	  sel=grep(countries[i],refer$country,fixed=T) # there may be warnings here ("input string [...] is invalid in this locale"), depending on the computer settings..
	  if(length(sel)==1){code_ISO[i]=as.character(refer$alpha3_code[sel])
	  }else{
		  code_ISO[i]="manual check"
	  }
  }	


  {# Manual check..
  unlist(unique(countries[which(code_ISO=="manual check")]))

    code_ISO[countries=="USA"]="USA"						# United States of America			
    code_ISO[countries=="South Korea"]="KOR"					# Korea (Republic of)
    code_ISO[countries=="China"]="CHN"						# China (more than one match)
    code_ISO[countries=="North Korea"]="PRK"					# Korea (Democratic People's Republic of)                          
    code_ISO[countries=="Ireland"]="IRL"						# Ireland (more than one match)
    code_ISO[countries=="Georgia"]="GEO"						# Georgia (more than one match)
    code_ISO[countries=="India"]="IND"						# India (more than one match)
    code_ISO[countries=="Laos"]="LAO"						# Lao People's Democratic Republic 
    code_ISO[countries=="Democratic Republic of the Congo"]="COD"			# Congo (Democratic Republic of the)
    code_ISO[countries=="Netherlands Antilles"]="manual check"			# dissolved, currently CURACAO + SAINT MARTIN + BONAIRE, SABA, and SINT EUSTATIUS falling under the direct administration of the Netherlands.
    code_ISO[countries=="Dominica"]="DMA"						# Dominica (more than one match)
    code_ISO[countries=="Guinea"]="GIN"						# Guinea (more than one match)
    code_ISO[countries=="Republic of the Congo"]="COG"				# Congo
    code_ISO[countries=="Serbia and Montenegro"]="manual check"			# dissolved, currently SERBIA + MONTENEGRO
    code_ISO[countries=="Sudan"]="manual check"					# dissolved, currently SUDAN + SOUTH SUDAN
    code_ISO[countries=="Atlantic Ocean"]="manual check"				# not present in the db
    code_ISO[countries=="Pacific Ocean"]="manual check"				# not present in the db
    code_ISO[countries=="Cote D'ivoire"]="CIV"					# Cote d'Ivoire
    code_ISO[countries=="Yugoslavia"]="manual check"				# dissolved
    code_ISO[countries=="Niger"]="NER"      					# Niger (more than one match)
    code_ISO[countries=="Kosovo"]="SRB"						# not present in the db, formerly belonging to Serbia
    code_ISO[countries=="Borneo"]="manual check"					# more than one countries
    code_ISO[countries=="Falkland Islands (Islas Malvinas)"]="FLK"		# Falkland Islands (Malvinas)
    code_ISO[countries=="Cape Verde"]="CPV"    					# Cabo Verde
    code_ISO[countries=="Johnston Atoll"]="USA"					# not present in the db (HAWAII??)
    code_ISO[countries=="Pitcairn Islands"]="PCN"					# Pitcairn
    code_ISO[countries=="Former Yugoslav Republic of Macedonia"]="MKD"		# Macedonia (the former Yugoslav Republic of)
    code_ISO[countries=="Burma"]="MMR"						# Myanmar
    code_ISO[countries=="Virgin Islands"]="manual check"				# US or British Virgin Islands?
    code_ISO[countries=="Samoa"]="WSM"						# Samoa (more than one match)
    code_ISO[countries=="Viet nam"]="VNM"						# Viet Nam
    code_ISO[countries=="British Virgin Islands"]="VGB"				# Virgin Islands (British)

    code_ISO[countries=="Netherlands Antilles"]= "ANT"
    code_ISO[countries=="Viet Nam,Viet Nam"]= "VNM"
    code_ISO[countries=="Thailand,Thailand"]= "THA"
    code_ISO[countries=="Malaysia,Malaysia"]= "MYS"
    code_ISO[countries=="China,China"]= "CHN"
    code_ISO[countries=="Taiwan,Taiwan"]= "TWN"
    code_ISO[countries=="Cambodia,Cambodia"]= "KHM"
    code_ISO[countries=="Japan,Japan"]= "JPN"
    code_ISO[countries=="Myanmar,Myanmar"]= "MMR"
    code_ISO[countries=="India,India"]= "IND"
    code_ISO[countries=="Sri Lanka,Sri Lanka"]= "LKA"
    code_ISO[countries=="Algeria,Algeria"]= "DZA"
    code_ISO[countries=="Democratic Republic of The Congo"]= "COD"
    code_ISO[countries=="USA,NA"]= "USA"
    code_ISO[countries=="Australia,Australia,Australia"]= "USA"
    code_ISO[countries=="Cote D'Ivoire"]= "CIV"
    code_ISO[countries=="Czech republic"]= "CZE"
  }

  code_ISO[which(code_ISO == "manual check")] = NA
  code_ISO = gsub(" ", "", code_ISO)

  }

  ## attach country codes to d..
  d$code_ISO = NA
  for(i in 1:length(d_countries)) {
      d$code_ISO[which(d$country == d_countries[i])] = code_ISO[i]
      if(i %% 100 == 0) {print(paste(date(),": ",i,"/",length(d_countries)," done", sep = ""))}
    }

  # attach georef_data (lat and lon + assigned) to the big table
  write.table(cbind(d, georef_match[,c("lon", "lat", "type")]), file = "GB_all_vertebrates_GEOREF.txt", row.names = F, sep = "\t", quote = F)
  
  } else {print("Damn! accession numbers do not match!!")}
  
