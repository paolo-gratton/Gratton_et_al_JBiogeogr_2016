## OPEN DATA

data=read.csv("GB_metadata_SEL.csv")

## START WORKING ONLY ON LOCALITIES -> ONCE REFERENCED ALL THE LOCALITIES WE CAN JOIN THE TABLE OF GEOGRAPHIC ATTRIBUTES AND THE ORIGINAL TABLE SIMPLY USING A MERGE

geo=as.character(data$country)
length(geo)

geo=unique(geo) # remove repeated localities
length(geo)


## SELECT ONLY RECORDS WITH LOCALITY INFORMATION
sel=unique(grep(":",geo))

# % of records with locality info:
geo.tot=as.character(data$country)
100*(length(grep(":",geo.tot))/length(geo.tot))
# [1] 72.64139

geo=geo[sel]

geo.split=as.data.frame(matrix(0,length(geo),2))
names(geo.split)=c("country","locality")


## SEPARATE COUNTRY FROM LOCALITY (strsplit does not work properly: some records have more than 1 ":")
for(i in 1:length(geo)){
	split=unlist(strsplit(geo[i],NULL,fixed=T))
	
	pos=grep(":",split)[1]					# position of the first ":"
	geo.split$country[i]=substr(geo[i],1,pos-1)
	geo.split$locality[i]=substr(geo[i],pos+1,length(split))
}


## ADD COUNTRY CODES TO THE GEODATABASE - GADM
refer=read.csv("countries_and_country_codes_ISO.csv",header=T)

geo.split$code_ISO=character(nrow(geo.split))

for(i in 1:nrow(geo.split)){
	sel=grep(geo.split$country[i],refer$country,fixed=T) # io qui ho dei warnings ("input string [...] is invalid in this locale"), ma sembra che funzioni bene lo stesso
	
	if(length(sel)==1){
		geo.split$code_ISO[i]=as.character(refer$alpha3_code[sel])
	}else{
		geo.split$code_ISO[i]="manual check"
	}
}	
	
{# MANUAL CHECK
unique(geo.split$country[which(geo.split$code_ISO=="manual check")])

geo.split$code_ISO[geo.split$country=="USA"]="USA"						# United States of America			
geo.split$code_ISO[geo.split$country=="South Korea"]="KOR"					# Korea (Republic of)
geo.split$code_ISO[geo.split$country=="China"]="CHN"						# China (more than one match)
geo.split$code_ISO[geo.split$country=="North Korea"]="PRK"					# Korea (Democratic People's Republic of)                          
geo.split$code_ISO[geo.split$country=="Ireland"]="IRL"						# Ireland (more than one match)
geo.split$code_ISO[geo.split$country=="Georgia"]="GEO"						# Georgia (more than one match)
geo.split$code_ISO[geo.split$country=="India"]="IND"						# India (more than one match)
geo.split$code_ISO[geo.split$country=="Laos"]="LAO"						# Lao People's Democratic Republic 
geo.split$code_ISO[geo.split$country=="Democratic Republic of the Congo"]="COD"			# Congo (Democratic Republic of the)
geo.split$code_ISO[geo.split$country=="Netherlands Antilles"]="manual check"			# dissolved, currently CURACAO + SAINT MARTIN + BONAIRE, SABA, and SINT EUSTATIUS falling under the direct administration of the Netherlands.
geo.split$code_ISO[geo.split$country=="Dominica"]="DMA"						# Dominica (more than one match)
geo.split$code_ISO[geo.split$country=="Guinea"]="GIN"						# Guinea (more than one match)
geo.split$code_ISO[geo.split$country=="Republic of the Congo"]="COG"				# Congo
geo.split$code_ISO[geo.split$country=="Serbia and Montenegro"]="manual check"			# dissolved, currently SERBIA + MONTENEGRO
geo.split$code_ISO[geo.split$country=="Sudan"]="manual check"					# dissolved, currently SUDAN + SOUTH SUDAN
geo.split$code_ISO[geo.split$country=="Atlantic Ocean"]="manual check"				# not present in the db
geo.split$code_ISO[geo.split$country=="Pacific Ocean"]="manual check"				# not present in the db
geo.split$code_ISO[geo.split$country=="Cote D'ivoire"]="CIV"					# Cote d'Ivoire
geo.split$code_ISO[geo.split$country=="Yugoslavia"]="manual check"				# dissolved
geo.split$code_ISO[geo.split$country=="Niger"]="NER"      					# Niger (more than one match)
geo.split$code_ISO[geo.split$country=="Kosovo"]="SRB"						# not present in the db, formerly belonging to Serbia
geo.split$code_ISO[geo.split$country=="Borneo"]="manual check"					# more than one country
geo.split$code_ISO[geo.split$country=="Falkland Islands (Islas Malvinas)"]="FLK"		# Falkland Islands (Malvinas)
geo.split$code_ISO[geo.split$country=="Cape Verde"]="CPV"    					# Cabo Verde
geo.split$code_ISO[geo.split$country=="Johnston Atoll"]="USA"					# not present in the db (HAWAII??)
geo.split$code_ISO[geo.split$country=="Pitcairn Islands"]="PCN"					# Pitcairn
geo.split$code_ISO[geo.split$country=="Former Yugoslav Republic of Macedonia"]="MKD"		# Macedonia (the former Yugoslav Republic of)
geo.split$code_ISO[geo.split$country=="Burma"]="MMR"						# Myanmar
geo.split$code_ISO[geo.split$country=="Virgin Islands"]="manual check"				# US or British Virgin Islands?
geo.split$code_ISO[geo.split$country=="Samoa"]="WSM"						# Samoa (more than one match)
geo.split$code_ISO[geo.split$country=="Viet nam"]="VNM"						# Viet Nam
geo.split$code_ISO[geo.split$country=="British Virgin Islands"]="VGB"				# Virgin Islands (British)
}

geo.split$code_ISO = gsub(" ", "", geo.split$code_ISO)

## save original locality
geo.split$original_locality = geo.split$locality

## Remove lat and lon from "country" field (and move it to the "lat_lon" field?!)
decimal_pattern_1 = "[0-9]{1,3}\\.[0-9]{0,18}\\s*?[NS],*?\\s[0-9]{1,3}\\.[0-9]{0,18}\\s*?[EW]"
decimal_pattern_2 = "-*?[0-9]{1,3}\\.[0-9]{0,6}[ ,]\\s*?-*?[0-9]{1,3}\\.[0-9]{0,6}"
degmin_pattern_1 = "[0-9]{1,3}\\s*?([Dd]eg|[Dd]egrees)\\s*?[0-9]{1,2}.[0-9]{1,6}'\\s*?[NS][,;]\\s*?[0-9]{1,3}\\s*?([Dd]eg|[Dd]egrees)\\s*?[0-9]{1,2}.[0-9]{1,6}'\\s*?[EW]"

# non si recupera un cazzo, ma ormai l'ho fatto!
geo.split$locality = gsub(decimal_pattern_1, "", geo.split$locality)
geo.split$locality = gsub(decimal_pattern_2, "", geo.split$locality)
geo.split$locality = gsub(degmin_pattern_1, "", geo.split$locality)


## REMOVE UNDESIRED GENERIC WORDS (including most allLOWERCASE words (THEY ARE NOT TOPONYMS, PROBABLY) AND ALL INDICATIONS OF DIRECTIONS (es. 25km E Madison) AND ASSIGN A NEW FIELD SEPARATOR CHARACTER (|)
geo.split$locality_new = NA


{	# REMOVE DIRECTIONS
	
	geo.split$locality = gsub("([0-9])([Mm][Ii]\\.*?|[Kk][Mm]\\.*?|[Mm][Ii][Ll][Ee][Ss])", "\\1 \\2", geo.split$locality) ## This line ADDS a space where it is missing between a number and a distance unit (e.g. 27 Miles N of Frascati). replaced on March 9.. this line was not effective on earlier runs!
	
	dir_pattern_1 = "([0-9]{1,3}\\.*[0-9]{0,2}\\s([Mm][Ii]\\.*?|[Kk][Mm]\\.*?|[Mm][Ii][Ll][Ee][Ss])\\s([NSEWnsew]{1,3}|[Nn][Oo][Rr][Tt][Hh]|[S][Oo][Uu][Tt][Hh]|[Ww][Ee][Ss][Tt]|[Ee][Aa][Ss][Tt]|)\\s([Ff][Rr][Oo][Mm]|[Oo][Ff])*\\s*|([NSWE]{1,3}|[Nn][Oo][Rr][Tt][Hh]|[S][Oo][Uu][Tt][Hh]|[Ww][Ee][Ss][Tt]|[Ee][Aa][Ss][Tt])\\s([Ff][Rr][Oo][Mm]|[Oo][Ff])*\\s)|[Nn]ear\\s|[Cc]lose\\sto\\s"	
	
	geo.split$locality = gsub(dir_pattern_1, "", geo.split$locality)
	
	dir_pattern_2 = "([0-9]{1,3},[0-9]{0,2}\\s([Mm][Ii]\\.*?|[Kk][Mm]\\.*?|[Mm][Ii][Ll][Ee][Ss])\\s([NSEWnsew]{1,3}|[Nn][Oo][Rr][Tt][Hh]|[S][Oo][Uu][Tt][Hh]|[Ww][Ee][Ss][Tt]|[Ee][Aa][Ss][Tt]|)\\s([Ff][Rr][Oo][Mm]|[Oo][Ff])*\\s*|([NSWE]{1,3}|[Nn][Oo][Rr][Tt][Hh]|[S][Oo][Uu][Tt][Hh]|[Ww][Ee][Ss][Tt]|[Ee][Aa][Ss][Tt])\\s([Ff][Rr][Oo][Mm]|[Oo][Ff])*\\s)|[Nn]ear\\s|[Cc]lose\\sto\\s"
	
	geo.split$locality = gsub(dir_pattern_2, "", geo.split$locality)
	dir_pattern_3 = "[0-9]{1,3}.[0-9]{0,2}\\s([Mm][Ii]\\.*?|[Kk][Mm]\\.*?|[Mm][Ii][Ll][Ee][Ss])\\s[NSEWnsew]{1,3}"
	geo.split$locality = gsub(dir_pattern_3, "", geo.split$locality)	
	dir_pattern_4 = "[0-9]{1,3}.[0-9]{0,2}\\s([Mm][Ii]\\.*?|[Kk][Mm]\\.*?|[Mm][Ii][Ll][Ee][Ss])\\s([Ff][Rr][Oo][Mm]|[Oo][Ff])"	
	geo.split$locality = gsub(dir_pattern_4, "", geo.split$locality)	
	dir_pattern_5 = "[0-9]{1,3}.[0-9]{0,2}\\s([Mm][Ii]\\.*?|[Kk][Mm]\\.*?|[Mm][Ii][Ll][Ee][Ss])"	
	geo.split$locality = gsub(dir_pattern_5, "", geo.split$locality)
	geo.split$locality = gsub("\\<From\\>", "", geo.split$locality)
}	
	
# 	pattern = "\\<[A-Z]{2,3}\\>" # pattern of provincial/state etc, codes..
# 	strings = geo.split$locality
# 	m = regexpr(pattern, strings)
# 	codes = gsub("^\\s*|\\s*$", "", gsub("([Ff][Rr][Oo][Mm]|[Oo][Ff])", "", regmatches(strings, m)))
#	country_for_codes = geo.split$country[grep(pattern, geo.split$locality)]
# 	head(sort(table(country_for_codes), decreasing=T), 5)
# 	country_for_codes
# #          USA    Brazil Australia    Canada     Italy 
# #         1865       796       702       209        82
#         sum(head(sort(table(country_for_codes), decreasing=T), 5))/length(codes)
# # 	[1] 0.8374971

## we are translating provincial/state codes for the 5 most represente countries (83.8%)

for(i in 1:nrow(geo.split)){
	string=geo.split$locality[i]
	
	{# REMOVE GENERIC WORDS AND OTHER CLEANING..
	  # NOTE: I am keeping "Natural", "Reserve", "Park", "Parque" because I think (hope) that they can be in geonames and make the search more specific..
	{## Preliminary..
	## Identify very common words that would resist cleaning (Uppercase)
	# all_words = unlist(strsplit(geo.split$locality, "[ ,:;\\)\\(]"))
	# all_words_Upper = all_words[-grep("(^ |[[:space:]])([[:lower:]]{1,})", all_words)]
	## these are the 100 most common uppercase words
	# sort(table(all_words_Upper), decreasing=T)[1:100]
	}
	
	# save special place	
	dir = gsub("District of Columbia", "District Of Columbia", string)
	
	# CHANGE "Is." TO "Island"
	dir = gsub("Is\\.", "Island", dir)
	
	dir = gsub("\\s[Cc][Oo]\\.*?([,\\. :;\\$\\))])|\\s[Cc]ounty\\1","\\1 ", dir) # county
	dir = gsub("\\<Co\\>>", "",dir) # county
	dir = gsub("\\<Province\\>", "", dir) # Province
	dir = gsub("\\<Provincia\\>|\\<Prov\\>|\\<Prov\\.", "", dir) # Provincia and variations
	
	dir = gsub("\\<Departement de|\\<Departement\\>", "", dir) # Departement
	dir = gsub("\\<Department\\>|\\<Dept\\>|\\<Dept\\.", "", dir) # Department and variations
	dir = gsub("Distr\\.|Dist\\.||District", "", dir) # District
	dir = gsub("Distrito de |\\<Distrito\\>", "", dir) # Distrito
	dir = gsub("\\<Region de|\\<Region\\>", "", dir) # Region
	dir = gsub("\\<Municipio de|\\<Municipio\\>", "", dir) # Municipio
	dir = gsub("\\<Municipality\\>", "", dir) # Municipality
	dir = gsub("\\<Republic of|\\<Republic\\>", "", dir) # Republic
	dir = gsub("\\<Estado de|\\<Estado\\>", "", dir) # Estado
	dir = gsub("\\<Oblast`\\>|\\<Oblast'\\>|\\<Oblast\\>|\\<Obl\\.*?\\>", "", dir) # Oblast
	dir = gsub("city of ", "", dir) # "city of"
	dir = gsub("Ter\\.", "Territory", dir) # "Territory", see below..
	dir = gsub("Rep\\.", "", dir) # "Rep." for "Republic", see below..
	# Remove "Territory" only outside Australia and Canada
	if(! geo.split$code_ISO[i] %in% c("AUS ", "CAN ")) {dir = gsub("\\<Territory\\>", "", dir)}
	dir = gsub("Just Before |Just ", "", dir) # a couple of Idiots told us that some place is "Just north of.." or "Just Before.."
	
	## Replace "Mt." with "Mount" (etc.) only in English speaking countries
	if(geo.split$code_ISO[i] %in% c(
	"AUS","USA","CAN","BLZ","NZL","GBR",
	"IRL","KNA","GRD","VCT","JAM","ATG",
	"GUY","PRI","LCA","CHL","ARG","NIC",
	"GNQ","BTN","AIA","CYM","GHA","PAK",
	"NGA","ZMB","BWA","IMN","KEN","NAM",
	"ZWE","SWZ","SLE","LBR","LSO","FLK",
	"SGS","MLT","WSM","VGB")){
	  dir = gsub("Mt\\.", "Mount ", dir)
	  dir = gsub("Mt ", "Mount ", dir)
	  dir = gsub("Mts\\.", "Mountains", dir)
	  dir = gsub("Mts ", "Mountains", dir)
	  dir = gsub("R\\.", "River", dir)
	  } else {
	  dir = gsub("Mt\\.", "", dir)
	  dir = gsub("Mt ", "", dir)
	  dir = gsub("Mts\\.", "", dir)
	  dir = gsub("Mts ", "", dir)
	  }

	# Solving some popoular acronyms (we might search for more.. or more *versions* of the F** USA!)
	if (geo.split$code_ISO[i] == "BRA"){
	dir = gsub("\\<AC\\>", ",Acre,", dir)
	dir = gsub("\\<AL\\>", ",Alagoas,", dir)
	dir = gsub("\\<AP\\>", ",Amapa,", dir)
	dir = gsub("\\<AM\\>", ",Amazonas,", dir)
	dir = gsub("\\<BA\\>", ",Bahia,", dir)
	dir = gsub("\\<CE\\>", ",Ceara,", dir)
	dir = gsub("\\<DF\\>", ",Distrito Federal,", dir)
	dir = gsub("\\<ES\\>", ",Espirito Santo,", dir)
	dir = gsub("\\<GO\\>", ",Goias,", dir)
	dir = gsub("\\<MA\\>", ",Maranhao,", dir)
	dir = gsub("\\<MT\\>", ",MatoGrosso,", dir)
	dir = gsub("\\<MS\\>", ",MatoGrosso do Sul,", dir)
	dir = gsub("\\<MG\\>", ",Minas Gerais,", dir)
	dir = gsub("\\<PA\\>", ",Para,", dir)
	dir = gsub("\\<PB\\>", ",Paraiba,", dir)
	dir = gsub("\\<PR\\>", ",Parana,", dir)
	dir = gsub("\\<PE\\>", ",Pernambuco,", dir)
	dir = gsub("\\<PI\\>", ",Piaui,", dir)
	dir = gsub("\\<RJ\\>", ",Rio de Janeiro,", dir)
	dir = gsub("\\<RN\\>", ",Rio Grande do Norte,", dir)
	dir = gsub("\\<RS\\>", ",Rio Grande do Sul,", dir)
	dir = gsub("\\<RO\\>", ",Rondonia,", dir)
	dir = gsub("\\<RR\\>", ",Roraima,", dir)
	dir = gsub("\\<SC\\>", ",Santa Catarina,", dir)
	dir = gsub("\\<SP\\>", ",Sao Paulo,", dir)
	dir = gsub("\\<SE\\>", ",Sergipe,", dir)
	dir = gsub("\\<TO\\>", ",Tocantins,", dir)
	}

	if (geo.split$code_ISO[i] == "CAN"){
	dir = gsub("\\<AB\\>", ",Alberta,", dir)
	dir = gsub("\\<BC\\>", ",British Columbia,", dir)
	dir = gsub("\\<MB\\>", ",Manitoba,", dir)
	dir = gsub("\\<NB\\>", ",New Brunswick,", dir)
	dir = gsub("\\<NL\\>", ",Newfoundland and Labrador,", dir)
	dir = gsub("\\<NS\\>", ",Nova Scotia,", dir)
	dir = gsub("\\<NT\\>", ",Northwest Territories,", dir)
	dir = gsub("\\<NU\\>", ",Nunavut,", dir)
	dir = gsub("\\<ON\\>", ",Ontario,", dir)
	dir = gsub("\\<PE\\>", ",Prince Edward Island,", dir)
	dir = gsub("\\<QC\\>", ",Quebec,", dir)
	dir = gsub("\\<SK\\>", ",Saskatchewan,", dir)
	dir = gsub("\\<YT\\>", ",Yukon,", dir)
	# other forms..
	dir = gsub("\\<NWT\\>", ",Northwest Territories,", dir)
	dir = gsub("NW Territor(ies|y)", ",Northwest Territories,", dir)
	dir = gsub("Northwest Territory", ",Northwest Territories,", dir)
	dir = gsub("\\<NBW\\>", ",New Brunswick,", dir)
	}

	if (geo.split$code_ISO[i] == "USA"){
	dir = gsub("\\<AL\\>", ",Alabama,", dir)
	dir = gsub("\\<AK\\>", ",Alaska,", dir)
	dir = gsub("\\<AZ\\>", ",Arizona,", dir)
	dir = gsub("\\<AR\\>", ",Arkansas,", dir)
	dir = gsub("\\<CA\\>", ",California,", dir)
	dir = gsub("\\<CO\\>", ",Colorado,", dir)
	dir = gsub("\\<CT\\>", ",Connecticut,", dir)
	dir = gsub("\\<DE\\>", ",Delaware,", dir)
	dir = gsub("\\<DC\\>", ",District of Columbia,", dir)
	dir = gsub("\\<FL\\>", ",Florida,", dir)
	dir = gsub("\\<GA\\>", ",Georgia,", dir)
	dir = gsub("\\<HI\\>", ",Hawaii,", dir)
	dir = gsub("\\<ID\\>", ",Idaho,", dir)
	dir = gsub("\\<IL\\>", ",Illinois,", dir)
	dir = gsub("\\<IN\\>", ",Indiana,", dir)
	dir = gsub("\\<IA\\>", ",Iowa,", dir)
	dir = gsub("\\<KS\\>", ",Kansas,", dir)
	dir = gsub("\\<KY\\>", ",Kentucky,", dir)
	dir = gsub("\\<LA\\>", ",Louisiana,", dir)
	dir = gsub("\\<ME\\>", ",Maine,", dir)
	dir = gsub("\\<MD\\>", ",Maryland,", dir)
	dir = gsub("\\<MA\\>", ",Massachusetts,", dir)
	dir = gsub("\\<MI\\>", ",Michigan,", dir)
	dir = gsub("\\<MN\\>", ",Minnesota,", dir)
	dir = gsub("\\<MS\\>", ",Mississippi,", dir)
	dir = gsub("\\<MO\\>", ",Missouri,", dir)
	dir = gsub("\\<MT\\>", ",Montana,", dir)
	dir = gsub("\\<NE\\>", ",Nebraska,", dir)
	dir = gsub("\\<NV\\>", ",Nevada,", dir)
	dir = gsub("\\<NH\\>", ",New Hampshire,", dir)
	dir = gsub("\\<NJ\\>", ",New Jersey,", dir)
	dir = gsub("\\<NM\\>", ",New Mexico,", dir)
	dir = gsub("\\<NY\\>", ",New York,", dir)
	dir = gsub("\\<NC\\>", ",North Carolina,", dir)
	dir = gsub("\\<ND\\>", ",North Dakota,", dir)
	dir = gsub("\\<OH\\>", ",Ohio,", dir)
	dir = gsub("\\<OK\\>", ",Oklahoma,", dir)
	dir = gsub("\\<OR\\>", ",Oregon,", dir)
	dir = gsub("\\<PA\\>", ",Pennsylvania,", dir)
	dir = gsub("\\<RI\\>", ",Rhode Island,", dir)
	dir = gsub("\\<SC\\>", ",South Carolina,", dir)
	dir = gsub("\\<SD\\>", ",South Dakota,", dir)
	dir = gsub("\\<TN\\>", ",Tennessee,", dir)
	dir = gsub("\\<TX\\>", ",Texas,", dir)
	dir = gsub("\\<UT\\>", ",Utah,", dir)
	dir = gsub("\\<VT\\>", ",Vermont,", dir)
	dir = gsub("\\<VA\\>", ",Virginia,", dir)
	dir = gsub("\\<WA\\>", ",Washington,", dir)
	dir = gsub("\\<WV\\>", ",West Virginia,", dir)
	dir = gsub("\\<WI\\>", ",Wisconsin,", dir)
	dir = gsub("\\<WY\\>", ",Wyoming,", dir)
	}

	if (geo.split$code_ISO[i] == "ITA"){
	dir = gsub("\\<AG\\>", ",Agrigento,", dir)
	dir = gsub("\\<AL\\>", ",Alessandria,", dir)
	dir = gsub("\\<AN\\>", ",Ancona,", dir)
	dir = gsub("\\<AO\\>", ",Aosta,", dir)
	dir = gsub("\\<AR\\>", ",Arezzo,", dir)
	dir = gsub("\\<AP\\>", ",Ascoli Piceno,", dir)
	dir = gsub("\\<AT\\>", ",Asti,", dir)
	dir = gsub("\\<AV\\>", ",Avellino,", dir)
	dir = gsub("\\<BA\\>", ",Bari,", dir)
	dir = gsub("\\<BT\\>", ",Barletta-Andria-Trani,", dir)
	dir = gsub("\\<BL\\>", ",Belluno,", dir)
	dir = gsub("\\<BN\\>", ",Benevento,", dir)
	dir = gsub("\\<BG\\>", ",Bergamo,", dir)
	dir = gsub("\\<BI\\>", ",Biella,", dir)
	dir = gsub("\\<BO\\>", ",Bologna,", dir)
	dir = gsub("\\<BZ\\>", ",Bolzano,", dir)
	dir = gsub("\\<BS\\>", ",Brescia,", dir)
	dir = gsub("\\<BR\\>", ",Brindisi,", dir)
	dir = gsub("\\<CA\\>", ",Cagliari,", dir)
	dir = gsub("\\<CL\\>", ",Caltanissetta,", dir)
	dir = gsub("\\<CB\\>", ",Campobasso,", dir)
	dir = gsub("\\<CI\\>", ",Carbonia-Iglesias,", dir)
	dir = gsub("\\<CE\\>", ",Caserta,", dir)
	dir = gsub("\\<CT\\>", ",Catania,", dir)
	dir = gsub("\\<CZ\\>", ",Catanzaro,", dir)
	dir = gsub("\\<CH\\>", ",Chieti,", dir)
	dir = gsub("\\<CO\\>", ",Como,", dir)
	dir = gsub("\\<CS\\>", ",Cosenza,", dir)
	dir = gsub("\\<CR\\>", ",Cremona,", dir)
	dir = gsub("\\<KR\\>", ",Crotone,", dir)
	dir = gsub("\\<CN\\>", ",Cuneo,", dir)
	dir = gsub("\\<EN\\>", ",Enna,", dir)
	dir = gsub("\\<FM\\>", ",Fermo,", dir)
	dir = gsub("\\<FE\\>", ",Ferrara,", dir)
	dir = gsub("\\<FI\\>", ",Florence,", dir)
	dir = gsub("\\<FG\\>", ",Foggia,", dir)
	dir = gsub("\\<FC\\>", ",Forli' - Cesena,", dir)
	dir = gsub("\\<FR\\>", ",Frosinone,", dir)
	dir = gsub("\\<GE\\>", ",Genoa,", dir)
	dir = gsub("\\<GO\\>", ",Gorizia,", dir)
	dir = gsub("\\<GR\\>", ",Grosseto,", dir)
	dir = gsub("\\<IM\\>", ",Imperia,", dir)
	dir = gsub("\\<IS\\>", ",Isernia,", dir)
	dir = gsub("\\<SP\\>", ",La Spezia,", dir)
	dir = gsub("\\<AQ\\>", ",L'Aquila,", dir)
	dir = gsub("\\<LT\\>", ",Latina,", dir)
	dir = gsub("\\<LE\\>", ",Lecce,", dir)
	dir = gsub("\\<LC\\>", ",Lecco,", dir)
	dir = gsub("\\<LI\\>", ",Livorno,", dir)
	dir = gsub("\\<LO\\>", ",Lodi,", dir)
	dir = gsub("\\<LU\\>", ",Lucca,", dir)
	dir = gsub("\\<MC\\>", ",Macerata,", dir)
	dir = gsub("\\<MN\\>", ",Mantua,", dir)
	dir = gsub("\\<MS\\>", ",Massa and Carrara,", dir)
	dir = gsub("\\<MT\\>", ",Matera,", dir)
	dir = gsub("\\<VS\\>", ",Medio Campidano,", dir)
	dir = gsub("\\<ME\\>", ",Messina,", dir)
	dir = gsub("\\<MI\\>", ",Milan,", dir)
	dir = gsub("\\<MO\\>", ",Modena,", dir)
	dir = gsub("\\<MB\\>", ",Monza and Brianza,", dir)
	dir = gsub("\\<NA\\>", ",Naples,", dir)
	dir = gsub("\\<NO\\>", ",Novara,", dir)
	dir = gsub("\\<NU\\>", ",Nuoro,", dir)
	dir = gsub("\\<OG\\>", ",Ogliastra,", dir)
	dir = gsub("\\<OT\\>", ",Olbia-Tempio,", dir)
	dir = gsub("\\<OR\\>", ",Oristano,", dir)
	dir = gsub("\\<PD\\>", ",Padua,", dir)
	dir = gsub("\\<PA\\>", ",Palermo,", dir)
	dir = gsub("\\<PR\\>", ",Parma,", dir)
	dir = gsub("\\<PV\\>", ",Pavia,", dir)
	dir = gsub("\\<PG\\>", ",Perugia,", dir)
	dir = gsub("\\<PU\\>", ",Pesaro E Urbino,", dir)
	dir = gsub("\\<PE\\>", ",Pescara,", dir)
	dir = gsub("\\<PC\\>", ",Piacenza,", dir)
	dir = gsub("\\<PI\\>", ",Pisa,", dir)
	dir = gsub("\\<PT\\>", ",Pistoia,", dir)
	dir = gsub("\\<PN\\>", ",Pordenone,", dir)
	dir = gsub("\\<PZ\\>", ",Potenza,", dir)
	dir = gsub("\\<PO\\>", ",Prato,", dir)
	dir = gsub("\\<RG\\>", ",Ragusa,", dir)
	dir = gsub("\\<RA\\>", ",Ravenna,", dir)
	dir = gsub("\\<RC\\>", ",Reggio Di Calabria,", dir)
	dir = gsub("\\<RE\\>", ",Reggio Nell'Emilia,", dir)
	dir = gsub("\\<RI\\>", ",Rieti,", dir)
	dir = gsub("\\<RN\\>", ",Rimini,", dir)
	dir = gsub("\\<RM\\>", ",Rome,", dir)
	dir = gsub("\\<RO\\>", ",Rovigo,", dir)
	dir = gsub("\\<SA\\>", ",Salerno,", dir)
	dir = gsub("\\<SS\\>", ",Sassari,", dir)
	dir = gsub("\\<SV\\>", ",Savona,", dir)
	dir = gsub("\\<SI\\>", ",Siena,", dir)
	dir = gsub("\\<SO\\>", ",Sondrio,", dir)
	dir = gsub("\\<SR\\>", ",Syracuse,", dir)
	dir = gsub("\\<TA\\>", ",Taranto,", dir)
	dir = gsub("\\<TE\\>", ",Teramo,", dir)
	dir = gsub("\\<TR\\>", ",Terni,", dir)
	dir = gsub("\\<TP\\>", ",Trapani,", dir)
	dir = gsub("\\<TN\\>", ",Trento,", dir)
	dir = gsub("\\<TV\\>", ",Treviso,", dir)
	dir = gsub("\\<TS\\>", ",Trieste,", dir)
	dir = gsub("\\<TO\\>", ",Turin,", dir)
	dir = gsub("\\<UD\\>", ",Udine,", dir)
	dir = gsub("\\<VA\\>", ",Varese,", dir)
	dir = gsub("\\<VE\\>", ",Venice,", dir)
	dir = gsub("\\<VB\\>", ",Verbano-Cusio-Ossola,", dir)
	dir = gsub("\\<VC\\>", ",Vercelli,", dir)
	dir = gsub("\\<VR\\>", ",Verona,", dir)
	dir = gsub("\\<VV\\>", ",Vibo Valentia,", dir)
	dir = gsub("\\<VI\\>", ",Vicenza,", dir)
	dir = gsub("\\<VT\\>", ",Viterbo,", dir)
	}
	
	if(geo.split$code_ISO[i] == "AUS"){
	dir = gsub("\\<NSW\\>", ",New South Wales,", dir)
	dir = gsub("\\<V[Ii][Cc]\\>", ",Victoria,", dir)
	dir = gsub("\\<NT\\>", ",Northern Territory,", dir)
	dir = gsub("\\<Q[Ll][Dd]\\>", ",Queensland,", dir)
	dir = gsub("\\<SA\\>", ",Southern Australia,", dir)
	dir = gsub("\\<WA\\>", ",Western Australia,", dir)
	dir = gsub("\\<T[Aa][Ss]\\>", ",Tasmania,", dir)
	} # ONCE (luckily NOT in Australia) NSW stands for "North-South-West"!!! Che cazzo vòr di'???!!!!)
	
	# Remove "State" except in "State Forest"
	dir = gsub("State Forest", "Xtate Forest", dir)
	dir = gsub("State", "", dir)
	dir = gsub("Xtate", "State", dir)

	{# Make some prepositions Uppercase (only when they are in between Uppercase words) to save them from the elimination of all_lowercase words..
	  dir = gsub("([A-Z][a-z]{1,} )de las( [A-Z][a-z]{1,})","\\1De Las\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )de los( [A-Z][a-z]{1,})","\\1De Los\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )de la( [A-Z][a-z]{1,})","\\1De La\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )della( [A-Z][a-z]{1,})","\\1Della\\2",dir) # recuperiamo 1 punto!! Monti dell Tolfa (se ce dice bene!)
	  dir = gsub("([A-Z][a-z]{1,} )des( [A-Z][a-z]{1,})","\\1Des\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )de( [A-Z][a-z]{1,})","\\1De\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )del( [A-Z][a-z]{1,})","\\1Del\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )di( [A-Z][a-z]{1,})","\\1Di\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )du( [A-Z][a-z]{1,})","\\1Du\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )da( [A-Z][a-z]{1,})","\\1Da\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )dos( [A-Z][a-z]{1,})","\\1Dos\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )do( [A-Z][a-z]{1,})","\\1Do\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )in( [A-Z][a-z]{1,})","\\1In\\2",dir) # inutile! Si recuperano, forse 2 posti in Italia e basta!!
	  dir = gsub("([A-Z][a-z]{1,}) en ([A-Z][a-z]{1,})","\\1-En-\\2",dir) # almeno in Francia, scrivono così.. saranno sì e no 3 o 4..
	  dir = gsub("([A-Z][a-z]{1,} )les( [A-Z][a-z]{1,})","\\1Les\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )d'([A-Z][a-z]{1,})","\\1D'\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )de'( [A-Z][a-z]{1,})","\\1De'\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )dei( [A-Z][a-z]{1,})","\\1Dei\\2",dir)
	  dir = gsub("([A-Z][a-z]{1,} )delle( [A-Z][a-z]{1,})","\\1Delle\\2",dir)
	  }	
	
	# REMOVE remaining two-letters allCAPITAL words (they are likely province/state codes..)
	dir = gsub("\\<[A-Z]{2,5}\\>", "", dir)
	
	}
	
	# REMOVE ")" AND SPLIT ON [(], [,], [;], [:], [/] AND [_]
	bra=gsub("\\)","",dir)
	spl=gsub("\\,|\\:|\\(|\\;|/|\\_","|",bra)
	
	# REMOVE LOWER CASE WORDS
	low = gsub("(^|\\s|\\|)[a-z]{1,}\\>","",spl)

	# REMOVE EMPTY FIELDS ("|||" or "| |   |")
	emp=gsub("\\|\\s*\\|*","|",low)
	emp=gsub("^\\|+","",emp)
	emp=gsub("\\|+$","",emp)
		
	
	## THESE STRINGS WERE ORIGINALLY IN THE GEOREFERENCING SCRIPT; BUT THEY ARE PROBABLY  BETTER PLACED HERE
	
	rec=unlist(strsplit(as.character(emp),"[|]"))
	
	if(length(rec)==0){geo.split$locality_new[i]=NA
	}else{

		# RETAIN TERMS WITH MORE THAN 75% OF ALPHABETIC CHARACTERS 
		for(j in 1:length(rec)){
			split=unlist(strsplit(rec[j],NULL))
			res.tmp=numeric(length(split))
		
			for(k in 1:length(split)){if(split[k] %in% c(letters,LETTERS," ")){res.tmp[k]=1}else{res.tmp[k]=0}}
			
			ratio=sum(res.tmp)/length(split)
			if(ratio>=0.75){rec[j]=rec[j]}else{rec[j]=""}
		}
	
		rec=rec[rec!=""]
		
		
		# REMOVE ISOLATED PUNCTUATION CHARACTERS 
		rec=gsub("(^|\\s+)[[:punct:]]{0,}\\s*"," ",rec)
		
		
		# REMOVE DOUBLE SPACING
		rec=gsub("\\s+"," ",rec)
	
		
		# REMOVE WHITE SPACES AT THE BEGINNING AND AT THE END OF THE STRING
		rec=gsub("^\\s*|\\s*$","",rec)
		if(length(grep("^[A-Z]{1,3}$", rec)) > 0){rec = rec [-grep("^[A-Z]{1,3}$", rec)]} # REMOVES ISOLATED REMAINING CODES (OR PARTS OF IT)
		# rec = rec[which(duplicated(rec) == F)] (remove duplicated words.. ABANDONED!!)
		if (length(grep("[0-9] (K[Mm]|M[Ii]\\.|M[Ii]) [NSEW]{1,2}", rec)) > 0) {rec = rec[-grep("[0-9] (K[Mm]|M[Ii]\\.|M[Ii]) [NSEW]{1,2}", rec)]}
		if (length(grep("[0-9] [NSEW]{1,2}", rec)) > 0) {rec = rec[-grep("[0-9] [NSEW]{1,2}", rec)]}
		geo.split$locality_new[i]=paste(rec,collapse="|")
		# REMOVE EMPTY FIELDS ("|||" or "| |   |")
		geo.split$locality_new[i]=gsub("\\|\\s*\\|*","|",geo.split$locality_new[i])
		geo.split$locality_new[i]=gsub("^\\|+","",geo.split$locality_new[i])
		geo.split$locality_new[i]=gsub("\\|+$","",geo.split$locality_new[i])
		
	}

}

## remove "locality" (save "original_locality")
geo.split = geo.split[,-which(colnames(geo.split) == "locality")]

write.csv(geo.split,"geo.only_split.csv")
