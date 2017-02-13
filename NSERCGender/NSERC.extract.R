###
# Extract Gender From NSERC Awards lists
###



#load Libraries ----------------
require(pdftools)
require(gender) #https://cran.rstudio.com/web/packages/gender/vignettes/predicting-gender.html
require(plyr)
require("DiversitySampler")
require(ggplot2)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  s = tolower(s)
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}


#Define Data -------------------- 
awards.list = c("CGSD2","CGSD3", "PGSD2", "PGSD3", "PDF") #this is list of awards through NSERC



#extract data set -------------


nserc.all.summary = c()
for(i in c(2013:2016)){

	#Define data sets ---------------
	location <- paste('http://www.nserc-crsng.gc.ca/NSERC-CRSNG/FundingDecisions-DecisionsFinancement/ScholarshipsAndFellowships-ConcoursDeBourses/', i, '/schol_master_', i, '_e.pdf', sep="")


	#load data sets -------------------
	txt <- pdf_text(location)


	#Mung out names, awards, and discipline ----
	txt.vector.spl = unlist(strsplit(txt,"\n"))
	txt.vector = unlist(strsplit(txt.vector.spl,"  "))
	txt.vector = txt.vector[txt.vector!=""] #creates a vector of each element within each row

	#extract name and award indices 
	txt.awards = which(txt.vector %in% awards.list)
	txt.ppl = txt.awards + 1

	#mung names to single first name as proper noun 
	ppl = txt.vector[txt.ppl]
	ppl.first = gsub(".*, ","",ppl)
	ppl.first = gsub(" .*","",ppl.first) #remove second-names
	ppl.first = gsub("-.*","",ppl.first)
	ppl.first = as.character(sapply(ppl.first,simpleCap))
	ppl.first = iconv(ppl.first, to='ASCII//TRANSLIT') #convert accents
	ppl.first = gsub("\'","",ppl.first)
	ppl.first = gsub("\`","",ppl.first)
	ppl.first = gsub("\\\"","",ppl.first)


	#estimate gender -------------------
	ppl.gender  =  data.frame(gender(ppl.first , years = 2000, method = "ssa"))
	miss = length(ppl.first[which(!ppl.first %in% ppl.gender$name)])
	ppl.miss = ppl.first[which(!ppl.first %in% ppl.gender$name)]
	ppl.gender = ppl.gender[!duplicated(ppl.gender$name),]


	#extract awards --------------------
	awards = txt.vector[txt.awards]

	#create df ------------------------
	nserc.gender = data.frame(cbind(name = ppl.first, award = awards) )
	nserc.gender = merge(nserc.gender, ppl.gender, by = "name")




	#summarize ----------------------
	nserc.summary <- ddply(nserc.gender, c("award"), summarise,
		               N    = length(gender),
		               num.M = length(gender[gender=="male"]),
		               prop = num.M/N	

		               )
	nserc.summary$rank = c(1,2,5,3,4)
	nserc.summary$year = rep(i, nrow(nserc.summary))



	nserc.all.summary = rbind(nserc.summary, nserc.all.summary)

}

nserc.all.summary$N.error = nserc.all.summary$N + 20
nserc.all.summary$prop.error = nserc.all.summary$num.M/nserc.all.summary$N.error


#write data set
write.table(nserc.all.summary, file="NSERC_Grad_Gender", quote=F, row.names=F)






