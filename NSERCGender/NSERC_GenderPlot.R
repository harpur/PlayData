###
# Plot Gender From NSERC Awards lists
###











#load Libraries ----------------
require(ggplot2)



#Define Data -------------------- 
awards.list <- c("CGSD2","CGSD3", "PGSD2", "PGSD3", "PDF") #this is list of awards through NSERC
nserc.all.summary <- read.table('NSERC_Grad_Gender', header = T)
#nserc.all.summary =nserc.all.summary[nserc.all.summary$award %in% c("PGSD3","CGSD3","PDF"),]

#correlation model--------------
x <- aov(nserc.all.summary$prop~nserc.all.summary$rank*nserc.all.summary$year)
summary(x)
x <- glm(nserc.all.summary$prop~nserc.all.summary$rank)




#plot functions -------------------
p <- ggplot(nserc.all.summary, aes(x = rank, y = prop, size = year)) +  
		geom_point(pch=1,stroke=1.4 )  +
		coord_cartesian(ylim = c(.50, .9)) +
		theme_bw() +
		scale_radius() +
		ylab("Proportion Male") +
		scale_x_continuous(breaks =  c(1:5), labels = awards.list) +
		#geom_abline(intercept = coef(x)[1], slope =coef(x)[2] , size = 1.1) + #from correlation model
		theme(
			axis.title.x = element_blank(),
			axis.text.x = element_text(size = 12,color="black"),
			axis.text.y = element_text(size = 12,color="black"),
			axis.title.y = element_text(size = 14),
			axis.line.x = element_line(color="black", size = 0.75),
			axis.line.y = element_line(color="black", size = 0.75),
			panel.grid.major = element_blank(),
			panel.grid.minor = element_blank(),
			panel.border = element_blank(),
			panel.background = element_blank(),
			legend.position = c(.1,0.9),
			legend.title=element_blank()

		) 

ggsave(p, file = "NSERC.png", height = 4, width = 4.5)














#	# coming back to this, but initial tests with award type had no difference in field vs award
#
#
#	#extract awards --------------------
#	awards = txt.vector[txt.awards]
#
#	#extract discipline-----------------
#	head(txt.vector[grep("^PDF", txt.vector)])
#
#	awards.list.grep = paste("^", awards.list,sep="")
#	matches <- unique (grep(paste(awards.list.grep,collapse="|"), 
#	                        txt.vector, value=TRUE))
#
#	matches.list = strsplit(matches,"  ")
#	fields = unlist(lapply(matches.list, function(x) x[length(x)]))
#	fields = gsub("^ ","",fields)
#	fields = gsub("--*.","", fields)
#	fields = gsub("Massachusetts Institute of Technology ","", fields)
#	fields = gsub("\\(.*","",fields)
#	fields = gsub("UT Southwestern Medical Ctr at Dallas ","", fields)
#	fields = gsub("University of California - Berkeley ","", fields)
#	fields = gsub("University of Massachusetts Amherst ","", fields)
#	fields = gsub("University of California Berkeley ","", fields)
#	fields = gsub("Ontario Institute of Technology ","", fields)
#
#
#	awards = unlist(lapply(matches.list, function(x) x[1]))
#
