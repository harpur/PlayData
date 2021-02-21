require(XML)
require(ggplot2)
require(ggthemes)
require(plyr)
library(stringr)


ant <- read.table('clipboard',sep='\t')
ant$V5 <- as.POSIXct(ant$V2, format = "%H:%M")


x<-as.character(ant$V4)
x[x=='Honey'] = 1
x[x=='Treatment'] = 19
ant$point <- x




#let's start with hot per year ----------------------
p <- ggplot(ant, aes(x = V5, y = V3,  group = 1)) +  
		geom_line(size = 1.2) +
		geom_point(aes(colour=factor(V4), 
		fill = factor(V4)), shape=21, size = 4) + 
		scale_fill_manual(values=c("white", "black")) + 
		scale_colour_manual(values=c("black", "black")) +
		facet_wrap(~V1, ncol=4,scale='free_x') +
		scale_y_continuous(expand = c(0, 3)) +
		theme_bw() +
		 ylab("No. of Ants at Feeder")   +
		theme(
			text=element_text(size=12,  family="Helvetica", colour = 'black'),
			axis.text.x = element_text(angle = 45, hjust = 1),
			axis.title.x = element_blank(),
			axis.title.y = element_text(size = 12),
			axis.line.x = element_line(color="black", size = 0.5),
			axis.line.y = element_line(color="black", size = 0.5),
			#panel.grid.major = element_blank(),
			panel.grid.minor = element_blank(),
			panel.border = element_blank(),
			strip.background = element_blank(),
			panel.background = element_blank(),
			legend.title=element_blank()

	) 


