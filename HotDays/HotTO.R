###
# Plotting heat alert data for Toronto
###




#data extracted from open Data TO
	#http://www1.toronto.ca/wps/portal/contentonly?vgnextoid=b95f614ba4353410VgnVCM10000071d60f89RCRD&vgnextchannel=74a6e03bb8d1e310VgnVCM10000071d60f89RCRD
	#http://app.toronto.ca/opendata/heat_alerts/heat_alerts_list.xml


#Information on heat warnings
	#http://www1.toronto.ca/wps/portal/contentonly?vgnextoid=923b5ce6dfb31410VgnVCM10000071d60f89RCRD

	#"Toronto's Medical Officer of Health has upgraded the Heat Warning to an Extended Heat Warning"
	#"HAU"
	#"Toronto's Medical Officer of Health has continued the Extended Heat Warning for today"
	#"EHAE"
	#"Toronto's Medical Officer of Health has continued the Heat Warning for today"
	#"HAE"
	#"Toronto's Medical Officer of Health has issued a Heat Warning"
	#"HA"


require(XML)
require(ggplot2)
require(ggthemes)
require(plyr)
library(stringr)
#load data.frames --------------
#heat alerts
xml_data <- xmlParse("http://app.toronto.ca/opendata/heat_alerts/heat_alerts_list.xml")
xml_data <- xmlToList(xml_data)

#nino
#https://www.climate.gov/news-features/blogs/enso/will-la-ni%C3%B1a-follow-el-ni%C3%B1o-what-past-tells-us
nino = read.table(file="http://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt", header=T)
temps = read.table(file="TOtemps",header=T)
#max/min temps data!


#extract HA data ---------------------
dates = as.Date(unlist(sapply(xml_data, function(x) return(x[2]))))
dates =  data.frame(dates)
dates = dates[order(dates$date),]
dates =  data.frame(dates)
dates$count = seq(1,nrow(dates),1)
dates$occur = rep(1, nrow(dates))
dates$year = format(dates$dates, "%Y")
dates$month = format(dates$dates, "%m")
dates$type = as.character(unlist(sapply(xml_data, function(x) return(x[3]))))

dates = dates[dates$type=="HA",]
hot.counts = ddply(dates, c("year", "month"), summarise, count=sum(occur))
hot.counts.years = ddply(dates, c("year"), summarise, count=sum(occur))
temp = ddply(dates, c("year","month"), summarise, count=sum(occur))
hot.counts.months = ddply(temp, c("month"), summarise, 
	mean=mean(count,na.rm=T), 
	sem=sd(count,na.rm=T)/sqrt(length(count)))

#extract el nino data ----------------
nino = nino[nino$YR>= 2001,] 
nino.year = ddply(nino, c("YR"), summarise, 
	mean=mean(ANOM,na.rm=T), 
	sem=sd(ANOM,na.rm=T)/sqrt(length(count)),
	min = min(ANOM), max = max(ANOM)
	)

nino.year$sign = nino.year$max > 0 & nino.year$min < 0
nino.year$YR1 = nino.year$YR +1

#make plots -----------------------
	#number each year
	#el nino?/Nina?
	#months?
	#I want to plot day of warning(count) for every year


df = hot.counts.years#[which(hot.counts$year == "2016"),]
#df$date = as.Date(paste(df$year, df$month, "1", sep='-'))


names(df)[1]="Year"
names(nino.year)[1] = "Year"
df = merge(df, temps, by = "Year")
df = merge(df, nino.year, by = "Year")
df.high = df[df$count>10,]
df.high$y = rep(0, nrow(df.high))
df.high$Maximum = paste(df.high$Maximum, "C", sep="")

#let's start with hot per year ----------------------
p <- ggplot(df, aes(x = as.numeric(Year), y = count)) +  
	geom_point(size = 4 )  +
	geom_line(size = 1.2) +
	geom_segment(aes(x = as.numeric(Year), y = y, 
		xend = as.numeric(Year), 
		yend = count,
		show.legend = FALSE),
		linetype = 2,
		data = df.high) +
	geom_text(data = df.high, aes(x = as.numeric(Year), y = count+1.5, label = Maximum)) +
	
	coord_cartesian(ylim = c(0, 20)) +
	scale_y_continuous(expand = c(0, 0)) +
	theme_bw() +
	ylab(str_wrap("Heat Warnings", width=15)) +
	theme(
		axis.text.x = element_blank(),
		axis.title.x = element_blank(),
		axis.title.y = element_text(size = 12),
		axis.text.x = element_text(),
		axis.line.x = element_line(color="black", size = 0.5),
		axis.line.y = element_line(color="black", size = 0.5),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.border = element_blank(),
		panel.background = element_blank()

	) 
	

p.sem <- ggplot(df, aes(x = as.numeric(Year), y = sem)) +  
	geom_point(size = 4 )  +
	geom_line(size = 1.2) +
	coord_cartesian(ylim = c(0, 1.5)) +
	scale_y_continuous(expand = c(0, 0)) +
	theme_bw() +
	ylab(str_wrap("Temperature Variation", width=30)) +
	theme(
		axis.title.x = element_blank(),
		axis.title.y = element_text(size = 12),
		axis.text.x = element_text(size = 12),
		axis.line.x = element_line(color="black", size = 0.5),
		axis.line.y = element_line(color="black", size = 0.5),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.border = element_blank(),
		panel.background = element_blank()
	) 

multiplot(p, p.sem)

tiff(file="HotTO.tiff", )
p
dev.off






cor.test(df$count, df$sem)
#
#	Pearson's product-moment correlation
#
#ata:  df$count and df$sem
# = 2.261, df = 14, p-value = 0.04021
#lternative hypothesis: true correlation is not equal to 0
#5 percent confidence interval:
#0.02888286 0.80620224
#ample estimates:
#     cor 
#.5171835 
#ttp://www.scientificamerican.com/article/study-strengthens-link-between-el-nino-and-climate-change/







