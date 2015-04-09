require(ggplot2)
require(reshape2)
require(plyr)
require(gridExtra)

#Get all items
AllItemsURL <- 'http://download.bls.gov/pub/time.series/jt/jt.data.1.AllItems'
AllItems <- read.csv(AllItemsURL, sep = '\t', stringsAsFactors=FALSE, header=TRUE)

head(AllItems)

#Get the series
SeriesURL <- 'http://download.bls.gov/pub/time.series/jt/jt.series'
Series <- read.csv(SeriesURL, sep = '\t', stringsAsFactors=FALSE, header=TRUE)

#Get the data element descriptions
DataElementURL <- 'http://download.bls.gov/pub/time.series/jt/jt.dataelement'
DataElement <- read.csv(DataElementURL, sep = '\t', stringsAsFactors=FALSE, header=FALSE)
DataElementNames <- as.character(DataElement[1,])
colnames(DataElement) <- DataElementNames

#Get the industries
IndustryURL <- 'http://download.bls.gov/pub/time.series/jt/jt.industry'
Industry <- read.csv(IndustryURL, sep = '\t', stringsAsFactors=FALSE, header=FALSE)
IndustryNames <- as.character(Industry[1,])
colnames(Industry) <- IndustryNames

#Add dataelement_code & ratelevel_code to AllItems
AllItemsWithDataElement_Code <- merge(AllItems, Series, by='series_id')
AllItemsWithDataElement_Code <- (AllItemsWithDataElement_Code[,c('series_id', 'year', 'period', 'value', 'footnote_codes.x', 'dataelement_code', 'industry_code', 'ratelevel_code')])

#Merge the dataelement description
AllItemsWithDataElement <- merge(AllItemsWithDataElement_Code, DataElement, by='dataelement_code')
AllItemsWithDataElement <- (AllItemsWithDataElement[,c('series_id', 'year', 'period', 'value', 'footnote_codes.x', 'dataelement_text', 'industry_code', 'ratelevel_code')])
colnames(AllItemsWithDataElement) <- c('series_id', 'year', 'period', 'value', 'footnote_codes', 'dataelement', 'industry_code', 'ratelevel_code')

#Merge the industry description
AllItemsWithIndustry <- merge(AllItemsWithDataElement, Industry, by = 'industry_code')
AllItemsWithIndustry <- (AllItemsWithIndustry[,c('series_id', 'year', 'period', 'value', 'footnote_codes', 'dataelement', 'industry_text', 'ratelevel_code')])
colnames(AllItemsWithIndustry) <- c('series_id', 'year', 'period', 'value', 'footnote_codes', 'dataelement', 'industry', 'ratelevel_code')

#Strip out Month 13s
AllItemsWithIndustry <- (subset(AllItemsWithIndustry, AllItemsWithIndustry$period != 'M13'))

##We only want to look at Rates, not Levels, to compare apples to apples
##so big industries don't dwarf small industries with subtle changes
AllItemsWithIndustry <- subset(AllItemsWithIndustry, AllItemsWithIndustry$ratelevel_code == 'R')


#Add some variables to summarize hires and terms, etc
AllItemsWithIndustry$Hires <- 0.0
AllItemsWithIndustry$JobOpenings <- 0.0
AllItemsWithIndustry$LayoffsAndDischarges <- 0.0
AllItemsWithIndustry$OtherSeparations <- 0.0
AllItemsWithIndustry$Quits <- 0.0
AllItemsWithIndustry$TotalSeparations <- 0.0

AllItemsWithIndustry$industry <- as.factor(AllItemsWithIndustry$industry)
AllItemsWithIndustry$dataelement <- as.factor(AllItemsWithIndustry$dataelement)
AllItemsWithIndustry$year <- as.factor(AllItemsWithIndustry$year)
AllItemsWithIndustry$period <- as.factor(AllItemsWithIndustry$period)
AllItemsWithIndustry[AllItemsWithIndustry$dataelement == 'Hires',]$Hires <- AllItemsWithIndustry[AllItemsWithIndustry$dataelement == 'Hires',]$value
AllItemsWithIndustry[AllItemsWithIndustry$dataelement == 'Job openings',]$JobOpenings <- AllItemsWithIndustry[AllItemsWithIndustry$dataelement == 'Job openings',]$value
AllItemsWithIndustry[AllItemsWithIndustry$dataelement == 'Layoffs and discharges',]$LayoffsAndDischarges <- AllItemsWithIndustry[AllItemsWithIndustry$dataelement == 'Layoffs and discharges',]$value
AllItemsWithIndustry[AllItemsWithIndustry$dataelement == 'Other separations',]$OtherSeparations <- AllItemsWithIndustry[AllItemsWithIndustry$dataelement == 'Other separations',]$value
AllItemsWithIndustry[AllItemsWithIndustry$dataelement == 'Quits',]$Quits <- AllItemsWithIndustry[AllItemsWithIndustry$dataelement == 'Quits',]$value
AllItemsWithIndustry[AllItemsWithIndustry$dataelement == 'Total separations',]$TotalSeparations <- AllItemsWithIndustry[AllItemsWithIndustry$dataelement == 'Total separations',]$value

#Summarize the data by industry, year and month
melted <- melt(data=AllItemsWithIndustry[,c('year', 'period', 'industry', 'Hires', 'JobOpenings', 'LayoffsAndDischarges', 'OtherSeparations', 'Quits', 'TotalSeparations')], id=c('year', 'period', 'industry'))
summarized <- dcast(melted, year + period + industry ~ variable, sum)

#Remove 'Total private'
summarized <- subset(summarized, summarized$industry != 'Total private')

#Concatenate Year plus month
summarized$Month <- as.factor(paste(as.character(summarized$year), as.character(summarized$period), sep = ' '))

#Let's concentrate on Jan 2008 through Jan 2015
summarized <- subset(summarized, as.character(summarized$year) >= '2007')


openingPlot <- ggplot(data = subset(summarized, summarized$year == '2014' & summarized$period == 'M10'), aes(x = reorder(industry, JobOpenings), y = JobOpenings, color=year))
openingPlot <- openingPlot + geom_point(shape = 19, alpha = .5, size=9,  data =(subset(summarized, summarized$year == '2007' & summarized$period == 'M10')))
openingPlot <- openingPlot + geom_point(shape = 19, alpha = .5, size=9,  data =(subset(summarized, summarized$year == '2008' & summarized$period == 'M10')))
openingPlot <- openingPlot + geom_point(shape = 19, alpha = .5, size=9, data =(subset(summarized, summarized$year == '2014' & summarized$period == 'M10')))
openingPlot <- openingPlot + coord_flip()
openingPlot <- openingPlot +scale_colour_manual(values=c('lightsteelblue','blue', 'black'))
openingPlot <- openingPlot + theme(panel.background = element_rect(fill = 'white', colour = 'white'))
openingPlot <- openingPlot + ggtitle('Job openings (thousands) during October, by industry')
openingPlot <- openingPlot + theme(text = element_text(size=20), axis.title = element_text(size=8))
openingPlot <- openingPlot + xlab('') + ylab('datasource:  Bureau of Labor Statistics http://download.bls.gov/pub/time.series/jt/')
openingPlot

