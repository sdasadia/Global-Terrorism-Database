library(ggmap)
library(maptools)
library(maps)
library(ggplot2)






# Read the file
data <- read.csv("globalterrorismdb_0616dist.csv")

# Remove incidents where there is doubt as to whether the incident is an act of terrorism.
data1 <- data[data$doubtterr == 0,]

# Cleaning data file:

data1 <- subset(data, select = -c(iday,approxdate,resolution,country,region,specificity,vicinity,location,summary,
                                  alternative,alternative_txt,attacktype1,attacktype2,attacktype3,attacktype2_txt,
                                  attacktype3_txt,targtype1,targsubtype1,target1,natlty1,targtype2,targtype2_txt,targsubtype2,
                                  targsubtype2_txt,corp2,target2,natlty2,natlty2_txt,targtype3,targtype3_txt,targsubtype3,
                                  corp3,target3,natlty3,natlty3_txt,gsubname,gname2,gsubname2,gname3,ingroup2,ingroup3,gsubname3,
                                  motive,guncertain2,guncertain3,claimed,claimmode,claimmode_txt,claimmode2,claimmode2_txt,
                                  claimmode3,claimmode3_txt,compclaim,weaptype1,weapsubtype1,weaptype2,weapsubtype2,weaptype2_txt,
                                  weaptype3,weapsubtype3,weaptype3_txt,weapsubtype2_txt,weapsubtype3_txt,weaptype4,weapsubtype4,weaptype4_txt,
                                  weapsubtype4_txt,provstate, doubtterr,targsubtype1,targsubtype3_txt,claim2,claim3,targsubtype1_txt,
                                  corp1,ingroup,guncertain1,weapsubtype1_txt,weapdetail,propcomment,propextent,ransomnote,addnotes,
                                  scite2,scite1,scite3,dbsource,INT_LOG,INT_IDEO,INT_MISC,INT_ANY,related))

# remove those attach where nkil is unknown - specific for this study
data1 <- data1[complete.cases(data1$nkill),]

# Manage NA in various features
is.na(data1$nperps) = data1$nperps < 0
is.na(data1$nperpcap) = data1$nperpcap < 0
is.na(data1$property) = data1$property < 0
is.na(data1$propvalue) = data1$propvalue < 0
is.na(data1$ishostkid) = data1$ishostkid < 0
is.na(data1$nhostkid) = data1$nhostkid < 0
is.na(data1$nhostkidus) = data1$nhostkidus < 0
is.na(data1$nhours) = data1$nhours < 0
is.na(data1$ndays) = data1$ndays < 0
is.na(data1$ransom) = data1$ransom < 0
is.na(data1$ransomamt) = data1$ransomamt < 0
is.na(data1$ransomamtus) = data1$ransomamtus < 0
is.na(data1$ransompaid) = data1$ransompaid < 0
is.na(data1$ransompaidus) = data1$ransompaidus < 0
is.na(data1$nreleased) = data1$nreleased < 0

# Add a frequency of attack column
a <- table(data1$country_txt)
a <- data.frame(a)
colnames(a) <- c("country_txt","freq_attack")
data3 <-merge(data1,a,by = "country_txt")

# Globely
# 1 Type of attacks histogram by year
# 2 Most deadly attacks by type
# 3 Most damaging attacks by type
# 4 Regions where most attack happened (map plot) 
# 5 Most target attacks
# 6 No of attach by restion and year one chart


# Couinty wise:
# 1 Type of attacks histogram by year
# 2 Most deadly attacks by type
# 3 Most damaging attacks by type
# 4 Regions where most attack happened (map plot) 
# 5 Most target attacks

# 2D World Plot

wplot <- subset(data3,select = c(latitude,longitude,nkill,freq_attack))
wplot <- wplot[complete.cases(wplot),]

heat <- log(wplot$nkill * wplot$freq_attack)

mp <- NULL
mapWorld <- borders("world", colour="grey50", fill="grey50", regions = -antarctica) # create a layer of borders
mp <- ggplot() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank(),axis.title.x=element_blank(),
                       axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                       axis.title.y=element_blank(),axis.text.y=element_blank(),
                       axis.ticks.y=element_blank()) + scale_color_gradient(low = "red", "High", "Low") + mapWorld
mp <- mp + geom_point(aes(x=wplot$longitude, y=wplot$latitude, colour = heat), 
                     size=1.0, alpha = 1) 
print(mp)





# Google GlobeGL Visulization
terror <- subset(data1, select = c(latitude,longitude,nkill))
colnames(terror) <- c("Latitude","Longitude","Population")
terror <- terror[complete.cases(terror),]
terror <- terror[terror$Population > 10,]
terror$Population <- terror$Population / max(terror$Population)
saveRDS(terror,"terror.rds")