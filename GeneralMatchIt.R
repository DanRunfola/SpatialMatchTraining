#Library that lets you load shapefiles
library(maptools)

#Library that lets you manipulate Shapefiles
library(sp)

#Library that lets you handle projections
library(rgdal)

#Library that handles matching
library(MatchIt)

#Library that helps with data manipulation
library(reshape2)

#Define the projection ("Coordinate Reference System")
proj.def <- CRS("+proj=longlat +datum=WGS84")

#The location of the DHS Shapefile Boundary with the Buffers
DHS.shpfile.location <- "/home/aiddata/Desktop/Github/SpatialMatchTraining/Analysis_Data/all_buffers_merged.shp" 

#Load in the buffers, and project them.  You always have to manually project in R.
DHS.locs <- readShapePoly(DHS.shpfile.location,proj4string=proj.def)

#The path to the boundary of liberia for visualizaitons.
Lib.bound.loc <- "/home/aiddata/Desktop/Github/SpatialMatchTraining/LiberiaBoundaries/LBR_adm2.shp"

#Load & project                
Lib.bound <- readShapePoly(Lib.bound.loc,proj4string=proj.def)

#Where the CSV with ancillary data is located (Seth's exports)
anc.data.loc <- "/home/aiddata/Desktop/Github/SpatialMatchTraining/Analysis_Data/liberia_data.csv"

#Load a CSV.  You can type View(anc.dta) to see what loaded (note capital V)
anc.dta <- read.csv(anc.data.loc)

#You can also type names(anc.dta) to see the column names, or head(anc.dta)
#to see a quick summary

#I'm merging the ancillary data into the shapefile.
#Note the @data - that tells R I'm looking at the Data of the shapefile.
#You can also type names(DHS.locs@data), head, etc. to see the data.
DHS.locs@data <- merge(DHS.locs@data, anc.dta, by="DHSID")

#A quick example of how to make a helpful visualization

#First, subset the data so it only contains one year:
DHS_2013 <- DHS.locs[DHS.locs@data$DHSYEAR.x == 2013,]

#Now, create a quick map.
saved_DHS_2013_map <- spplot(DHS_2013["sslp_e"], main="2013", sp.layout=list(Lib.bound))

#Plot it by simply calling the name you saved it as:
saved_DHS_2013_map

#Note there is a point WAY outside of Liberia.  
#You can quickly remove all points
#That don't fall inside Liberia by subsetting the data
#Spatially:
DHS_2013.subset <- DHS_2013[Lib.bound,]

#Now, plot the map just like before, but with the subset:
DHS_2013_map.subset <- spplot(DHS_2013.subset["sslp_e"], main="2013", sp.layout=list(Lib.bound))

#And, remember to call your new map:
DHS_2013_map.subset 

#Now, for a very quick example analysis.
#To do this, we'll need to construct some data.
#We also want to make sure we're doing the analysis on the subset
#So we don't get the erroneous point included

#First, let's consider the data we have:
names(DHS_2013.subset)

#Because we aren't using a panel model (rather, we're cross-sectional)
#We need to summarize this data.
#I.e., the average monthly temperature for a single point in time
#doesn't do us much good (at41_200411e would be the mean (e) temperature (at)
#in Novermber of 2004)

#Luckily for you, we already have functions to do this!
#They are included here, but you may want to skip over this part
#as it's a bit more advanced.
#If you want to check it out, look at the file "SciClone_functions.R".
#--------------------------------------------------
source("SciClone_functions.R")

#First, let's make a copy of our data to save any permutations into.
analysis_DTA <- DHS_2013.subset

#There are two types of aggregations we have tools for - trends and averages
#First we'll calculate an average:
#To do this you need the data you're creating the summaries from 
#(DHS_2013.subset in this case), the prefix of the temporal data
#you want to summarize (here, we using pc41_, which is precip;
#to see all names, remember the names(analysis_DTA@data) command).
#You then need the affix - in this case we're using "e", which stands for mean
#(i.e., we're summarizing the mean monthly precip).
#The last three are the start year, end year, and type of dates.
#Here we're calculating 1992 to 2005, with "year month" dates.
#When run, this saves the new variable into "precip_average".
analysis_DTA@data$precip_preAverage <- timeRangeAvg(DHS_2013.subset, "pc41_","e",1992, 2005, "ym")
#View what we calculated:
analysis_DTA@data$precip_preAverage 
hist(analysis_DTA@data$precip_preAverage)
analysis_DTA@data$precip_postAverage <- timeRangeAvg(DHS_2013.subset, "pc41_","e",2005, 2014, "ym")

#here, we calculate the linear trend over a given time period.
#This is a little trickier.
#Just like the average, the first command is the data.
#The second is the search command for the data you want to calculate over.
#In this example, names(DHS_2013.subset) shows us the format of precipitation is
#pc41_201410e".  All we do is replace the numeric values (years and months)
#wiht [0-9].  If you had only years, you would just use [0-9] four times.
#The next arguments are start year, end year, unique ID, and finally the
#type of data (yearly or year-month)
analysis_DTA$precip_preTrend <-timeRangeTrend(DHS_2013.subset,"pc41_[0-9][0-9][0-9][0-9][0-9][0-9]e",1992,2005,"DHSID", "ym")
analysis_DTA@data$precip_preTrend
hist(analysis_DTA@data$precip_preTrend)

#Here's an example with a different variable and range (nighttime lights):
#Note for this variable we only have yearly data.
analysis_DTA@data$NTL_preAverage <- timeRangeAvg(DHS_2013.subset, "ncc4_","e",1992, 2005, "y")
analysis_DTA@data$NTL_preTrend <- timeRangeTrend(DHS_2013.subset,"ncc4_[0-9][0-9][0-9][0-9]e",1992,2005,"DHSID", "y")
analysis_DTA@data$NTL_postAverage <- timeRangeAvg(DHS_2013.subset, "ncc4_","e",2005, 2012, "y")
#Now we have some example data we can produce an example model with.
#First, we need to load in our treatment indicators.
treatment_data <- read.csv("/home/aiddata/Desktop/Github/SpatialMatchTraining/Analysis_Data/merged_final_treatments.csv")

#Merge the data
analysis.trt.data <- merge(analysis_DTA, treatment_data, by="DHSID")

#For this analysis, let's only look at Rural areas to start.
analysis.trt.data <- analysis.trt.data[analysis.trt.data@data$URBAN_RURA == "U",]

#A simple definition of treatment - overlap greater than the median:
#set all units to 0:
analysis.trt.data$Treatment <- 0

#Calculate the Median Overlap
med_overlap <- median(analysis.trt.data@data$percent_overlap_dissolved)

#Set units with overlap >med_overlap to 1:
analysis.trt.data$Treatment <- (as.numeric(analysis.trt.data@data$percent_overlap_dissolved) > med_overlap)

#Double check:
summary(analysis.trt.data$Treatment)

#Check how many units of observation this leaves us with (our initial n for the
#analysis).
length(analysis.trt.data)

#First, we want to create a paired dataset where
#we match treatment and control units
#on relevant covariates.
#We're going to just use the stock MatchIt functionality for now.

#Make a list of all the variables you're using in the analysis, 
#we'll feed this in to MatchIt later.
aVars <- c("Treatment", "tc00_e","dari_e","droa_e","selv_e","sslp_e","am50_e","gpw3_2000e","NTL_preTrend","NTL_preAverage","precip_preTrend","precip_preAverage", "NTL_postAverage","precip_postAverage")

#MatchIt can only use "complete cases" - 
#i.e., you must have measurements for all of your
#covariates and observations.
#This line removes cases that are not complete.
analysis.trt.data <- analysis.trt.data[complete.cases(analysis.trt.data@data[aVars]),]

#Configure matchit
#This is the first stage of a propensity score model
matchit.results <- matchit(Treatment ~ tc00_e + dari_e + droa_e + selv_e + sslp_e + am50_e + gpw3_2000e + NTL_preTrend + NTL_preAverage + precip_preTrend + precip_preAverage, 
                           data=analysis.trt.data@data[aVars],
                                 method="nearest", distance="logit", 
                                 caliper=0.5)

#Examine our balance
summary(matchit.results)

#Subset our data to only include matches
modelData <- match.data(matchit.results)

#Let's construct a very simple outcome measure - the post-average of nighttime lights compared
#to the pre-average of nighttime lights.
modelData$outcome <- modelData$NTL_postAverage - modelData$NTL_preAverage

#Fit the model using out matched data from the matchit procedure
linearModel <- lm(outcome ~ Treatment + tc00_e + dari_e + droa_e + selv_e + sslp_e + am50_e + gpw3_2000e + NTL_preTrend + NTL_preAverage + precip_preTrend + precip_preAverage, 
                                 data=modelData)