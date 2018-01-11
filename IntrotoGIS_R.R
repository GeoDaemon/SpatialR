##Intro to GIS with R
#STACEY MAPLES – GEOSPATIAL MANAGER – STANFORD GEOSPATIAL CENTER – STACEMAPLES@STANFORD.EDU
#DAVID MEDEIROS – GIS INSTRUCTION & SUPPORT SPECIALIST - STANFORD GEOSPATIAL CENTER - DAVIDMED@STANFORD.EDU
#Rewritten for R by ANA RIVERA - GIS ASSISTANT - STANFROD GEOSPATIAL CENTER - ARIVERAR@STANFORD.EDU
#This introductory session will focus upon the fundamental concepts and skills needed to begin using
#Geographic Information Systems software for the exploration and analysis of spatial data using
#R.  Topics will include:
#What is GIS?
#Spatial Data Models and Formats
#Projections and Coordinate Systems
#Basic Data Management
#The ArcMap User Interface
#Simple Analysis using Visualization.
##GIS RESOURCES:
#Stanford Geospatial Center website - http://gis.stanford.edu/
#Stanford GIS Listserv - https://mailman.stanford.edu/mailman/listinfo/stanfordgis
#DOWNLOAD TUTORIAL DATA
#1. Go to https://stanford.box.com/SGCIntroGIS and click on the drop-down arrow to the right of each folder to download individual datasets. Save the Dataset to your Desktop.
#2.	Right-click on the resulting *.zip file and select Extract All…
#3.	Accept all defaults to extract the data file.
#OPEN R STUDIO AND EXPLORE THE USER INTERFACE
#RStudio is a free and open-source integrated development environment (IDE) for R, a programming language for statistical computing and graphics.
#The first thing we want to do is OPEN R Studio and get familiar with the User Interface, divided into 4 Panes
##1. Source: Top-left,
##2. Console: Bottom-left
##3. Environment/History: Top-right
##4. Files/Plots/Packages/Help/Viewer
#Set a working directory
#Remember the dreaded “Absolute Paths” problem, endemic to GIS Softwares? To fix this issue, we set the absolute paths we changed the "Absolute Paths" to "Relative Paths" in the
#ArcMAp settings.
#In R Studio (as in life) it is a good practice to have a single folder for each project. In this case it is our IntrotoGIS_R folder
#We will set our working directory to this folder and save our script within this folder, to then use relative paths.
#setwd(INSERT WORKSHOP FOLDER PATH)
setwd("R:/IntrotoGIS_R/Data")

#Click on the Files Tab, go to More> Go to working Directory. You should see a Data Folder and an IntrotoGIS_R.R document
#EXPLORE THE FILE folder
#Using the File tab, click and expand the Data folder. Note that, while there are 23 files in this folder, there are actually only 3 Shapefiles and a dbf table here.
#This is because a Shapefile isn’t really a file but a collection of files.  You are looking at this folder in Windows Explorer in order to illustrate a very important point about many types of geographic data formats: Geographic datasets are often not easily manageable using software not specifically designed for handling GIS data.  In the case of the Shapefile, for example, if you wish to rename or move a shapefile, you must move or rename ALL of its component files in exactly the same way, or you can corrupt the shapefile.

install.packages("tidyverse")
install.packages("rgdal")
library(rgdal)
library(tidyverse)
library(sp)
library(raster) #to access metadata and CRS 

#Read WOrld Countries shapefile
World_Countries <- readOGR("R:/IntrotoGIS_R/Data", "World_Countries")
class(World_Countries)
plot(World_Countries)

#View the Coordinate System
crs(World_Countries)

#Explore World Countries attribute table
head(World_Countries@data, 3)
View(World_Countries)

#Read Cities Shapefile, explore attribute table and coordinate system
Cities <- readOGR("R:/IntrotoGIS_R/Data", "Cities")
plot(Cities, add=TRUE) #add=TRUE for layer visibility (same plot)
head(Cities@data, 3)
crs(Cities)
#This (GCS WGS 1984) is actually the coordinate system of all of the layers.

##Examining the Cities attribute table
#The most basic method of analysis in GIS is selection and sub-setting of data by attribute values.  Now that the Cities Layer is visible again, we can begin to address the fact that this layer is a bit overpopulated for our purposes.  Let’s say we are interested in visualizing the global distribution of cities with populations greater than or equal to 1 million.  First we need to see if the data necessary to do this exists in our dataset.
View(Cities)
#Scroll to the right until you can see the POP, POP_RANK and POP_CLASS Attribute Fields
#Click on the POP Field Header and select Sort Descending (Arrow Down).
#Scroll down through the Attribute table to examine the relationship between these three variables.
##Selecting by Expression
#What we would like to do is select all of the cities in this dataset that have a population of 1 million or greater.  This can be accomplished using any one of these three of these variables, but we will use the POP_RANK variable for the sake of simplicity.
#Subset Cities and save as a new Major Cities object
MajorCities <- subset(Cities, POP_RANK<=2)

#When we do the str(MajorCities) we will se that the POP_CLASS is a Factor and has 7 levels.
#We know that we only should have 2 levels (POP_RANK = 1 and POP_RANK = 2). To remove levels from a factor
#we will apply the factor() to POP_CLASS after subsetting: 
MajorCities$POP_CLASS <- factor(MajorCities$POP_CLASS)

#verify the 2 levels: levels(MajorCities$POP_CLASS)

MajorCities
View(MajorCities)
plot(MajorCities, add=TRUE, col=("green"))

#create a new shapefile for Major Cities
#getwd to save it to our curren working directory
writeOGR(MajorCities, getwd(), "MajorCities", driver="ESRI Shapefile")
#Go to your File > Data folder. You should see 4 files that make the MajorCities shapefile.


#Read the World Countries dbf and View the Table
#Install foreign package to read dbf
install.packages("foreign")
library(foreign)
World_Population07<-read.dbf("R:/IntrotoGIS_R/Data/World_Population_2007.dbf")
View(World_Population07)
str(World_Population07)

#Join table to shapefile
#Now we will turn our attention to the World_Countries Layer.
#Ultimately, we would like to visualize the layer based upon population density.
#However, the attribute table for this layer doesn’t contain data on population.
#Fortunately we have a table (World_Population2007) with the necessary population attribute.
#View the World_Population2007 table. Note the FIPS_CNTRY Attribute Field
#Now, view the World_Countries table and nothe that it also has a FIPS_CNTRY Field.
#Since this attribute exists in both of these attribute tables, and its values are identical across the two datasets, we can use this attribute as the “Key Field” for our table join.
#We want to join only does attributes that match
CountriesPop <- merge(World_Countries, World_Population07, by='FIPS_CNTRY')
View(CountriesPop)

#View the CountriesPop table and note the POP 2007 attribute (along with all other attributes from the World_Population_2007 table).
##Definition Queries
#You may have notice that many of the features in the CountriesPop table had values of -999999 for the POP2007 attributes.
#This normally indicates NODATA for the particular feature in demographic datasets.  In this case, we would like to exclude this value from our Map.
#We could use the method used to subset the Cities layer earlier in the tutorial, and create a new shapefile, like the ones we have used to create the Major Cities shapefile.
#but this time we will use another method, that does not require creating a new dataset (preventing redundancy in data storage) and does not alter the dataset being referenced, only our view of it in QGIS.
WorldPop <- CountriesPop[CountriesPop@data$POP2007 !="-99999", ]
View(WorldPop)


#LET's Map using R
#Clean your plots window.
dev.off()

#Even though the POP2007 variable is a raw counts variable, we can use divide the POP2007 variable by the area of the features to create the density value.
WorldPop$PopDen <- WorldPop$POP2007/WorldPop$SQMI


#Define Color Ramp and Classification Method
#https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf

library(RColorBrewer)
nclr <- 5 #Number of classes 
plotclr <- brewer.pal(nclr, "Reds")

library(classInt)
class <- classIntervals(WorldPop$PopDen, nclr, style = "quantile", dataPrecision = 0) #dataPrecision 0
#cols <- carto.pal(pal1 = "red.pal", n=5)
colcode <- findColours(class, plotclr)


#Symbolize Countries by Population Density
plot(WorldPop, col=colcode, border="grey", bg="lightblue1") #border = NA to omit borders. 



#Plot Major Cities 
#https://www.statmethods.net/advgraphs/parameters.html

#We have two classes of POP_RANK to work with, and would like to distinguish them from one another, visually. 


#Create a color pallete for each class 
citiesPalette <- c("grey40", "black")[MajorCities$POP_CLASS]

#Create vector with point size
citiesSize <- c(0.3, .75)[MajorCities$POP_CLASS]

citiesSymbol <- c(20, 19)[MajorCities$POP_CLASS]

#Symbolize Cities by Size
plot(MajorCities, col=citiesPalette, pch=citiesSymbol, cex=citiesSize, add=TRUE)


# Label Cities 
CityLabel <- subset(MajorCities, POP_RANK<=1)
text(CityLabel, labels=CityLabel$CITY_NAME, cex = 0.5, font=2, pos=4) #font = 2 bold

#Zoom to region (Bookmarks)
#https://cran.r-project.org/web/packages/zoom/zoom.pdf
#install.packages("zoom")
#library(zoom)


#Allows to click on two corners of a region. ESC to finish. 
#sq.zoom()

#Allows to click on what will be the center of the plot. 
#move.to.click.zoom()



##Adding Map Elements
#Legend

legend(-150, 0, legend=as.character(levels(MajorCities$POP_CLASS)),
       pch = c(20, 19),
       col = c("grey40", "black"),
       cex=.6, #Legend size
       bty = "n", #avoid border line
       title = "Urban Agglomerations\n Population")


legend(-150, -25, legend=names(attr(colcode, "table")), 
       fill=attr(colcode, "palette"),
       cex =.6,
       bg="white",
       bty = "n",
       title = "Population Density \n Persons per square mile")


#Add Scale
library(maps)
#https://cran.biodisk.org/web/packages/maps/maps.pdf
map.scale(-150,-70, relwidth = .10, metric = FALSE, ratio = FALSE, cex=0.5)


#Title/Author
#title("Urban Agglomerations")
#Adj = c(0,0) Left alignment; Default is centered text
text(120, -75, "Cartography by Ana Rivera \n January 2018", cex=.5, adj =c(0,0))



#Border 
box(which = "plot", lty="solid", lwd = 2)
box(which = "outer", lty="solid", lwd = 2) #For a neatline


#Export map
#Export as png or as eps to open the file using Adobe Illustrator



