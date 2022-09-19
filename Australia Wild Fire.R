
#installing necessary packages
  install.packages(c("maps", "mapdata"))                #package for plotting cities
  install.packages("oz")                                #package for australia map 
  install.packages('ozmaps')                            #package for australia map
  install.packages("scales")                            #package for setting transparency values of points
  
  
  library('ozmaps')
  library(sp)
  library(scales)
  library( maps )
  library( mapdata ) 
  library(oz)

  #setting working directory
  setwd("C:\\Users\\91941\\OneDrive\\Documents\\Uni\\Courses\\Visualization\\Assignment2")
  
  #reading M6 fires data file in fia dataframe
  fia <- read.csv("fire_nrt_M6_96062.csv", sep=",")

  #ensure that the formatting of date is 'correct'
  fia$acq_date <- as.Date(fia$acq_date, format = "%d/%m/%Y")
  
  #take a look at data
  summary(fia)
  head(fia)
  
  #taking fires sample from Terra satellite with confidence level >75
  fia <- fia[fia$satellite=='Terra',]
  fia <- fia[fia$confidence>75,]

  #opening a new dev window
  dev.new(width=15, height=15, unit='px')
  
  # Set the layout
  nf <- layout(
    matrix(c(1,1,2,3), ncol=2), 
    widths=c(3,1), 
    heights=c(2,2)
  )
  
  #show layout
  layout.show(nf)
  
  #setting margins
  par(mar=c(1,4,1,0.25))
  
  #plotting australia map with grey color 
  ozmap("abs_ste", col='grey', bg="#99CCFF")
  
  
  #filtering hotspots with brightness levels and creating a new column colour
  fia$colour="#FF8000"
  fia$colour[fia$brightness>=400] = "yellow"
  fia$colour[fia$brightness<=345]='#990000'
  
  #plotting fire hotspots on map
  points(fia$longitude, fia$latitude, pch=17, col=fia$colour)  
  map.text("world", "Australia", add=TRUE, exact=TRUE, cex= 1.8, font=9)
  #adding cities with min population 500000
  map.cities(country="Australia", minpop = 500000, pch=16, label=TRUE, font=3, adj=0.08, cex= 1.2, col="black")
  
  #adding title and subtitles
  text(108, -5, "Wildfires in Australia", col="#990000", cex=2.0, font= )
  text(116, -7, "NASA Fire Information for Resource Management System (FIRMS) satellite data",cex= 1.0, col="black") 
  text(107, -8, "between 05-12-2019 and 05-01-2020", cex= 1.0, col="black")
  text(155, -48, "The hotspots shown are for confidence level >75 captured by TERRA satellite", cex=0.8)
  
  #adding legend
  #legend(95,-38, title ="Temperature of fire pixels", 
  #       legend=c("Brightness <= 345", "Brightness 345-400", "Brightness >= 400"),
   #      col=c("#990000", "#FF8000", "yellow"), border="black", cex=0.9, pch=17, bty="n", pt.cex= 1.0)
  
  legend(98,-36,  
                legend=c("Brightness temperature <= 345", 
                         "Brightness temperature 345-400", 
                         "Brightness temperature >= 400"),
               col=c("#990000", "#FF8000", "yellow"), cex=0.9, pch=17, bty="n",pt.cex= 1.0)
         
  #adding bounding box for most affected area
  rect(145,-38,155,-27, border = "#FF33FF", lwd=2.0)
  text(150, -38.5, "Most affected area" , col="#FF33FF", cex= 0.7)  
  
  #adding annotations and segments 
  segments(145, -15, 155, -15,
           col = "darkgrey" )
  text(158,-15,"Detected fire", col="yellow")
  segments(155, -27, 175, -18,
           col = "#FF33FF", lty="dashed", lwd=2.0)
  segments(155,-38,175,-40, col="#FF33FF", lty="dashed", lwd=2.0)
  
  
  #-----------------------------#2nd---------------
  
  #serting margins for 2nd layout
  par(mar=c(1,0,1,1))
  
  #setting colour column based on daynight attribute
  fia$colour="#063852"
  fia$colour[fia$daynight =='D'] = "#FF9933"
  par(mar=c(1,0,1,1))
  
  #plotting map for most affected area with xlim and ylim for latitude and longitude
  ozmap("abs_ste", xlim=c(145,155), ylim=c(-38,-27) , col='grey', bg="#99CCFF")
  
  #plotting points
  points(fia$longitude, fia$latitude, pch=16, col=alpha(fia$colour,0.8), cex=1.0) 
  map.cities(country="Australia", minpop = 500000, label=TRUE, font=3, adj=0.23, cex= 1.0, col="black")
  legend(151,-35, legend=c("Day", "Night"),
         col=c("#FF9933", "#063852"), cex=0.9, pch=16, bty="n", pt.cex= 1.0)
  
  
  #-------------------------------3 rd------------------
  #setting margin for 3rd layout
  #FFCC99
  par(mar=c(1,0,1,1))
  #plotting map
  ozmap("abs_ste", xlim=c(145,155), ylim=c(-38,-27), col='grey', bg="#99CCFF")
  par(bg='#99CCFF')
  
  #filtering hotspots based on acq_date in weekly groups 
  fia$group_dates[fia$acq_date>="2019-12-05" & fia$acq_date<"2019-12-12"]="#FF0000"    #red
  fia$group_dates[fia$acq_date>="2019-12-12" & fia$acq_date<"2019-12-19"]="#FFFF00"    #yellow
  fia$group_dates[fia$acq_date>="2019-12-19" & fia$acq_date<"2019-12-26"]="#00FF80"    #green
  fia$group_dates[fia$acq_date>="2019-12-26" & fia$acq_date<="2019-12-31"]="#3399FF"   #blue
  fia$group_dates[fia$acq_date>="2020-01-01" & fia$acq_date<="2020-01-05"]="#9933FF"   #purple 
  
  #plotting points
  points(fia$longitude, fia$latitude, pch=16, col=fia$group_dates, cex=1.0) 
  map.cities(country="Australia", minpop = 500000, label=TRUE, font=3, adj=0.23, cex= 1.0, col="black")
  legend(151,-34, legend=c("05Dec-12Dec", "12Dec-19Dec",
                                 "19Dec-26Dec","26Dec-31Dec",
                                 "01Jan-05Jan"),
         col=c("#FF0000", "#FFFF00", "#00FF80", "#3399FF", "#9933FF"), 
         cex=0.8, pch=16, bty="n", pt.cex= 1.0 )
  
  
  dev.off()
  
  