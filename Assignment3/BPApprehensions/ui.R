#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


T2010 <- read.csv("/Users/mac/Desktop/MA415/assignment3/bp appre 2010.csv")
T2017 <- read.csv("/Users/mac/Desktop/MA415/assignment3/bp appre 2017.csv")

rownames(T2010) <- T2010[,1]
rownames(T2017) <- T2017[,1]

# sum apprehensions of sector and month in 2010
T2010 <- subset(T2010, select = -c(Sector))
# sort month in order 
T2010 <- cbind(subset.data.frame(T2010)[,4:12],subset.data.frame(T2010)[,1:3])
monthSum2010 <- colSums((T2010))
T2010 <- rbind(T2010,monthSum2010)
rownames(T2010)[10] <- "Sector Total"

sectorSum2010 <- rowSums(T2010)
T2010 <- cbind(T2010,sectorSum2010)
colnames(T2010)[13] <- "Month Total" 

# sum apprehensions of sector and month in 2017
T2017 <- subset(T2017, select = -c(Sector))
# sort month in order
T2017 <- cbind(subset.data.frame(T2017)[,4:12],subset.data.frame(T2017)[,1:3])
monthSum2017 <- colSums((T2017))
T2017 <- rbind(T2017,monthSum2017)
rownames(T2017)[10] <- "Sector Total"

sectorSum2017 <- rowSums(T2017)
T2017 <- cbind(T2017,sectorSum2017)
colnames(T2017)[13] <- "Month Total" 

#compare apprehensions by months
sector <- rownames(T2010)
month <- colnames(T2010)

#The function takes in the index of the sector as input
plot_by_months <- function(i){ 
  year <- rbind(T2010[i,1:12],T2017[i,1:12])
  barplot(as.matrix(year), beside = T, col = c("red", "yellow"), bty="n",las=2,
          main = "Compare Apprehensions Between 2010&2017 by Months")
  legend("topright", 
         c(paste(sector[i] ,"2010"),paste(sector[i],"2017")), 
         pch=15,  
         col=c("red","yellow"),  
         bty="n"
  )
}

#The function takes in month as input
plot_by_sectors <- function(i){
  T2010<-t(T2010)
  T2017<-t(T2017)
  year <- rbind(T2010[i,1:9],T2017[i,1:9])
  getOption("scipen")
  opt <- options("scipen" = 20)
  barplot(year, beside = TRUE, col = c("red", "yellow"), bty="n",las=2,
          main = "Compare Apprehensions between 2010&2017 by Sectors"
  )
  legend("topleft", 
         c(paste(month[i] ,"2010"),
           paste(month[i],"2017")),
         pch=15,  
         col=c("red", "yellow"),  
         bty="n")
}



library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("BP Apprehensions in 2010 & 2017"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(12,
           sidebarPanel(
             selectInput("sector", "choose a region:", choices = sector))
    ),
    column(12,
           plotOutput('sector'))
  ),
  
  fluidRow(
    column(12,
           sidebarPanel(
             selectInput("month", "Choose a month:", choices = month))
    ),
    column(12,
           plotOutput('month'))
  ))
)
