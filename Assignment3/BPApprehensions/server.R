#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

bp2010 <- read.csv("/Users/eileen/Desktop/MA415/assignment3/bp appre 2010.csv")
bp2017 <- read.csv("/Users/eileen/Desktop/MA415/assignment3/bp appre 2017.csv")


#add setor sums and smonth sums in 2010
rownames(bp2010) <- bp2010[,1]
bp2010 <- subset(bp2010, select = -c(Sector))
sectorSum2010 <- rowSums(bp2010)
monthSum2010 <- colSums((bp2010))
bp2010 <- cbind(bp2010, sectorSum2010)
bp2010 <- rbind(bp2010,monthSum2010)


######## break line ########
#add setor sums and smonth sums in 2017
rownames(bp2017) <- bp2017[,1]
bp2017 <- subset(bp2017, select = -c(Sector))
sectorSum2017 <- rowSums(bp2010)
monthSum2017 <- colSums((bp2017))
bp2017 <- rbind(bp2017,monthSum2017)
bp2017 <- cbind(bp2017, sectorSum2017)


#input is the index of the sector you want to look up
graph_months <- function(i){ 
  comparison <- rbind(bp2010[i,1:12],bp2017[i,1:12])
  barplot(as.matrix(comparison), beside = T, col = c("red", "yellow"), bty="n",las=2,
          main = "Compare 2010 & 2017 Apprehensions by Months")
  legend("topright", c("Month Total 2010","Month Total 2017"), 
         pch=15,  
         col=c("red","yellow"),  
         bty="n"
  )
}


graph_sectors <- function(i){
  bp2010<-t(bp2010)
  bp2017<-t(bp2017)
  comparison <- rbind(bp2010[i,1:9],bp2017[i,1:9])
  barplot(comparison,
          beside = TRUE, 
          col = c("red", "yellow"), 
          bty="n",
          las=2,
          main = "Compare 2010 & 2017 Apprehensions by Sectors"
  )
  legend("topleft", c("Sector Total 2010","Sector Total 2017"),
         pch=15,  
         col=c("red", "yellow"),  
         bty="n")
}


library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$sector <- renderPlot({
    
    graph_months(input$sector)
    
  })
  
  output$month <- renderPlot({
    
    graph_sectors(input$month)
    
  })
  
})
