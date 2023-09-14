#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#source("http://news.mrdwab.com/install_github.R")
#install_github("mrdwab/koboloadeR")
library(koboloadeR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(chron)
library(DT)

neg<-function(x){-1*x}
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Asking Better Questions, Edited For Deployment Testing, With Yet More Edits"),
   
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("v",
                     "Version",choices = c("Rating","Ranking")
      )),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
         tabPanel("Results",plotOutput("Plot1"),dataTableOutput("Data1")),
         tabPanel("Meta-Results",plotOutput("Plot2"),tableOutput("Tab1"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  dataset1<-kobo_data_downloader("206489",user="test_s4sd:test_s4sd")
  dataset2<-kobo_data_downloader("206490",user="test_s4sd:test_s4sd")
  colnames(dataset2)<-gsub("grp1/","",colnames(dataset2))
  colnames(dataset1)<-gsub("grp1/","",colnames(dataset1))
  
  head(dataset1)
  dataset1<-subset(dataset1,as.numeric(substr(dataset1$start,9,10))>15)
  dataset2<-subset(dataset2,as.numeric(substr(dataset2$start,9,10))>15)
  
  
  if(nrow(dataset1)>3&nrow(dataset2)>3){
  
   output$Plot1 <- renderPlot({
     if(input$v=="Rating"){
       
      
      p1<- dataset2 %>% group_by(`_uuid`) %>% gather(key = "Attribute",value="Score",Variety:Speed) %>% ungroup() %>%
         mutate(Score=ifelse(Score=="n/a",NA,as.numeric(Score)),Attribute2=reorder(Attribute,Score,mean,na.rm=T)) %>%
         ggplot(aes(y=Score,x=Attribute2,fill=Attribute))+geom_violin(scale = "width")+
         scale_y_continuous(breaks=1:5,labels=c("Not Important",2:4,"Very Important"))+ylab("")+ggtitle("Results of Rating Assessment")+
        xlab("")+theme(axis.text = element_text(size=12))
     }
 if(input$v=="Ranking"){
       
      
   
   p1<-  dataset1 %>% group_by(`_uuid`)%>% gather(key = "Attribute",value="Rank",Variety:Speed) %>% ungroup() %>%
     mutate(Rank=ifelse(Rank=="n/a",NA,as.numeric(Rank)),Rank2=-1*Rank+rnorm(mean = 0,sd=0.1,n=n())) %>%
     mutate(Attribute2=reorder(Attribute,Rank2,mean,na.rm=T)) %>%
        ggplot(aes(y=-1*Rank,x=Attribute2,fill=Attribute))+geom_violin(scale = "width")+
     scale_y_continuous(breaks=-1:-9,labels=1:9,limits=c(-9,-1))+
    ylab("")+ggtitle("Results of Ranking Assessment")+
     xlab("")+theme(axis.text = element_text(size=12))
     }
   p1
   })
   

   
     output$Data1 <- renderDataTable({

       v1.1<- dataset1 %>% group_by(`_uuid`)%>% gather(key = "Attribute",value="Rank",Variety:Speed) %>%
         mutate(Rank=ifelse(Rank=="n/a",NA,as.numeric(Rank)),Attribute2=reorder(Attribute,-1*Rank,mean,na.rm=T)) %>%
         group_by(Attribute) %>%
         summarise(AverageRank=mean(Rank,na.rm=TRUE))%>%
         mutate(RankedRank=rank(AverageRank))
       
       v2.1<-  dataset2 %>% group_by(`_uuid`) %>% gather(key = "Attribute",value="Score",Variety:Speed) %>%
         mutate(Score=ifelse(Score=="n/a",NA,as.numeric(Score)),Attribute=reorder(Attribute,Score,mean,na.rm=T)) %>%
         group_by(Attribute) %>%
         summarise(AverageScore=mean(Score,na.rm=TRUE)) %>%
         mutate(RankedScore=rank(AverageScore*-1))%>%
         full_join(v1.1)
       colnames(v2.1)<-c("Attribute","Mean Rating","Ranked Rating","Mean Rank","Ranked Rank")
       as.data.frame(v2.1)
     })
     
     
     dataset1$starttime<-times(substr(dataset1$start,12,19))
     dataset1$endtime<-times(substr(dataset1$end,12,19))
     dataset1$duration<-as.numeric(dataset1$endtime-dataset1$starttime)*60*60*24
     
     dataset2$starttime<-times(substr(dataset2$start,12,19))
     dataset2$endtime<-times(substr(dataset2$end,12,19))
     dataset2$duration<-as.numeric(dataset2$endtime-dataset2$starttime)*60*60*24
     
     dataset1$Assessment<-"Ranking"
     dataset2$Assessment<-"Rating"
     
     combo<-subset(merge(dataset1[,c("Assessment","duration")],dataset2[,c("Assessment","duration")],all=TRUE),is.na(duration)==FALSE)
     
     output$Plot2<-renderPlot({
       ggplot(data=combo,aes(y=duration,x=Assessment))+geom_violin()+xlab("Assessment")+ylab("Time taken to complete assessment (seconds)")+
         theme(axis.text = element_text(size=12),axis.title = element_text(size=14))
     })
     
     output$Tab1<-renderTable({
       combo %>% group_by(Assessment) %>% summarise(n=n(),AverageTime=mean(duration,na.rm=T),MinTime=min(duration,na.rm=T),MaxTime=max(duration,na.rm=T))
     })
  }
  else{
    stop("Not enough data. Give me more data")
  }
}

# Run the application 
shinyApp(ui = ui, server = server)

