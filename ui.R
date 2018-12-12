library(googleVis)
library(stringr)
library(rgdal)
library(leaflet)
library(htmltools)
library(shiny)
library(dplyr)
library(plotly)
library(ggthemes)


data=read.csv("results1.csv",header = TRUE)
data$Population=as.numeric(data$Population)
data$Age.in.Years=as.numeric(data$Age.in.Years)
data$Year=as.numeric(data$Year)
data=na.omit(data)
data=arrange(data,Year)

subtable=subset(data,Year=="2010")
Table6=with(subtable,aggregate(Population,by=list(Race),sum))
colnames(Table6)=c("Race","Population")
Table6$Year="2010"


subtable1=subset(data,Year=="2011")
Table5=with(subtable1,aggregate(Population,by=list(Race),sum))
colnames(Table5)=c("Race","Population")
Table5$Year="2011"


subtable2=subset(data,Year=="2012")
Table4=with(subtable2,aggregate(Population,by=list(Race),sum))
colnames(Table4)=c("Race","Population")
Table4$Year="2012"


subtable3=subset(data,Year=="2013")
Table3=with(subtable3,aggregate(Population,by=list(Race),sum))
colnames(Table3)=c("Race","Population")
Table3$Year="2013"


subtable4=subset(data,Year=="2014")
Table2=with(subtable4,aggregate(Population,by=list(Race),sum))
colnames(Table2)=c("Race","Population")
Table2$Year="2014"

subtable5=subset(data,Year=="2015")
Table1=with(subtable5,aggregate(Population,by=list(Race),sum))
colnames(Table1)=c("Race","Population")
Table1$Year="2015"


data1=rbind(Table1,Table2,Table3,Table4,Table5,Table6)
data1$Year=as.numeric(data1$Year)


suicide2015<- read.csv("2015suicide.csv",header = T,sep = ",")
suicide2015$Population<- as.numeric(suicide2015$Population)
rows=suicide2015$Race=="All Races" 
subtable=suicide2015[rows,]
rows=subtable$Sex%in%c("Males","Females")
subtable1=subtable[rows,]
countTable3=with(subtable1,aggregate(Population,by=list(Sex),sum))
colnames(countTable3)=c("Sex","Pop")
countTable3$Year="2015"
countTable3a=with(subtable,aggregate(Population,by=list(State),sum))
colnames(countTable3a)=c("State","Pop")
countTable3a=countTable3a[-1,]
countTable3b=with(subtable,aggregate(Population,by=list(Age.Group),sum))
colnames(countTable3b)=c("Age","2015")

suicide2014<- read.csv("2014suicide.csv",header = T,sep = ",")
suicide2014$Population<- as.numeric(suicide2014$Population)
rows=suicide2014$Race=="All Races" 
subtable=suicide2014[rows,]
rows=subtable$Sex%in%c("Males","Females")
subtable1=subtable[rows,]
countTable6=with(subtable1,aggregate(Population,by=list(Sex),sum))
colnames(countTable6)=c("Sex","Pop")
countTable6$Year="2014"
countTable6a=with(subtable,aggregate(Population,by=list(State),sum))
colnames(countTable6a)=c("State","Pop")
countTable6a=countTable6a[-1,]
countTable6b=with(subtable,aggregate(Population,by=list(Age.Group),sum))
colnames(countTable6b)=c("Age","2014")

suicide2013<- read.csv("2013suicide.csv",header = T,sep = ",")
suicide2013$Population<- as.numeric(suicide2013$Population)
rows=suicide2013$Race=="All Races" 
subtable=suicide2013[rows,]
rows=subtable$Sex%in%c("Males","Females")
subtable1=subtable[rows,]
countTable2=with(subtable1,aggregate(Population,by=list(Sex),sum))
colnames(countTable2)=c("Sex","Pop")
countTable2$Year="2013"
countTable2a=with(subtable,aggregate(Population,by=list(State),sum))
colnames(countTable2a)=c("State","Pop")
countTable2a=countTable2a[-1,]
countTable2b=with(subtable,aggregate(Population,by=list(Age.Group),sum))
colnames(countTable2b)=c("Age","2013")



suicide2012<- read.csv("2012suicide.csv",header = T,sep = ",")
suicide2012$Population<- as.numeric(suicide2012$Population)
rows=suicide2012$Race=="All Races" 
subtable=suicide2012[rows,]
rows=subtable$Sex%in%c("Males","Females")
subtable1=subtable[rows,]
countTable4=with(subtable1,aggregate(Population,by=list(Sex),sum))
colnames(countTable4)=c("Sex","Pop")
countTable4$Year="2012"
countTable4a=with(subtable,aggregate(Population,by=list(State),sum))
colnames(countTable4a)=c("State","Pop")
countTable4a=countTable4a[-1,]
countTable4b=with(subtable,aggregate(Population,by=list(Age.Group),sum))
colnames(countTable4b)=c("Age","2012")

suicide2011<- read.csv("2011suicide.csv",header = T,sep = ",")
suicide2011$Population<- as.numeric(suicide2011$Population)
rows=suicide2011$Race=="All Races" 
subtable=suicide2011[rows,]
rows=subtable$Sex%in%c("Males","Females")
subtable1=subtable[rows,]
countTable1=with(subtable1,aggregate(Population,by=list(Sex),sum))
colnames(countTable1)=c("Sex","Pop")
countTable1$Year="2011"
countTable1a=with(subtable,aggregate(Population,by=list(State),sum))
colnames(countTable1a)=c("State","Pop")
countTable1a=countTable1a[-1,]
countTable1b=with(subtable,aggregate(Population,by=list(Age.Group),sum))
colnames(countTable1b)=c("Age","2011")


suicide2010<- read.csv("suicide2010.csv",header = T,sep = ",")
suicide2010$Population<- as.numeric(suicide2010$Population)
rows=suicide2010$Race=="All Races" 
subtable=suicide2010[rows,]
rows=subtable$Sex%in%c("Males","Females")
subtable1=subtable[rows,]
countTable5=with(subtable1,aggregate(Population,by=list(Sex),sum))
colnames(countTable5)=c("Sex","Pop")
countTable5$Year="2010"
countTable5a=with(subtable,aggregate(Population,by=list(State),sum))
colnames(countTable5a)=c("State","Pop")
countTable5a=countTable5a[-1,]
countTable5b=with(subtable,aggregate(Population,by=list(Age.Group),sum))
colnames(countTable5b)=c("Age","2010")

sextotal=rbind(countTable5,countTable1,countTable4,countTable2,countTable6,countTable3)
sextotal$Year=as.numeric(sextotal$Year)
colnames(sextotal)=c("Sex","Population","Year")

state = readOGR("C:/Users/87520/Documents/data/myshinyapp/cb_2017_us_state_500k","cb_2017_us_state_500k")
state1=subset(state,is.element(state$NAME,countTable5a$State))
state2=subset(state,is.element(state$NAME,countTable4a$State))
state3=subset(state,is.element(state$NAME,countTable3a$State))
state4=subset(state,is.element(state$NAME,countTable6a$State))
state5=subset(state,is.element(state$NAME,countTable1a$State))
state6=subset(state,is.element(state$NAME,countTable2a$State))
countTable5a=countTable5a[order(match(countTable5a$State,state1$NAME)),]
countTable4a=countTable4a[order(match(countTable4a$State,state2$NAME)),]
countTable3a=countTable3a[order(match(countTable3a$State,state3$NAME)),]
countTable6a=countTable6a[order(match(countTable6a$State,state4$NAME)),]
countTable1a=countTable1a[order(match(countTable1a$State,state5$NAME)),]
countTable2a=countTable2a[order(match(countTable2a$State,state6$NAME)),]

bins <- c(40000, 50000, 60000,70000,80000,90000,100000,Inf)
pal1 <- colorBin("Blues", domain = countTable5a$Pop, bins = bins)
pal2 <- colorBin("Blues", domain = countTable4a$Pop, bins = bins)
pal3 <- colorBin("Blues", domain = countTable3a$Pop, bins = bins)
pal4 <- colorBin("Blues", domain = countTable6a$Pop, bins = bins)
pal5 <- colorBin("Blues", domain = countTable1a$Pop, bins = bins)
pal6 <- colorBin("Blues", domain = countTable2a$Pop, bins = bins)

df=data.frame(Year=c("2010","2011","2012","2013","2014","2015"),"Female"=c(1217965,1220115,1225361,1226368,1226213,1220943),"Male"=c(1198279,1201913,1202417,1201185,1198060,1195524))
df2=data.frame(countTable5b,countTable1b$`2011`,countTable4b$`2012`,countTable2b$`2013`,countTable6b$`2014`,countTable3b$`2015`)
colnames(df2)=c("Age","2010","2011","2012","2013","2014","2015")
df2=df2[-20,]
df2=df2[-19,]
df4=rbind(countTable5b$`2010`,countTable1b$`2011`,countTable4b$`2012`,countTable2b$`2013`,countTable6b$`2014`,countTable3b$`2015`)
colnames(df4)=countTable6b$Age
rownames(df4)=c("2010","2011","2012","2013","2014","2015")
df4=subset(df4,select = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))


CauseOfDeath2012<- read.csv("CauseOfDeath2012.csv",header = T, sep = ",")
CauseOfDeath2013<- read.csv("CauseOfDeath2013.csv",header = T, sep = ",")
CauseOfDeath2014<- read.csv("CauseOfDeath2014.csv",header = T, sep = ",")
CauseOfDeath2015<- read.csv("CauseOfDeath2015.csv",header = T, sep = ",")


Numberize1<- function(inputVector)
{
  inputVector<-str_replace_all(inputVector,",","")
  return(as.numeric(inputVector))
  Numberize(testFrame$X)
}
CauseOfDeath2012$crudeNumber <- Numberize1(CauseOfDeath2012$crudeNumber)
CauseOfDeath2013$crudeNumber <- Numberize1(CauseOfDeath2013$crudeNumber)
CauseOfDeath2014$crudeNumber <- Numberize1(CauseOfDeath2014$crudeNumber)
CauseOfDeath2015$crudeNumber <- Numberize1(CauseOfDeath2015$crudeNumber)

cause1=with(CauseOfDeath2012,aggregate(crudeNumber,by=list(intentName),sum))
colnames(cause1)=c("State","Pop")
cause1$Year="2012"
cause2=with(CauseOfDeath2013,aggregate(crudeNumber,by=list(intentName),sum))
colnames(cause2)=c("State","Pop")
cause2$Year="2013"
cause3=with(CauseOfDeath2014,aggregate(crudeNumber,by=list(intentName),sum))
colnames(cause3)=c("State","Pop")
cause3$Year="2014"
cause4=with(CauseOfDeath2015,aggregate(crudeNumber,by=list(intentName),sum))
colnames(cause4)=c("State","Pop")
cause4$Year="2015"


shinyUI(navbarPage("Data Visualization",
                   tabPanel("Interactive Map",
                            div(class="outer",
                                leafletOutput("Map",width=1600, height=700),
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",color = "blue",
                                              selectInput("Year",
                                                          label = "Choose a Year:",
                                                          choices=list("2010",
                                                                       "2011",
                                                                       "2012",
                                                                       "2013",
                                                                       "2014",
                                                                       "2015")))
                            )),

                   tabPanel("Plots",
                            titlePanel("Other Plots"),
                            sidebarLayout(
                              sidebarPanel(
                                h3("US Causes of Death Rate"),
                                inputPanel(
                                  selectInput("year",
                                          label = "Choose a Year:",
                                          choices=list("2012",
                                                       "2013",
                                                       "2014",
                                                       "2015"))),
                                br(),
                                br(), 
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(), 
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                h3("US Suicide Population"),
                                inputPanel(
                                  selectizeInput("race", label = "Ethnicity of Interest",
                                             choices = unique(data1$Race), multiple = T,
                                             options = list(maxItems = 4, placeholder = 'Select a ethnicity'),
                                             selected = "Black"),
                                  plotOutput("termPlot", height = 200)
                                  )),
                              mainPanel(
                                  htmlOutput("Pie"),
                                  br(),
                                  br(), 
                                  br(),
                                  br(),

                                  plotlyOutput("trendPlot"),
                                  br(),
                                  br(), 
                                  br(),
                                  br(),
                                  htmlOutput("Chart")
                                  ))
                              ))
                   )
