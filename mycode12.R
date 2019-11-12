#install Tidyverse package
#install.packages("tidyverse")
#install.packages("readr")

#loading necessary libraries
library("tidyverse")
library("readxl")
library("readr")

#Main Dataset Sheet1
dataset <- readxl::read_excel("C:\\Users\\alire\\Google Drive\\Sorna\\PhD\\Sona_ClayDB2_Final.xlsx",sheet=1,col_names=TRUE)

#Number of unique Region
length(unique(dataset$Region))
#87 including NA, 86

#Unique Region to Excel
Regionlist <- unique(dataset$Region)
write_excel_csv(data.frame(Regionlist),"C:\\Users\\alire\\Google Drive\\Sorna\\PhD\\Result\\Region.csv")

#How many Regions are official
nrow(dataset)
nrow(dataset %>% filter(Type=="official"))
# 502 out of all 2189
nrow(dataset %>% filter(Type=="personal"))
# 1687 personal of all 2189


#Geo Bar
#PLT2
ggplot(data=dataset %>% filter(Region!="")) + 
  geom_bar(mapping=aes(Region,fill=Type)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_grey()
  scale_fill_manual( values = c("#000000", "#FFFFFF")) +

#Number of personal with REGIONS 4
View(dataset %>% filter(Region!="") %>% filter(Type=="personal"))


#How many unique Position
length(unique(dataset$Position))
#25 unique Position

d1 <- dataset %>% filter(Type=="official")
length(unique(d1$Position))
#18
d2 <- dataset %>% filter(Type=="personal")
length(unique(d2$Position))
#13

setdiff(unique(d1$Position),unique(d2$Position))
setdiff(unique(d2$Position),unique(d1$Position))

#Geo Bar
ggplot(data=dataset %>% filter(Position!="") %>% filter(Position!="(?)")) + 
  geom_bar(mapping=aes(Position,fill=Type)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#csvfile <- "C:\\Users\\alire\\Google Drive\\Sorna\\PhD\\Sona_ClayDB2.txt"
#dataset <- read.table(csvfile,sep="\t",header=TRUE)

#Sys.setlocale("LC_ALL", 'en_US.UTF-8')
#dataset <- read_excel("C:\\Users\\alire\\Google Drive\\Sorna\\PhD\\Sona_ClayDB2.xlsx",sheet=1)


#Sys.setlocale("LC_ALL", "English_United States.932")
#dataset <- read.csv("C:\\Users\\alire\\Google Drive\\Sorna\\PhD\\Sona_ClayDB2.csv",sep='#', encoding="UTF-8", stringsAsFactors=FALSE)

#dataset <- read.csv("C:\\Users\\alire\\Google Drive\\Sorna\\PhD\\Sona_ClayDB2.csv",sep='#', encoding="UTF-8")


#dataset <- read_delim("C:\\Users\\alire\\Google Drive\\Sorna\\PhD\\Sona_ClayDB2.csv","#",locale = locale(encoding = "UTF-8-BOM"))
#dataset <- read.csv("C:\\Users\\alire\\Google Drive\\Sorna\\PhD\\Sona_ClayDB2.csv",sep='#', encoding = "UTF-8-BOM")#,as.is = TRUE)

#print.listof(read.csv("C:\\Users\\alire\\Google Drive\\Sorna\\PhD\\Sona_ClayDB2.csv", encoding = "UTF-8"))

ggplot(data=dataset) +
  geom_point(mapping=aes(x=Type,y=Region))


ggplot(data=dataset) +
  geom_point(mapping=aes(x=Type,y=Position))


datasetB <- dataset %>%
  #  filter(Organization!="") %>%
  filter(Position!="")

ggplot(data=datasetB) +
  geom_point(mapping=aes(x=Type,y=Position))



dataset2 <- dataset %>%
  filter(Type=="official") %>%
  filter(Organization!="") %>%
  filter(Region!="")

ggplot(data=dataset2) +
  geom_point(mapping=aes(x=Type,y=Region))

#PLT 3
ggplot(data=dataset2) +
  #  ggtitle("Region vs Position") +
  #  theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold")) +
  geom_point(mapping=aes(x=Region,y=Position,color=Organization,shape=Organization)) +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  scale_fill_grey()


# ggplot(data=dataset2) +
#   geom_point(mapping=aes(x=Region,y=Position,color=Organization)) +
#   facet_wrap(~Organization,nrow=2)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


ggplot(data=dataset2) +
  geom_point(mapping=aes(x=Organization,y=Position,color=Organization)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# ggplot(data=dataset2) +
#   geom_point(mapping=aes(x=Organization,y=Region,color=Type)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#Geo Bar
#PLT 1
ggplot(data=dataset2) + 
  geom_bar(mapping=aes(Organization,fill=Organization)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_grey()



#PLT 4
ggplot(data=dataset2) + 
  geom_bar(mapping=aes(Region,fill=Organization)) +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  scale_fill_grey()


# ggplot(data=dataset2) + 
#   geom_bar(mapping=aes(Region,fill=Type,y = ..prop..,group=1)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
# 

#PLT 5
ggplot(data=dataset2) + 
  geom_bar(mapping=aes(Position,fill=Organization)) +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=10,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  scale_fill_grey()


#PLT 5 no filter
ggplot(data=dataset %>% filter(Organization!="") %>% filter(Region!="")) + 
  geom_bar(mapping=aes(Position,fill=Organization)) +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=10,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  scale_fill_grey()


#Removed PLT 6  filter personal filter
ggplot(data=dataset %>% filter(Type=="personal")) + # %>% filter(Organization!="") %>% filter(Region!="")) + 
  geom_bar(mapping=aes(Region,fill=Organization)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

ggplot(data=dataset2) + 
  geom_bar(mapping=aes(Region,fill=Position)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  


ggplot(data=dataset2) + 
  geom_bar(mapping=aes(Organization,fill=Position), position = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#PLT 7
ggplot(dataset2,aes(Region,fill=Organization)) + geom_bar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(Organization ~ Type) +
  scale_fill_grey()


unique(dataset2$Position)

#dataset3 <- dataset2 %>%
#  filter(Position!='Spāhbed' || Position!='Messenger(?)')
#unique(dataset3$Position)

colnames(dataset)

dataset3<- group_by(dataset2,Position,Region,Organization)
dataset4 <- summarise(dataset3,count=n())

ggplot(data=dataset4) +
  geom_point(mapping=aes(x=Region,y=Position,size=count,color=Organization)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(data=dataset4 %>% group_by(Position,Region)) +
  geom_point(mapping=aes(x=Region,y=Position,size=count,color=count)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  geom_text(aes(x=Region,y=Position,label=count), size = 3, vjust = 2)  

################################
#PLT 8
ggplot(data=dataset4) +
  geom_point(mapping=aes(x=Region,y=Position,size=count,color=Organization,shape=Organization)) +
  #theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  geom_text(aes(x=Region,y=Position,label=count), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10))
################################

ggplot(data=dataset2) +
  geom_count(mapping=aes(x=Region,y=Position,color=Organization)) +
  #geom_point(mapping=aes(x=Region,y=Position,size=count,color=Organization)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) 
#geom_text(aes(x=Region,y=Position,label=count), size = 3, vjust = 2)  


dataset2 %>% 
  count(Position,Region,Organization) %>%
  ggplot(mapping = aes(x = Region, y = Position)) +
  geom_tile(mapping = aes(fill = n)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  geom_text(aes(x=Region,y=Position,label=n), size = 3, vjust = 3)  

#########################
#Official       dataset2
#########################
dataset5 <- dataset2 %>% 
  separate(`co-impressionINDEX`,c('primarycoimp','secondarycoimp'),',', extra = "merge") 
unique(dataset5$primarycoimp)
length(unique(dataset5$primarycoimp))

View(unique(dataset5$primarycoimp))

###########################
#CLEANUP Database
dataset7 <- dataset5 %>%
  select(Position,Region,primarycoimp,Organization) %>%
  group_by(Position,Region,Organization,primarycoimp) %>%
  #group_by(primarycoimp) %>%
  summarise(count=n())

dataset6 <- dataset5 %>%
  select(Position,Region,primarycoimp,Organization) %>%
  #group_by(Position,Region,primarycoimp) %>%
  group_by(primarycoimp) %>%
  summarise(count=n())

#to identify duplicated
dataset7$primarycoimp[duplicated(dataset7$primarycoimp)]
#no duplicared now, it is ok!
###########################

dataset7 <- dataset5 %>%
  select(Position,Region,Organization,primarycoimp) %>%
  group_by(Position,Region,Organization,primarycoimp) %>%
  summarise(count=n())

#PLT 9
ggplot(data=dataset7) +
  geom_point(mapping=aes(x=Region,y=Position,size=count,color=primarycoimp,shape=Organization)) +
  geom_text(aes(x=Region,y=Position,label=count), size = 3, vjust = 2)  +
  #  geom_text(aes(x=Region,y=Position,label=primarycoimp), size = 3, vjust = 2,hjust=-1)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  guides(size=FALSE,color=FALSE)


#DF<-data.frame(dataset)

#Bar chart unique co impression
#PLT 10
ggplot(data=dataset7) + 
  geom_bar(mapping=aes(Region,fill=Organization)) +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  scale_fill_grey()


dataset8 <- dataset7 %>% 
  filter(Position=="Magus" & Region=="Gurgān")

#########################
#personal       dataset22
#########################
dataset22 <- dataset %>%
  filter(Type=="personal") %>%
  filter(Position!="") # || 
#filter(Region!="")


dataset55 <- dataset22 %>% 
  separate(`co-impressionINDEX`,c('primarycoimp','secondarycoimp'),',', extra = "merge") 
unique(dataset55$primarycoimp)
length(unique(dataset55$primarycoimp))
#90 unique primary personal

View(unique(dataset55$primarycoimp))

dataset77 <- dataset55 %>%
  select(Position,Region,Organization,primarycoimp) %>%
  group_by(Position,Region,Organization,primarycoimp) %>%
  summarise(count=n())

#PLT 11
ggplot(data=dataset77) +
  geom_point(mapping=aes(x=Region,y=Position,size=count,color=primarycoimp)) + #,shape=Organization)) +
  geom_text(aes(x=Region,y=Position,label=count), size = 3, vjust = 2)  +
  #  geom_text(aes(x=Region,y=Position,label=primarycoimp), size = 3, vjust = 2,hjust=-1)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  guides(size=FALSE,color=FALSE)

#####################
####### offical and personal
#datasetB <- dataset %>%
#  filter(Position!="")
#####################
datasetC <- datasetB %>% 
  separate(`co-impressionINDEX`,c('primarycoimp','secondarycoimp'),',', extra = "merge") 
unique(datasetC$primarycoimp)
length(unique(datasetC$primarycoimp))
#number of primary coimpression is 254 for both of offfical and personal

datasetD <- datasetC %>%
  select(Type,Position,Region,Organization,primarycoimp) %>%
  group_by(Type,Position,Region,Organization,primarycoimp) %>%
  summarise(count=n())

#PLT 12
ggplot(data=datasetD) + # %>% filter(Position!="(?)")) + 
  geom_bar(mapping=aes(Position,fill=Type)) +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=10,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  scale_fill_grey()


View(datasetD %>% filter(Position=="Naxwār")) 
View(datasetD %>% filter(Position=="Ōstāndār")) 
View(datasetD %>% filter(Position=="Šahrab")) 


#########################################

#Main sheet , separate co impresson to find similar co impression

#count number of co-impression in each row

DF <- dataset

library(stringr)
DF$numberofCO <- str_count(dataset$`co-impressionINDEX`, ",")

DF2 <- DF %>%
  separate(`co-impressionINDEX`,c('primarycoimp','secondarycoimp'),',', extra = "merge")

#install.packages("splitstackshape")
library(splitstackshape) 
DF3<- cSplit(DF2,"secondarycoimp",",")


DF4 <- DF3 %>%
  gather(tail(colnames(DF3),max(DF3$numberofCO)),key="columnname", value='coimp2', na.rm = TRUE) #, factor_key=TRUE)


DF5 <- dplyr::add_count(DF4, coimp2)

#DF6 is Final for identify relation between secondary co impression
DF6 <- DF5 %>%
  filter(n>1 & coimp2!="2d")

DF7 <- DF5 %>%
  filter(coimp2!="2d")


#Write final tidy dataset to tab delimited
#write_csv(DF6, "C:\\Users\\alire\\Google Drive\\Sorna\\PhD\\AlirezaDF6.csv") #,sep="\t",row.names=FALSE)


#question1 : find which primarycoimpression has same secondaycoimpression
#*********************************************************************************************************************************
#*********************************************************************************************************************************
#*********************************************************************************************************************************

#PLT 13
ggplot(data=DF6) + # %>%  filter(!is.na(Position))  ) +# %>% filter(Region!="") ) +
  geom_point(mapping=aes(x=primarycoimp,y=coimp2,size=n ,color=Region)) + #,shape=Region)) +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  theme(axis.text.y = element_text(angle = 0,colour="grey20",size=8,hjust=1,vjust=.5,face="plain")) + 
  #guides(size=FALSE,color=guide_legend(ncol=1))
  guides(color=guide_legend(ncol=1))


ggplot(data=DF6) + # %>% filter(Region!="") ) +
  geom_point(mapping=aes(x=primarycoimp,y=coimp2,size=n ,color=Region)) + #,shape=Region)) +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  geom_text(aes(x=primarycoimp,y=coimp2,label=Region,color=Region),size = 3, hjust = 1)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  theme(axis.text.y = element_text(angle = 0,colour="grey20",size=8,hjust=1,vjust=.5,face="plain")) + 
  guides(size=FALSE,color=guide_legend(ncol=1))



#PLT 14
ggplot(data=DF6) + # %>% filter(Position!="")) +
  geom_point(mapping=aes(x=primarycoimp,y=coimp2,size=n ,color=Position)) + #,shape=Region)) +
  geom_text(aes(x=primarycoimp,y=coimp2,label=Position,color=Position),size = 3, hjust = 1,vjust=1)  +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) + 
  guides(size=FALSE,color=guide_legend(ncol=1))
#*********************************************************************************************************************************
#*********************************************************************************************************************************
#*********************************************************************************************************************************

ggplot(data=DF6) + # %>% filter(Position!="")) +
  geom_point(mapping=aes(x=coimp2,y=Position,size=n ,color=primarycoimp,group=primarycoimp)) + #,shape=Region)) +
  #  geom_text(aes(x=coimp2,y=Position,label=primarycoimp,color=primarycoimp),size = 3, hjust = 1,vjust=1)  +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) + 
  guides(size=FALSE,color=FALSE) #guide_legend(ncol=1))


ggplot(data=DF6) + # %>% filter(Position!="")) +
  geom_point(mapping=aes(x=coimp2,y=Region,size=n ,color=primarycoimp,group=primarycoimp)) + #,shape=Region)) +
  #  geom_text(aes(x=coimp2,y=Position,label=primarycoimp,color=primarycoimp),size = 3, hjust = 1,vjust=1)  +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) + 
  guides(size=FALSE,color=FALSE) #guide_legend(ncol=1))




DF10 <- dplyr::add_count(DF6, primarycoimp,coimp2)


DF11 <- DF10 %>%
  filter(nn>1)

#only officail
DF12 <- DF11 %>%
  filter(Type=="official")



DF13 <- DF12 %>%
  group_by(primarycoimp,coimp2)

#PLT 15
ggplot(data=DF13) +
  geom_point(mapping=aes(x=Region,y=Position,size=nn,color=Organization,shape=Organization)) +
  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10))


#PLT 16
ggplot(data=DF13) +
  geom_point(mapping=aes(x=Region,y=primarycoimp,size=nn,color=Organization,shape=Organization)) +
  geom_text(aes(x=Region,y=primarycoimp,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10))


#########################################
#Shiny app

#install.packages("shiny")
library(shiny)


ui <- fluidPage(
  selectInput("pposition", "Choose a Position:",
              dataset$Position
              
  ),
  
  sliderInput(inputId="num",label="Choose your value",value=25,min=0,max=90),
  textInput("caption", "Caption", "Data Summary"),
  verbatimTextOutput("value"),
  plotOutput(outputId="ggplotposition")
)

server <- function(input,output){
  output$value <- renderText({ input$pposition })
  output$ggplotposition <- renderPlot({
    ggplot2::ggplot(dplyr::filter(dataset2,Position==input$pposition)) + 
      ggplot2::geom_bar(mapping=ggplot2::aes(Position,fill=Region)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = input$num, hjust = 1)) 
  })
}
shiny::shinyApp(ui=ui, server=server)

#########################################
#Sheet2
#########################################

Codataset <- readxl::read_excel("C:\\Users\\alire\\Google Drive\\Sorna\\PhD\\Sona_ClayDB2_Final.xlsx",sheet=2,col_names=TRUE)
#View(Codataset2)

#Reading Icongraphy keywords

keywords <- read.csv("C:\\Users\\alire\\Google Drive\\Sorna\\PhD\\iconography.txt",sep=',', stringsAsFactors=FALSE)



Codataset_delimiter <- separate(Codataset,Iconography,into=c("Iconography","reseticon"),sep=',',extra="merge")

Codataset2 <- separate_rows(Codataset_delimiter,Iconography)

#sapply(Codataset2$Iconography,toupper)

keywordsV <- as.vector(keywords$keyword) #%>% sapply(toupper)

Codataset3 <- Codataset2 %>% filter(Iconography %in% keywordsV)




ggplot(data=Codataset3) +
  geom_point(mapping=aes(x=Iconography,y=`Co-impressionINDEX`)) + #,shape=Region)) +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  theme(axis.text.y = element_text(angle = 0,colour="grey20",size=8,hjust=1,vjust=.5,face="plain")) 


###################################
DFMERGE <- merge(Codataset3,DF7,by.x="Co-impressionINDEX" , by.y="coimp2")
###################################


ggplot(data=DFMERGE) +
  geom_point(mapping=aes(x=Iconography,y=Position)) + #,shape=Region)) +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  theme(axis.text.y = element_text(angle = 0,colour="grey20",size=8,hjust=1,vjust=.5,face="plain")) 


#PLT 17
ggplot(data=DFMERGE %>% filter(Position!="") %>% filter(Position!="(?)")) +
  geom_point(mapping=aes(x=Iconography,y=Position)) + #,shape=Region)) +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  theme(axis.text.y = element_text(angle = 0,colour="grey20",size=8,hjust=1,vjust=.5,face="plain")) 

########################################
#PLT 18
ggplot(data=DFMERGE) + # %>% filter(Position!="") %>% filter(Position!="(?)")) +
  geom_bar(mapping=aes(Iconography,fill=Type))  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  theme(axis.text.y = element_text(angle = 0,colour="grey20",size=8,hjust=1,vjust=.5,face="plain")) +
  scale_fill_grey()

########################################


########################################
#PLT 19
ggplot(data=DFMERGE %>% filter(Type=="official") %>% filter(Organization!=""))+ # %>% filter(Position!="") %>% filter(Position!="(?)")) +
  geom_bar(mapping=aes(Iconography,fill=Organization))  +
  facet_grid(Organization ~ Type) +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=10,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  theme(axis.text.y = element_text(angle = 0,colour="grey20",size=10,hjust=1,vjust=.5,face="plain")) +
  scale_fill_grey()

########################################



ggplot(data=DFMERGE %>% filter(Type=="official") %>% filter(Organization!=""))+ # %>% filter(Position!="") %>% filter(Position!="(?)")) +
  geom_point(mapping=aes(x=Iconography,y=Position)) + #,shape=Region)) +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  theme(axis.text.y = element_text(angle = 0,colour="grey20",size=8,hjust=1,vjust=.5,face="plain")) 


DFMERGE2 <- DFMERGE %>% filter(Type=="official") %>% filter(Organization!="") 

DFMERGE3<- group_by(DFMERGE2,Position,Iconography)   

DFMERGE4 <- summarise(DFMERGE3,count=n())

#PLT 20

ggplot(data=DFMERGE4 )+ # %>% filter(Position!="") %>% filter(Position!="(?)")) +
  geom_point(mapping=aes(x=Iconography,y=Position,size=count,color=count)) + #,shape=Region)) +
  geom_text(aes(x=Iconography,y=Position,label=count), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  theme(axis.text.y = element_text(angle = 0,colour="grey20",size=8,hjust=1,vjust=.5,face="plain")) 

################################### n>1
DFMERGEA <- merge(Codataset3,DF6,by.x="Co-impressionINDEX" , by.y="coimp2") %>% filter(Type=="official") %>% filter(Organization!="")
###################################

###################################
#PLT 21

ggplot(data=DFMERGEA  ) + # %>% group_by(Iconography,`Co-impressionINDEX`,n))+ 
  geom_point(mapping=aes(x=Iconography,y=`Co-impressionINDEX`,size=n,color=Position)) + #,shape=Region)) +
  geom_text(aes(x=Iconography,y=`Co-impressionINDEX`,label=Position,color=Position),size = 3, hjust = 1.2,vjust=1)  +
  geom_text(aes(x=Iconography,y=`Co-impressionINDEX`,label=Region),size = 3, hjust = -0.2,vjust=1)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  theme(axis.text.y = element_text(angle = 0,colour="grey20",size=8,hjust=1,vjust=.5,face="plain")) 
###################################


#Question2 : find overlap coimpresiion based on organziation
##########################################################
# DF6 where Ncoimp>1
# filter type=official

xoDF <- DF6 %>% filter(Type=="official")

#PLT 22
ggplot(data=xoDF) + # %>% filter(Position!="")) +
  geom_point(mapping=aes(x=coimp2,y=Position,size=n ,color=primarycoimp)) + #,group=primarycoimp)) + #,shape=Region)) +
  #  geom_text(aes(x=coimp2,y=Position,label=primarycoimp,color=primarycoimp),size = 3, hjust = 1,vjust=1)  +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) + 
  guides(size=FALSE,color=FALSE) #guide_legend(ncol=1))

#PLT 23
ggplot(data=xoDF) + # %>% filter(Position!="")) +
  geom_point(mapping=aes(x=coimp2,y=Region,size=n ,color=primarycoimp,group=primarycoimp)) + #,shape=Region)) +
  #  geom_text(aes(x=coimp2,y=Position,label=primarycoimp,color=primarycoimp),size = 3, hjust = 1,vjust=1)  +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) + 
  guides(size=FALSE,color=FALSE) #guide_legend(ncol=1))

#PLT 24
ggplot(data=xoDF) + # %>% filter(Position!="")) +
  geom_point(mapping=aes(x=coimp2,y=Organization,size=n ,color=primarycoimp) )+#,group=primarycoimp)) + #,shape=Region)) +
  #  geom_text(aes(x=coimp2,y=Position,label=primarycoimp,color=primarycoimp),size = 3, hjust = 1,vjust=1)  +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) + 
  guides(size=FALSE,color=FALSE) #guide_legend(ncol=1))


###########
### shahd n>1 where different Position
xoDF2 <- xoDF %>%
  select(Position,Region,coimp2) %>%
  unique()

xoDF3 <- xoDF2 %>%
  count(coimp2) %>%
  filter(n>1)
#  dplyr::add_count(Position,coimp2) 

sameimpdifferentposition <- pull(xoDF3,coimp2)

#PLT23 version unique
ggplot(data=xoDF %>% filter(coimp2 %in% sameimpdifferentposition)) + # %>% filter(Position!="")) +
  geom_point(mapping=aes(x=coimp2,y=Region,size=n ,color=primarycoimp,group=primarycoimp)) + #,shape=Region)) +
  geom_text(aes(x=coimp2,y=Region,label=Position,color=Position),size = 3, hjust = 1.2,vjust=1)  +
  geom_text(aes(x=coimp2,y=Region,label=n,color=Position),size = 3, hjust =-1,vjust=1)  +
  #  geom_text(aes(x=coimp2,y=Position,label=primarycoimp,color=primarycoimp),size = 3, hjust = 1,vjust=1)  +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) + 
  guides(size=FALSE,color=FALSE) #guide_legend(ncol=1))



###########
### shahd n>1 where different Organization
xoDF4 <- xoDF %>%
  select(Organization,coimp2) %>%
  unique()

xoDF5 <- xoDF4 %>%
  count(coimp2) %>%
  filter(n>1)

sameimpdifferentOrganization <- pull(xoDF5,coimp2)

#PLT  24 version unique
ggplot(data=xoDF %>% filter(coimp2 %in% sameimpdifferentOrganization)) + # %>% filter(Position!="")) +
  geom_point(mapping=aes(x=coimp2,y=Organization,size=n ,color=primarycoimp,group=primarycoimp)) + #,shape=Region)) +
  geom_text(aes(x=coimp2,y=Organization,label=Position,color=Position),size = 3, hjust = 1.2,vjust=1)  +
  geom_text(aes(x=coimp2,y=Organization,label=n,color=Position),size = 3, hjust =-1,vjust=1)  +
  geom_text(aes(x=coimp2,y=Organization,label=Region,color=Position),size = 3, hjust =1,vjust=3)  +
  #  geom_text(aes(x=coimp2,y=Position,label=primarycoimp,color=primarycoimp),size = 3, hjust = 1,vjust=1)  +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) + 
  guides(size=FALSE,color=FALSE) #guide_legend(ncol=1))




###########
### shahd n>1 where different Position and Region
xoDF6 <- xoDF %>%
  select(Position,Region,coimp2) %>%
  unique()

xoDF7 <- xoDF6 %>%
  count(coimp2) %>%
  filter(n>1)
#  dplyr::add_count(Position,coimp2) 

sameimpdifferentpositionRegion <- pull(xoDF7,coimp2)

#PLT 22 Version Unique
ggplot(data=xoDF %>% filter(coimp2 %in% sameimpdifferentpositionRegion)) + # %>% filter(Position!="")) +
  geom_point(mapping=aes(x=coimp2,y=Region,size=n ,color=primarycoimp,group=primarycoimp)) + #,shape=Region)) +
  geom_text(aes(x=coimp2,y=Region,label=Position,color=Position),size = 3, hjust = 1.2,vjust=1)  +
  geom_text(aes(x=coimp2,y=Region,label=n,color=Position),size = 3, hjust =-1,vjust=1)  +
  #  geom_text(aes(x=coimp2,y=Position,label=primarycoimp,color=primarycoimp),size = 3, hjust = 1,vjust=1)  +
  #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) + 
  guides(size=FALSE,color=FALSE) #guide_legend(ncol=1))



############################################
ui <- fluidPage(
  selectInput("ccoimp2", "Choose a coimp2:",
              xoDF3$coimp2
              
  ),
  
  sliderInput(inputId="num",label="Choose your value",value=25,min=0,max=90),
  textInput("caption", "Caption", "Data Summary"),
  verbatimTextOutput("value"),
  plotOutput(outputId="ggplotposition")
)

server <- function(input,output){
  output$value <- renderText({ input$pposition })
  output$ggplotposition <- renderPlot({
    
    ggplot2::ggplot(dplyr::filter(xoDF,coimp2==input$ccoimp2)) + # %>% filter(Position!="")) +
      ggplot2::geom_point(mapping=aes(x=coimp2,y=Position,size=n ,color=primarycoimp) )+#,group=primarycoimp)) + #,shape=Region)) +
      #  geom_text(aes(x=coimp2,y=Position,label=primarycoimp,color=primarycoimp),size = 3, hjust = 1,vjust=1)  +
      #  geom_text(aes(x=Region,y=Position,label=nn), size = 3, vjust = 2)  +
      ggplot2::theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) + 
      ggplot2::guides(size=FALSE,color=FALSE) #guide_legend(ncol=1))
    
  })
}
shiny::shinyApp(ui=ui, server=server)

################################################################################
########################################
#Icongraphy main sheet

IDF <- dataset %>% 
  filter(Type=="personal") %>%
  filter(Position=="Magus")

keywords <- read.csv("C:\\Users\\alire\\Google Drive\\Sorna\\PhD\\iconography.txt",sep=',', stringsAsFactors=FALSE)


IDF_delimiter <- separate(IDF,Icongraphy,into=c("Icongraphy","reseticon"),sep=',',extra="merge")

IDF2 <- separate_rows(IDF_delimiter,Icongraphy)

#sapply(Codataset2$Iconography,toupper)

keywordsV <- as.vector(keywords$keyword) #%>% sapply(toupper)

IDF3 <- IDF2 %>% filter(Icongraphy %in% keywordsV)

#PLT 27
ggplot(data=IDF3) + 
  geom_bar(mapping=aes(Icongraphy,fill= "Position")) +
  scale_fill_manual( values = c("#0000AA", "#F8366D")) +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=10,hjust=1,vjust=.5,face="plain"),text = element_text(size=10))


library(stringr)
IDF3$numberofCO <- str_count(IDF3$`co-impressionINDEX`, ",")

IDF4 <- IDF3 %>%
  separate(`co-impressionINDEX`,c('primarycoimp','secondarycoimp'),',', extra = "merge")

#install.packages("splitstackshape")
library(splitstackshape) 
IDF5<- cSplit(IDF4,"secondarycoimp",",")

IDF6 <- IDF5 %>%
  gather(tail(colnames(IDF5),max(IDF5$numberofCO)),key="columnname", value='coimp2', na.rm = TRUE) #, factor_key=TRUE)


IDF7 <- dplyr::add_count(IDF6, coimp2)

#DF6 is Final for identify relation between secondary co impression
IDF8 <- IDF7 %>%
  filter(n>1 & coimp2!="2d")

IDF9 <- IDF7 %>%
  filter(coimp2!="2d")

IDFMERGEA <- merge(Codataset3,IDF9,by.x="Co-impressionINDEX" , by.y="coimp2") # %>% filter(Type=="official") %>% filter(Organization!="")

#PLT 28
ggplot(data=IDFMERGEA  ) + # %>% group_by(Iconography,`Co-impressionINDEX`,n))+ 
  geom_point(mapping=aes(x=Icongraphy,y=Iconography,size=n,color=Position)) + #,shape=Region)) +
  #geom_text(aes(x=Iconography,y=Iconography,label=Position,color=Position),size = 3, hjust = 1.2,vjust=1)  +
  #geom_text(aes(x=Iconography,y=Iconography,label=Region),size = 3, hjust = -0.2,vjust=1)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  theme(axis.text.y = element_text(angle = 0,colour="grey20",size=8,hjust=1,vjust=.5,face="plain")) 

library("igraph")

IDFMERGEA2 <- IDFMERGEA %>%
  select(Icongraphy,Iconography,n)

Ig1 <- graph_from_data_frame(IDFMERGEA2,directed = FALSE)

V(Ig1)$color <- ifelse( V(Ig1)$name %in% IDFMERGEA2$Icongraphy,'red','gold')

net <- igraph::simplify(Ig1, remove.multiple = T, remove.loops = T) 

layout1 <- layout_nicely(net)
layout1 <- layout_components(net)  #layout_nicely(Ig1)

plot(net, #vertex.color=as.numeric(factor(edgesF$Region)),
     vertex.size=5,
     vertex.frame.color="gray",
     vertex.label.color="black",
     edge.arrow.size = 0.05,
     vertex.label.cex=0.7,
     vertex.label.dist=0,
     edge.curved=0.20,
     edge.width=E(net)$n,
     layout = layout1) #layout_with_graphopt) #as_star) 

###############
##############
#Question  Sephdbod.  coimp for sepahbod, sealed what other sealing

SDF <- DF6 %>%
  filter(Position=="Spāhbed")

spahbodcoimp2 <- unique(SDF$coimp2)

SDF2 <- DF7 %>%
  filter(coimp2 %in% spahbodcoimp2)


#PLT 29
ggplot(data=SDF2  ) + # %>% group_by(Iconography,`Co-impressionINDEX`,n))+ 
  geom_point(mapping=aes(x=coimp2,y=primarycoimp,size=n,color=Position,shape=Type)) + #,shape=Region)) +
  geom_text(aes(x=coimp2,y=primarycoimp,label=Position,color=Position),size = 3, hjust = 1.2,vjust=1)  +
  geom_text(aes(x=coimp2,y=primarycoimp,label=Region),size = 3, hjust = -0.2,vjust=1)  +
  theme(axis.text.x = element_text(angle = 90,colour="grey20",size=8,hjust=1,vjust=.5,face="plain"),text = element_text(size=10)) +
  theme(axis.text.y = element_text(angle = 0,colour="grey20",size=8,hjust=1,vjust=.5,face="plain")) 



########################################
#NETWORK Modeling
########################################
#install.packages("igraph")
library("igraph")

#PLT25
NDF <- DF6 %>%
  filter(DF6$Type=="official") %>%
  select(primarycoimp,coimp2,n)

  

g1 <- graph_from_data_frame(NDF,directed = TRUE)

V(g1)$color <- ifelse( V(g1)$name %in% NDF$primarycoimp,'red','gold')

layout1 <-  layout_with_fr(g1) #layout_in_circle(g1) #layout_as_tree(g1) #
layout1 <- layout_nicely(g1)

plot(g1, #vertex.color=as.numeric(factor(edgesF$Region)),
     vertex.size=5,
     vertex.frame.color="gray",
     vertex.label.color="black",
     edge.arrow.size = 0.05,
     vertex.label.cex=0.7,
     vertex.label.dist=0,
     edge.curved=0.20,
     edge.width=E(g1)$n,
     layout = layout1) #layout_with_graphopt) #as_star) 


########################################

#PLT 26
NDF2 <- DF6 %>%
  filter(Type!="official") %>%
  select(primarycoimp,coimp2,n)



g1 <- graph_from_data_frame(NDF2,directed = TRUE)

V(g1)$color <- ifelse( V(g1)$name %in% NDF$primarycoimp,'red','gold')

layout1 <-  layout_with_fr(g1) #layout_in_circle(g1) #layout_as_tree(g1) #
payout <- layout_nicely(g1)

plot(g1, #vertex.color=as.numeric(factor(edgesF$Region)),
     vertex.size=5,
     vertex.frame.color="gray",
     vertex.label.color="black",
     edge.arrow.size = 0.05,
     vertex.label.cex=0.7,
     vertex.label.dist=0,
     edge.curved=0.20,
     edge.width=E(g1)$n,
     layout = layout1) #layout_with_graphopt) #as_star) 


########################################


#PLT 27
NDF2 <- DF6 %>%
#  filter(Type!="official") %>%
  select(primarycoimp,coimp2,n)



g1 <- graph_from_data_frame(NDF2,directed = TRUE)

V(g1)$color <- ifelse( V(g1)$name %in% NDF$primarycoimp,'red','gold')

layout1 <-  layout_with_fr(g1) #layout_in_circle(g1) #layout_as_tree(g1) #
payout <- layout_nicely(g1)

plot(g1, #vertex.color=as.numeric(factor(edgesF$Region)),
     vertex.size=5,
     vertex.frame.color="gray",
     vertex.label.color="black",
     edge.arrow.size = 0.05,
     vertex.label.cex=0.7,
     vertex.label.dist=0,
     edge.curved=0.20,
     edge.width=E(g1)$n,
     layout = layout1) #layout_with_graphopt) #as_star) 


########################################

NDF <- DF6 %>%
  filter(Type=="official") %>%
  select(Position,coimp2,n)



g1 <- graph_from_data_frame(NDF,directed = TRUE)

V(g1)$color <- ifelse( V(g1)$name %in% NDF$Position,'red','gold')

layout1 <-  layout_with_fr(g1) #layout_in_circle(g1) #layout_as_tree(g1) #
layout1 <- layout_nicely(g1)
layout1 <- layout_on_grid(g1)
plot(g1, #vertex.color=as.numeric(factor(edgesF$Region)),
     vertex.size=5,
     vertex.shape="circle",
     vertex.frame.color="gray",
     vertex.label.color="black",
     edge.arrow.size = 0.05,
     vertex.label.cex=0.7,
     vertex.label.dist=0,
     edge.curved=0.20,
     edge.width=E(g1)$n,
    # vertex.shapes=
     layout = layout1) #layout_with_graphopt) #as_star) 



edgesF <- DF6 %>%
  group_by(primarycoimp,coimp2,n)
elF <- cbind(edgesF$primarycoimp,edgesF$coimp2)
grr <- graph.edgelist(elF)

E(grr)$width <-  edgesF$n

plot(grr,vertex.color=edgesF$n, vertex.size=4, vertex.frame.color="gray", vertex.label.color="black", edge.arrow.size = 0.3,vertex.label.cex=0.7, vertex.label.dist=0, edge.curved=0.1,layout = layout_with_graphopt) #as_star) 
#in_circle) #with_graphopt) #"bipartite|merge|norm|sugiyama|tree#layout_with_graphopt) #,vertex.label=NA)


###################
###################
edgesF <- DF6 %>%
  filter(Type=="official") %>%
  filter(Region!="") %>%
  group_by(Region,coimp2,n,Region)
elF <- cbind(edgesF$Region,edgesF$coimp2)
grr <- graph.edgelist(elF)

E(grr)$width <-  edgesF$n

plot(grr,vertex.color=as.numeric(factor(edgesF$Region)), vertex.size=4, vertex.frame.color="gray", vertex.label.color="black", edge.arrow.size = 0.3,vertex.label.cex=0.7, vertex.label.dist=0, edge.curved=0.1,layout =layout_nicely ) #layout_with_graphopt) #as_star) 

plot(grr,vertex.color=as.numeric(factor(edgesF$Region)), vertex.size=7, vertex.frame.color="gray", vertex.label.color="black", edge.arrow.size = 0.3,vertex.label.cex=0.7, vertex.label.dist=0, edge.curved=0.1,layout =layout_with_dh ) #layout_with_graphopt) #as_star)

plot(grr,vertex.color=as.numeric(factor(edgesF$Region)), vertex.size=7, vertex.frame.color="gray", vertex.label.color="black", edge.arrow.size = 0.3,vertex.label.cex=0.7, vertex.label.dist=0, edge.curved=0.1,layout =layout_with_fr ) #layout_with_graphopt) #as_star) 



plot(clp,grr, vertex.size=7, vertex.frame.color="gray", vertex.label.color="black", edge.arrow.size = 0.3,vertex.label.cex=0.7, vertex.label.dist=0, edge.curved=0.1,layout =layout_with_fr ) #layout_with_graphopt) #as_star) 



#in_circle) #with_graphopt) #"bipartite|merge|norm|sugiyama|tree#layout_with_graphopt) #,vertex.label=NA)

clp <- cluster_label_prop(grr)
plot(clp, grr,vertex.color=as.numeric(factor(edgesF$Region)), vertex.size=4, vertex.frame.color="gray", vertex.label.color="black", edge.arrow.size = 0.3,vertex.label.cex=0.7, vertex.label.dist=0, edge.curved=0.1,layout = layout_with_graphopt)

net <- igraph::simplify(grr, remove.multiple = T, remove.loops = T) 
plot(grr,vertex.color=as.numeric(factor(edgesF$Region)), vertex.size=8, vertex.frame.color="black", vertex.label.color="black", edge.arrow.size = 0.2,vertex.label.cex=0.7, vertex.label.dist=0, edge.curved=0.1,layout =layout_with_fr ) #layout_with_graphopt) #as_star) 



#deg <- degree(grr, mode="all")
#plot(grr,vertex.color=as.numeric(factor(edgesF$Region)), vertex.size=deg, vertex.frame.color="gray", vertex.label.color="black", edge.arrow.size = 0.3,vertex.label.cex=0.7, vertex.label.dist=0, edge.curved=0.1,layout =layout_with_fr ) #layout_with_graphopt) #as_star) 


NDF <- DF6 %>%
  filter(Type=="official") %>%
  select(primarycoimp,coimp2)

g <- graph_from_data_frame(NDF,directed=TRUE)

#edge.attributes(g)
#vertex.attributes(g)

plot(g,vertex.label.color="black", vertex.size=7,layout = layout_in_circle,edge.color="black",edge.curved=.2)

###################
###################
NDFF <- DF6 %>%
  filter(Type=="official") %>%
  filter(Region!="") %>%
  group_by(Region,coimp2,n,Region) %>%
  select(Region,coimp2,n,Region)

g <- graph_from_data_frame(NDF,directed=TRUE)

E(g)$width <-  NDFF$n

plot(grr,vertex.color=as.numeric(factor(edgesF$Region)),
     vertex.size=7,
     vertex.frame.color="gray",
     vertex.label.color="black",
     edge.arrow.size = 0.3,
     vertex.label.cex=0.7,
     vertex.label.dist=0,
     edge.curved=0.1,
     layout =layout_nicely ) #layout_with_graphopt) #as_star) 

plot(g,vertex.label.color="black",
     vertex.frame.color="gray",
     vertex.size=7,
     edge.arrow.size = 0.3,
     vertex.label.cex=0.7,
     vertex.label.dist=0,
     layout =layout_nicely,
     edge.color="black",
     edge.curved=.2)


































































#install.packages("igraph")
library(igraph)
library(tidyverse)

NetDF <- DF6 %>%
  select(primarycoimp,coimp2)

g <- graph.edgelist(as.matrix(NetDF),directed = TRUE)

#return all vertices
V(g)
V(g)[[1:5]]

#Retunr all edges
E(g)
E(g)[[1:5]]
#total number of vertices
gorder(g)

#total number of edges
gsize(g)

plot(g,
     vertex.size=5,
     vertex.color.label="black")

#g <- set_vertex_attr(g,"Region",as.matrix(DF6$Region))

g <- set_edge_attr(g,"width",value=as.matrix(DF6$n))

vertex.attributes(g)
edge.attributes(g)

E(g)[[width>10]]
E(g)[[inc('178a')]]


V(g)$color <- ifelse(V(g)$name %in% DF6$primarycoimp,'red','gold')

