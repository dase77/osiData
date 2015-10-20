library(shiny)
library(ggplot2)
library(stringr)
library(scales)
library(wordcloud)

data=read.csv("historija (copy).csv",sep=";",na.strings=c(" ","","\"\"","NA","N/A")) # load tha data
geografija=read.csv("geografija.csv",sep=";",na.strings=c(" ","","\"\"","NA","N/A")) # load tha data
vjeronauk=read.csv("vjeronauk.csv",sep=";",na.strings=c(" ","","\"\"","NA","N/A")) # load tha data

names(vjeronauk)=names(data) # same names in order to merge data
names(geografija)=names(data) # same names in order to merge data

data=rbind(data,vjeronauk,geografija)

levels(data$Negativni.oblici.ponašanja)[c(3,4)]=c("Da","Ne")
levels(data$Univerzalne.vrijednosti)[3]="Da"
levels(data$Predmet)[2]="Vjeronauka"


data[,5]=factor(data[,5],labels=c("šesti","sedmi","osmi","deveti"))
data[,4]=as.character(data[,4])
data[,7]=as.character(data[,7])
  
shinyServer(
  function(input, output) {
    
######################################### plots with values
    output$plotosi <- renderPlot({
      
      #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      
      ### in predmet is selected
      if(input$checkbox == TRUE)
      {
        qplot(Razred,fill=data1[,as.numeric(input$var)],data=data1,facets = .~Predmet)+scale_x_discrete("razred")+
          scale_y_continuous("broj lekcija")+ theme(legend.title=element_blank())
      
      }
      else
      qplot(data1[,5],fill=data1[,as.numeric(input$var)])+scale_x_discrete("razred")+scale_y_continuous("broj lekcija")+
        theme(legend.title=element_blank())
    },height = 400, width = 600)
    
    
######################################### table with values
    output$tableosi <- renderPrint({ 
     
       #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      
      ### in predmet is selected
      if(input$checkbox == TRUE)
      {
        addmargins(table(data1[,as.numeric(input$var)],droplevels(data1[,5]),data1[,3]))
      }
      else
        addmargins(table(data1[,as.numeric(input$var)],droplevels(data1[,5])))
    })
    
    
    ######################################### plots with percentages
    output$plot1osi <- renderPlot({ 
      
      #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      
      ### in predmet is selected
      if(input$checkbox == TRUE)
      {
        qplot(Razred,fill=data1[,as.numeric(input$var)],position="fill",data=data1,facets = .~Predmet)+scale_x_discrete("razred")+
          scale_y_continuous("procenat lekcija",labels=percent)+ theme(legend.title=element_blank())
        
      }
      else
      qplot(data1[,5],fill=data1[,as.numeric(input$var)],position="fill")+scale_x_discrete("razred")+
        scale_y_continuous("procenat lekcija",labels=percent)+ theme(legend.title=element_blank())
    },height = 400, width = 600)
    
    
    ######################################### tables with percentages
    output$table1osi <- renderPrint({ 
      
      #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      ### in predmet is selected
      if(input$checkbox == TRUE)
      {
        prop.table(ftable(table(data1[,3],droplevels(data1[,5]),data1[,as.numeric(input$var)])),margin = 1)*100
      }
      else
      
      
      prop.table(table(data1[,as.numeric(input$var)],droplevels(data1[,5])),margin = 2)*100
    })
    
    
    ######################################### table with examples
    output$table2osi <- renderTable({ 
      
      #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      a=data.frame(na.omit(data1[,as.numeric(input$var)+1]))
      a=as.character(a[,1]) # converting to character
      a=str_split(a," ") # breaking the strings
      a=unlist(a)
      a=str_replace(a,",","") #removing comma
      a=str_trim(a) #removing whitespaces
      b=str_detect(a, "-") #detect -
      c=data.frame(a,b)
      c=c[c[,2]!=TRUE,1]
      c=as.character(c)
      c=data.frame(table(c))
      c=c[order(-c[,2]),]
      names(c)=c("Sadržaji","frekvencija")
      c
      },include.rownames=FALSE)
    
    ######################################### tables with naziv lekcija
    
    output$table3osi <- renderTable({ 
      
      #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      data.frame(data1[,c(5,7)])
      
      ######################################### plots with worcloud
      output$plot3osi <- renderPlot({ 
        
        #### changing NPP
        if(input$var1 == "svi")
        {
          data=data
        }
        else
          data=data[data[,4]==input$var1,]
        
        data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
        wordcloud(data[,7],random.order = F,min.freq=5,colors=brewer.pal(4,"Dark2"))
        
      },height = 300, width = 400)
        
    })
    
  }
)
