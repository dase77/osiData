library(shiny)
library(ggplot2)
library(stringr)

data=read.csv("../historija (copy).csv",sep=";",na.strings=c(" ","","\"\"","NA","N/A")) # load tha data
data[,5]=factor(data[,5],labels=c("šesti","sedmi","osmi","deveti"))
data[,4]=as.character(data[,4])
data[,7]=as.character(data[,7])


shinyServer(
  function(input, output) {
    
    output$plotosi <- renderPlot({
      
      #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      qplot(data1[,5],fill=data1[,as.numeric(input$var)])+scale_x_discrete("razred")+scale_y_continuous("broj lekcija")+
        theme(legend.title=element_blank())
    },height = 300, width = 400)
    
    
    
    output$tableosi <- renderTable({ 
     
       #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      addmargins(table(data1[,as.numeric(input$var)],droplevels(data1[,5])))
    })
    
    output$plot1osi <- renderPlot({ 
      
      #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      qplot(data1[,5],fill=data1[,as.numeric(input$var)],position="fill")+scale_x_discrete("razred")+
        scale_y_continuous("procenat lekcija",labels=percent)+ theme(legend.title=element_blank())
    },height = 300, width = 400)
    
    
    output$table1osi <- renderTable({ 
      
      #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      prop.table(table(data1[,as.numeric(input$var)],droplevels(data1[,5])),margin = 2)*100
    })
    
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
