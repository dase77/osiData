library(shiny)
library(ggplot2)
library(stringr)
library(scales)
library(wordcloud)

data=read.csv("historija (copy).csv",sep=";",na.strings=c(" ","","\"\"","NA","N/A")) # load tha data
geografija=read.csv("geografija.csv",sep=";",na.strings=c(" ","","\"\"","NA","N/A")) # load tha data
vjeronauk=read.csv("vjeronauk.csv",sep=";",na.strings=c(" ","","\"\"","NA","N/A")) # load tha data
knjizevnost=read.csv("V5baza_jezik i knjizevnost_cijela.csv",sep=";",na.strings=c(" ","","\"\"","NA","N/A")) # load tha data
levels(knjizevnost$Negativni.oblici.ponašanja)[c(3)]=c("Ne")
knjizevnost=knjizevnost[,-27]



names(vjeronauk)=names(data) # same names in order to merge data
names(geografija)=names(data) # same names in order to merge data
names(knjizevnost)=names(data) # same names in order to merge data

data=rbind(data,vjeronauk,geografija,knjizevnost)

levels(data$Negativni.oblici.ponašanja)[c(3,4)]=c("Da","Ne")
levels(data$Univerzalne.vrijednosti)[c(3,4)]=c("Da","Ne")
levels(data$Predmet)[2]="Vjeronauka"
levels(data[,12])[3]="Ne"


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
      
      ### if predmet is selected
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
      
      ### if predmet is selected
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
      
      ### if predmet is selected
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
      ### if predmet is selected
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
       names(c)=c("Primjer","frekvencija")
       c
    },include.rownames=FALSE)
    
    ############################################ table with examples * predmet
    output$table4osi <- renderTable({ 
      
      #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      
      data2=data1[!is.na(data1[,as.numeric(input$var)+1]),]
      data2=data.frame(data2[,as.numeric(input$var)+1],data2[,3])
      huh=levels(data2[,2])
      l=lapply(huh,function(x) data2[data2[,2]==x,])
      
      ls=list()
      for(i in 1:length(huh)){
        b=data.frame(l[i])# unlisting
        b=data.frame(l[i])[,1]# unlisting 
        b=as.character(b) # converting to character
        b[b==""]=NA # empty to NA
        b[b==" "]=NA # empty to NA
        b=na.omit(b) #remove NA
        b=as.character(b) # converting to character
        b=str_split(b," ") # breaking the strings
        b=unlist(b) 
        b=str_replace(b,",","") #removing comma
        b=str_trim(b) #removing whitespaces
        b[b==""]=NA # empty to NA
        b=na.omit(b) #remove NA
        c=str_detect(b, "-") #detect -
        d=data.frame(b,c)
        d=d[d[,2]!=TRUE,1]
        d=as.character(d) # converting to character
        d=data.frame(table(d))
        d=d[order(-d[,2]),]
        
        len=length(d) #so we can set how long the data is going to be
        
        #adding predmet
        d1=data.frame(d,rep(huh[i],dim(d)[1]))
        ls[[i]]=d1
       
      }
      ls=do.call(rbind, ls) ####merging the list of dataframes - rbindlist(ls) in the data.table package seems to be 
      #the fastet way but is masks melt from reshape
      names(ls)=c("Primjer","Frekvencija","Predmet")
      
      print(ls)
    },include.rownames=FALSE)
    
   
    ############################################ načini - table with examples * predmet
    output$table5osi <- renderPrint({ 
      
      #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      
      if(input$var != 12)
        {
        print(table("Molimo odaberite Partikularizaciju"))
      }
      else
        
      data
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      
      
      
      data2=data1[!is.na(data1[,as.numeric(input$var)+2]),]
      data2=data.frame(data2[,as.numeric(input$var)+2],data2[,3])
      huh=levels(data2[,2])
      l=lapply(huh,function(x) data2[data2[,2]==x,])
      
      ls=list()
      for(i in 1:length(huh)){
        b=data.frame(l[i])# unlisting
        b=data.frame(l[i])[,1]# unlisting 
        b=as.character(b) # converting to character
        b[b==""]=NA # empty to NA
        b[b==" "]=NA # empty to NA
        b=na.omit(b) #remove NA
        b=as.character(b) # converting to character
        b=str_split(b," ") # breaking the strings
        b=unlist(b) 
        b=str_replace(b,",","") #removing comma
        b=str_trim(b) #removing whitespaces
        b[b==""]=NA # empty to NA
        b=na.omit(b) #remove NA
        c=str_detect(b, "-") #detect -
        d=data.frame(b,c)
        d=d[d[,2]!=TRUE,1]
        d=as.character(d) # converting to character
        d=data.frame(table(d))
        d=d[order(-d[,2]),]
        
        len=length(d) #so we can set how long the data is going to be
        
        #adding predmet
        d1=data.frame(d,rep(huh[i],dim(d)[1]))
        ls[[i]]=d1
        
      }
      ls=do.call(rbind, ls) ####merging the list of dataframes - rbindlist(ls) in the data.table package seems to be 
      #the fastet way but is masks melt from reshape
      names(ls)=c("Primjer","Frekvencija","Predmet")
      
      print(ls,row.names=F)
    })
    
    
    
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
        
    
  }
)
