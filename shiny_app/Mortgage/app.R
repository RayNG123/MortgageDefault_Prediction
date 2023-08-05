#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(sf)
library(tmap)
library(formattable)
library(shiny)
library(plotly)
mortgage_norm <- read_csv('dcr_norm.csv')
mortgage <- read_csv('dcr_clean.csv')
mortgage_norm <- mortgage_norm %>% mutate(x = first_time_date) %>% separate(x,into=c('year','month','day'), sep = '-') 
mortgage <- mortgage %>% mutate(state = state_orig_time)
mortgage$age<- mortgage$time-mortgage$orig_time
mortgage$TTM<-mortgage$mat_time-mortgage$orig_time
mortgage$date=paste(as.character(mortgage$year),as.character(mortgage$month),as.character(mortgage$day),sep='-')
mortgage_first<-mortgage[match(unique(mortgage$id), mortgage$id),]
mortgage_first$first_time_date<-mortgage_first$date
mortgage_first$first_time<-mortgage_first$time
first <- mortgage_first %>% select(id, first_time, orig_time,mat_time,rate_time,REtype_CO_orig_time,REtype_PU_orig_time,REtype_SF_orig_time,investor_orig_time,balance_orig_time:Interest_Rate_orig_time,state_orig_time,hpi_orig_time,first_time_date, age, TTM)
get_last <- mortgage %>% select(id,date,time,default_time,payoff_time,status_time) %>% group_by(id) %>% summarise_all(last)
get_last$last_time_date<-get_last$date
get_last$last_time<-get_last$time
get_last$status_last<-get_last$status_time
atlast <- get_last %>% select(id,last_time_date:status_last,default_time)
meanvalue <- mortgage %>% group_by(id) %>% summarise(interest_rate_mean = mean(interest_rate_time),gdp_mean = mean(gdp_time), risk_free_mean = mean(rate_time),hpi_mean = mean(hpi_time),uer_mean = mean(uer_time))
mortgage_all <- first %>% left_join(atlast, by = 'id') %>% left_join(meanvalue, by = 'id')
year <- mortgage %>% pull(year) %>% unique %>% sort()
state <- mortgage %>% pull(state_orig_time) %>% unique() %>% sort()


x <- variable.names(mortgage)
y <- variable.names(mortgage)
ui <- navbarPage("Interactive page of our mortgage visualization",
                 tabPanel("Insights of Default behaviors from Regional Factors",sidebarLayout(
                     sidebarPanel(sliderInput("year2", "Year Range:",
                                                           min = 2000, max = 2014,
                                                           value = c(2000,2014)),
                         sliderInput("age2","Age Range:",min = 1,max = 100,value = c(1,100)),
                         sliderInput("fico2","Fico Score Range:",min = 400,max = 840,value = c(400,840)),
                         sliderInput("ltv2","LTV Ratio Range",min = 50.1,max = 218.5,value = c(50.1,218.5))
                         
                     ), 
                     mainPanel( checkboxGroupInput(inputId = "status",
                                                   label = "status",
                                                   choices = c("dafault","payback","haven't reach maturity"),
                                                   selected = c("dafault","payback","haven't reach maturity"
                                                                )),
                                plotOutput("plot2"))
                 )),
                 tabPanel(
                     "Distribution of Investors' Traits among Regions/Years",
                     sidebarLayout(
                       sidebarPanel(selectInput(inputId = "weidu3",
                                                label = "Select Age/ Family Status/ Adjustability/Investor Status",
                                                choices = c("Family Status","Age","Adjust-ability","Investor Status"),selected = "Balance"),
                                    
                                    selectInput(inputId = "state3",label = "state",
                                                choices = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS","KY", "LA", "MA", "MD", "ME" ,"MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV",
                                                            "NY", "OH", "OK", "OR", "PA" ,"PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VI", "VT" ,"WA", "WI",
                                                            "WV" ,"WY"),
                                                selected = "AK"),
                                    
                                    sliderInput("year3", "Year Range:",
                                                min = 2000, max = 2014,
                                                value = c(2000,2014))
                                 
                         ),
                         mainPanel( plotOutput("plot3"))
                     )),
                 tabPanel(
                     "Insights of Default behaviors from Socialeconomic Factors",
                     sidebarLayout(
                         sidebarPanel(selectInput(inputId = "weidu4",
                                               label = "Select Balance Size/ Loan to Value Ratio/ FICO Score",
                                               choices = c("Balance","Loan to Value Ratio","FICO Score"),selected = "Balance"),
                                               
                                      sliderInput("age4","Age Range:",min = 18,max = 80,value = c(18,80))
                                      ,
                                      sliderInput("year4", "Year Range:",
                                                               min = 2000, max = 2014,
                                                               value = c(2000,2014))
                                            
                                                   
                         ),
                         mainPanel( plotOutput("plot4"))
                     )),
                 tabPanel(
                     "Insights of Default behaviors from personal Financial Situations",
                     sidebarLayout(
                         sidebarPanel(
                            selectInput(inputId = "weidu5",
                                                label = "Select Unemployment Rate/ Interest/ GDP Growth",
                                                choices = c("unemployment_rate","gdp","risk_free_rate","interest_rate","house_price_index"),selected = "gdp"),
                            
                            sliderInput("age5","Age Range:",min = 18,max = 80,value = c(18,80))
                            ,
                            sliderInput("year5", "Year Range:",
                                                     min = 2000, max = 2014,
                                                     value = c(2000,2014))
                         ),
                         mainPanel( plotOutput("plot5"))
                     )),
                 tabPanel(
                         "Create Your Own Insights!!!!",
                         sidebarLayout(
                             sidebarPanel(
                               selectInput(inputId = "x6",
                                           label = "Select X-axis Variable",
                                           choices = c("gdp_mean","risk_free_mean","hpi_mean","interest_rate_mean","LTV_orig_time","FICO_orig_time"),selected = "gdp_mean"),
                               selectInput(inputId = "y6",
                                           label = "Select Y-axis Variable",
                                           choices = c("gdp_mean","risk_free_mean","hpi_mean","interest_rate_mean","LTV_orig_time","FICO_orig_time"),selected = "interest_rate_mean"),
                               selectInput(inputId = "c6",
                                           label = "Select Color Variable",
                                           choices = c("gdp_mean","risk_free_mean","hpi_mean","interest_rate_mean","LTV_orig_time","FICO_orig_time"),selected = "LTV_orig_time"),
                               selectInput(inputId = "s6",
                                           label = "Select Size Variable",
                                           choices = c("gdp_mean","risk_free_mean","hpi_mean","interest_rate_mean","LTV_orig_time","FICO_orig_time"),selected = "FICO_orig_time")
                                 
                                 
                               )
                             ,
                             mainPanel( plotOutput("plot6"))
                         )),
                 tabPanel(
                     "Component 7",
                     sidebarLayout(
                         sidebarPanel(
                             checkboxGroupInput(inputId = "state",
                                                label = "state",
                                                choices = state)
                         ),
                         mainPanel( plotOutput("plot7"))
                     )),
                 tabPanel(
                     "Component 8",
                     sidebarLayout(
                         sidebarPanel(
                             checkboxGroupInput(inputId = "state",
                                                label = "state",
                                                choices = state)
                         ),
                         mainPanel( plotOutput("plot8"))
                     )),
                 tabPanel(
                     "Component 9",
                     sidebarLayout(
                         sidebarPanel(
                             checkboxGroupInput(inputId = "state",
                                                label = "state",
                                                choices = state)
                         ),
                         mainPanel( plotOutput("plot9"))
                     )),
                 tabPanel(
                     "Component 10",
                     sidebarLayout(
                         sidebarPanel(
                             checkboxGroupInput(inputId = "state",
                                                label = "state",
                                                choices = state)
                         ),
                         mainPanel( plotOutput("plot10"))
                     ))
)

server <- function(input, output) {
    output$distPlot <- renderPlot({
        plot1 <- mortgage %>% filter(state_orig_time %in% input$state)
        plot1 <-plot1 %>% count(default_time, investor_orig_time) %>% mutate(SF = ifelse(investor_orig_time == 1, 'With Investor','Without Investor')) %>% mutate(Default = ifelse(default_time == 1, 'Default','Payoff'))
        ggplot(plot1) + geom_bar(aes(x= factor(SF), y = n, fill= factor(Default)),position = 'dodge', stat='identity') + geom_text(aes(x= factor(SF), y = n,label= n), color = 'white',size = 3.5, position = position_dodge(width=0.9), vjust=1.6,fontface = 'bold')+ theme_bw() + labs(x = '', y = 'Total Number', title = 'Compare Default or Not and Investor Status',fill = 'Mortgage Status')+ theme(plot.title = element_text(face="bold",size = 15))+ scale_fill_manual(values = c('steelblue4','steelblue2'))
        
    })
    
    
    
    output$plot2 <- renderPlot({ 
        location <-read_csv('world_country_and_usa_states_latitude_and_longitude_values.csv')
        p <-left_join(mortgage, location, by = c("state_orig_time"="usa_state_code"))
        p <- p %>% filter(age < input$age2[2] & age > input$age2[1]) %>% filter(FICO_orig_time< input$fico2[2] &FICO_orig_time > input$fico2[1])%>%
            filter(year < input$year2[2] & year > input$year2[1]) %>%
            filter(LTV_orig_time< input$ltv2[2] &LTV_orig_time > input$ltv2[1])  %>% 
            mutate(status = ifelse(status_time == 1,"default", (ifelse(status_time == 2, "payback","haven't reach maturity" )))) %>%
            filter(status %in% input$status) %>%
            group_by(state_orig_time) %>% summarize(usa_state_longitude = mean(usa_state_longitude),
                                                    usa_state_latitude = mean(usa_state_latitude),count = n())
        
        epsg_us_equal_area <- 2163
        
        us_states <- st_read("cb_2019_us_state_20m/cb_2019_us_state_20m.shp")%>% 
            st_transform(epsg_us_equal_area)
        not_contiguous <-
            c("Guam", "Commonwealth of the Northern Mariana Islands",
              "American Samoa", "Puerto Rico", "Alaska", "Hawaii",
              "United States Virgin Islands")
        
        us_cont <- us_states %>%
            filter(!NAME %in% not_contiguous) %>% filter(NAME != "District of Columbia") %>%
            transmute(STUSPS,STATEFP,NAME, geometry)
        p <- us_cont %>% inner_join(p, by = c("STUSPS" = "state_orig_time"))
        ggplot() + 
            geom_sf(data=p,size=0.1) +
            geom_sf(data=p,lwd=2,aes(fill=count))   
    })
    
    
    output$plot3 <- renderPlot({
      B <- mortgage_norm %>% filter(year >= input$year3[1] & year <=input$year3[2])  %>%filter(state_orig_time %in% input$state3)
     
       if (input$weidu3 == "Age") {
        B <- B %>% mutate(adj = ifelse(age > 18,'Age_over_18','Age_Under_18')) %>% count(adj)
        names(B)[2] = c('Total_Number_of_Mortgages')
        B %>% ggplot(aes(x="", y=Total_Number_of_Mortgages, fill =factor(adj)))+ geom_bar(width = 1, stat = "identity",color ='white',size =2,position = "fill")+coord_polar("y", start=0) + theme_bw()+theme(panel.grid=element_blank()) + scale_fill_brewer(palette="Blues") + theme(axis.title.y = element_blank())+ labs(title ='Portions of Age over 18 or Not',fill = 'Color Indicator', x = 'Number of Mortgages') + theme(plot.title = element_text(face="bold",size = 12)) + scale_y_continuous(labels = scales::percent_format()) + scale_fill_manual(values = c('steelblue4','steelblue2'))
      } else{
        if(input$weidu3 == "Adjust-ability"){
          B <- B %>% mutate(adj = ifelse(Interest_Rate_orig_time != interest_rate_mean,'Adjustable','Non_Adjustable')) %>% count(adj)
          names(B)[2] = c('Total_Number_of_Mortgages')
          B %>% ggplot(aes(x="", y=Total_Number_of_Mortgages, fill =factor(adj)))+ geom_bar(width = 1, stat = "identity",color ='white',size =2,position = "fill")+coord_polar("y", start=0) + theme_bw()+theme(panel.grid=element_blank()) + scale_fill_brewer(palette="Blues") + theme(axis.title.y = element_blank())+ labs(title ='Portions of Interest Adjustable or Non Adjustable',fill = 'Color Indicator', x = 'Number of Mortgages') + theme(plot.title = element_text(face="bold",size = 12)) + scale_y_continuous(labels = scales::percent_format()) + scale_fill_manual(values = c('steelblue4','steelblue2'))
        } else {
          if(input$weidu3 == "Family Status"){
            B <- B %>% mutate(adj = ifelse(REtype_SF_orig_time == 1, 'Single_Family','Not_Single_Family')) %>% count(adj)
            names(B)[2] = c('Total_Number_of_Mortgages')
            B %>% ggplot(aes(x="", y=Total_Number_of_Mortgages, fill =factor(adj)))+ geom_bar(width = 1, stat = "identity",color ='white',size =2,position = "fill")+coord_polar("y", start=0) + theme_bw()+theme(panel.grid=element_blank()) + scale_fill_brewer(palette="Blues") + theme(axis.title.y = element_blank())+ labs(title ='Portions of Single Family or Not',fill = 'Color Indicator', x = 'Number of Mortgages') + theme(plot.title = element_text(face="bold",size = 12)) + scale_y_continuous(labels = scales::percent_format()) + scale_fill_manual(values = c('steelblue4','steelblue2'))
          } else{
          if (input$weidu3 == "Investor Status"){
            B <- B %>% mutate(adj = ifelse(investor_orig_time == 1, 'With Investor','Without Investor')) %>% count(adj)
            names(B)[2] = c('Total_Number_of_Mortgages')
            B %>% ggplot(aes(x="", y=Total_Number_of_Mortgages, fill =factor(adj)))+ geom_bar(width = 1, stat = "identity",color ='white',size =2,position = "fill")+coord_polar("y", start=0) + theme_bw()+theme(panel.grid=element_blank()) + scale_fill_brewer(palette="Blues") + theme(axis.title.y = element_blank())+ labs(title ='Portions of Mortgages With Investor or Without Investor',fill = 'Color Indicator', x = 'Number of Mortgages') + theme(plot.title = element_text(face="bold",size = 12)) + scale_y_continuous(labels = scales::percent_format()) + scale_fill_manual(values = c('steelblue4','steelblue2'))
            
            }}
        }
      }
      
    })
    
    
    output$plot4 <- renderPlot({
        m <- mortgage_norm %>% filter(year >= input$year4[1] & year <=input$year4[2])  %>% filter(age >= input$age4[1] & age <= input$age4[2])
        if(input$weidu4 == "FICO Score") {
            
            m %>% filter(status_last!= 0) %>% mutate(status = ifelse(default_time == 1, 'Default','Payoff')) %>%ggplot(aes(x=FICO_orig_time, fill= status))+ geom_density(alpha=.5,colour = 'white')+  theme_bw()+ scale_fill_manual(name = 'Distribution',values=c("steelblue4", "steelblue1")) + labs(x = "FICO Score", y = 'Density', title = 'Mortgages Distribution Map According to FICO Score') + theme(plot.title = element_text(face="bold",size = 15))
            }else {
            if(input$weidu4 == "Loan to Value Ratio"){
                m %>% filter(status_last!= 0) %>% filter(LTV_orig_time <110)%>% mutate(status = ifelse(default_time == 1, 'Default','Payoff')) %>%ggplot(aes(x=LTV_orig_time, fill= status))+ geom_density(alpha=.5,colour = 'white')+  theme_bw()+ scale_fill_manual(name = 'Distribution',values=c("steelblue4", "steelblue1")) + labs(x = "FICO Score", y = 'Density', title = 'Mortgages Distribution Map According to Loan to Value Ratio') + theme(plot.title = element_text(face="bold",size = 15))
                
            }else{
                if(input$weidu4 == "Balance"){
                    m %>% filter(status_last!= 0) %>% filter(balance_orig_time < 336000) %>% mutate(status = ifelse(default_time == 1, 'Default','Payoff')) %>%ggplot(aes(x=balance_orig_time, fill= status))+ geom_density(alpha=.5,colour = 'white')+  theme_bw()+ scale_fill_manual(name = 'Distribution',values=c("steelblue4", "steelblue1")) + labs(x = "FICO Score", y = 'Density', title = 'Mortgages Distribution Map According to Balance size') + theme(plot.title = element_text(face="bold",size = 15))
                    
                    
                    }
            }
        }
        
      })
    
    
    output$plot5 <- renderPlot({
        n <- mortgage_norm %>% filter(year >= input$year5[1] & year <= input$year5[2]) %>% filter(age >= input$age5[1] & age <= input$age5[2])
        nn <- mortgage %>% filter(year >= input$year5[1] & year <= input$year5[2]) %>% filter(age >= input$age5[1] & age <= input$age5[2])
        
            if (input$weidu5 == "unemployment_rate") {
            ggplot(data = n,aes(x=uer_mean,fill= factor(default_time)))+ geom_histogram(aes(fill= factor(default_time)), color = 'white', position='identity',binwidth = 0.2) + theme_bw()+ scale_fill_manual(name = 'Mortgage Status',values = c('steelblue4','steelblue2'),labels = c('Payoff','Default')) + labs(x = 'Unemployment Rate (%)', y = 'Total Mortgages',title = 'Compare Default Portion and Unemployment Rate')+ theme(plot.title = element_text(face="bold",size = 13)) 
            
            } else{if (input$weidu5 == "interest_rate"){
                n <- n %>% filter(interest_rate_mean < 15)
            ggplot(data = n,aes(x=interest_rate_mean,fill= factor(default_time)))+ geom_histogram(aes(fill= factor(default_time)), color = 'white', position='identity',binwidth = 0.2) + theme_bw()+ scale_fill_manual(name = 'Mortgage Status',values = c('steelblue4','steelblue2'),labels = c('Payoff','Default')) + labs(x = 'Interest Rate (%)', y = 'Total Mortgages',title = 'Compare Default Portion and Interest Rate')+ theme(plot.title = element_text(face="bold",size = 13)) 
            
        }else{if (input$weidu5 == "risk_free_rate"){
                 ggplot(data = n,aes(x=risk_free_mean,fill= factor(default_time)))+ geom_histogram(aes(fill= factor(default_time)), color = 'white', position='identity',binwidth = 0.2) + theme_bw()+ scale_fill_manual(name = 'Mortgage Status',values = c('steelblue4','steelblue2'),labels = c('Payoff','Default')) + labs(x = 'Risk Free Rate (%)', y = 'Total Mortgages',title = 'Compare Default Portion and Risk Free Rate')+ theme(plot.title = element_text(face="bold",size = 13)) 
                
            }else{if (input$weidu5 == "gdp") {ggplot(data = n, aes(x=gdp_mean,fill= factor(default_time)))+ geom_histogram(aes(fill= factor(default_time)), color = 'white', position='identity',binwidth = 0.2) + theme_bw()+ scale_fill_manual(name = 'Mortgage Status',values = c('steelblue4','steelblue2'),labels = c('Payoff','Default')) + labs(x = 'GDP Growth Rate (%)', y = 'Total Mortgages',title = 'Compare Default Portion and GDP Growth Rate')+ theme(plot.title = element_text(face="bold",size = 13)) 
            } 
              }}}
        })
    
    
    
    output$plot6 <- renderPlot({
      xx <- as.numeric(input$x6)
      yy<- as.numeric(input$y6)
      cc <- as.numeric(input$c6)
      ss <-as.numeric(input$s6)
      ggplot(mortgage_norm) + theme_bw() + labs(x = xx, y = yy, color = cc, size = ss,title="Compariso")+ theme(plot.title = element_text(face="bold",size = 12))  + geom_point(aes(x = xx, y= yy,color = cc,size = ss),shape = 15)+ scale_color_gradient(low = 'skyblue1', high = 'dodgerblue4')
      
      })
    
    
    
    
    output$plot7 <- renderPlot({
        GDP <- mortgage %>% group_by(year) %>% summarise(Mean_gdp = mean(gdp_time))
        f <- mortgage %>% filter(default_time == 0) %>% count(year, default_time) 
        plot7 <- mortgage %>% filter(default_time == 1) %>% count(year, default_time) %>% left_join(f, by = 'year') %>% mutate(default_Rate = n.x/n.y)  %>% left_join(GDP, by = 'year') 
         ggplot(plot7) + geom_line(aes(x = year, y = default_Rate*100,color = "Default Rate"),size = 0.7) + geom_line(aes(x = year, y = Mean_gdp,color = "GDP Growth"),size = 0.7) + geom_point(aes(x = year, y = default_Rate*100,color = "Default Rate"),shape = 19,size = 1.5) + geom_point(aes(x = year, y = Mean_gdp,color = "GDP Growth"),shape = 19,size = 1.5) + scale_y_continuous(name = "GDP Growth", sec.axis = sec_axis(~.*1/100, name="Defualt Rate"))+ theme_bw()+theme(axis.line.y = element_line(color = "steelblue4",size = 2),axis.line.y.right = element_line(color = "steelblue2",size = 2),axis.text.y.right = element_text(color = "steelblue2"), axis.text.y = element_text(color = "steelblue4"),axis.title.y = element_text(color = "steelblue4"),axis.title.y.right = element_text(color = "steelblue2"),plot.title = element_text(face="bold",size = 15)) +labs(x = 'Year',title = 'Compare GDP Growth and Default Rate') + scale_color_manual(name = 'Indexes',breaks = c('GDP Growth','Default Rate'), values=c('GDP Growth'='steelblue4','Default Rate'= 'steelblue2'))
    })
    output$plot8 <- renderPlot({ 
        
        mortgage_norm %>% count(default_time, REtype_SF_orig_time) %>% mutate(SF = ifelse(REtype_SF_orig_time == 1, 'SingleFamily','Non_SingleFamily')) %>% mutate(Default = ifelse(default_time == 1, 'Default','Payoff'))%>% ggplot(aes(x= factor(SF), y = n, fill= factor(Default))) + geom_bar(position = 'dodge', stat='identity') + geom_text(aes(label= n), color = 'white',size = 4, position = position_dodge(width=0.9), vjust=5, fontface = 'bold')+ theme_bw() + labs(x = '', y = 'Total Number', title = 'Compare Default or Not and Single Family Status',fill = 'Mortgage Status')+ theme(plot.title = element_text(face="bold",size = 15))+ scale_fill_manual(values = c('steelblue4','steelblue2'))
        })
    output$plot9 <- renderPlot({ 
        mortgage_norm %>% count(default_time, investor_orig_time) %>% mutate(SF = ifelse(investor_orig_time == 1, 'With Investor','Without Investor')) %>% mutate(Default = ifelse(default_time == 1, 'Default','Payoff'))%>% ggplot(aes(x= factor(SF), y = n, fill= factor(Default))) + geom_bar(position = 'dodge', stat='identity') + geom_text(aes(label= n), color = 'white',size = 3.5, position = position_dodge(width=0.9), vjust=1.6,fontface = 'bold')+ theme_bw() + labs(x = '', y = 'Total Number', title = 'Compare Default or Not and Investor Status',fill = 'Mortgage Status')+ theme(plot.title = element_text(face="bold",size = 15))+ scale_fill_manual(values = c('steelblue4','steelblue2'))
        })
    output$plot10 <- renderPlot({ 
        mortgage_norm %>% mutate(Default = ifelse(default_time == 1, 'Default','Payoff')) %>% filter(balance_orig_time <= 650000) %>% ggplot() + geom_boxplot(aes(x = factor(Default), y = balance_orig_time, fill = factor(Default)),size = 0.5, color = 'grey40') + theme_bw()+ scale_fill_manual(values = c('steelblue4','steelblue2')) + labs(y = 'Mortgage Size', x = '', title = 'Compare Default Status and Mortgage Size', fill = 'Mortgage Status')+ theme(plot.title = element_text(face="bold",size = 15))
        })
    output$plot11 <- renderPlot({ 
        
        mortgage_norm %>% mutate(Default = ifelse(default_time == 1, 'Default','Payoff')) %>% filter(balance_orig_time <= 650000) %>% ggplot() + geom_boxplot(aes(x = factor(Default), y = balance_orig_time, fill = factor(Default)),size = 0.5, color = 'grey40') + theme_bw()+ scale_fill_manual(values = c('steelblue4','steelblue2')) + labs(y = 'Mortgage Size', x = '', title = 'Compare Default Status and Mortgage Size', fill = 'Mortgage Status')+ theme(plot.title = element_text(face="bold",size = 15))
        })
}

shinyApp(ui = ui, server = server)

