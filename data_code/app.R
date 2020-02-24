# Shiny App - Chicago Crime Data Analysis
# Last Updated - 10/26/2019

library('ggplot2')
library('reshape')
library('dplyr')
library('lubridate')
library('leaflet')
library('stringi')
library('htmltools')
library('shiny')

# Loading Data
crime_2018 <- read.csv('Crimes_-_2018.csv', header = T)
# dim(crime_2018)
# head(crime_2018)

crime_df <- as.data.frame(crime_2018)%>% select(Case.Number, Date, Primary.Type, Location.Description,Latitude,
                                                Longitude)
rename
# dim(crime_df)
# head(crime_df)
# str(crime_df)

crime_df$Month <- months(strptime(crime_df$Date, "%m/%d/%Y %I:%M:%S %p"))
crime_df$TimeOfDay <- hour(strptime(crime_df$Date, "%m/%d/%Y %I:%M:%S %p"))
crime_df$Date <- as.Date(crime_df$Date, format = "%m/%d/%Y")

crime_df <- distinct(crime_df, Case.Number, .keep_all= TRUE)
# summary(is.na(crime_df))

crime_df <- crime_df[complete.cases(crime_df),]

# length(unique(crime_df$Primary.Type))

crime_df$Primary.Type <- as.character(crime_df$Primary.Type)

crime_df$PrimaryTypeDCD <- ifelse(crime_df$Primary.Type %in% c('NARCOTICS', 'OTHER NARCOTIC VIOLATION'),
                                  "NARCOTICS", crime_df$Primary.Type)

crime_df$PrimaryTypeDCD <- ifelse(crime_df$Primary.Type %in% c('ROBBERY', 'THEFT', 'BURGLARY', 'MOTOR VEHICLE THEFT'),
                                  "THEFT", crime_df$PrimaryTypeDCD)

crime_df$PrimaryTypeDCD <- ifelse(crime_df$PrimaryTypeDCD %in% c('PUBLIC PEACE VIOLATION', 'INTERFERENCE WITH PUBLIC OFFICER'
                                                                 ,'LIQUOR LAW VIOLATION', 'STALKING', 'GAMBLING', 
                                                                 'INTIMIDATION', 'OBSCENITY', 'NON-CRIMINAL', 'PUBLIC INDECENCY',
                                                                 'NON-CRIMINAL (SUBJECT SPECIFIED)'),"NON VIOLENCE", crime_df$PrimaryTypeDCD)

crime_df$PrimaryTypeDCD <- ifelse(crime_df$PrimaryTypeDCD %in% c('CRIM SEXUAL ASSAULT', 'SEX OFFENSE', 'PROSTITUTION')
                                  , "SEXUAL", crime_df$PrimaryTypeDCD)

crime_df$PrimaryTypeDCD <- ifelse(crime_df$PrimaryTypeDCD %in% c('WEAPONS VIOLATION', 'OFFENSE INVOLVING CHILDREN', 'KIDNAPPING', 
                                                                 'CONCEALED CARRY LICENSE VIOLATION', 'HUMAN TRAFFICKING'), "VIOLENCE", crime_df$PrimaryTypeDCD)

crime_df$PrimaryTypeDCD <- ifelse(crime_df$PrimaryTypeDCD %in% c('CRIMINAL DAMAGE'), "DAMAGE", crime_df$PrimaryTypeDCD)
crime_df$PrimaryTypeDCD <- ifelse(crime_df$PrimaryTypeDCD %in% c('CRIMINAL TRESPASS'), "TRESPASS", crime_df$PrimaryTypeDCD)
crime_df$PrimaryTypeDCD <- ifelse(crime_df$PrimaryTypeDCD %in% c('DECEPTIVE PRACTICE'), "DECEPTIVE", crime_df$PrimaryTypeDCD)
crime_df$PrimaryTypeDCD <- ifelse(crime_df$PrimaryTypeDCD %in% c('OTHER OFFENSE'), "OTHERS", crime_df$PrimaryTypeDCD)

crime_df$PrimaryTypeDCD <- stri_trans_totitle(crime_df$PrimaryTypeDCD)
crime_df$Location.Description <- stri_trans_totitle(crime_df$Location.Description)


FreqMonthCrimeType <- crime_df %>% group_by(PrimaryTypeDCD,Month) %>% summarise(Freq = n())

FreqCrimeByLocation <- crime_df %>% group_by(PrimaryTypeDCD,Location.Description) %>% summarise(Freq = n())

FreqCrimeTime <- crime_df %>% group_by(PrimaryTypeDCD, TimeOfDay) %>% summarise(Freq = n())

month <- rev(unique(crime_df$Month))
Loc <- rev(sort(table(crime_df$Location.Description)))[1:15]
location <- names(Loc)

# Defining the UI


ui <- fluidPage(
  
  # Tab layout with a input and output definitions
  navbarPage("Chicago Crime Analysis - 2018",
             
             tabPanel("Frequency of crime by month & type", selectInput("monthInput", "Select Month",
                                                                        choices = month),
                      mainPanel(
                        plotOutput("FreqofCrimeByMonthType"))), 
             tabPanel("Frequency of crime by location & type", selectInput("locationInput", "Select Location",
                                                                           choices = location),
                      mainPanel(
                        plotOutput("FreqofCrimeByLocationType"))),
             tabPanel("Heatmap of crime by time of day", mainPanel(
               plotOutput("FreqCrimeTime"))),
             tabPanel("Crimes by date on a map", dateInput("dateInputforMap", "Select Date", 
                                                           value = "2018-01-01",format = "yyyy-mm-dd",min = "2018-01-01", max = "2018-12-31"),
                      mainPanel(
                        leafletOutput("Map")))
  )
  
)

server <- function(input, output, session) {
  output$FreqofCrimeByMonthType <- renderPlot({
    
    plot_data1 <- filter(FreqMonthCrimeType, Month == input$monthInput) %>% select( PrimaryTypeDCD, Freq)
    ggplot(plot_data1, aes(x=PrimaryTypeDCD, y=Freq))  + geom_bar(stat="identity", width=0.8, fill="orange1") +
      geom_text(aes(label=format(as.numeric(Freq),nsmall = 0, big.mark = ",")), vjust=1.6, color="black", size=3.5) + 
      labs(x = 'Crime Type', y = 'Frequency') + ggtitle('Frequency of Crime by Month & Type') +
      theme(plot.title = element_text(size = 20, face = 'bold', hjust = 0.5))
    
  })
  
  output$FreqofCrimeByLocationType <- renderPlot({
    
    plot_data2 <- filter(FreqCrimeByLocation, Location.Description == input$locationInput) %>% select( PrimaryTypeDCD, Freq)
    ggplot(plot_data2, aes(x=PrimaryTypeDCD, y=Freq)) + 
      geom_bar(stat="identity", width=0.8, fill="orange1") +
      geom_text(aes(label=format(as.numeric(Freq),nsmall = 0, big.mark = ",")), vjust=1.6, color="black", size=3.5) + 
      labs(x = 'Crime Type', y = 'Frequency') + ggtitle('Frequency of Crime by Location & Type') +
      theme(plot.title = element_text(size = 20, face = 'bold', hjust = 0.5))
    
  })
  
  output$FreqCrimeTime <- renderPlot({
    
    ggplot(FreqCrimeTime, aes(x=PrimaryTypeDCD, y=TimeOfDay, fill = Freq )) + 
      geom_tile()+ scale_fill_gradient('Freq', low = 'peachpuff1', high = 'orange1') +
      geom_text(aes(label =format(as.numeric(FreqCrimeTime$Freq),nsmall = 0, big.mark = ",") )) + scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24)) +
      labs(x = 'Crime Type', y = 'Time of the day') + ggtitle('Crime Type vs Time of the day') +
      theme(plot.title = element_text(size = 20, face = 'bold', hjust = 0.5))
    
    
  })
  output$Map <- renderLeaflet({
    plot_data3 <- filter(crime_df, Date == input$dateInputforMap) %>% select(PrimaryTypeDCD, Location.Description, Latitude,
                                                                             Longitude, TimeOfDay)
    
    label <- lapply(seq(nrow(plot_data3)), function(i) {
      paste0( '<p>','<b> Crime Type: ', plot_data3[i, "PrimaryTypeDCD"] ,'</b>', '<p></p>', 
              '<b> Time of the day: ', plot_data3[i, "TimeOfDay"] , '</b>', '<p></p>',
              '<b> Location : ', plot_data3[i, "Location.Description"], '</b> ', '</p>' ) 
    })
    
    leaflet() %>% addTiles() %>% 
      addProviderTiles(providers$Stamen.TonerLite,options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = plot_data3,lng = ~Longitude, lat = ~Latitude, label =lapply(label, htmltools::HTML),
                 labelOptions = lapply(1:nrow(plot_data3), function(x) {
                   labelOptions(opacity=0.9)
                 })) 
    
    
  })
}


shinyApp(ui, server)

