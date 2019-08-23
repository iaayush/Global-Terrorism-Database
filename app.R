
#importing libraries required
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(shinythemes)
library(plyr)
library(ggplot2)
library(reshape)
library(dplyr)

# reading the dataframes 
terrorism <- read.csv("gtd_final.csv", header = TRUE)
names(terrorism)[1] <- paste("year")  #renaming column
region_target <- read.csv("region_target_year.csv", header = TRUE)
names(region_target)[1] <- paste("year")  #renaming column
region_attack <- read.csv("region_attack_type.csv", header = TRUE)
names(region_attack)[1] <- paste("year")  #renaming column

# Define UI for application that draws Navigation Bar
ui <- navbarPage(
  "Global Terrorism",
  theme = shinytheme("cosmo"),
  tabPanel(
    # First panel of Navbar
    "Introduction",
           fluidPage(
             column(
               6,
               p(h4("Overview")),
               p(
                 "Global Terrorism Dashboard provides a comprehensive summary of the key global trends and patterns in terrorism from 1970 to 2016."
               ),
               p(
                 "The research presented in this report highlights a complex and rapidly changing set of dynamics in global terrorism. While on
                 the one hand the top-line statistics highlight an improvement in the levels of global terrorism, the continued intensification
                 of terrorism in some countries is a cause for serious concern, and highlights the fluid nature of modern terrorist activity."
               ),
               p(h4("By Country")),
               p(
                 "Will give you an overview of terrorism for the choosen country in selected timeline, the leaflet includes clustering
                 that provides an idea of region most prodominent of terrorist attacks, further investigation and zoom will give information about
                 the attack that happened in location providing information like date, fatalities, injured. First line chart gives idea on about the pattern of
                 fatalities and injured. Second line chart gives pattern of Number of attacks for the country."
               ),
               p(h4("By Region")),
               p(
                 "Provides you with information by region(continental view). You can view information on Type of Attacks and Type of preferred Target and Review the number of Fatalities and Attacks for selected year."
               ),
               p(h4("Summary")),
               p("Results and Trends"),
               p(h4("References"))
               
               ),
             # adding word cloud on intro page
             column(6, img(src = 'Terrorism-Word-Cloud.jpg', align = "centre"))
             
               )),
  
  
  
  tabPanel(
    #Third Panel in navbar
    "By Country",
    sidebarPanel(
      # Slider Input to select range for year
      sliderInput(
        inputId = "timeline",
        label = "Timeline",
        min = as.numeric(min(terrorism$year)),
        max = as.numeric(max(terrorism$year)),
        value = c(as.numeric(min(terrorism$year)), as.numeric(max(terrorism$year)))
      ),
      
      
    #Drop Down menu to select country  
      selectInput(
        inputId = "country",
        label = "Select Country",
        choices = (unique(terrorism$country_txt))
      )
      
      
    ),
    
    mainPanel(
      includeCSS("myStyles.css"), #css script
      
      # Java script to modify value box
      tags$style(
        HTML(
          '.small-box {padding:2px;padding-left:10px;background-color:#ff7b25;color:White;min-height: 30px; border-radius: 5px; margin-bottom: 10px;}
          .icon-large {font-size: 20px !important;}
          .small-box h3 {font-size: 14px;font-weight:bold}
          .small-box p {font-size: 14px; font-weight: 700;}
          icon::before{p;}'
        )
        ),
      
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Map",
          hr(style = "margin: 15px;"),
          fluidRow(
            valueBoxOutput("TotalBox"), #adding Number of Incident valuebox
            valueBoxOutput("DeathBox"), #adding Number of Fatalities valuebox
            valueBoxOutput("InjuredBox") #adding Number of Injured valuebox
          ),
          # output of leaflet
          leafletOutput("leaflet")
        ),
        tabPanel(
          "Pattern",
          hr(style = "margin: 15px;"),
          plotlyOutput("countryfatal") # line chart using plotly
        ),
        tabPanel(
          "Attacks",
          hr(style = "margin: 15px;"),
          plotlyOutput("countryattacks") # line chart using plotly
        )
      )
      
        )
      ),
  
  tabPanel(
    #Third Panel in navbar
    "By Region",
    sidebarPanel(
      # Drop down menu to select year
      selectInput(
        inputId = "time_line",
        label = "Select Year",
        choices = (unique(region_attack$year)),
        "2016"
      )
      
    ),
    
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel(
        "Target",
        hr(style = "margin: 15px;"),
        plotlyOutput("regiontarget") # by region target type
      ),
      tabPanel(
        "Attack",
        hr(style = "margin: 15px;"),
        plotlyOutput("regionattack") # by region attack type
      ),
      tabPanel(
        "Fatalities",
        hr(style = "margin: 15px;"),
        plotlyOutput("deathattack")  # by region number of deaths and number of attacks
      )
    ))
    
    
  ),
  tabPanel(
    # fourth panel of Navbar
    "Summary",
           fluidPage(
             column(
               6,
               p(h1("Results")),
               p(
                 "1. Deaths caused by terrorism decreased by 13 per cent from
                 2015 to 2016. There were 25,673 deaths in 2016. This is the
                 second consecutive year that the number of deaths from
                 terrorism have decreased. Deaths have now fallen by 22 per
                 cent since the peak in 2014."
               ),
               p(
                 "2. Four of the five countries with the highest impact from
                 terrorism recorded a reduction in the number of deaths;
                 Afghanistan, Nigeria, Syria and Pakistan. Together with Iraq,
                 these five countries accounted for three quarters of all
                 deaths from terrorism in 2016."
               ),
               p(
                 "3. Nigeria saw the greatest reduction in deaths with 3,100
                 fewer people killed by terrorism in 2016 than in 2015. This
                 was due to an 80 per cent reduction in the number of
                 people killed by Boko Haram."
               ),
               p(
                 "4. Substantial decreases in deaths from
                 terrorism in Yemen, Afghanistan and Syria, which
                 collectively witnessed over 500 fewer deaths in 2016 than
                 in the prior year."
               ),
               p(
                 "5. Iraq experienced a 40 per cent increase in deaths in 2016
                 in reflecting the increased intensity of ISIL activity following
                 attacks by the Iraqi Armed Forces to reclaim several major
                 urban centres."
               )
               ),
             column(6, p(
               h1("Trends"),
               p(
                 "1. Since 2002, eight of the nine regions in the world
                 experienced an increase in terrorism. North America was
                 the only region to experience a reduced impact."
               ),
               p(
                 "2. Over the last 15 years, South Asia experienced the most
                 terrorist activity while Central and South America were
                 least affected. The MENA region had the sharpest increase in
                 terrorism."
               ),
               p(
                 "3. Egypt and Turkey witnessed very large increases in
                 terrorism following government crackdowns. In Egypt,
                 terrorism deaths increased nine-fold and in Turkey this figure
                 has increased by 16 times."
               ),
               p(
                 "4. Globally, attacks against civilians increased by 17 per
                 cent from 2015 to 2016. The primary targets of terrorists
                 are private citizens and property."
               ),
               p(
                 "5. Terrorist attacks are deadlier in conflict-affected
                 countries where there is an average of 2.4 fatalities per
                 attack in 2016 compared to 1.3 fatalities in non-conflict
                 countries."
               )
               ))
               )),
  tabPanel(
    # last panel of Navbar
    "References",fluidPage(column(6,p("1. https://rstudio.github.io/leaflet/shiny.html"),
                                         p("2. https://plot.ly/r/"),
                                         p("3. https://shiny.rstudio.com/gallery/"),
                                         p("4. https://rstudio.github.io/leaflet/basemaps.html"),
                                         p("5. http://economicsandpeace.org/wp-content/uploads/2016/11/Global-Terrorism-Index-2016.2.pdf"))))
               )






# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # defining the reactive values
  incidentsStats <- reactiveValues()
  
  
  # creating leaflet map based on input of timeline and country
  output$leaflet <- renderLeaflet({
    terrorism <-
      terrorism[terrorism$year >= input$timeline[1] &
                  terrorism$year <= input$timeline[2], ]
    terrorism <-
      terrorism[terrorism$country_txt == input$country, ]
    
    
    m <- leaflet(data = terrorism) %>% addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addCircleMarkers(
        clusterOptions = TRUE,
        popup = ~ paste(
          "<b>Date:</b>",
          terrorism$Date,
          "<br>",
          "<b>Attack Type:</b>",
          terrorism$Attack.Type,
          "<br>",
          "<b>Dead/Injured:</b>",
          terrorism$Fatalities,
          "/",
          terrorism$Injured ,
          "<br>",
          "<b>Group:</b>",
          terrorism$Terrorist.Group,
          "<br>",
          "<b>Summary:</b>",
          terrorism$Summary
        )
      ) %>%
      addMiniMap(tiles = providers$Esri.WorldImagery,
                 toggleDisplay = TRUE) # adding minimap
  })
  
  
  # creating infobox the values will be updated based on input of timeline and country
  output$TotalBox <- renderInfoBox({
    valueBox(incidentsStats$Incidents,
             "Incidents",
             icon = icon("hashtag", lib = "font-awesome"))
  })
  
  # creating infobox the values will be updated based on input of timeline and country
  output$DeathBox <- renderInfoBox({
    valueBox(incidentsStats$Injured,
             "Injured",
             icon = icon("medkit", lib = "font-awesome"))
  })
  
  # creating infobox the values will be updated based on input of timeline and country
  output$InjuredBox <- renderInfoBox({
    valueBox(incidentsStats$Fatalities,
             "Fatalities",
             icon = icon("bomb", lib = "font-awesome"))
  })
  
  
  # a function calculating the values that needs to be put in value box based on filter
  observe({
    terrorism <-
      terrorism[terrorism$year >= input$timeline[1] &
                  terrorism$year <= input$timeline[2], ]
    
    terrorism <-
      terrorism[terrorism$country_txt == input$country, ]
    
    
    incidentsStats$Incidents <- nrow(terrorism)
    incidentsStats$Injured <- sum(terrorism$Injured)
    incidentsStats$Fatalities <- sum(terrorism$Fatalities)
    
    
    
  })
  
  # creating plotly line chart for number of fatalities and injured
  output$countryfatal <- renderPlotly({
    
    # applying filter on data
    terrorism <-
      terrorism[terrorism$year >= input$timeline[1] &
                  terrorism$year <= input$timeline[2], ]
    terrorism <-
      terrorism[terrorism$country_txt == input$country, ]
    
    fatalyear <-
      aggregate(terrorism$Fatalities,
                by = list(year = terrorism$year),
                FUN = sum)
    names(fatalyear)[2] <- "yearfatal"
    injuredyear <-
      aggregate(terrorism$Injured,
                by = list(year = terrorism$year),
                FUN = sum)
    names(injuredyear)[2] <- "yearinjured"
    merged_data <- merge(fatalyear, injuredyear, by = 'year')
    
    p <-
      plot_ly(
        merged_data,
        x = ~ year,
        y = ~ yearfatal,
        name = 'Fatalities',
        type = 'scatter',
        mode = 'lines'
      ) %>%
      add_trace(y = ~ yearinjured,
                name = 'Injured',
                mode = 'lines+markers') %>%
      layout(
        title = paste(
          "NUMBER OF FATALITIES AND INJURED FROM",
          min(terrorism$year),
          "TO",
          max(terrorism$year)
        ),
        xaxis = list(title = 'YEARS'),
        yaxis = list(title = "FATALITIES/INJURED COUNT")
      )
    
  })
  
  # creating plotly line chart for number of attacks for given selection
  output$countryattacks <- renderPlotly({
    
    # applying filter on data
    terrorism <-
      terrorism[terrorism$year >= input$timeline[1] &
                  terrorism$year <= input$timeline[2], ]
    terrorism <-
      terrorism[terrorism$country_txt == input$country, ]
    
    terrorism <- plyr::count(terrorism, "year")
    
    p <-
      plot_ly(
        terrorism,
        x = ~ year,
        y = ~ freq,
        name = 'Incidents',
        type = 'scatter',
        mode = 'lines'
      ) %>%
      layout(
        title = paste(
          "NUMBER OF TERRORIST ATTACKS FROM",
          min(terrorism$year),
          "TO",
          max(terrorism$year)
        ),
        xaxis = list(title = 'YEARS'),
        yaxis = list(title = "NUMBER OF ATTACKS")
      )
    
  })
  
  output$regiontarget <- renderPlotly({
    
    # applying filter on data
    region_target <-
      region_target[region_target$year == input$time_line, ]
    
    # creating stacked bar chart for PERCENTAGE OF ATTACKS BY TARGET TYPE for given year
    ggplotly(
      ggplot(
        region_target,
        aes(x = Region, y = Percentage, fill = Target.Type)
      )
      + geom_bar(stat = "identity", position = "fill")
      + theme(plot.margin = unit(c(0, 0.5, 0.5, 1.2), "cm"))
      + theme(legend.position = "bottom")
      + theme(
        axis.text.x = element_text(
          angle = 90,
          hjust = 2,
          vjust = 0.5
        ),
        panel.background = element_rect(fill = "white", colour = "grey50")
      )
      + theme(legend.text = element_text(margin = margin(l = 50)))
      + theme(plot.title = element_text(margin = margin(10, 0, 10, 0)))
      + theme(axis.text.y = element_text(face = "bold"))
      + labs(x = "")
      + ggtitle(
        paste("PERCENTAGE OF ATTACKS BY TARGET TYPE,", region_target$year)
      )
    ) %>%
      layout(legend = list(
        orientation = "v",
        x = 0.35,
        y = -1.5
      )) %>% layout(autosize = F,
                    width = 850,
                    height = 500)
    
    
  })
  
  
  output$regionattack <- renderPlotly({
    
    #applying filter on data
    region_attack <-
      region_attack[region_attack$year == input$time_line, ]
    
    #creating ggplotly stacked bar chart for PERCENTAGE OF ATTACKS BY ATTACK TYPE in selected year
    ggplotly(
      ggplot(
        region_attack,
        aes(x = Region, y = Percentage, fill = Attack.Type)
      )
      + geom_bar(stat = "identity", position = "fill")
      + theme(plot.margin = unit(c(0, 0.5, 0.5, 1.2), "cm"))
      + theme(legend.position = "bottom")
      + theme(
        axis.text.x = element_text(
          angle = 90,
          hjust = 2,
          vjust = 0.5
        ),
        panel.background = element_rect(fill = "white", colour = "grey50")
      )
      + theme(legend.text = element_text(margin = margin(l = 50)))
      + theme(plot.title = element_text(margin = margin(10, 0, 10, 0)))
      + theme(axis.text.y = element_text(face = "bold"))
      + labs(x = "")
      + ggtitle(
        paste("PERCENTAGE OF ATTACKS BY ATTACK TYPE,", region_attack$year)
      )
    ) %>%
      layout(legend = list(
        orientation = "v",
        x = 0.35,
        y = -1.5
      )) %>% layout(autosize = F,
                    width = 890,
                    height = 500)
    
    
  })
  
  output$deathattack <- renderPlotly({
    
    #applying filter on data frame
    terrorism <- terrorism[terrorism$year == input$time_line, ]
    
    # we have more then 12 regions for simplicity I am recoding them to 9 regions based on geography
    terrorism$region_txt <-
      recode(terrorism$region_txt, "Middle East & North Africa" = "MENA")
    terrorism$region_txt <-
      recode(terrorism$region_txt,
             "Eastern Europe" = "Europe",
             "Western Europe" = "Europe")
    terrorism$region_txt <-
      recode(
        terrorism$region_txt,
        "East Asia" = "Asia-Pacific",
        "Southeast Asia" = "Asia-Pacific",
        "Australasia & Oceania" = "Asia-Pacific"
      )
    
    
    # aggregate function to calculate number of fatalities by region
    terrorism_fatal <-
      aggregate(
        terrorism$Fatalities,
        by = list(region_txt = terrorism$region_txt),
        FUN = sum
      )
    
    # renaming column
    names(terrorism_fatal)[2] <- "Fatalities"
    terrorism_attack <- plyr::count(terrorism, "region_txt") # getting the counts of region_txt
    names(terrorism_attack)[2] <- "Attacks" # renaming column
    merged_data <-
      merge(terrorism_fatal, terrorism_attack, by = 'region_txt')  # mergin two data frames
    merged_data <- melt(merged_data, id = c("region_txt"))  # melting the data frame by reshape library
    names(merged_data)[1] <- "Region"  # renaming column
    names(merged_data)[2] <- "Type"   # renaming column
    names(merged_data)[3] <- "Count"  # renaming column
    
    # ordering the data frame by count and region
    merged_data <-
      merged_data[order(merged_data$Region, -merged_data$Count),]
    
    # finally creating ggplotly dodge bar graph to compare number of Fatalities and Number of attacks
    ggplotly(
      ggplot(data = merged_data, aes(
        x = Region, y = Count, fill = Type
      ))
      + geom_bar(stat = "identity", position = position_dodge())
      + theme(
        axis.text.x = element_text(
          angle = 90,
          hjust = 2,
          vjust = 0.5
        ),
        panel.background = element_rect(fill = "white", colour = "grey50")
      )
      + theme(legend.text = element_text(margin = margin(l = 50)))
      + theme(plot.margin = unit(c(0.5, 0.6, 1, 1.2), "cm"))
      + theme(axis.text.y = element_text(face = "bold"))
      + theme(legend.position = "bottom")
      + labs(x = "") + labs(y = "Number")
      + ggtitle(
        paste(
          "NUMBER OF FATALITIES AND ATTACKS BY REGION IN",
          terrorism$year
        )
      )
      
      
      
    ) %>%
      layout(legend = list(
        orientation = "h",
        y = -1,
        x = 0.3
      )) %>% layout(autosize = F,
                    width = 900,
                    height = 500)
    
    
    
    
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
