library(shiny)
library(dataRetrieval)
library(shinycssloaders)
library(writexl)
library(leaflet)
library(sf)
library(plotly)
library(lubridate)
library(crayon)
library(raster)
library(tidyverse)

# Get a list of all parameter codes:
codes.all <- readNWISpCode("all")

# Combine codes and code descriptions:
codes.dat <- data.frame(CODES = codes.all$parameter_cd, desc = codes.all$parameter_nm)

# Format code options for input:
codesD <- paste0(codes.dat$CODES,' (',codes.dat$desc,')')


ui <- fluidPage(
  
  # Define page title:
  titlePanel("USGS Data Download"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tags$head(
        tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
      ),
      
      
      
      # Allow user to select state of interest (FL by default):
      selectInput("state", label = strong("State"),
                  choices = state.abb,selected = 'FL'),
      
      # Allow user to input parameter code:
      selectizeInput(inputId = "pCode",
                     label = a("Parameter code", 
                               href = 'https://help.waterdata.usgs.gov/parameter_cd_nm',
                               target = "_blank"),
                     choices = NULL),
      
      # Display link for site info from USGS website:
      strong(htmlOutput("selected_site",container=span)),
      
      # Allow user to input site number:
      textInput(inputId = "site",
                label = "Site number"),
      
      
      # Allow user to select the data type (mean daily or real time):
      selectInput("type", label = strong("Data type"), 
                  choices = c("Daily" = 'daily',
                              "Real Time" = 'real_time',
                              "Water Quality" = "water_quality"), 
                  selected = 'Daily'),
      
      # Allow user to select the data type (mean daily or real time):
      selectInput("stat", label = strong("Stat Code"), 
                  choices = c("00001 (Max)" = "00001",
                              "00002 (Min)" = "00002",
                              "00003 (Mean)" = '00003',
                              "00006 (Sum)" = '00006',
                              "00008 (Median)" = '00008'), 
                  selected = '00003'),
      
      
      
      # Allow user to select date range:
      dateRangeInput("dates", label = strong("Date range")),
      
      
      # Generate button for user to load data after imputing parameters:
      actionButton("load", label = "Load Data", class = "btn-success",
                   icon = icon("refresh")),
      
      hr(),
      br(),
      
      # Generate download button for user to download data:
      downloadButton("downloadData", "Download Data"),
      
      hr(),
      
      h5("This dashboard utilizes the",
         code("dataRetrieval"),
         "R package developed by the USGS. Please visit the USGS",
         a("blog post",
           href = "https://waterdata.usgs.gov/blog/dataretrieval/"),
         "for more information."),
      
      
      br(),
      h3(strong('How to use USGS downloader')),
      
      br(),
      h4(strong('Step 1: Select a State')),
      h5("- Use the drop down menu to select a state of
            interest."),
      h5(strong('*Note: State selection only used for viewing sites
          in the map tab.')),
      
      br(),
      h4(strong('Step 2: Input Parameter Code')),
      h5("- Use the dropdown menu to select a parameter code
            or enter the description in the parameter code box."),
      h5("- Click the 'Parameter code' link to view a list of codes
            and code information on the USGS website."),
      h5("- The 'Map' tab will display sites that contain data from
            the entered parameter code. Enter a new code and click
            'Load Data' to view new sites."),
      
      br(),
      h4(strong('Step 3: Input Site Number')),
      h5("- If site is known, simply enter site in the box.
            Once 'Load Data' is clicked, a link to the site
            from the USGS website will be displayed above
            the site number."),
      h5("- If site is not known, enter a parameter code, click 'Load Data', select
            the map tab, and search for a site of interest. Click
            the site pin, copy the site number in the info box,
            and paste into the site number box in the side panel"),
      
      br(),
      h4(strong("Step 4: Select Data type")),
      h5("- Using the 'Data type' drop down
            menu, select the type of data to display."),
      
      br(),
      h4(strong("Step 5: Select Stat Code")),
      h5("- Using the 'Stat Code' drop down
            menu, select the statistic to be applied to the data."),
      
      br(),
      h4(strong("Step 6: Select Date Range")),
      h5("- After selecting a date range, click 'Load Data'
            to view a table of data in the 'Table' window."),
      h5("- Toggle to the 'Plot' window to view a plot of the data."),
      
      br(),
      h4(strong("Step 7: Download the Data")),
      h5("- Click 'Download Data', to download the data
            in the output table to an excel file."),
      
      br(),
      h5(strong("*Note: the site data/info, plot, and map tabs will not automatically
                update, 'Load Data' must be clicked to view outputs with
                updated information (i.e., parameter code, date range, etc.)."))
      
    ),
    
    # Display tabs for table of data, plot of data, and map of USGS sites:
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Site Data", tableOutput("table") %>% withSpinner(color="blue")),
                  tabPanel("Site Info", tableOutput("Infotable") %>% withSpinner(color="blue")),
                  tabPanel("Linear Time Series", plotlyOutput("plot") %>% withSpinner(color="blue")),
                  tabPanel("3D Time Series", plotlyOutput("fr_plot") %>% withSpinner(color="blue")),
                  tabPanel('Map', leafletOutput('map',height = 900) %>% 
                             withSpinner(color="blue")))
    )
  )
)


server <- function(input, output, session) {
  
  
  site <- eventReactive(input$load, {
    x <- whatNWISdata(siteNumber = input$site) %>% drop_na(parm_cd)
    
    f <- data.frame(x$parm_cd)
    for (i in 1:nrow(f)){
      f$desc[i] <- subset(codes.dat$desc, codes.dat$CODES==f$x.parm_cd[i])
    }
    
    y <- data.frame(x$site_no, x$station_nm,x$parm_cd,f$desc, x$data_type_cd,
                    x$stat_cd,as.character(x$begin_date),as.character(x$end_date))
    colnames(y) <- c("Site Number","Station Name","Parameter Code","Description", 
                     "Data Type", "Stat Code", "Start Date","End Date")
    
    y$`Data Type` <- ifelse(y$`Data Type` == 'dv', 'Daily', 
                      ifelse(y$`Data Type` == 'uv', 'Real Time',
                       ifelse(y$`Data Type` == 'qw', 'Water Quality',
                        ifelse(y$`Data Type` == 'gwlevel', 'Groundwater',
                         ifelse(y$`Data Type` == 'rating', 'Rating Curve',
                          ifelse(y$`Data Type` == 'peak', 'Peak Flow',
                           ifelse(y$`Data Type` == 'meas', 'Surfacewater',"")))))))

    return(y)
  })
  
  # Generate table of data and display on screen:
  output$Infotable <- renderTable({site()})
  
  
  
  # Populate parameter code choices. Discharge selected as default since it
  # is the most popular:
  updateSelectizeInput(session, 'pCode', choices = codesD, server = TRUE,
                       selected = '00060 (Discharge, cubic feet per second)')
  
  
  #   # When user clicks 'Load Data' run the USGS 'dataRetrieval' package either 
  #   # for mean daily or real time based on user selection:
  usgs.data <- eventReactive(input$load, {
    validate(need(nchar(input$site) >= 8, "Error: Site number must be at least 8 digits."))
    validate(need(nchar(input$site) <= 15, "Error: Site number must be less than 15 digits."))
    validate(need(!is.na(as.numeric(input$site)), "Error: Invalid site number, only numbers are accepted."))
    validate(need(input$dates[1] < input$dates[2], "Error: Start date must be less than end date."))
    if (input$type == 'daily') {
      df1 <- readNWISdv(siteNumbers = input$site,
                        parameterCd = substr(input$pCode, 1,5),
                        statCd = substr(input$stat, 1,5),
                        startDate = input$dates[1],
                        endDate = input$dates[2])
      df1$Date <- as.character(df1$Date)
      df1 <- renameNWISColumns(df1)
      return(df1)
    }
    if (input$type == 'real_time') {
      df1 <- readNWISuv(siteNumbers = input$site,
                        parameterCd = substr(input$pCode, 1,5),
                        startDate = input$dates[1],
                        endDate = input$dates[2])
      df1$dateTime <- as.character(df1$dateTime-18000)
      df1 <- renameNWISColumns(df1)
      return(df1)
    }
    #TODO: USGS is retiring this function, update when non-functional
    # vignette('qwdata_changes', package = 'dataRetrieval')
    if (input$type == 'water_quality') {
      df1 <- readNWISqw(siteNumbers = input$site,
                        parameterCd = substr(input$pCode, 1,5),
                        startDate = input$dates[1],
                        endDate = input$dates[2])
      df1$dateTime <- as.character(df1$date)
      df1 <- renameNWISColumns(df1)
      return(df1)
    }
  })
  
  # Generate table of data and display on screen:
  output$table <- renderTable({
    tbl <- usgs.data()
    validate(
      need(nrow(tbl) > 0,"No data available.
      
This could be due to:
      1) Incorrect site number
      2) Parameter code not available at site
      3) Stat code not available at site
      4) Data type is 'Daily' while date range is set for same date"))
    
    tbl
  })
  
  
  # Download the datatable and create name based on site and type selected:
  output$downloadData <- downloadHandler(
    filename = function(){paste0('USGS_',input$site,'_', names(renameNWISColumns(usgs.data()))[4], "_",
                                 input$type,'_',min(year(usgs.data()[,3])),'_',
                                 max(year(usgs.data()[,3])),'.xlsx')},
    content = function(fname) {
      write_xlsx(usgs.data(),fname)   
    })
  
  
  # Generate plot of output table:
  output$plot <- renderPlotly({
    plot_ly(usgs.data(), type = 'scatter', mode='lines',height=800) %>%
      add_trace(x = if (input$type == 'daily') ~Date else ~dateTime, 
                y = ~usgs.data()[,4], line=list(color='darkblue')) %>%
      layout(title = list(text = readNWISsite(input$site)$station_nm),
        showlegend = FALSE, yaxis = list(title = str_sub(input$pCode,8,-2)))
  })
  
  
  
  # Generate plot of output table:
  output$fr_plot <- renderPlotly({
    FlowMatrix <- data.frame(Day = yday(usgs.data()$Date), Year = year(usgs.data()$Date), Var = usgs.data()[,4])
    var_mat <- as.matrix(rasterFromXYZ(FlowMatrix))
    rownames(var_mat) <- rev(seq(min(FlowMatrix$Year),max(FlowMatrix$Year),1))
    y <- as.numeric(rownames(var_mat))
    tck_num <- round(seq(1, 366, by = 30.5))
    tck_mo <- month.abb[parse_date_time(tck_num, orders = "j") %>% 
      month()]

    plot_ly(z = var_mat, y = y, type = "surface", height = 900,
            colorscale = list(thresholds_colors = seq(0, 1, length = 6),
                              colors = c('gray','blue','skyblue','green','yellow','red')),
            showscale=TRUE) %>%
      layout(title = list(text = readNWISsite(input$site)$station_nm,x=0.47, y=0.92),
             scene=list(
               yaxis=list(title='Year',dtick=5),
               zaxis = list(title = paste0(names(renameNWISColumns(usgs.data()))[4],
                                           " (",readNWISpCode(substring(input$pCode,1,5))$parameter_units,")")),
               xaxis = list(title = 'Month',autorange="reversed",
                            tickvals = seq(1, 366, by = 30.5),
                            ticktext = tck_mo),
               camera = list(eye = list(x = 1.3, y = 1.3, z = 1.5))))
  })
  
  # Define the URL to use display the USGS website based on user site input:
  site.info <- eventReactive(input$load, {
    paste0('https://waterdata.usgs.gov/monitoring-location/',input$site,'/')
  })
  
  # Generate hyperlink text from user site input that goes to USGS site:
  output$selected_site <- renderUI(a("Site Info", target="_blank", # site link
                                     href = site.info()
  ))
  
  # Use USGS site info function to get location for each of the sites that match
  # the state and parameter code defined by the user:
  map.sites <- eventReactive(input$load, {
    state_sites <- whatNWISsites(stateCd = input$state,
                                 parameterCd = substr(input$pCode,1,5))
    sf_points <- st_as_sf(state_sites,
                          coords = c("dec_long_va", "dec_lat_va"),
                          crs = 4269)
  })
  
  # Display the map on the map tab:
  output$map <- renderLeaflet({
    leaflet(map.sites()) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(clusterOptions = markerClusterOptions(zoomToBoundsOnClick = T), 
                 popup = ~paste(
                   paste('<b>', 'ID:', '</b>', site_no), 
                   paste('<b>',  'Station Name:', '</b>', station_nm),
                   sep = '<br/>'),
                 popupOptions = popupOptions(closeButton = FALSE))
  })
  
  
}


shinyApp(ui, server)
