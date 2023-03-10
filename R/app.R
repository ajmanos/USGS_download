library(shiny)
library(dataRetrieval)
library(shinycssloaders)
library(writexl)
library(leaflet)
library(sf)
library(plotly)
library(lubridate)
library(raster)
library(DT)
library(tidyverse)

# List of codes:
# 00010 = Water Temp (deg C) 
# 00045 = Total Precipitation (in)
# 00060 = Stream Flow (ft^3/s)
# 00065 = Gage height (ft)
# 00095 = Specific Conductance (uS/cm @ 25C)
# 00300 = Dissolved Oxygen (mg/L)
# 00400 = pH
# 00480 = Salinity (ppt)
# 63160 = Stream water level abov NAVD 1988 (ft)
# 72254 = Water Velocity (ft/s)
# 99133 = NO3 + NO2 (mg/L-N)


# Get info for most common codes, full list of codes slows program:
codeList <- c('00010', '00045', '00060', '00065', '00095', '00300', '00400',
               '00480', '63160', '72254', '99133')
codePull <- readNWISpCode(codeList)

# Dataframe needed for site info tab formatting:
codeDat <- data.frame(codes = codePull$parameter_cd, desc = codePull$parameter_nm)

# Format codes and description for input code list:
codesFmt <- paste0(codePull$parameter_cd, ' (', codePull$parameter_nm, ')')

typeSupp <- c('Daily','Real Time')



ui <- fluidPage(
  
  titlePanel('USGS Downloader'),
  
  sidebarLayout(
    sidebarPanel(
      
      # Set error message color:
      tags$head(
        tags$style(HTML('.shiny-output-error-validation {
        color: red;
        font-weight: bold;}'
      ))
    ),
      
  
      # State selection
      selectInput('state', label = strong('State'),
                  choices = state.name, selected = 'Florida'),
      
      # Allow user to input parameter code:
      selectizeInput(inputId = 'pCode',
                     label = a('Parameter code', 
                               href = 'https://help.waterdata.usgs.gov/parameter_cd_nm',
                               target = '_blank'),
                     choices = codesFmt,
                     selected = '00060 (Discharge, cubic feet per second)'),
      
      # Display link for site info from USGS website:
      strong(htmlOutput("selected_site",container=span)),
      
      # Allow user to input site number:
      textInput(inputId = "site",
                label = "Site number"),
      
      
      # Allow user to select the data type (mean daily or real time):
      selectInput("type", label = strong("Data type"), 
                  choices = c("Daily" = 'daily',
                              "Real Time" = 'real_time'), 
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
      actionButton("load", label = "Load Data", class = "btn-primary"),
      
    br(),
    br(),
    
    # Generate button for user to load data after imputing parameters:
    actionButton("map_load", label = "Refresh Map", class = "btn-success",
                 icon = icon("refresh")),

    hr(),
    
      # Generate download button for user to download data:
      downloadButton("downloadData", "Download Data"),
      
      hr(),
      
      h5("This dashboard utilizes the",
         code("dataRetrieval"),
         "R package developed by the USGS. Please visit the USGS",
         a("blog post",
           href = "https://waterdata.usgs.gov/blog/dataretrieval/"),
         "for more information."),
      
    ),
    
    # Display tabs for table of data, plot of data, and map of USGS sites:
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Site Data", tableOutput("table") %>% withSpinner(color="blue")),
                  tabPanel("Site Info", dataTableOutput("infoTable") %>% withSpinner(color="blue")),
                  tabPanel("Linear Time Series", plotlyOutput("ts_line") %>% withSpinner(color="blue")),
                  tabPanel("3D Time Series", plotlyOutput("ts_3d") %>% withSpinner(color="blue")),
                  tabPanel('Map', leafletOutput('map',height = 900) %>% 
                             withSpinner(color="blue")),
                  tabPanel("Instructions",  h3(strong('How to use USGS downloader')),

                           br(),
                           h4(strong('Step 1: Select a State')),
                           tags$li("Use the drop down menu to select a state of
                                    interest."),
                           tags$li("Click the 'Map' tab to view sites available for selected parameter code."),
                           tags$li(strong('*Note: State selection only used for viewing sites in the map tab.
                                       State selection will not impact other parameters.')),

                           br(),
                           h4(strong('Step 2: Input Parameter Code')),
                           tags$li("Use the dropdown menu to select a parameter code
                                 or enter a code/description in the parameter code box."),
                           tags$li("Click the 'Parameter code' link to view a list of codes
                                 and code information on the USGS website."),
                           tags$li(strong("*Note: Not all parameters codes are available for
                                          selection as there are nearly 25,000 codes which would
                                          slow operating speeds. Codes can be added by request.")),

                           br(),
                           h4(strong('Step 3: Input Site Number')),
                           tags$li("If site is known, simply enter site in the box and click 'Load Data'.
                                 Once data are loaded, a link to the site from the USGS website will
                                 be displayed above the site number."),
                           tags$li("If site is not known, enter a parameter code, click 'Load Data', select
                                 the map tab, and search for a site of interest. Click
                                 the site pin, copy the site number from the info box,
                                 and paste into the site number box in the side panel"),

                           br(),
                           h4(strong("Step 4: Select Data type")),
                           tags$li("Using the 'Data type' drop down
                                 menu, select the type of data to display."),
                           tags$li("Daily = mean daily data"),
                           tags$li("Real Time = 15-minute intervals"),

                           br(),
                           h4(strong("Step 5: Select Stat Code")),
                           tags$li("Using the 'Stat Code' drop down menu, select the statistic to be
                                 applied to the data."),
                           tags$li("To view available site codes, load site data and click the 'Site Info'
                                 tab for possible parameter/stat codes, data types, and date ranges."),

                           br(),
                           h4(strong("Step 6: Select Date Range")),
                           tags$li("After selecting a date range, click 'Load Data' to view a table of
                                 data in the 'Site Data' tab"),
                           tags$li("Toggle to the 'Plot' window to view a plot of the data."),

                           br(),
                           h4(strong("Step 7: View Graphed Data")),
                           tags$li("Select the 'Linear Time Series' tab to view a time series of the
                                 selected parameters"),
                           tags$li("Selectthe '3D Time Series' tab to view a 3D representation of the
                                 time series data."),

                           br(),
                           h4(strong("Step 8: Download the Data")),
                           tags$li("Click 'Download Data', to download the data
                                 in the output table to an excel file."),

                           br(),
                           tags$li(strong("*Note: Tabs will not automatically update when new parameters
                                     are set, 'Load Data' must be clicked to view outputs with
                                     updated information (i.e., parameter code, site number, etc.)."))))
    )
  )
)



server <- function(input, output, session) {
  

  usgsData <- eventReactive(input$load, {
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
  })
  
  # Generate table of data and display on screen:
  output$table <- renderTable({
    tbl <- usgsData()
    validate(
      need(nrow(tbl) > 0,"No data available.
      
    This could be due to:
          1) Incorrect site number
          2) Parameter code not available at site
          3) Stat code not available at site
          4) Data type is 'Daily' while date range is set for same date"))
    
    tbl
  })
  
  


  siteInfo <- eventReactive(input$load, {
    
    siteData <- whatNWISdata(siteNumber = input$site)
    
    for (i in 1:nrow(siteData)){
      if (!is.na(siteData$parm_cd[i])){
        siteData$desc[i] <- readNWISpCode(siteData$parm_cd[i])$parameter_nm
      } else {
        siteData$desc[i] <- NA
      }
    }
    
    siteData2 <- data.frame(siteData$site_no, 
                            siteData$station_nm,
                            siteData$parm_cd,
                            siteData$desc, 
                            siteData$data_type_cd,
                            siteData$stat_cd,
                            as.character(siteData$begin_date),
                            as.character(siteData$end_date)) %>% arrange(siteData.parm_cd)
    colnames(siteData2) <- c("Site Number","Station Name","Parameter Code","Description",
                             "Data Type", "Stat Code", "Start Date","End Date")
    
    siteData2$`Data Type` <- ifelse(siteData2$`Data Type` == 'dv', 'Daily',
                              ifelse(siteData2$`Data Type` == 'uv', 'Real Time',
                               ifelse(siteData2$`Data Type` == 'qw', 'Water Quality',
                                ifelse(siteData2$`Data Type` == 'sv', 'Site Visits',
                                 ifelse(siteData2$`Data Type` == 'ad', 'USGS Annual Water Report',
                                  ifelse(siteData2$`Data Type` == 'pk', 'Peak Flow',
                                   ifelse(siteData2$`Data Type` == 'aw', 'Groundwater Level',
                                    ifelse(siteData2$`Data Type` == 'id', 'Historical Instantaneous',""))))))))
    return(siteData2)
  })

  # Generate table of data and display on screen:
  output$infoTable <- renderDataTable(
    datatable(siteInfo()) %>%
      formatStyle(' ',
                  target = 'row',
                  backgroundColor = styleEqual(as.numeric(rownames(siteInfo()[siteInfo()$`Parameter Code` 
                                                                              %in% codeList & siteInfo()$`Data Type` 
                                                                              %in% typeSupp,])),
                                               'lightgreen')))



  # Download the datatable and create name based on site and type selected:
  output$downloadData <- downloadHandler(
    filename = function(){paste0('USGS_',input$site,'_', names(renameNWISColumns(usgsData()))[4], "_",
                                 input$type,'_',min(year(usgsData()[,3])),'_',
                                 max(year(usgsData()[,3])),'.xlsx')},
    content = function(fname) {
      write_xlsx(usgsData(),fname)
    })
  


  # Generate plot of output table:
  output$ts_line <- renderPlotly({
    plot_ly(usgsData(), type = 'scatter', mode='lines',height=800) %>%
      add_trace(x = if (input$type == 'daily') ~Date else ~dateTime,
                y = ~usgsData()[,4], line=list(color='darkblue')) %>%
      layout(title = list(text = readNWISsite(input$site)$station_nm),
        showlegend = FALSE, yaxis = list(title = str_sub(input$pCode,8,-2)))
  })



  # Generate 3D plot of output table:
  output$ts_3d <- renderPlotly({
    #TODO: 3d time series for real-time data
    validate(need(names(usgsData()[3]) == 'Date', "3D time series plot does not currently support real time data."))
    FlowMatrix <- data.frame(Day = yday(usgsData()$Date), Year = year(usgsData()$Date), Var = usgsData()[,4])
    var_mat <- as.matrix(rasterFromXYZ(FlowMatrix))
    rownames(var_mat) <- rev(seq(min(FlowMatrix$Year),max(FlowMatrix$Year),1))
    y <- as.numeric(rownames(var_mat))
    tck_num <- round(seq(1, 366, by = 30.5))
    tck_mo <- month.abb[parse_date_time(tck_num, orders = "j") %>%
      month()]

    leg_title <- paste0(names(renameNWISColumns(usgsData()))[4],
                 " (",readNWISpCode(substring(input$pCode,1,5))$parameter_units,")")


    plot_ly(height = 900) %>%
      add_surface(z = var_mat,
                  y = y,
                  colorbar = list(title=leg_title),
                  colorscale = list(thresholds_colors = seq(0, 1, length = 6),
                  colors = c('gray','blue','skyblue','green','yellow','red')),
                  showscale=TRUE) %>%
      layout(title = list(text = readNWISsite(input$site)$station_nm,x=0.47, y=0.92),
             scene=list(
               yaxis=list(title='Year',dtick=5),
               zaxis = list(title = leg_title),
               xaxis = list(title = 'Month', autorange="reversed",
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
  map.sites <- eventReactive(input$map_load, {
    state_sites <- whatNWISsites(stateCd = input$state, 
                                 parameterCd = substr(input$pCode,1,5))
    
    state_data <- whatNWISdata(siteNumbers = state_sites$site_no,
                               service = ifelse(input$type == 'daily','dv','uv'), 
                               parameterCd = substr(input$pCode,1,5))


    sf_points <- st_as_sf(state_data,
                          coords = c("dec_long_va", "dec_lat_va")) 
  })

  # Display the map on the map tab:
  output$map <- renderLeaflet({
    leaflet(map.sites()) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(clusterOptions = markerClusterOptions(zoomToBoundsOnClick = T),
                 popup = ~paste(
                   paste('<b>', 'ID:', '</b>', site_no),
                   paste('<b>', 'Station Name:', '</b>', station_nm),
                   paste('<b>', 'Parameter:', '</b>', input$pCode),
                   paste('<b>', 'Data Type:', '</b>', ifelse(data_type_cd[1] == 'dv',"Daily","Real Time (15-minute)")),
                   paste('<b>', 'Start Date:', '</b>', begin_date),
                   paste('<b>', 'End Date:', '</b>', end_date),
                   sep = '<br/>'),
                 popupOptions = popupOptions(closeButton = FALSE))
  })
  outputOptions(output, "map", suspendWhenHidden = FALSE)

}

shinyApp(ui, server)
