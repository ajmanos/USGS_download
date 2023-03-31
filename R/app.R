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


# Only using a subset of parameter codes available as loading in all 24,898 codes
# significantly slows app functionality. More codes can be added upon request.
codeList <- c('00010', '00045', '00060', '00065', '00095', '00300', '00400',
               '00480', '63160', '72254', '99133')
codePull <- readNWISpCode(codeList)

# Dataframe needed for site info tab formatting:
codeDat <- data.frame(codes = codePull$parameter_cd, desc = codePull$parameter_nm)

# Format codes and description for input code list:
codesFmt <- paste0(codePull$parameter_cd, ' (', codePull$parameter_nm, ')')

# Supported data types:
typeSupp <- c('Daily','Continuous')


ui <- fluidPage(
  
  titlePanel('USGS Downloader'),
  
  sidebarLayout(
    sidebarPanel(
      
      # Set error message color:
      tags$head(
        tags$style(HTML('.shiny-output-error-validation {
        color: red;
        font-weight: bold;
        font-size: 20px}'
      ))
    ),
  
      # State:
      selectInput('state', label = strong('State'),
                  choices = state.name, selected = 'Florida'),
      
      # Parameter code:
      selectizeInput(inputId = 'pCode',
                     label = a('Parameter code', 
                               href = 'https://help.waterdata.usgs.gov/parameter_cd_nm',
                               target = '_blank'),
                     choices = codesFmt,
                     selected = '00060 (Discharge, cubic feet per second)'),
    
    # Data type:
    selectInput("type", label = strong("Data type"), 
                choices = c("Daily" = 'daily',
                            "Continuous" = 'continuous'), 
                selected = 'Daily'),
      
      # Display link for site info from USGS website:
      strong(htmlOutput("selected_site",container=span)),
      
      # Site number:
      textInput(inputId = "site",
                label = "Site number"),
      
      # Stat code:
      selectInput("stat", label = strong("Stat Code"), 
                  choices = c("00001 (Max)" = "00001",
                              "00002 (Min)" = "00002",
                              "00003 (Mean)" = '00003',
                              "00006 (Sum)" = '00006',
                              "00008 (Median)" = '00008'), 
                  selected = '00003'),
      
      # Date range:
      dateRangeInput("dates", label = strong("Date Range")),
      
      # Load data button:
      actionButton("load", label = "Load Data", class = "btn-primary"),
      
      br(),
      br(),
    
      # Map refresh button:
      actionButton("map_load", label = "Refresh Map", class = "btn-success",
                 icon = icon("refresh")),

      hr(),
    
      # Download data button:
      downloadButton("downloadData", "Download Data"),
      
      hr(),
      
      h5("This application utilizes the",
         code("dataRetrieval"),
         "package developed by the USGS. Please visit the USGS",
         a("blog post",
           href = "https://waterdata.usgs.gov/blog/dataretrieval/",target = '_blank'),
         "for more information."),
      
    ),
    
    # Generate tabs for site data/info, plots, map, and app instructions:
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Site Data", tableOutput("table") %>% withSpinner(color="blue")),
                  tabPanel("Site Info", span("▆",style = "color:lightgreen"),"= Supported data type",
                           dataTableOutput("infoTable") %>% withSpinner(color="blue")),
                  tabPanel("Linear Time Series", plotlyOutput("ts_line") %>% withSpinner(color="blue")),
                  tabPanel("3D Time Series", plotlyOutput("ts_3d") %>% withSpinner(color="blue")),
                  tabPanel('Map', leafletOutput('map',height = 900) %>% 
                             withSpinner(color="blue")),
                  tabPanel("Instructions",  h3(strong('How to use USGS downloader')),
                           br(),
                           h4(strong('Step 1: Select a State')),
                           tags$li("Use the drop down menu to select a state of
                                    interest."),
                           tags$li("Navigate to the",code('Map'), "tab, then click  the", code('Refresh Map'),
                                    "button on the side panel to view sites available for selected 
                                   parameter code."),
                           tags$li("To view sites with different parameters available, select an
                                   option from the", code('Parameter Code'), "drop down menu, then click", code('Refresh Map'),"."),
                           tags$li(strong('*Note: State selection only used for viewing sites in the map tab.
                                       State selection will not impact other parameters.')),
                           br(),
                           h4(strong('Step 2: Input Parameter Code')),
                           tags$li("Use the dropdown menu to select a", code('Parameter Code'),
                                 "or enter a code/description in the parameter code box."),
                           tags$li("Clicking the",code('Parameter Code'),"name will open the
                                   USGS page of available parameter codes and information."),
                           tags$li(strong("*Note: Not all parameters codes are available for
                                          selection as there are nearly 25,000 codes which would
                                          slow operating speeds. Codes can be added by request to almanos@pinellas.gov.")),
                           br(),
                           h4(strong("Step 3: Select Data type")),
                           tags$li("Using the", code('Data type')," drop down
                                 menu, select the type of data to display."),
                           tags$li("Daily = daily aggregated data"),
                           tags$li("Continuous = 15-minute intervals"),
                           br(),
                           h4(strong('Step 4: Input Site Number')),
                           tags$li("If site is known, simply enter site in the", code('Site Number'), "."),
                           tags$li("Once ", code('Load Data'), " is clicked a link to the site info from the USGS website will
                                 be displayed above the site number."),
                           tags$li("If site is not known, select a parameter code and data type, then load the map
                                    (see step 1). Search for a site of interest. Click the site pin, copy the site 
                                    number from the info box, and paste into the site number box in the side panel."),
                           br(),
                           h4(strong("Step 5: Select Stat Code")),
                           tags$li("Using the", code('Stat Code'), "drop down menu, select the statistic to be
                                 applied to the data."),
                           tags$li("To view available stat codes, load site data and click the", code('Site Info'),
                                 "tab for possible parameter/stat codes, data types, and date ranges."),
                           br(),
                           h4(strong("Step 6: Select Date Range")),
                           tags$li("Select a start and end date from the", code('Date Range'), "boxes."),
                           br(),
                           h4(strong("Step 7: Load Data")),
                           tags$li("Click the", code('Load Data'), "button to load the data from the selected inputs."),
                           tags$li("The raw data will appear in the", code('Site Data'), "tab."),
                           tags$li(code('Site Info'), "will be updated whenever new site data is loaded."),
                           h4(strong("Step 8: View Graphed Data")),
                           tags$li("Select the", code('Linear Time Series'), "tab to view a time series of the
                                 selected parameter."),
                           tags$li("Select the", code('3D Time Series'), "tab to view a 3D representation of the
                                 time series data."),
                           br(),
                           h4(strong("Step 9: Download the Data")),
                           tags$li("Click", code('Download Data'), "to download the raw data
                                 into an excel file."),
                           br(),
                           br()))
    )
  )
)

server <- function(input, output, session) {
  
  # Download USGS data based on user inputs:
  usgs.data <- eventReactive(input$load, {
    shiny::validate(need(nchar(input$site) >= 8, "Error: Site number must be at least 8 digits."))
    shiny::validate(need(nchar(input$site) <= 15, "Error: Site number must be less than 15 digits."))
    shiny::validate(need(!is.na(as.numeric(input$site)), "Error: Invalid site number, only numbers are accepted."))
    shiny::validate(need(input$dates[1] < input$dates[2], "Error: Start date must be less than end date."))
    if (input$type == 'daily') {
      fDat <- readNWISdv(siteNumbers = input$site,
                        parameterCd = substr(input$pCode, 1,5),
                        statCd = substr(input$stat, 1,5),
                        startDate = input$dates[1],
                        endDate = input$dates[2])
      fDat$Date <- as.character(fDat$Date)
      fDat <- renameNWISColumns(fDat)
      return(fDat)
    }
    if (input$type == 'continuous') {
      tz <- readNWISsite(input$site)$tz_cd
      fDat <- readNWISuv(siteNumbers = input$site,
                        parameterCd = substr(input$pCode, 1,5),
                        startDate = input$dates[1],
                        endDate = input$dates[2],
                        tz = ifelse(tz =='EST', 'America/New_York', tz))
      fDat$dateTime <- as.character(fDat$dateTime)
      fDat <- renameNWISColumns(fDat)
      return(fDat)
    }
  })
  
  # Generate table of data:
  output$table <- renderTable({
    tbl <- usgs.data()
    shiny::validate(need(nrow(tbl) > 0,"No data available. Check site number."))
    tbl
  })

  # Get available site data:
  site.info <- eventReactive(input$load, {
    shiny::validate(need(nchar(input$site) >= 8, "Error: Site number must be at least 8 digits."))
    shiny::validate(need(nchar(input$site) <= 15, "Error: Site number must be less than 15 digits."))
    shiny::validate(need(!is.na(as.numeric(input$site)), "Error: Invalid site number, only numbers are accepted."))
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
                              ifelse(siteData2$`Data Type` == 'uv', 'Continuous',
                               ifelse(siteData2$`Data Type` == 'qw', 'Water Quality',
                                ifelse(siteData2$`Data Type` == 'sv', 'Site Visits',
                                 ifelse(siteData2$`Data Type` == 'ad', 'USGS Annual Water Report',
                                  ifelse(siteData2$`Data Type` == 'pk', 'Peak Flow',
                                   ifelse(siteData2$`Data Type` == 'aw', 'Groundwater Level',
                                    ifelse(siteData2$`Data Type` == 'id', 'Historical Instantaneous',""))))))))
    return(siteData2)
  })

  # Generate table of available site data:
  output$infoTable <- renderDataTable(
#   shiny::validate(need(nrow(tbl) > 0,"No data available. Check site number.")),
    datatable(site.info(),options = list(iDisplayLength = 25)) %>%
      formatStyle(' ',target = 'row',
                  backgroundColor = styleEqual(as.numeric(rownames(
                                          site.info()[site.info()$`Parameter Code` 
                                            %in% codeList & site.info()$`Data Type` 
                                            %in% typeSupp,])), 'lightgreen')))

  # Allow user to download data and give file descriptive name:
  output$downloadData <- downloadHandler(
    filename = function(){paste0('USGS_',input$site,'_', names(renameNWISColumns(usgs.data()))[4], "_",
                                 input$type,'_',min(year(usgs.data()[,3])),'_',
                                 max(year(usgs.data()[,3])),'.xlsx')},
    content = function(fname) {
      write_xlsx(usgs.data(),fname)
    })
  
  # Generate plot of output table:
  output$ts_line <- renderPlotly({
    plot_ly(usgs.data(), type = 'scatter', mode='lines',height=800) %>%
      add_trace(x = if (input$type == 'daily') ~Date else ~dateTime,
                y = ~usgs.data()[,4], line=list(color='darkblue')) %>%
      layout(title = list(text = readNWISsite(input$site)$station_nm),
        showlegend = FALSE, yaxis = list(title = str_sub(input$pCode,8,-2)))
  })

  # Generate 3D plot of output table:
  output$ts_3d <- renderPlotly({
    #TODO: 3d time series for continuous data
    shiny::validate(need(names(usgs.data()[3]) == 'Date', "3D time series plot does not currently support continuous data."))
    FlowMatrix <- data.frame(Day = yday(usgs.data()$Date), Year = year(usgs.data()$Date), Var = usgs.data()[,4])
    var_mat <- as.matrix(rasterFromXYZ(FlowMatrix))
    rownames(var_mat) <- rev(seq(min(FlowMatrix$Year),max(FlowMatrix$Year),1))
    y <- as.numeric(rownames(var_mat))
    tck_num <- round(seq(1, 366, by = 30.5))
    tck_mo <- month.abb[parse_date_time(tck_num, orders = "j") %>%
      month()]

    leg_title <- paste0(names(renameNWISColumns(usgs.data()))[4],
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
  site.url <- eventReactive(input$load, {
    paste0('https://waterdata.usgs.gov/monitoring-location/',input$site,'/')
  })

  # Generate hyperlink text from user site input that goes to USGS site:
  output$selected_site <- renderUI(a("Site Info", target="_blank", href = site.url()
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
  
  # Generate map of sites:
  output$map <- renderLeaflet({
    leaflet(map.sites()) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE),
                 popup = ~paste(
                   paste('<b>', 'ID:', '</b>', site_no),
                   paste('<b>', 'Station Name:', '</b>', station_nm),
                   paste('<b>', 'Parameter:', '</b>', codesFmt[grep(parm_cd[1],codesFmt)]),
                   paste('<b>', 'Data Type:', '</b>', ifelse(data_type_cd[1] == 'dv',"Daily","Continuous (15-minute)")),
                   paste('<b>', 'Start Date:', '</b>', begin_date),
                   paste('<b>', 'End Date:', '</b>', end_date),
                   sep = '<br/>'),
                 popupOptions = popupOptions(closeButton = FALSE))
  })
  
  # Display site map:
  outputOptions(output, "map", suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
