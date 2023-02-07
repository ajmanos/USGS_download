# Import required packages:
library(shiny)
library(dataRetrieval)
library(shinycssloaders)
library(writexl)
library(leaflet)
library(sf)
library(stringr)
library(plotly)
library(lubridate)


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
      
      # Allow user to select the data interval (mean daily or 15-minute):
      selectInput("interval", label = strong("Time interval"), 
                  choices = c("Mean Daily" = 'mean_daily',
                              "15-minute" = '15_minute'), 
                  selected = 'Mean Daily'),
    
    
      
      # Allow user to select date range:
      dateRangeInput("dates", label = strong("Date range")),
      

      
      # Generate button for user to load data after imputing parameters:
      actionButton("load", label = "Load Data"),
      br(),
      br(),
      
      # Generate download button for user to download data:
      downloadButton("downloadData", "Download Data"),

      hr(),
      
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
      h4(strong("Step 4: Select Time Interval")),
      h5("- Using the 'Time Interval' drop down
            menu, select either mean daily or 15-minute."),
      
      br(),
      h4(strong("Step 5: Select Date Range")),
      h5("- After selecting a date range, click 'Load Data'
            to view a table of data in the 'Table' window."),
      h5("- Toggle to the 'Plot' window to view a plot of the data."),
      
      br(),
      h4(strong("Step 6: Download the Data")),
      h5("- Click 'Download Data', to download the data
            in the output table to an excel file."),
      
      br(),
      h5(strong("*Note: the table, plot, and map tabs will not automatically
                update, 'Load Data' must be clicked to view outputs with
                updated information (i.e., parameter code, date range, etc.)."))
      
    ),
    
    # Display tabs for table of data, plot of data, and map of USGS sites:
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Table", tableOutput("table") %>% withSpinner(color="blue")),
                  tabPanel("Plot", plotlyOutput("plot")),
                  tabPanel('Map', leafletOutput('map',height = 900) %>% 
                                                     withSpinner(color="blue")))
    )
  )
)

server <- function(input, output, session) {
  
  # Populate parameter code choices. Discharge selected as default since it
  # is the most popular:
  updateSelectizeInput(session, 'pCode', choices = codesD, server = TRUE,
                       selected = '00060 (Discharge, cubic feet per second)')

  
  # When user clicks 'Load Data' run the USGS 'dataRetrieval' package either 
  # for mean daily or 15-minute based on user selection:
  usgs.data <- eventReactive(input$load, {
    if (input$interval == 'mean_daily') {
      df1 <- readNWISdv(siteNumbers = input$site,
                        parameterCd = substr(input$pCode, 1,5),
                        startDate = input$dates[1],
                        endDate = input$dates[2])  
      df1$Date <- as.character(df1$Date)
      df1 <- renameNWISColumns(df1)
      return(df1)
    }  
    if (input$interval == '15_minute') {
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
  output$table <- renderTable({usgs.data()})
  
  
  # Download the datatable and create name based on site and interval selected:
  output$downloadData <- downloadHandler(
    filename = function(){paste0('USGS_',input$site,'_', names(renameNWISColumns(usgs.data()))[4], "_",
                                 input$interval,'_',min(year(usgs.data()[,3])),'_',
                                 max(year(usgs.data()[,3])),'.xlsx')},
    content = function(fname) {
      write_xlsx(usgs.data(),fname)   
    })
  
  
  # Generate plot of output table:
  output$plot <- renderPlotly({
    plot_ly(usgs.data(), type = 'scatter', mode='lines',height=800) %>%
      add_trace(x = if (input$interval == 'mean_daily')  ~Date else ~dateTime, 
                y = ~usgs.data()[,4], line=list(color='darkblue')) %>%
      layout(showlegend = FALSE, yaxis = list(title = str_sub(input$pCode,8,-2)))
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
                   paste('<b>',  'Monitoring Location:', '</b>', station_nm),
                   sep = '<br/>'),
                 popupOptions = popupOptions(closeButton = FALSE))
  })
}

shinyApp(ui, server)
