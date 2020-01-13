library(shiny); library(dplyr); library(ggplot2); library(tidycensus)
library(sf); library(leaflet); library(tigris); library(scales); library(shinyalert)

# setwd('C:/Users/Tyler/Documents/censusPortal/censusPortal')

# census_api_key("")

blockGroups <- block_groups(state = 'PA', county = 'Philadelphia', class = 'sf')
blockGroups <- st_transform(blockGroups, '+proj=longlat +datum=WGS84')
blockGroups$selected <- 0
azaveaNeighborhoods <- st_read('./Neighborhoods_Philadelphia.shp')
azaveaNeighborhoods <- st_transform(azaveaNeighborhoods, '+proj=longlat +datum=WGS84')
ucd <- st_read('./UCD Boundary_realigned.shp')
ucd <- st_transform(ucd, '+proj=longlat +datum=WGS84')
zips <- st_read('./Zipcodes_Poly.shp')
colnames(zips)[colnames(zips) == 'CODE'] <- 'GEOID'
phl <- st_read('./city_limits.shp')
phl <- st_transform(phl, '+proj=longlat +datum=WGS84')
peerBG <- read.csv('./peerBlockGroups.csv')
ucdBG <- as.character(peerBG$philadelphia[!is.na(peerBG$philadelphia)])



ageTables <- c('B01001_003', 'B01001_004', 'B01001_027', 'B01001_028', 
               'B01001_005', 'B01001_006', 'B01001_029', 'B01001_030',
               'B01001_007', 'B01001_008', 'B01001_009', 'B01001_010',
               'B01001_031', 'B01001_032', 'B01001_033', 'B01001_034',
               'B01001_011', 'B01001_012', 'B01001_035', 'B01001_036',
               'B01001_013', 'B01001_014', 'B01001_015', 'B01001_037',
               'B01001_018', 'B01001_039', 
               'B01001_016', 'B01001_017', 'B01001_018', 'B01001_019',
               'B01001_040', 'B01001_041', 'B01001_042', 'B01001_043',
               'B01001_020', 'B01001_021',
               'B01001_022', 'B01001_023', 'B01001_024',
               'B01001_025', 'B01001_044', 'B01001_045', 'B01001_046',
               'B01001_047', 'B01001_048', 'B01001_049', 'B01001_001')
sexTables <- c('B01001_001', 'B01001_002', 'B01001_026')
raceTables <- c('B02001_001', 'B02001_002', 'B02001_003', 'B02001_004',
                'B02001_005', 'B02001_006', 'B02001_007', 'B02001_008')
homeownerTables <- c('B25003_001', 'B25003_002', 'B25003_003')
incomeTables <- c('B19001_001', 'B19001_002', 'B19001_003', 'B19001_004',
                  'B19001_005', 'B19001_006', 'B19001_007', 'B19001_008',
                  'B19001_009', 'B19001_010', 'B19001_011', 'B19001_012',
                  'B19001_013', 'B19001_014', 'B19001_015', 'B19001_016',
                  'B19001_017')
unempTables <- c('B23025_001', 'B23025_003', 'B23025_004', 'B23025_005',
                 'B23025_006', 'B23025_007')
vacancyTables <- c('B25002_001', 'B25002_002', 'B25002_003', 'B25004_001',
                   'B25004_002', 'B25004_003', 'B25004_004', 'B25004_005',
                   'B25004_006', 'B25004_007', 'B25004_008')
homeValTables <- c('B25075_001', 'B25075_002', 'B25075_003', 'B25075_004',
                   'B25075_005', 'B25075_006', 'B25075_007', 'B25075_008',
                   'B25075_009', 'B25075_010', 'B25075_011', 'B25075_012',
                   'B25075_013', 'B25075_014', 'B25075_015', 'B25075_016',
                   'B25075_017', 'B25075_018', 'B25075_019', 'B25075_020',
                   'B25075_021', 'B25075_022', 'B25075_023', 'B25075_024',
                   'B25075_025', 'B25075_026', 'B25075_027')
rentTables <- c('B25056_002', 'B25056_003', 'B25056_004', 'B25056_005',
                'B25056_006', 'B25056_007', 'B25056_008', 'B25056_009',
                'B25056_010', 'B25056_011', 'B25056_012', 'B25056_013',
                'B25056_014', 'B25056_015', 'B25056_016', 'B25056_017',
                'B25056_018', 'B25056_019', 'B25056_020', 'B25056_021',
                'B25056_022', 'B25056_023', 'B25056_024', 'B25056_025',
                'B25056_026',
                'B25070_001', 'B25070_002', 'B25070_003', 'B25070_004',
                'B25070_005', 'B25070_006', 'B25070_007', 'B25070_008',
                'B25070_009', 'B25070_010')

# censusBG <- get_acs(geography = 'block group',
#                     variables = c(ageTables, sexTables, raceTables,
#                                   homeownerTables, incomeTables, unempTables,
#                                   vacancyTables, homeValTables, rentTables), geometry = F,
#                     state = 'PA', county = 'Philadelphia', year = 2017)
# 
# censusZip <- get_acs(geography = 'zcta',
#                      variables = c(ageTables, sexTables, raceTables,
#                                    homeownerTables, incomeTables, unempTables,
#                                    vacancyTables, homeValTables, rentTables), geometry = F, year = 2017)
# censusZip <- censusZip[censusZip$GEOID %in% zips$GEOID,]
# 
# save(censusBG, censusZip, file = 'F:/Tyler/dataPortal/dataPortal/censusData.RData')

load('./censusData.RData')






ui <- fluidPage(
    
    useShinyalert(),
    
    titlePanel('Philadelphia ACS Portal'),
    
    fluidRow(
        column(width = 4,
               em('To visualize American Community Survey data for a chosen geographic area, set specifications below.'),
               hr(),
               
               radioButtons(
                   inputId = 'defineGeography',
                   label = 'Define your geographical area of interest:',
                   choices = c('By zip code', 'By neighborhood',
                               'Select a point with surrounding radius', 'Select Census block groups', 'Upload shapefile'),
                   selected = F
               ),
               
               checkboxGroupInput(
                   inputId = 'defineVariables',
                   label = 'Which variables interest you?',
                   choices = c('Age', 'Sex', 'Race', 'Household income', '(Un)employment',
                               'Housing vacancy', 'Homeownership', 'Home values', 'Rent')
               ),
               
               hr(),
               
               actionButton(
                   inputId = 'nextButton',
                   label = 'Show results/Refresh',
                   style = 'color: #fff; background-color: #337ab7; border-color: #2e6da4'
               )
        ),
        
        column(width = 8,
               # leafletOutput("mymap")
               textOutput(outputId = 'myText'),
               
               conditionalPanel(
                   condition = "input.defineGeography == 'University City District boundaries'", 
                   leafletOutput('ucdMap')
               ),
               
               conditionalPanel(
                   condition = "input.defineGeography == 'By zip code'",
                   selectInput(inputId = 'zipInput', 
                               label = 'Select a zip code(s) within the city of Philadelphia.',
                               multiple = T,
                               choices = sort(as.character(zips$GEOID)))
               ),
               
               conditionalPanel(
                   condition = "input.zipInput && input.defineGeography == 'By zip code'", 
                   leafletOutput('zipMap')
               ),
               
               conditionalPanel(
                   condition = "input.defineGeography == 'By neighborhood'",
                   selectInput(inputId = 'neighborhoodInput', 
                               label = 'Select a neighborhood(s) for which you would like to see data.',
                               multiple = T,
                               choices = sort(as.character(azaveaNeighborhoods$MAPNAME)))
               ),
               
               conditionalPanel(
                   condition = "input.neighborhoodInput && input.defineGeography == 'By neighborhood'", # Only show map if user wants to select geography by neighborhood AND if they've already selected a neighborhood(s) from dropdown
                   leafletOutput('neighborhoodMap')
               ),
               
               conditionalPanel(
                   # 'Select a point on the map below, then choose a radius size to buffer around that point.'
                   condition = "input.defineGeography == 'Select a point with surrounding radius'",
                   
                   h5('Use this option if you are interested in a specific point. First, enter a value for the radius length of a buffer. 
           Then, click the map within city boundaries (blue) to choose the point the buffer will surround.'),
                   
                   numericInput(inputId = 'radiusInput',
                                # label = 'Use this option if you are interested in a specific point. First, enter a value for the radius length of a buffer.
                                # Then, click the map to choose the point the buffer will surround.',
                                label = 'Enter a buffer radius:',
                                value = 400),
                   radioButtons(inputId = 'radiusUnit', choices = c('meters', 'feet'), label = NA),
                   
                   h5(strong('Click map to select buffer center or to refresh.')),
                   
                   leafletOutput('radiusMap')
               ),
               
               conditionalPanel(
                   condition = "input.defineGeography == 'Select Census block groups'",
                   
                   h5('On the map below, select the Census block groups for which you would like to see data.'),
                   
                   leafletOutput('censusMap'),
                   actionButton(inputId = 'clearSelection', label = 'Clear selection')
               ),
               
               conditionalPanel(
                   condition = "input.defineGeography == 'Upload shapefile'",
                   
                   h5(paste0('Upload a polygon shapefile of the boundaries of your geographical area of interest. You must upload three files with extensions .shp, .shx, and .prj. To upload these together, hold "ctrl" and click the three files.')),
                   
                   h5(em('Note: While it is recommended to upload a polygon shapefile, other feature types (line, point, etc.) are acceptable. In these cases, the Census block groups
              that intersect the features will be selected.')),
                   
                   fileInput(inputId = 'uploadShp',
                             label = NA,
                             multiple = T,
                             accept = c('.shp', '.shx', '.prj')),
                   leafletOutput('shpMap')
               )
        )
    ),
    
    fluidRow(
        
        hr(),
        
        conditionalPanel(
            condition = "input.defineVariables.indexOf('Age') > -1 && input.nextButton", # basically wanting to use an equivalent %in% operator in javascript...https://stackoverflow.com/questions/29637285/conditionalpanel-javascript-conditions-in-shiny-is-there-r-in-operator-in-jav
            
            h2('Age'),
            
            br(),
            
            br(),
            
            column(width = 4,
                   tableOutput(outputId = 'ageTable')
            ),
            
            column(width = 8,
                   plotOutput(outputId = 'agePlot')
            ),
            
            hr()
            
        ), 
        
        conditionalPanel(
            condition = "input.defineVariables.indexOf('Sex') > -1 && input.nextButton", # basically wanting to use an equivalent %in% operator in javascript...https://stackoverflow.com/questions/29637285/conditionalpanel-javascript-conditions-in-shiny-is-there-r-in-operator-in-jav
            
            h2('Sex'),
            
            br(),
            
            br(),
            
            column(width = 4,
                   tableOutput(outputId = 'sexTable')
            ),
            
            column(width = 8,
                   plotOutput(outputId = 'sexPlot')
            ),
            
            hr()
            
        ),
        
        conditionalPanel(
            condition = "input.defineVariables.indexOf('Race') > -1 && input.nextButton", # basically wanting to use an equivalent %in% operator in javascript...https://stackoverflow.com/questions/29637285/conditionalpanel-javascript-conditions-in-shiny-is-there-r-in-operator-in-jav
            
            h2('Race'),
            
            br(),
            
            br(),
            
            column(width = 4,
                   tableOutput(outputId = 'raceTable')
            ),
            
            column(width = 8,
                   plotOutput(outputId = 'racePlot')
            ),
            
            hr()
            
        ),
        
        conditionalPanel(
            condition = "input.defineVariables.indexOf('Household income') > -1 && input.nextButton", # basically wanting to use an equivalent %in% operator in javascript...https://stackoverflow.com/questions/29637285/conditionalpanel-javascript-conditions-in-shiny-is-there-r-in-operator-in-jav
            
            h2('Household Income'),
            
            br(),
            
            br(),
            
            column(width = 4,
                   tableOutput(outputId = 'incomeTable')
            ),
            
            column(width = 8,
                   plotOutput(outputId = 'incomePlot')
            ),
            
            hr()
        ),
        
        conditionalPanel(
            condition = "input.defineVariables.indexOf('(Un)employment') > -1 && input.nextButton", # basically wanting to use an equivalent %in% operator in javascript...https://stackoverflow.com/questions/29637285/conditionalpanel-javascript-conditions-in-shiny-is-there-r-in-operator-in-jav
            
            h2('Employment'),
            
            br(),
            
            br(),
            
            column(width = 4,
                   tableOutput(outputId = 'employTable')
            ),
            
            column(width = 8,
                   plotOutput(outputId = 'employPlot')
            ),
            
            hr()
        ),
        
        conditionalPanel(
            condition = "input.defineVariables.indexOf('Housing vacancy') > -1 && input.nextButton", # basically wanting to use an equivalent %in% operator in javascript...https://stackoverflow.com/questions/29637285/conditionalpanel-javascript-conditions-in-shiny-is-there-r-in-operator-in-jav
            
            h2('Vacancy'),
            
            br(),
            
            br(),
            
            column(width = 4,
                   tableOutput(outputId = 'occupancyTable')
            ),
            
            column(width = 8,
                   plotOutput(outputId = 'occupancyPlot')
            ),
            
            column(width = 4,
                   tableOutput(outputId = 'vacancyTable')
            ),
            
            column(width = 8,
                   plotOutput(outputId = 'vacancyPlot')
            ),
            
            hr()
        ),
        
        conditionalPanel(
            condition = "input.defineVariables.indexOf('Homeownership') > -1 && input.nextButton", # basically wanting to use an equivalent %in% operator in javascript...https://stackoverflow.com/questions/29637285/conditionalpanel-javascript-conditions-in-shiny-is-there-r-in-operator-in-jav
            
            h2('Homeownership'),
            
            br(),
            
            br(),
            
            column(width = 4,
                   tableOutput(outputId = 'homeownerTable')
            ),
            
            column(width = 8,
                   plotOutput(outputId = 'homeownerPlot')
            ),
            
            hr()
        ),
        
        conditionalPanel(
            condition = "input.defineVariables.indexOf('Home values') > -1 && input.nextButton", # basically wanting to use an equivalent %in% operator in javascript...https://stackoverflow.com/questions/29637285/conditionalpanel-javascript-conditions-in-shiny-is-there-r-in-operator-in-jav
            
            h2('Home Values'),
            
            br(),
            
            br(),
            
            column(width = 4,
                   tableOutput(outputId = 'homeValTable')
            ),
            
            column(width = 8,
                   plotOutput(outputId = 'homeValPlot')
            ),
            
            hr()
        ),
        
        conditionalPanel(
            condition = "input.defineVariables.indexOf('Rent') > -1 && input.nextButton", # basically wanting to use an equivalent %in% operator in javascript...https://stackoverflow.com/questions/29637285/conditionalpanel-javascript-conditions-in-shiny-is-there-r-in-operator-in-jav
            
            h2('Residential Rent'),
            
            br(),
            
            br(),
            
            
            column(width = 4,
                   tableOutput(outputId = 'rentTable')
            ),
            
            column(width = 8,
                   plotOutput(outputId = 'rentPlot')
            ),
            
            column(width = 4,
                   tableOutput(outputId = 'rentIncTable')
            ),
            
            column(width = 8,
                   plotOutput(outputId = 'rentIncPlot')
            ),
            
            hr()
        ),
        
        conditionalPanel(
            condition = 'input.nextButton',
            h2('Geographic Selection'),
            leafletOutput('selectionMap')
        )
        
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Below snippet of code is just for testing purposes
    output$myText <- renderPrint({
        # input$uploadShp$datapath
        # input$defineVariables
        # chosenVariables()
        invisible(paste(selectionCounter$counter, input$clearSelection, bgMemory$dat, selectedGEOIDs(), sep = ',')) # for some reason, have to print this or the Clear Selection button doesn't work correctly
        # as.character(shpMapFile())
        # unlist(input$uploadShp)
        # class(input$uploadShp)
        # str(input$uploadShp)
        # paste0('You picked', input$defineVariables)
        # paste(censusClick()$lat, censusClick()$lng, censusClick()$id, sep = ',')
    })
    
    # reactive({
    #   paste(selectionCounter$counter, input$clearSelection, bgMemory$dat, selectedGEOIDs(), sep = ',')
    # })
    
    # buttonCounter <- reactiveValues(counter = 0)
    # observeEvent(input$nextButton, {
    #   buttonCounter$counter <- buttonCounter$counter + 1
    # })
    
    output$ucdMap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite) %>%
            setView(lng = -75.202530 , lat = 39.952354, zoom = 14) %>% 
            addPolygons(data = ucd, fillOpacity = 0.4, fillColor = 'red', weight = 1.5)
    })
    
    output$zipMap <- renderLeaflet({
        selectedZips <- zips %>%
            filter(GEOID %in% input$zipInput)
        
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite) %>%
            setView(lng=-75.137349 , lat = 39.978797, zoom = 10) %>% # Approximate center of city
            addPolygons(data = zips, fillOpacity = 0.4, fillColor = 'gray', weight = 1.5, popup = zips$GEOID) %>%
            addPolygons(data = selectedZips, fillOpacity = 0.4, fillColor = 'red', weight = 1.5)
    })
    
    output$neighborhoodMap <- renderLeaflet({
        zoomNeighborhood <- 11
        centroidNeighborhood <- c(-75.137349, 39.978797)
        selectedNeighborhood <- azaveaNeighborhoods %>%
            filter(MAPNAME %in% input$neighborhoodInput) # had previously used req(input$neighborhoodInput) but that's not necessary with a previous assignment to centroidNeighborhood that can be rewritten
        if (nrow(selectedNeighborhood) == 1){
            centroidNeighborhood <- req(selectedNeighborhood) %>%
                st_centroid() %>%
                st_coordinates()
        }
        if (nrow(selectedNeighborhood) > 1){ # If user selects multiple neighborhoods, zoom out to see whole city
            centroidNeighborhood <- c(-75.137349, 39.978797) # Approximate center of city
            zoomNeighborhood <- 10
        }
        
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite) %>%
            setView(lng = centroidNeighborhood[1] , lat = centroidNeighborhood[2], zoom = zoomNeighborhood) %>%
            addPolygons(data = azaveaNeighborhoods, fillOpacity = 0.4, fillColor = 'gray', weight = 1.5, popup = azaveaNeighborhoods$MAPNAME) %>%
            addPolygons(data = selectedNeighborhood, fillOpacity = 0.4, fillColor = 'red', weight = 1.5) %>%
            addControl('Neighborhoods defined by Azavea.', position = 'bottomleft')
    })
    
    output$radiusMap <- renderLeaflet({ # create a leaflet map that users can click on to choose a point for buffer center
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite) %>%
            setView(lng = -75.163601, lat = 39.952392, zoom = 11) %>% # City Hall
            addPolygons(data = phl, fillOpacity = 0.1, fillColor = 'blue', weight = 1.5)
    })
    
    clickedPoint <- reactive({
        input$radiusMap_click # when a user clicks on a map, reassign coords to clickedPoint()
    })
    
    bufferRadius <- reactive({
        input$radiusInput
    })
    
    observeEvent(clickedPoint(), { # in the event that there's a change to clickedPoint(), do the following...
        leafletProxy(mapId = 'radiusMap') %>% # make changes to leaflet map without completely redrawing
            clearMarkers() %>% # this gets rid of previous markers, so if a user clicks twice, there aren't two markers
            addMarkers(lng = clickedPoint()$lng, lat = clickedPoint()$lat) # add marker when user clicks on the map
    })
    
    observeEvent(clickedPoint(), { # in the event that there's a change to clickedPoint(), do the following...
        
        if(input$radiusUnit == 'meters'){
            thisBuffer <- data.frame(lat = clickedPoint()$lat, lng = clickedPoint()$lng) %>%
                st_as_sf(coords = c('lng', 'lat')) %>%
                st_set_crs(4326) %>%
                # below I'll project to be friendly with feet as units
                st_transform('+proj=lcc +lat_1=39.93333333333333 +lat_2=40.96666666666667 +lat_0=39.33333333333334 +lon_0=-77.75 +x_0=600000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs') %>%
                st_buffer(dist = bufferRadius()*3.28084) %>% # Convert to meters
                st_transform(4326)
        } else{
            thisBuffer <- data.frame(lat = clickedPoint()$lat, lng = clickedPoint()$lng) %>%
                st_as_sf(coords = c('lng', 'lat')) %>%
                st_set_crs(4326) %>%
                # below I'll project to be friendly with feet as units
                st_transform('+proj=lcc +lat_1=39.93333333333333 +lat_2=40.96666666666667 +lat_0=39.33333333333334 +lon_0=-77.75 +x_0=600000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs') %>%
                st_buffer(dist = bufferRadius()) %>% 
                st_transform(4326)
        }
        
        leafletProxy(mapId = 'radiusMap') %>% # make changes to leaflet map without completely redrawing
            clearShapes() %>% # this gets rid of previous buffers, so if a user clicks twice, there aren't two buffers
            addPolygons(data = phl, fillOpacity = 0.1, fillColor = 'blue', weight = 1.5) %>%
            addPolygons(data = thisBuffer, fillOpacity = 0.4, fillColor = 'red', weight = 1.5)
    })
    
    output$censusMap <- renderLeaflet({ 
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite) %>%
            setView(lng = -75.163601, lat = 39.952392, zoom = 11) %>% # City Hall
            addPolygons(data = blockGroups, fillOpacity = 0, fillColor = NA, weight = 1.5, layerId = ~GEOID)
    })
    
    censusClick <- reactive({
        input$censusMap_shape_click
    })
    
    # selectionCounter <- reactiveValues(counterValue = 0)
    # observeEvent(input$censusMap_shape_click, {
    #   selectionCounter$counterValue <- selectionCounter$counterValue + 1
    # })
    
    bgMemory <- reactiveValues(dat = NULL)
    bgTriggers <- reactive({
        list(censusClick(), input$clearSelection)
    })
    
    # selectedGEOIDs <- eventReactive(censusClick(), {
    selectedGEOIDs <- eventReactive(bgTriggers(), {
        isolate(dat <- bgMemory$dat)
        if (is.null(dat)) {
            bgMemory$dat <- blockGroups$GEOID[blockGroups$GEOID %in% censusClick()$id]
        } 
        
        if ((selectionCounter$counter == 1 & input$clearSelection == 1) | (selectionCounter$counter == 1 & input$clearSelection > 1)){
            bgMemory$dat <- NULL
            # selectionCounter$counter <- 0
            return(bgMemory$dat)
        }
        if ((selectionCounter$counter == 0 & input$clearSelection == 0) | (selectionCounter$counter != input$clearSelection)){
            bgMemory$dat <- c(bgMemory$dat, blockGroups$GEOID[blockGroups$GEOID %in% censusClick()$id])
            return(bgMemory$dat)
        }
    })
    
    selectionTriggers <- reactive({
        # list(input$censusMap_shape_click, input$clearSelection)
        list(input$censusMap_shape_click, selectionCounter$counter)
    })
    
    selectionCounter <- reactiveValues(counter = 0)
    observeEvent(input$clearSelection, {
        selectionCounter$counter <- selectionCounter$counter + 1
    })
    
    censusSelected <- eventReactive(selectionTriggers(), {
        if ((selectionCounter$counter == 1 & input$clearSelection == 1)  | (selectionCounter$counter == 1 & input$clearSelection > 1)){
            selectionCounter$counter <- 0
            return(NULL)
        } 
        if ((selectionCounter$counter == 0 & input$clearSelection == 0) | (selectionCounter$counter != input$clearSelection)){
            blockGroups %>%
                filter(GEOID %in% selectedGEOIDs())
        }
    })
    
    observeEvent(input$clearSelection, {
        leafletProxy(mapId = 'censusMap') %>% # make changes to leaflet map without completely redrawing
            clearShapes() %>%
            addPolygons(data = blockGroups, fillOpacity = 0, fillColor = NA, weight = 1.5, layerId = ~GEOID)
    })
    
    observeEvent(censusSelected(), { # in the event that there's a change to censusClick(), do the following...
        leafletProxy(mapId = 'censusMap') %>% # make changes to leaflet map without completely redrawing
            addPolygons(data = req(censusSelected()), fillOpacity = 1, fillColor = 'red', weight = 1.5)
    })
    
    shpMapFile <- reactive({ # this code is from https://paula-moraga.github.io/book-geospatial/sec-shinyexample.html#fig:shiny-snapshot-app2-htmlwidgets
        shpdf <- input$uploadShp # a dataframe with name, size, type, and datapath of uploaded files
        if (length(as.character(shpdf$datapath[1])) > 0){
            tempDirName <- dirname(shpdf$datapath[1]) # name of the temporary directory where the files were uploaded
            for (i in 1:nrow(shpdf)){ # rename files
                file.rename(
                    shpdf$datapath[i],
                    paste0(tempDirName, '/', shpdf$name[i])
                )
            }
            
            # map <- st_read( # this is pretty much the same as Paula Moraga's code, except I'm using sf instead of readOGR/sp
            #   paste(
            #     tempDirName,
            #     shpdf$name[grep(pattern = '*.shp$', shpdf$name)],
            #     sep = '/'
            #   )
            # )
            mapPath <- paste(
                tempDirName,
                shpdf$name[grep(pattern = '*.shp$', shpdf$name)],
                sep = '/'
            )
            
            uploadedShp <- st_read(mapPath) %>%
                st_transform(4326)
            uploadedShp
        }
    })
    
    output$shpMap <- renderLeaflet({
        # uploadedShp <- st_read(shpMapFile()) %>%
        #   st_transform(4326)
        
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite) %>%
            # setView(lng = -75.163601, lat = 39.952392, zoom = 11) %>% # City Hall
            addPolygons(data = req(shpMapFile()), fillOpacity = 0.4, fillColor = NA, weight = 1.5) # req() avoids the error on initiation (when files have yet to be uploaded)
    })
    
    chosenVariables <- eventReactive(input$nextButton, {
        chosenVariables <- NULL
        if('Age' %in% input$defineVariables){
            chosenVariables <- c(chosenVariables, ageTables)
        }
        if('Sex' %in% input$defineVariables){
            chosenVariables <- c(chosenVariables, sexTables)
        }
        if('Race' %in% input$defineVariables){
            chosenVariables <- c(chosenVariables, raceTables)
        }
        if('Household income' %in% input$defineVariables){
            chosenVariables <- c(chosenVariables, incomeTables)
        }
        if('(Un)employment' %in% input$defineVariables){
            chosenVariables <- c(chosenVariables, unempTables)
        }
        if('Housing vacancy' %in% input$defineVariables){
            chosenVariables <- c(chosenVariables, vacancyTables)
        }
        if('Homeownership' %in% input$defineVariables){
            chosenVariables <- c(chosenVariables, homeownerTables)
        }
        if('Home values' %in% input$defineVariables){
            chosenVariables <- c(chosenVariables, homeValTables)
        }
        if('Rent' %in% input$defineVariables){
            chosenVariables <- c(chosenVariables, rentTables)
        }
        chosenVariables <- unique(chosenVariables)
        chosenVariables
    })
    
    censusData <- eventReactive(input$nextButton, {
        if (input$defineGeography != 'By zip code'){ # if user selects zip code as their geography, we'll use zcta. Otherwise, block groups.
            # get_acs(geography = 'block group',
            #         variables = chosenVariables(), geometry = F,
            #         state = 'PA', county = 'Philadelphia', year = 2017)
            censusBG
        } else{
            # get_acs(geography = 'zcta',
            #         variables = chosenVariables(), geometry = T,
            #         state = 'PA', county = 'Philadelphia', year = 2017)
            censusZip
        }
    })
    
    chosenGeography <- eventReactive(input$nextButton, {
        if (input$defineGeography == 'University City District boundaries'){
            thisOutput <- ucd
            # thisOutput <- 'abcdefg'
        }
        if (input$defineGeography == 'By zip code'){
            thisOutput <- zips %>%
                filter(GEOID %in% input$zipInput) %>%
                st_union()
        }
        if (input$defineGeography == 'By neighborhood'){
            thisOutput <- azaveaNeighborhoods %>%
                filter(MAPNAME %in% input$neighborhoodInput) %>%
                st_union()
        }
        if (input$defineGeography == 'Select a point with surrounding radius'){
            if(input$radiusUnit == 'meters'){
                thisOutput <- data.frame(lat = clickedPoint()$lat, lng = clickedPoint()$lng) %>%
                    st_as_sf(coords = c('lng', 'lat')) %>%
                    st_set_crs(4326) %>%
                    # below I'll project to be friendly with feet as units
                    st_transform('+proj=lcc +lat_1=39.93333333333333 +lat_2=40.96666666666667 +lat_0=39.33333333333334 +lon_0=-77.75 +x_0=600000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs') %>%
                    st_buffer(dist = bufferRadius()*3.28084) %>% # Convert to meters
                    st_transform(4326)
            } else{
                thisOutput <- data.frame(lat = clickedPoint()$lat, lng = clickedPoint()$lng) %>%
                    st_as_sf(coords = c('lng', 'lat')) %>%
                    st_set_crs(4326) %>%
                    # below I'll project to be friendly with feet as units
                    st_transform('+proj=lcc +lat_1=39.93333333333333 +lat_2=40.96666666666667 +lat_0=39.33333333333334 +lon_0=-77.75 +x_0=600000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs') %>%
                    st_buffer(dist = bufferRadius()) %>% 
                    st_transform(4326)
            }
        }
        if (input$defineGeography == 'Select Census block groups'){
            thisOutput <- censusSelected() %>%
                st_union()
        }
        if (input$defineGeography == 'Upload shapefile'){
            thisOutput <- shpMapFile()
        }
        thisOutput
    })
    
    
    intersectingBGs <- reactive({
        if (input$defineGeography == 'Select Census block groups'){
            blockGroups[unlist(st_covers(chosenGeography(), blockGroups)),]
        } else if (input$defineGeography == 'By zip code'){
            zips[unlist(st_covers(chosenGeography(), zips)),]
            # censusZip[unlist(st_covers(chosenGeography(), censusZip)),]
        } else if (input$defineGeography == 'University City District boundaries'){
            blockGroups[blockGroups$GEOID %in% ucdBG,]
        } else {
            blockGroups[unlist(st_intersects(chosenGeography(), blockGroups)),]
        }
    })
    
    observeEvent(input$nextButton, {
        thisGeography <- chosenGeography()
        thisCentroid <- st_centroid(thisGeography)
        thisCentroid <- st_coordinates(thisCentroid)
        
        output$selectionMap <- renderLeaflet({
            leaflet() %>%
                addProviderTiles(providers$Stamen.TonerLite) %>%
                # setView(lng = -75.163601, lat = 39.952392, zoom = 11) %>% # City Hall
                setView(lng = thisCentroid[1], lat = thisCentroid[2], zoom = 11) %>% 
                # addPolygons(data = censusDataSelected(), fillOpacity = 0, fillColor = NA, weight = 1.5)
                addPolygons(data = chosenGeography(), fillOpacity = 0.3, fillColor = 'blue', weight = 1.5) %>%
                addPolygons(data = req(intersectingBGs()), fillOpacity = 0, fillColor = NA, color = 'red', weight = 1.5) %>%
                addControl('Blue polygons show selected geography. Red polygons show Census geography used for data output.', position = 'bottomleft')
        })
        
        ageData <- reactive({
            if('Age' %in% input$defineVariables){
                selectedData <- censusData() %>%
                    filter(GEOID %in% intersectingBGs()$GEOID) %>% # just the specified census geographies
                    filter(variable %in% ageTables) # just the specified variables
                
                # age 9 and under
                age0to9 <- c('B01001_003', 'B01001_004', 'B01001_027', 'B01001_028')
                p0to9 <- sum(selectedData$estimate[selectedData$variable %in% age0to9], na.rm = T)/sum(selectedData$estimate[selectedData$variable == 'B01001_001'], na.rm = T)
                
                # age 10 to 17
                age10to17 <- c('B01001_005', 'B01001_006', 'B01001_029', 'B01001_030')
                p10to17 <- sum(selectedData$estimate[selectedData$variable %in% age10to17], na.rm = T)/sum(selectedData$estimate[selectedData$variable == 'B01001_001'], na.rm = T)
                
                # age 18 to 24
                age18to24 <- c('B01001_007', 'B01001_008', 'B01001_009', 'B01001_010',
                               'B01001_031', 'B01001_032', 'B01001_033', 'B01001_034')
                p18to24 <- sum(selectedData$estimate[selectedData$variable %in% age18to24], na.rm = T)/sum(selectedData$estimate[selectedData$variable == 'B01001_001'], na.rm = T)
                
                # age 25 to 34
                age25to34 <- c('B01001_011', 'B01001_012', 'B01001_035', 'B01001_036')
                p25to34 <- sum(selectedData$estimate[selectedData$variable %in% age25to34], na.rm = T)/sum(selectedData$estimate[selectedData$variable == 'B01001_001'], na.rm = T)
                
                # age 35 to 49
                age35to49 <- c('B01001_013', 'B01001_014', 'B01001_015', 'B01001_037',
                               'B01001_018', 'B01001_039')
                p35to49 <- sum(selectedData$estimate[selectedData$variable %in% age35to49], na.rm = T)/sum(selectedData$estimate[selectedData$variable == 'B01001_001'], na.rm = T)
                
                # age 50 to 64
                age50to64 <- c('B01001_016', 'B01001_017', 'B01001_018', 'B01001_019',
                               'B01001_040', 'B01001_041', 'B01001_042', 'B01001_043')
                p50to64 <- sum(selectedData$estimate[selectedData$variable %in% age50to64], na.rm = T)/sum(selectedData$estimate[selectedData$variable == 'B01001_001'], na.rm = T)
                
                # age 65+
                age65plus <- c('B01001_020', 'B01001_021',
                               'B01001_022', 'B01001_023', 'B01001_024',
                               'B01001_025', 'B01001_044', 'B01001_045', 'B01001_046',
                               'B01001_047', 'B01001_048', 'B01001_049')
                p65plus <- sum(selectedData$estimate[selectedData$variable %in% age65plus], na.rm = T)/sum(selectedData$estimate[selectedData$variable == 'B01001_001'], na.rm = T)
                
                ageData <- data.frame(
                    ageBins = c('0-9', '10-17', '18-24', '25-34', '35-49', '50-64', '65+'),
                    pAge = c(p0to9, p10to17, p18to24, p25to34, p35to49, p50to64, p65plus)
                )
                ageData$ageBins <- factor(ageData$ageBins, levels = ageData$ageBins)
                ageData
            }
        })
        
        output$agePlot <- renderPlot({
            ggplot() +
                geom_bar(data = ageData(), aes(x = ageBins, y = pAge), stat = 'identity') +
                theme(panel.grid.major.y = element_line(colour = "gray"),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_blank(),
                      panel.border = element_rect(colour = NA, fill=NA, size=1),
                      plot.caption = element_text(colour='gray50')) +
                labs(title = 'Age Breakdown',
                     x = '', y = '',
                     caption = 'Source: American Community Survey 5-year estimates, 2013-2017') +
                scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 1))
        })
        
        output$ageTable <- renderTable({
            ageDataClean <- tbl_df(ageData())
            ageDataClean$pAge <- percent(ageDataClean$pAge)
            colnames(ageDataClean) <- c('Age Groups', 'Proportion of Residents')
            ageDataClean
        })
        
        sexData <- reactive({
            if('Sex' %in% input$defineVariables){
                selectedData <- censusData() %>%
                    filter(GEOID %in% intersectingBGs()$GEOID) %>% # just the specified census geographies
                    filter(variable %in% sexTables) # just the specified variables
                
                pMale <- sum(selectedData$estimate[selectedData$variable == 'B01001_002'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B01001_001'], na.rm = T)
                pFemale <- sum(selectedData$estimate[selectedData$variable == 'B01001_026'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B01001_001'], na.rm = T)
                
                sexData <- data.frame(
                    sexes = c('Female', 'Male'),
                    pSex = c(pFemale, pMale)
                )
                sexData$sexes <- factor(sexData$sexes, levels = sexData$sexes)
                sexData
            }
        })
        
        output$sexPlot <- renderPlot({
            ggplot() +
                geom_bar(data = sexData(), aes(x = sexes, y = pSex), stat = 'identity') +
                theme(panel.grid.major.y = element_line(colour = "gray"),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_blank(),
                      panel.border = element_rect(colour = NA, fill=NA, size=1),
                      plot.caption = element_text(colour='gray50')) +
                labs(title = 'Sex Breakdown',
                     x = '', y = '',
                     caption = 'Source: American Community Survey 5-year estimates, 2013-2017') +
                scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 1))
        })
        
        output$sexTable <- renderTable({
            sexDataClean <- tbl_df(sexData())
            sexDataClean$pSex <- percent(sexDataClean$pSex)
            colnames(sexDataClean) <- c('', 'Proportion of Residents')
            sexDataClean
        })
        
        raceData <- reactive({
            if('Race' %in% input$defineVariables){
                selectedData <- censusData() %>%
                    filter(GEOID %in% intersectingBGs()$GEOID) %>% # just the specified census geographies
                    filter(variable %in% raceTables) # just the specified variables
                
                pWhite <- sum(selectedData$estimate[selectedData$variable == 'B02001_002'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B02001_001'], na.rm = T)
                pBlack <- sum(selectedData$estimate[selectedData$variable == 'B02001_003'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B02001_001'], na.rm = T)
                pNative <- sum(selectedData$estimate[selectedData$variable == 'B02001_004'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B02001_001'], na.rm = T)
                pAsian <- sum(selectedData$estimate[selectedData$variable == 'B02001_005'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B02001_001'], na.rm = T)
                pHawaii <- sum(selectedData$estimate[selectedData$variable == 'B02001_006'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B02001_001'], na.rm = T)
                pOther <- sum(selectedData$estimate[selectedData$variable == 'B02001_007'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B02001_001'], na.rm = T)
                p2plus <- sum(selectedData$estimate[selectedData$variable == 'B02001_008'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B02001_001'], na.rm = T)
                
                raceData <- data.frame(
                    races = c('American Indian or\nAlaska Native', 'Asian', 'Black or\nAfrican American',
                              'Native Hawaiian or\nPacific Islander', 'White', 'Other', 'Two or more races'),
                    pRace = c(pNative, pAsian, pBlack,
                              pHawaii, pWhite, pOther, p2plus)
                )
                raceData$races <- factor(raceData$races, levels = raceData$races)
                raceData
            }
        })
        
        output$racePlot <- renderPlot({
            ggplot() +
                geom_bar(data = raceData(), aes(x = races, y = pRace), stat = 'identity') +
                theme(panel.grid.major.y = element_line(colour = "gray"),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_blank(),
                      panel.border = element_rect(colour = NA, fill=NA, size=1),
                      plot.caption = element_text(colour='gray50')) +
                labs(title = 'Race Breakdown',
                     x = '', y = '',
                     caption = 'Source: American Community Survey 5-year estimates, 2013-2017') +
                scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 1))
        })
        
        output$raceTable <- renderTable({
            raceDataClean <- tbl_df(raceData())
            raceDataClean$pRace <- percent(raceDataClean$pRace)
            colnames(raceDataClean) <- c('Races', 'Proportion of Residents')
            raceDataClean
        })
        
        
        incomeData <- reactive({
            if('Household income' %in% input$defineVariables){
                selectedData <- censusData() %>%
                    filter(GEOID %in% intersectingBGs()$GEOID) %>% # just the specified census geographies
                    filter(variable %in% incomeTables) # just the specified variables
                
                # under $20k/year
                inc0to20 <- c('B19001_002', 'B19001_003', 'B19001_004')
                p0to20 <- sum(selectedData$estimate[selectedData$variable %in% inc0to20], na.rm = T)/sum(selectedData$estimate[selectedData$variable == 'B19001_001'], na.rm = T)
                
                # 20 to 40
                inc20to40 <- c('B19001_005', 'B19001_006', 'B19001_007', 'B19001_008')
                p20to40 <- sum(selectedData$estimate[selectedData$variable %in% inc20to40], na.rm = T)/sum(selectedData$estimate[selectedData$variable == 'B19001_001'], na.rm = T)
                
                # 40 to 60
                inc40to60 <- c('B19001_009', 'B19001_010', 'B19001_011')
                p40to60 <- sum(selectedData$estimate[selectedData$variable %in% inc40to60], na.rm = T)/sum(selectedData$estimate[selectedData$variable == 'B19001_001'], na.rm = T)
                
                # 60 to 75
                inc60to75 <- 'B19001_012'
                p60to75 <- sum(selectedData$estimate[selectedData$variable %in% inc60to75], na.rm = T)/sum(selectedData$estimate[selectedData$variable == 'B19001_001'], na.rm = T)
                
                # 75 to 100
                inc75to100 <- 'B19001_013'
                p75to100 <- sum(selectedData$estimate[selectedData$variable %in% inc75to100], na.rm = T)/sum(selectedData$estimate[selectedData$variable == 'B19001_001'], na.rm = T)
                
                # 100 to 150
                inc100to150 <- c('B19001_014', 'B19001_015')
                p100to150 <- sum(selectedData$estimate[selectedData$variable %in% inc100to150], na.rm = T)/sum(selectedData$estimate[selectedData$variable == 'B19001_001'], na.rm = T)
                
                # over 150
                inc150plus <- c('B19001_016', 'B19001_017')
                p150plus <- sum(selectedData$estimate[selectedData$variable %in% inc150plus], na.rm = T)/sum(selectedData$estimate[selectedData$variable == 'B19001_001'], na.rm = T)
                
                incomeData <- data.frame(
                    incomeBins = c('Under $20k', '$20k - $40k', '$40k - $60k', '$60k - $75k',
                                   '$75k - $100k', '$100k - $150k', '$150k or more'),
                    pIncome = c(p0to20, p20to40, p40to60, p60to75, p75to100, p100to150, p150plus)
                )
                incomeData$incomeBins <- factor(incomeData$incomeBins, levels = incomeData$incomeBins)
                incomeData
            }
        })
        
        output$incomePlot <- renderPlot({
            ggplot() +
                geom_bar(data = incomeData(), aes(x = incomeBins, y = pIncome), stat = 'identity') +
                theme(panel.grid.major.y = element_line(colour = "gray"),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_blank(),
                      panel.border = element_rect(colour = NA, fill=NA, size=1),
                      plot.caption = element_text(colour='gray50')) +
                labs(title = 'Household Income Breakdown',
                     x = '', y = '',
                     caption = 'Source: American Community Survey 5-year estimates, 2013-2017') +
                scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 1))
        })
        
        output$incomeTable <- renderTable({
            incomeDataClean <- tbl_df(incomeData())
            incomeDataClean$pIncome <- percent(incomeDataClean$pIncome)
            colnames(incomeDataClean) <- c('Income Groups', 'Proportion of Residents')
            incomeDataClean
        })
        
        
        employData <- reactive({
            if('(Un)employment' %in% input$defineVariables){
                selectedData <- censusData() %>%
                    filter(GEOID %in% intersectingBGs()$GEOID) %>% # just the specified census geographies
                    filter(variable %in% unempTables) # just the specified variables
                
                pEmployed <- sum(selectedData$estimate[selectedData$variable == 'B23025_004'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B23025_003'], na.rm = T)
                pUnemployed <- sum(selectedData$estimate[selectedData$variable == 'B23025_005'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B23025_003'], na.rm = T)
                pArmed <- sum(selectedData$estimate[selectedData$variable == 'B23025_006'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B23025_001'], na.rm = T)
                pNotLaborForce <- sum(selectedData$estimate[selectedData$variable == 'B23025_007'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B23025_001'], na.rm = T)
                
                
                employData <- data.frame(
                    statuses = c('Employed\n(In labor force)', 'Unemployed\n(In labor force)', 'Armed forces', 'Not in labor force'),
                    pEmploy = c(pEmployed, pUnemployed, pArmed, pNotLaborForce)
                )
                employData$statuses <- factor(employData$statuses, levels = employData$statuses)
                employData
            }
        })
        
        output$employPlot <- renderPlot({
            ggplot() +
                geom_bar(data = employData(), aes(x = statuses, y = pEmploy), stat = 'identity') +
                theme(panel.grid.major.y = element_line(colour = "gray"),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_blank(),
                      panel.border = element_rect(colour = NA, fill=NA, size=1),
                      plot.caption = element_text(colour='gray50')) +
                labs(title = 'Employment Breakdown',
                     x = '', y = '',
                     caption = 'Source: American Community Survey 5-year estimates, 2013-2017\nNote: Percentages may not sum to 100% because populations in and not in labor force are included.') +
                scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 1))
        })
        
        output$employTable <- renderTable({
            employDataClean <- tbl_df(employData())
            employDataClean$pEmploy <- percent(employDataClean$pEmploy)
            colnames(employDataClean) <- c('Employment Status', 'Proportion of Residents')
            employDataClean
        })
        
        
        occupancyData <- reactive({
            if('Housing vacancy' %in% input$defineVariables){
                selectedData <- censusData() %>%
                    filter(GEOID %in% intersectingBGs()$GEOID) %>% # just the specified census geographies
                    filter(variable %in% vacancyTables) # just the specified variables
                # sum(selectedData$estimate[selectedData$variable == 'B25002_002'], na.rm = T)
                # unique(selectedData$variable)
                pOccupied <- sum(selectedData$estimate[selectedData$variable == 'B25002_002'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25002_001'], na.rm = T)
                pVacant <- sum(selectedData$estimate[selectedData$variable == 'B25002_003'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25002_001'], na.rm = T)
                
                occupancyData <- data.frame(
                    statuses = c('Occupied', 'Vacant'),
                    pOccupancy = c(pOccupied, pVacant)
                )
                occupancyData$statuses <- factor(occupancyData$statuses, levels = occupancyData$statuses)
                occupancyData
            }
        })
        
        output$occupancyPlot <- renderPlot({
            ggplot() +
                geom_bar(data = occupancyData(), aes(x = statuses, y = pOccupancy), stat = 'identity') +
                theme(panel.grid.major.y = element_line(colour = 'gray'),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_blank(),
                      panel.border = element_rect(colour = NA, fill = NA, size = 1),
                      plot.caption = element_text(colour='gray50')) +
                labs(title = 'Residential Occupancy Breakdown',
                     x = '', y = '',
                     caption = 'Source: American Community Survey 5-year estimates, 2013-2017') +
                scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 1))
        })
        
        output$occupancyTable <- renderTable({
            occupancyDataClean <- tbl_df(occupancyData())
            occupancyDataClean$pOccupancy <- percent(occupancyDataClean$pOccupancy)
            colnames(occupancyDataClean) <- c('Occupancy Status', 'Proportion of Residences')
            occupancyDataClean
            # occupancyData()
        })
        
        
        
        vacancyData <- reactive({
            if('Housing vacancy' %in% input$defineVariables){
                selectedData <- censusData() %>%
                    filter(GEOID %in% intersectingBGs()$GEOID) %>% # just the specified census geographies
                    filter(variable %in% vacancyTables) # just the specified variables
                
                pForRent <- sum(selectedData$estimate[selectedData$variable == 'B25004_002'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25004_001'], na.rm = T)
                pRentedUnocc <- sum(selectedData$estimate[selectedData$variable == 'B25004_003'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25004_001'], na.rm = T)
                pForSale <- sum(selectedData$estimate[selectedData$variable == 'B25004_004'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25004_001'], na.rm = T)
                pSoldUnocc <- sum(selectedData$estimate[selectedData$variable == 'B25004_005'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25004_001'], na.rm = T)
                pRec <- sum(selectedData$estimate[selectedData$variable == 'B25004_006'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25004_001'], na.rm = T)
                pMigrant <- sum(selectedData$estimate[selectedData$variable == 'B25004_007'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25004_001'], na.rm = T)
                pOther <- sum(selectedData$estimate[selectedData$variable == 'B25004_008'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25004_001'], na.rm = T)
                
                
                vacancyData <- data.frame(
                    statuses = c('For rent', 'Rented, unoccupied', 'For sale', 'Sold, unoccupied',
                                 'For seasonal\nuse', 'For migrant\nworkers', 'Other vacant'),
                    pVacancy = c(pForRent, pRentedUnocc, pForSale, pSoldUnocc,
                                 pRec, pMigrant, pOther)
                )
                vacancyData$statuses <- factor(vacancyData$statuses, levels = vacancyData$statuses)
                vacancyData
            }
        })
        
        output$vacancyPlot <- renderPlot({
            ggplot() +
                geom_bar(data = vacancyData(), aes(x = statuses, y = pVacancy), stat = 'identity') +
                theme(panel.grid.major.y = element_line(colour = "gray"),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_blank(),
                      panel.border = element_rect(colour = NA, fill=NA, size=1),
                      plot.caption = element_text(colour='gray50')) +
                labs(title = 'Breakdown of Vacant Residential Units',
                     x = '', y = '',
                     caption = 'Source: American Community Survey 5-year estimates, 2013-2017') +
                scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 1))
        })
        
        output$vacancyTable <- renderTable({
            vacancyDataClean <- tbl_df(vacancyData())
            vacancyDataClean$pVacancy <- percent(vacancyDataClean$pVacancy)
            colnames(vacancyDataClean) <- c('Vacancy Status', 'Proportion of Vacant Residences')
            vacancyDataClean
        })
        
        
        homeownerData <- reactive({
            if('Homeownership' %in% input$defineVariables){
                selectedData <- censusData() %>%
                    filter(GEOID %in% intersectingBGs()$GEOID) %>% # just the specified census geographies
                    filter(variable %in% homeownerTables) # just the specified variables
                
                pOwner <- sum(selectedData$estimate[selectedData$variable == 'B25003_002'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25003_001'], na.rm = T)
                pRenter <- sum(selectedData$estimate[selectedData$variable == 'B25003_003'], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25003_001'], na.rm = T)
                
                homeownerData <- data.frame(
                    statuses = c('Owner-occupied', 'Renter-occupied'),
                    pHomeowner = c(pOwner, pRenter)
                )
                homeownerData$statuses <- factor(homeownerData$statuses, levels = homeownerData$statuses)
                homeownerData
            }
        })
        
        output$homeownerPlot <- renderPlot({
            ggplot() +
                geom_bar(data = homeownerData(), aes(x = statuses, y = pHomeowner), stat = 'identity') +
                theme(panel.grid.major.y = element_line(colour = "gray"),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_blank(),
                      panel.border = element_rect(colour = NA, fill=NA, size=1),
                      plot.caption = element_text(colour='gray50')) +
                labs(title = 'Homeownership Breakdown',
                     x = '', y = '',
                     caption = 'Source: American Community Survey 5-year estimates, 2013-2017') +
                scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 1))
        })
        
        output$homeownerTable <- renderTable({
            homeownerDataClean <- tbl_df(homeownerData())
            homeownerDataClean$pHomeowner <- percent(homeownerDataClean$pHomeowner)
            colnames(homeownerDataClean) <- c('Status', 'Proportion of Residences')
            homeownerDataClean
        })
        
        
        homeValData <- reactive({
            if('Home values' %in% input$defineVariables){
                selectedData <- censusData() %>%
                    filter(GEOID %in% intersectingBGs()$GEOID) %>% # just the specified census geographies
                    filter(variable %in% homeValTables) # just the specified variables
                
                val0to100 <- c('B25075_002', 'B25075_003', 'B25075_004',
                               'B25075_005', 'B25075_006', 'B25075_007', 'B25075_008',
                               'B25075_009', 'B25075_010', 'B25075_011', 'B25075_012',
                               'B25075_013', 'B25075_014')
                p0to100 <- sum(selectedData$estimate[selectedData$variable %in% val0to100], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25075_001'], na.rm = T)
                
                val100to200 <- c('B25075_015', 'B25075_016',
                                 'B25075_017', 'B25075_018')
                p100to200 <- sum(selectedData$estimate[selectedData$variable %in% val100to200], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25075_001'], na.rm = T)
                
                val200to300 <- c('B25075_019', 'B25075_020')
                p200to300 <- sum(selectedData$estimate[selectedData$variable %in% val200to300], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25075_001'], na.rm = T)
                
                val300to400 <- 'B25075_021'
                p300to400 <- sum(selectedData$estimate[selectedData$variable %in% val300to400], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25075_001'], na.rm = T)
                
                val400to500 <- 'B25075_022'
                p400to500 <- sum(selectedData$estimate[selectedData$variable %in% val400to500], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25075_001'], na.rm = T)
                
                val500to750 <- 'B25075_023'
                p500to750 <- sum(selectedData$estimate[selectedData$variable %in% val500to750], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25075_001'], na.rm = T)
                
                val750plus <- c('B25075_024', 'B25075_025', 'B25075_026', 'B25075_027')
                p750plus <- sum(selectedData$estimate[selectedData$variable %in% val750plus], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25075_001'], na.rm = T)
                
                
                homeValData <- data.frame(
                    statuses = c('$0-$100k', '$100k-$200k', '$200k-$300k', '$300k-$400k', '$400k-$500k', '$500k-$750k', 'More than $750k'),
                    pHomeVal = c(p0to100, p100to200, p200to300, p300to400, p400to500, p500to750, p750plus)
                )
                homeValData$statuses <- factor(homeValData$statuses, levels = homeValData$statuses)
                homeValData
            }
        })
        
        
        output$homeValPlot <- renderPlot({
            ggplot() +
                geom_bar(data = homeValData(), aes(x = statuses, y = pHomeVal), stat = 'identity') +
                theme(panel.grid.major.y = element_line(colour = "gray"),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_blank(),
                      panel.border = element_rect(colour = NA, fill=NA, size=1),
                      plot.caption = element_text(colour='gray50')) +
                labs(title = 'Home Value Breakdown',
                     x = '', y = '',
                     caption = 'Source: American Community Survey 5-year estimates, 2013-2017') +
                scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 1))
        })
        
        output$homeValTable <- renderTable({
            homeValDataClean <- tbl_df(homeValData())
            homeValDataClean$pHomeVal <- percent(homeValDataClean$pHomeVal)
            colnames(homeValDataClean) <- c('Value', 'Proportion of Residences')
            homeValDataClean
        })
        
        
        rentData <- reactive({
            if('Rent' %in% input$defineVariables){
                selectedData <- censusData() %>%
                    filter(GEOID %in% intersectingBGs()$GEOID) %>% # just the specified census geographies
                    filter(variable %in% rentTables) # just the specified variables
                
                rent300Less <- c('B25056_003', 'B25056_004', 'B25056_005', 'B25056_006', 'B25056_007')
                p300Less <- sum(selectedData$estimate[selectedData$variable %in% rent300Less], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25056_002'], na.rm = T)
                
                rent300to500 <- c('B25056_008', 'B25056_009', 'B25056_010', 'B25056_011')
                p300to500 <- sum(selectedData$estimate[selectedData$variable %in% rent300to500], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25056_002'], na.rm = T)
                
                rent500to750 <- c('B25056_012', 'B25056_013', 'B25056_014', 'B25056_015', 'B25056_016')
                p500to750 <- sum(selectedData$estimate[selectedData$variable %in% rent500to750], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25056_002'], na.rm = T)
                
                rent750to1000 <- c('B25056_017', 'B25056_018', 'B25056_019')
                p750to1000 <- sum(selectedData$estimate[selectedData$variable %in% rent750to1000], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25056_002'], na.rm = T)
                
                rent1000to1500 <- c('B25056_020', 'B25056_021')
                p1000to1500 <- sum(selectedData$estimate[selectedData$variable %in% rent1000to1500], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25056_002'], na.rm = T)
                
                rent1500to2500 <- c('B25056_022', 'B25056_023')
                p1500to2500 <- sum(selectedData$estimate[selectedData$variable %in% rent1500to2500], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25056_002'], na.rm = T)
                
                rent2500to3500 <- c('B25056_024', 'B25056_025')
                p2500to3500 <- sum(selectedData$estimate[selectedData$variable %in% rent2500to3500], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25056_002'], na.rm = T)
                
                rent3500plus <- 'B25056_026'
                p3500plus <- sum(selectedData$estimate[selectedData$variable %in% rent3500plus], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25056_002'], na.rm = T)
                
                rentData <- data.frame(
                    statuses = c('Less than $300', '$300-$500', '$500-$750', '$750-$1000', '$1000-$1500', '$1500-$2500',
                                 '$2500-$3500', 'More than $3500'),
                    pRent = c(p300Less, p300to500, p500to750, p750to1000, p1000to1500, p1500to2500,
                              p2500to3500, p3500plus)
                )
                
                rentData$statuses <- factor(rentData$statuses, levels = rentData$statuses)
                rentData
            }
        })
        
        
        output$rentPlot <- renderPlot({
            ggplot() +
                geom_bar(data = rentData(), aes(x = statuses, y = pRent), stat = 'identity') +
                theme(panel.grid.major.y = element_line(colour = "gray"),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_blank(),
                      panel.border = element_rect(colour = NA, fill=NA, size=1),
                      plot.caption = element_text(colour='gray50')) +
                labs(title = 'Residential Rent Breakdown',
                     x = 'Monthly Rent', y = '',
                     caption = 'Source: American Community Survey 5-year estimates, 2013-2017') +
                scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 1))
        })
        
        output$rentTable <- renderTable({
            rentDataClean <- tbl_df(rentData())
            rentDataClean$pRent <- percent(rentDataClean$pRent)
            colnames(rentDataClean) <- c('Value', 'Proportion of Residences')
            rentDataClean
        })
        
        
        rentIncData <- reactive({
            if('Rent' %in% input$defineVariables){
                selectedData <- censusData() %>%
                    filter(GEOID %in% intersectingBGs()$GEOID) %>% # just the specified census geographies
                    filter(variable %in% rentTables) # just the specified variables
                
                rentInc10Less <- 'B25070_002'
                p10Less <- sum(selectedData$estimate[selectedData$variable %in% rentInc10Less], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25070_001'], na.rm = T)
                
                rentInc10to20 <- c('B25070_003', 'B25070_004')
                p10to20 <- sum(selectedData$estimate[selectedData$variable %in% rentInc10to20], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25070_001'], na.rm = T)
                
                rentInc20to30 <- c('B25070_005', 'B25070_006')
                p20to30 <- sum(selectedData$estimate[selectedData$variable %in% rentInc20to30], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25070_001'], na.rm = T)
                
                rentInc30to40 <- c('B25070_007', 'B25070_008')
                p30to40 <- sum(selectedData$estimate[selectedData$variable %in% rentInc30to40], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25070_001'], na.rm = T)
                
                rentInc40to50 <- 'B25070_009'
                p40to50 <- sum(selectedData$estimate[selectedData$variable %in% rentInc40to50], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25070_001'], na.rm = T)
                
                rentInc50Plus <- 'B25070_010'
                p50Plus <- sum(selectedData$estimate[selectedData$variable %in% rentInc50Plus], na.rm = T)/
                    sum(selectedData$estimate[selectedData$variable == 'B25070_001'], na.rm = T)
                
                
                rentIncData <- data.frame(
                    statuses = c('Less than 10%', '10%-20%', '20%-30%', '30%-40%', '40%-50%', 'More than 50%'),
                    pRentInc = c(p10Less, p10to20, p20to30, p30to40, p40to50, p50Plus)
                )
                
                rentIncData$statuses <- factor(rentIncData$statuses, levels = rentIncData$statuses)
                rentIncData
            }
        })
        
        
        output$rentIncPlot <- renderPlot({
            ggplot() +
                geom_bar(data = rentIncData(), aes(x = statuses, y = pRentInc), stat = 'identity') +
                theme(panel.grid.major.y = element_line(colour = "gray"),
                      panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_blank(),
                      panel.border = element_rect(colour = NA, fill=NA, size=1),
                      plot.caption = element_text(colour='gray50')) +
                labs(title = 'Rent as a Percentage of Household Income',
                     x = '% of Household Income Spent on Rent', y = '',
                     caption = 'Source: American Community Survey 5-year estimates, 2013-2017') +
                scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 1))
        })
        
        output$rentIncTable <- renderTable({
            rentIncDataClean <- tbl_df(rentIncData())
            rentIncDataClean$pRentInc <- percent(rentIncDataClean$pRentInc)
            colnames(rentIncDataClean) <- c('Value', 'Proportion of Residences')
            rentIncDataClean
        })
        
        
        
        
    }) # this closes input$nextButton observeEvent
    
}





# mtcars$qsec %<>% as.integer







# Run the application 
shinyApp(ui = ui, server = server)

