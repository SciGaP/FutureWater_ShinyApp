library(shiny)
library(leaflet) # for map
library(DBI) # for SQL database connection and queries
library(odbc)
library(RColorBrewer) # for map color pallettes
library(dplyr) # for data summary/transformation
library(ggplot2)
library(plotly) # for interactive plots
library(DT) # for data table
library(raster) # aggregate function for changing HUC level
library(shinyBS)

# ---------------------------------------------------------------
# ---------------------------------------------------------------
# user interface set up

ui <- fluidPage(
  titlePanel("Future Water Indiana: Visualizing climate change impacts on the hydrology of the Wabash Basin"),
  
  bsModal(id = 'startupModal', title = h2('Welcome to the Future Water Indiana Data Explorer'), trigger = '',
          size = 'large', 
          p("This interactive interface created with ", 
            a("R Shiny", href = "https://shiny.rstudio.com/", target = "_blank"), 
            " can be used to explore maps, graphs, and tables of projected climate 
            change impacts on the hydrology of the Wabash River Basin. The dataset behind these visualizations consists of outputs from the 
            computer-based watershed model ", 
            a("SWAT.", href = "https://swat.tamu.edu/", target = "_blank"), 
            " The climate change projections 
            used to drive the SWAT model are freely available ", 
            a("here", href = "http://www.crc.nd.edu/~kbyun/CMIP5_IN_CCIA.html", target = "_blank", "."), 
            br(),
            p("The model results presented have been published in an", 
              a("open-access peer-reviewed journal article.", 
                href = "https://www.mdpi.com/2073-4441/12/1/181", target = "_blank"), 
              "This dataset can be used to facilitate teaching and scholarly research collaboration. If you use 
              the dataset, please cite the paper:"),
            p(tags$b("Dierauer, J.R. and Zhu, C. (2020). Drought in the twenty-first century in a water-rich region: modeling 
              study of the Wabash River watershed, USA. Water, 12(1). https://doi.org/10.3390/w12010181")),
            br(),
            p("The Wabash River Basin hydrological model is a live model. It will be continually updated 
              with new and better data and additional model components, such as 3-dimensional groundwater flow."))
          
            ),
  
  fluidRow(
    tabsetPanel(
      tabPanel("Interactive Map", 
               br(),
               fluidRow(style = "padding-left:20px",
                        column(2, 
                               # input: select variable to map
                               selectInput("map.var", "Variable:", 
                                           c("Precipitation" = "_precip",
                                             "Evapotranspiration" = "_et",
                                             "Soil water content" = "_sw",
                                             "Groundwater Recharge" = "_perc", 
                                             "Baseflow" = "_gw_q",
                                             "Streamflow" = "_flow_out",
                                             "Water Yield" = "_wyld"))
                        ),
                        column(2, 
                               # input: select time period
                               selectInput("map.stype", "Summary Period:", 
                                           c("Annual" = "ann",
                                             #"Winter (DJF)" = c(12, 1, 2),
                                             #"Spring (MAM)" = c(3, 4, 5), 
                                             #"Summer (JJA)" = c(6, 7, 8), 
                                             #"Fall (SON)" = c(9, 10, 11),
                                             "January" = 1,
                                             "February" = 2,
                                             "March" = 3, 
                                             "April" = 4,
                                             "May" = 5,
                                             "June" = 6,
                                             "July" = 7,
                                             "August" = 8,
                                             "September" = 9,
                                             "October" = 10,
                                             "November" = 11,
                                             "December" = 12))
                        ),
                        column(2, 
                               # input: select time period
                               selectInput("map.period", "Time Period:", 
                                           c("2020s" = "2020", 
                                             "2050s" = "2050",
                                             "2080s" = "2080"))
                        ),
                        
                        column(3, 
                               selectInput("map.rcp", "Emissions Scenario:", 
                                           c("Medium (RCP 4.5)" = "45",
                                             "High (RCP 8.5)" = "85"))
                        ), 
                        
                        column(3, 
                               selectInput("huc", "Subwatershed Size:", 
                                           c("Small (HUC 12)" = 12,
                                             "Medium (HUC 10)" = 10, 
                                             "Large (HUC 8)" = 8))
                        )
               ),
               fluidRow(style = "padding-left:20px",
                        
                        column(6,
                               htmlOutput("map.text")
                        ),
                        column(6,
                               htmlOutput("plot.text")
                        )
               ),
               hr(),
               fluidRow(style = "padding-left:20px",
                        column(6, 
                               leafletOutput("map", height = 500)
                        ),
                        column(6,
                               plotlyOutput("map.plot")
                        )
               )
               
               
      ), # end of interactive map panel      
      
      tabPanel("Interactive Plot", br(),
               fluidRow(style = "padding-left:20px",
                        column(3, 
                               # input: select variable to map
                               selectInput("plot.var", "Variable:", 
                                           c("Precipitation" = "_precip",
                                             "Evapotranspiration" = "_et",
                                             "Soil water content" = "_sw",
                                             "Groundwater Recharge" = "_perc", 
                                             "Baseflow" = "_gw_q",
                                             "Streamflow" = "_flow_out",
                                             "Water Yield" = "_wyld"))
                        ),
                        column(3, 
                               # input: select variable to map
                               selectInput("plot.type", "Plot Type:", 
                                           c("Annual Change (%)" = "annual",
                                             "Monthly Change (%)" = "monthly"))
                        )
               ), 
               hr(),
               
               fluidRow(style = "padding-left:20px", 
                        column(10, offset = 1, 
                               htmlOutput("text1")
                        )
               ),
               
               br(),
               
               fluidRow(style = "padding-left:20px", 
                        column(10, offset = 1, 
                               plotlyOutput("plot")
                        ))
               
               
      ), # end of interactive plot panel
      
      tabPanel("Data Download", br(),
               fluidRow(style = "padding-left:20px",
                        column(3, 
                               # input: select variable to map
                               selectInput("table.var", "Variable:", 
                                           c("Precipitation" = "_precip",
                                             "Evapotranspiration" = "_et",
                                             "Soil water content" = "_sw",
                                             "Groundwater Recharge" = "_perc", 
                                             "Baseflow" = "_gw_q",
                                             "Streamflow" = "_flow_out",
                                             "Water Yield" = "_wyld"))
                        ),
                        column(3, 
                               # input: select time period
                               selectInput("table.period", "Time Period:", 
                                           c("Historical" = "1980", 
                                             "2020s" = "2020", 
                                             "2050s" = "2050",
                                             "2080s" = "2080"))
                        ),
                        column(3, 
                               # input: select time period
                               selectInput("table.stype", "Summary Period:", 
                                           c("Annual" = "ann", 
                                             "Monthly" = "month"))
                        ),
                        column(3, 
                               conditionalPanel("input.table.period != '1980'", 
                                                # input: select rcp
                                                selectInput("table.rcp", "Emissions Scenario:", 
                                                            c("Medium (RCP 4.5)" = "45",
                                                              "High (RCP 8.5)" = "85"))
                               )
                               
                        )
                        
               ),
               
               hr(),
               
               fluidRow(style = "padding-left:20px",
                        column(10, align = 'center', offset = 1,
                               DT::dataTableOutput("querytable")
                        )
               ),
               
               hr(),
               
               fluidRow(style = "padding-left:20px",
                        column(4, offset = 1,
                               downloadButton("downloadData", "Download the data in this table")
                        )
               )
               
      ) # end of data download user interface setup
    )
  )
          ) # end of user-inferface setup


# load the data - shapefile for mapping, SQL database connection, annual and monthly plot .r codes

basins <- readRDS(url("https://ndownloader.figshare.com/files/17406317", "r"))
basins.huc8 <- aggregate(basins, by = 'HUC8')
basins.huc10 <- aggregate(basins, by = 'HUC10')
basins.huc8@data$id <- basins.huc8@data$HUC8
basins.huc10@data$id <- basins.huc10@data$HUC10


# connect to MySQL database
db <- dbConnect(odbc(), 
                Driver = "MySQL ODBC 8.0 Unicode Driver",
                Server = "sasrdsmp01.uits.iu.edu", 
                Database = "pfechyd_swat", 
                user = "pfechyd_read", 
                password = "WabashBasin123!#", 
                port = 3306)

options(stringsAsFactors = F) # change default so database queries return characters as strings, not factors

# get column names for monthly and annual tables
columns.month <- dbGetQuery(db, "SELECT column_name FROM information_schema.columns WHERE table_name = 'hydro_month'")$column_name
columns.annual <- dbGetQuery(db, "SELECT column_name FROM information_schema.columns WHERE table_name = 'hydro_ann'")$column_name

columns.month.pct <- dbGetQuery(db, "SELECT column_name FROM information_schema.columns WHERE table_name = 'hydro_month_pct_chng'")$column_name
columns.annual.pct <- dbGetQuery(db, "SELECT column_name FROM information_schema.columns WHERE table_name = 'hydro_ann_pct_chng'")$column_name

# gcm lookup table for plot pop-ups - used to show GCM name instead of the integer ID
gcm.lu <- dbGetQuery(db, "SELECT gcm_id, gcm_name FROM gcm")

# lookup table for input$var - used to grab labels for map legend and pop-ups
labels <- data.frame(input.var = c("_precip",
                                   "_et",
                                   "_sw",
                                   "_perc", 
                                   "_gw_q",
                                   "_flow_out",
                                   "_wyld"), 
                     label = c("precipitation",
                               "evapotranspiration",
                               "soil water",
                               "gw recharge",
                               "baseflow",
                               "streamflow",
                               "water yield"))


server <- function(input, output, session) {
  
  toggleModal(session, "startupModal", toggle = "open")
  
  # set up a reactive value to store the subbasin id based on map click
  activeSubbasin <- reactiveVal()
  
  # reactive to update the percent change data plotted on map based on drop-down menu choices
  mapData <- reactive({
    
    # build the SQL query from the user selections
    if (input$map.stype == "ann") {
      col.name <- columns.annual.pct[grep(input$map.var, columns.annual.pct)]
      proj.query <- paste0("SELECT ", col.name, ", subbasin FROM hydro_ann_pct_chng WHERE (period = ", input$map.period,
                           ") AND (rcp = ", input$map.rcp, ")")
    } else {
      col.name <- columns.month.pct[grep(input$map.var, columns.month.pct)]
      proj.query <- paste0("SELECT ", col.name, ", subbasin FROM hydro_month_pct_chng WHERE (period = ", input$map.period,
                           ") AND (rcp = ", input$map.rcp, ") AND (calendar_month = ", 
                           input$map.stype, ")")
    }
    
    # query the database
    pct.change <- dbGetQuery(db, proj.query)
    
    # rename the columns for use with different variables - enables the following code to be generic (for any variable)
    colnames(pct.change) <- c("value", "subbasin")
    pct.change <- pct.change %>% group_by(subbasin) %>% summarize(value = mean(value))
    pct.change <- pct.change$value
    
    # summarize by huc level
    if (input$huc == 10) {
      # summarize by huc 10 codes from basin shapefile
      pct.change <- tapply(pct.change, basins@data$HUC10, mean, na.rm = T)
      
    } else if (input$huc == 8) {
      # summarize by huc 10 codes from basin shapefile
      pct.change <- tapply(pct.change, basins@data$HUC8, mean, na.rm = T)
      
    } else if (input$huc == 12) {
      pct.change <- pct.change
    }
    
  })
  
  
  
  mapLayer <- reactive({
    
    if (input$huc == 8) {
      map.layer <- basins.huc8
    } else if (input$huc == 10) {
      map.layer <- basins.huc10
    } else if (input$huc == 12) {
      map.layer <- basins
    }
    
  })
  
  # generate the base map
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("OpenStreetMap.DE") %>% #"Stamen.TonerLite", group = "Toner Lite"
      fitBounds(lng1 = -88.9, lat1 = 37.78, lng2 = -84.4, lat2 = 41.35)
    
  })
  
  # observer to update the map whenever the user-selections change (i.e. the mapData() reactive)
  # leafletProxy allows the base map to remain and just the shapefile to regenerate
  observe({
    
    pct.change <- mapData()
    
    # generate a color pallette from reactive expression output
    mbreaks <- c(0, quantile(abs(pct.change), c(0.20, 0.4, 0.6, 0.8), na.rm = T), max(abs(pct.change), na.rm = T))
    mbreaks <- ceiling(mbreaks)
    mbreaks <- unique(c(rev(-1 * mbreaks), mbreaks))
    pal <- colorBin(palette = "RdBu", domain = pct.change, bins = mbreaks)
    
    shapefile <- mapLayer()
    
    id <- as.vector(shapefile$id)
    
    leafletProxy("map") %>% 
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(data = shapefile,
                  layerId = id, stroke = T, color = "black", # move this to a proxy so that whole map is not rebuilt
                  smoothFactor = 1, weight = 1, 
                  fillColor = ~pal(pct.change), fillOpacity = 0.65, 
                  popup = paste0("<b>Ensemble Mean</b><br> % change in ", labels$label[grep(input$map.var, labels$input.var)], ": ", round(pct.change, 1)), 
                  highlight = highlightOptions(weight = 2.5, fillOpacity = 0.65, bringToFront = T)) %>% 
      addLegend("bottomleft", pal = pal, values = pct.change,
                title = paste0("% Change in<br>", labels$label[grep(input$map.var, labels$input.var)], 
                               "<br>(Ensemble Mean)"),
                opacity = 0.65)
  })
  
  # observer to clear activeSubbasin value when watershed summary level is changed
  observeEvent(input$huc, {
    activeSubbasin(NULL)
  })
  
  # observer for click events on the map - updates that activeSubbasin and outlines it in red
  observeEvent(input$map_shape_click, {
    
    p <- input$map_shape_click # get the subbasin data from click
    activeSubbasin(p$id) # update the reactive value
    
    # if null, do nothing
    if (is.null(p)) 
      return()
    
    # filter shapefile to return only the selected (clicked) subbasin
    shapefile <- mapLayer()
    selected <- shapefile[shapefile$id == p$id,]
    
    # update the leaflet map - outline selected subbasin in red
    proxy <- leafletProxy("map")
    proxy %>% addPolygons(data = selected, 
                          color = "red", 
                          fillColor = NA,
                          weight = 3, 
                          stroke = T, 
                          layerId = "Selected")
    
  })
  
  # reactive that creates the plot data based on the map click
  plotData <- reactive({
    
    current.subbasin <- activeSubbasin()
    
    if (is.null(current.subbasin)) 
      return()
    
    if (input$huc == 10) {
      sub.query <- basins@data$subbasin[basins@data$HUC10 == current.subbasin]
      sub.query <- paste(sub.query, collapse = ", ")
      sub.name <- unique(basins@data$NameHUC10[basins@data$HUC10 == current.subbasin])
    } else if (input$huc == 8) {
      sub.query <- basins@data$subbasin[basins@data$HUC8 == current.subbasin]
      sub.query <- paste(sub.query, collapse = ", ")
      sub.name <- unique(basins@data$NameHUC8[basins@data$HUC8 == current.subbasin])
    } else if (input$huc == 12) {
      sub.query <- current.subbasin
      sub.name <- basins@data$NameHUC12[basins@data$subbasin == current.subbasin]
    }
    
    if (nchar(sub.query) > 0) {
      sub.query <- paste0("(", sub.query, ")")
      
      if (input$map.stype == "ann") {
        # build query for annual values
        col.name.map <- columns.annual[grep(input$map.var, columns.annual)]
        map.query <- paste0("SELECT ", col.name.map, 
                            ", rcp, period, gcm_id FROM hydro_ann WHERE subbasin ", 
                            "IN ", sub.query)
        
      } else {
        col.name.map <- columns.month[grep(input$map.var, columns.month)]
        map.query <- paste0("SELECT ", col.name.map, 
                            ", rcp, period, gcm_id FROM hydro_month WHERE subbasin ", 
                            "IN ", sub.query, 
                            " AND calendar_month = ", input$map.stype)
      }
      
      # query the database
      map.plot.data <- dbGetQuery(db, map.query)
      
      # change column name to be consistent between different variable choices - same as with map - allows following code to be generic
      colnames(map.plot.data)[colnames(map.plot.data) == col.name.map] <- "value"
      
      # further summarize the data - mean value by scenario
      map.plot.data <- map.plot.data %>% dplyr::filter(rcp != "26") %>% 
        group_by(rcp, period, gcm_id) %>% 
        summarize(mean = mean(value)) %>% 
        arrange(gcm_id) # sort so that the historical value (gcm_id 11) is last
      
      periods <- map.plot.data$period[map.plot.data$period != "1980"]
      periods <- paste0(periods, "s")
      rcps <- map.plot.data$rcp[map.plot.data$period != "1980"]
      rcps[rcps == "45"] <- "4.5"
      rcps[rcps == "85"] <- "8.5"
      period.rcp <- paste(periods, rcps, sep = "-")
      
      # create an ordered factor so things plot in desired order
      period.rcp <- factor(period.rcp, levels = unique(period.rcp[order(periods, rcps)]), ordered = T)
      
      # calculate percent change from historical
      dat.pct <- map.plot.data$mean[map.plot.data$period != "1980"]
      dat.pct <- ((dat.pct - map.plot.data$mean[map.plot.data$period == "1980"]) / 
                    map.plot.data$mean[map.plot.data$period == "1980"]) * 100
      
      dat <- data.frame(pct.change = round(dat.pct, 1), period.rcp = period.rcp, 
                        gcm_id = map.plot.data$gcm_id[map.plot.data$period != "1980"], 
                        period = periods, rcp = rcps)
      
      # calculate mean values to add as another point layer
      dat.mean <- dat %>% group_by(period.rcp) %>% summarize(pct.change = round(mean(pct.change), 1), 
                                                             period = unique(period), 
                                                             rcp = unique(rcp))
      
      dat <- inner_join(dat, gcm.lu, by = 'gcm_id')
      dat$GCM <- as.factor(dat$gcm_name)
      
      # set size based on current map selection
      selected.period.rcp <- paste0(input$map.period, "s-", 
                                    ifelse(input$map.rcp == "26", "2.6", 
                                           ifelse(input$map.rcp == "45", "4.5", "8.5")))
      dat$size <- 1.75
      dat$size[dat$period.rcp == selected.period.rcp] <- 2.5
      dat$alpha <- 0.3
      dat$alpha[dat$period.rcp == selected.period.rcp] <- 1
      dat.mean$size <- 3
      dat.mean$size[dat.mean$period.rcp == selected.period.rcp] <- 4
      dat.mean$alpha <- 0.4
      dat.mean$alpha[dat.mean$period.rcp == selected.period.rcp] <- 1
      
      dat <- list('data' = dat, 'mean' = dat.mean, 'sub.name' = sub.name)
      return(dat)
    } else {
      return()
    }
    
  })
  
  # plot based on map click - selected subbasin
  output$map.plot <- renderPlotly({
    
    dat <- plotData()
    if (is.null(dat)) {
      
    } else {
      dat.mean <- dat[['mean']]
      period.rcp <- dat[['period.rcp']]
      dat <- dat[['data']]
      
      p2 <- ggplot(dat, aes(x = period.rcp, y = pct.change, fill = dat$GCM)) + 
        geom_point(aes(color = dat$GCM, 
                       text = paste("% Change: ", dat$pct.change, 
                                    "<br>Climate Model: ", dat$GCM, 
                                    "<br>Period: ", dat$period, 
                                    "<br>Emissions Scenario: ", ifelse(dat$rcp == "2.6", "Low",
                                                                       ifelse(dat$rcp == "4.5", "Medium", "High")))), 
                   size = dat$size, alpha = dat$alpha) +
        geom_point(data = dat.mean, aes(text = paste("<b>Ensemble Mean</b>", 
                                                     "<br>% Change: ", dat.mean$pct.change,  
                                                     "<br>Period: ", dat.mean$period, 
                                                     "<br>Emissions Scenario: ", ifelse(dat.mean$rcp == "2.6", "Low",
                                                                                        ifelse(dat.mean$rcp == "4.5", "Medium", "High")))), 
                   shape = 22, size = dat.mean$size, color = "black", fill = NA, alpha = dat.mean$alpha) +
        geom_hline(yintercept = 0, linetype = "dashed") + 
        theme_classic() + 
        ylab("Percent change") + 
        xlab("") + 
        theme(axis.text.x = element_text(angle = 45)) + 
        theme(legend.position = "none", plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm"))
      
      ggplotly(p2, tooltip = "text")
    }
    
  })
  
  output$map.text <- renderText({
    
    mean.val <- mean(mapData())
    
    paste0("The map below shows ", 
           ifelse(input$map.stype == "ann", "", 
                  paste0(month.name[as.integer(input$map.stype)], " ")), 
           labels$label[grep(input$map.var, labels$input.var)], 
           " as the percent change from the long-term average (1971-2000), 
           calculated as the average from the 10 different 
           climate models used in this study. On average across the Wabash River Basin, ", 
           ifelse(input$map.stype == "ann", "", 
                  paste0(month.name[as.integer(input$map.stype)], " ")), 
           labels$label[grep(input$map.var, labels$input.var)], 
           " is projected to be ", round(mean.val, 1), "% ", 
           ifelse(mean.val > 0, "higher ", "lower "), 
           "in the ", input$map.period, 
           "s under a ", ifelse(input$map.rcp == 85, "high", "medium"), 
           "-emissions scenario compared to the 1971-2000 average.
           However, this projected change in ", labels$label[grep(input$map.var, labels$input.var)], 
           " varies between subbasins and between the 10 different climate models. 
           The variation between different climate models and the resulting hydrological model outputs
           is referred to as the model spread. Click on the map to bring up a plot showing the model 
           spread for a single subbasin.")
    
  })
  
  output$plot.text <- renderText({
    
    dat <- plotData()
    sub.name <- dat$sub.name
    if (is.null(dat)) {
      paste("<b>Select a subbasin on the map to display a graph in the space below.</b>")
    } else {
      paste0("The plot below shows ", 
             ifelse(input$map.stype == "ann", "", 
                    paste0(month.name[as.integer(input$map.stype)], " ")),  
             labels$label[grep(input$map.var, labels$input.var)], 
             " as the percent change from the long-term average (1971-2000) for the ",
             sub.name, " watershed. Results are shown as the average over three future 
             30-year time periods: 
             2020s (2011-2040), 2050s (2041-2070), and 2080s (2071-2100) under two 
             different Representative Concentration (RCP) pathways, i.e. emissions scenarios, 
             medium-emissions (RCP 4.5), and high-emissions (RCP 8.5) 
             scenarios.
             Colored circles show the % change from the 10 different climate models used in this study. 
             Black squares show the average value across the 10 climate models, i.e. the ensemble mean. 
             The selected scenario, for which the ensemble mean is displayed on the map, 
             is highlighted on the figure below with 
             darker colors and larger symbols. Hover over plot points for more details.")
    }
    
    })
  
  output$text1 <- renderText({
    
    paste0("The plot below shows ", 
           ifelse(input$plot.type == "annual", "annual ", 
                  "monthly "),  
           labels$label[grep(input$plot.var, labels$input.var)], 
           " as the percent change from the long-term average (1971-2000) over the entire 
           Wabash River Basin. Results are shown as the average over three future 30-year time periods: 
           2020s (2011-2040), 2050s (2041-2070), and 2080s (2071-2100) under two 
           different Representative Concentration (RCP) pathways, i.e. emissions scenarios, 
           including the medium-emissions (RCP 4.5) and high-emissions (RCP 8.5) 
           scenarios.", 
           ifelse(input$plot.type == "annual", 
                  "Colored circles show the % change from the 10 different climate models 
                  used in this study. Black squares show the average value across the 10 climate models, 
                  i.e. the ensemble mean. Hover over plot points for more information.", 
                  "Solid black lines show the average % change across the 10 different climate models 
                  used in this study. Solid gray lines show the model spread. Hover over the plot for 
                  more details."))
  })
  
  
  # generate a plot of the data - for the interactive plot tab
  output$plot <- renderPlotly({
    
    if (input$plot.type == "annual") {
      # build query for annual values
      col.name.ann <- columns.annual[grep(input$plot.var, columns.annual)]
      query.annual <- paste0("SELECT ", col.name.ann, ", rcp, period, gcm_id, subbasin FROM hydro_ann")
      # query the database
      annual <- dbGetQuery(db, query.annual)
      # change column name to be consistent between different variable choices - same as with map - allows following code to be generic
      colnames(annual)[colnames(annual) == col.name.ann] <- "value"
      # further summary of the annual data - mean annual value across all subbasins
      # the plot_annual_gateway function needs a vector of length 61 - mean annual value for each gcm-rcp combination
      annual <- annual %>% dplyr::filter(rcp != "26") %>% group_by(rcp, period, gcm_id) %>% 
        summarize(mean = mean(value)) %>% 
        arrange(gcm_id) # sort so that the historical value (gcm_id 11) is last
      
      periods <- annual$period[annual$period != 1980]
      periods <- paste0(periods, "s")
      rcps <- annual$rcp[annual$period != 1980]
      rcps[rcps == "45"] <- "4.5"
      rcps[rcps == "85"] <- "8.5"
      period.rcp <- paste(periods, rcps, sep = "-")
      
      # create an ordered factor so things plot in desired order
      period.rcp <- factor(period.rcp, levels = unique(period.rcp[order(periods, rcps)]), ordered = T)
      
      
      # calculate percent change from historical
      dat.pct <- annual$mean[annual$period != 1980]
      dat.pct <- ((dat.pct - annual$mean[annual$period == 1980]) / annual$mean[annual$period == 1980]) * 100
      
      dat <- data.frame(pct.change = round(dat.pct, 1), 
                        period.rcp = period.rcp, gcm_id = annual$gcm_id[annual$period != 1980], 
                        period = periods, rcp = rcps)
      # calculate mean values to add as another point layer
      dat.mean <- dat %>% group_by(period.rcp) %>% summarize(pct.change = round(mean(pct.change), 1), 
                                                             period = unique(period), 
                                                             rcp = unique(rcp))
      
      dat <- inner_join(dat, gcm.lu, by = 'gcm_id')
      dat$GCM <- as.factor(dat$gcm_name)
      p1 <- ggplot(dat, aes(period.rcp, pct.change)) + 
        geom_point(aes(color = dat$GCM, 
                       text = paste("% Change: ", dat$pct.change, 
                                    "<br>Climate Model: ", dat$GCM, 
                                    "<br>Period: ", dat$period, 
                                    "<br>Emissions Scenario: ", ifelse(dat$rcp == "2.6", "Low",
                                                                       ifelse(dat$rcp == "4.5", "Medium", "High")))), size = 3) +
        geom_point(data = dat.mean, aes(text = paste("<b>Ensemble Mean</b>", 
                                                     "<br>% Change: ", dat.mean$pct.change,  
                                                     "<br>Period: ", dat.mean$period, 
                                                     "<br>Emissions scenario: ", ifelse(dat.mean$rcp == "2.6", "Low",
                                                                                        ifelse(dat.mean$rcp == "4.5", "Medium", "High")))), 
                   shape = 22, color = "black", fill = NA, size = 4) +
        geom_hline(yintercept = 0, linetype = "dashed") + 
        theme_classic() + 
        ylab("Percent change - annual") + 
        xlab("") + 
        theme(legend.position = "none", plot.margin = unit(c(1,3,1,1), "cm"))
      
      ggplotly(p1, tooltip = "text")
      
    } else {
      
      # build query for monthly values
      col.name.monthly <- columns.month[grep(input$plot.var, columns.month)]
      query.monthly <- paste0("SELECT ", col.name.monthly, 
                              ", rcp, period, calendar_month, gcm_id, subbasin FROM hydro_month")
      
      monthly <- dbGetQuery(db, query.monthly)
      colnames(monthly)[colnames(monthly) == col.name.monthly] <- "value"
      
      monthly$period <- paste0(as.character(monthly$period), "s")
      monthly$rcp[monthly$rcp == 45] <- "4.5"
      monthly$rcp[monthly$rcp == 85] <- "8.5"
      
      monthly <- inner_join(monthly, gcm.lu, by = 'gcm_id')
      colnames(monthly)[colnames(monthly) == 'gcm_name'] <- "GCM"
      
      # further summary of the monthly data - calculate the mean across all subbasins
      monthly <- monthly %>% dplyr::filter(rcp != "26") %>% 
        group_by(rcp, period, calendar_month, GCM) %>%
        summarize(value = mean(value))
      
      # calculate percent change
      hist <- monthly %>% filter(period == "1980s") %>% arrange(calendar_month)
      monthly <- monthly %>% filter(period != "1980s")
      
      monthly$pct.change <- round(((monthly$value - hist$value[monthly$calendar_month]) / hist$value[monthly$calendar_month]) * 100, 1)
      
      dat.summary <- monthly %>% group_by(period, rcp, calendar_month) %>% 
        summarize(Ensemble_mean = round(mean(pct.change), 1), 
                  Ensemble_max = round(max(pct.change), 1), 
                  Ensemble_min = round(min(pct.change), 1))
      
      p1 <- ggplot(dat.summary, aes(x = calendar_month)) + 
        geom_line(aes(y = Ensemble_max), 
                  color = "gray50") +
        geom_line(aes(y = Ensemble_min), color = "gray50") + 
        geom_line(aes(y = Ensemble_mean)) +
        facet_grid(rcp ~ period) +
        geom_hline(yintercept = 0, linetype = "dashed") + 
        theme_bw() + 
        ylab("Percent change - monthly") + 
        xlab("") + 
        theme(legend.position = "none", plot.margin = unit(c(1,3,1,1), "cm")) + 
        scale_x_continuous(breaks = c(1:12), labels = month.abb)
      
      ggplotly(p1)
      
    }
    
    
  })
  
  # reactive expression to generate table - for download and display
  datasetInput <- reactive({
    
    # build query based on user-selections
    if (input$table.stype == "ann") {
      col.name <- columns.annual[grep(input$table.var, columns.annual)]
      if (input$table.period == "1980") {
        query <- paste0("SELECT ", col.name, ", subbasin, gcm_id FROM hydro_ann WHERE (period = ", input$table.period,
                        ")")
      } else {
        query <- paste0("SELECT ", col.name, ", subbasin, gcm_id FROM hydro_ann WHERE (period = ", input$table.period,
                        ") AND (rcp = ", input$table.rcp, ")")
      }
    } else {
      col.name <- columns.month[grep(input$table.var, columns.month)]
      if (input$table.period == "1980") {
        query <- paste0("SELECT ", col.name, ", subbasin, gcm_id, calendar_month FROM hydro_month WHERE (period = ", input$table.period,
                        ")")
      } else {
        query <- paste0("SELECT ", col.name, ", subbasin, gcm_id, calendar_month FROM hydro_month WHERE (period = ", input$table.period,
                        ") AND (rcp = ", input$table.rcp, ")")
      }
    }
    
    # query the database
    dbGetQuery(db, query)
    
  })
  
  output$querytable <- renderDataTable({
    
    # use reactive expression "datasetInput()" to query the SQL database and return the data based on the user-selected inputs
    dat.table <- datasetInput()
    
    # round numeric values to 1 decimal point
    mcols <- vapply(dat.table, is.numeric, FUN.VALUE = logical(1))
    dat.table[,mcols] <- round(dat.table[,mcols], 1)
    
    # merge with GCM character names and remove integer id
    dat.table <- inner_join(dat.table, gcm.lu, by = 'gcm_id')
    dat.table <- dat.table[colnames(dat.table) != 'gcm_id']
    
    # create the display table
    DT::datatable(dat.table, rownames= FALSE, fillContainer = F,
                  caption = "All variables (e.g., precipitation, evapotranspiration) are in 
                  units of mm/year (Summary Period: Annual) 
                  or mm/month (Summary Period: Monthly), 
                  with the exception of streamflow, which has units in cubic meters per second")
    
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste0(input$table.rcp, "_", input$table.period, input$table.var, ".csv")
    },
    
    content = function(file) {
      write.csv(datasetInput(), file, row.names = F)
    }
  )
  
  
  }


shinyApp(ui, server)