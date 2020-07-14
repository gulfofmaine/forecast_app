library(leaflet.extras)
library(shiny)
library(shinydashboard)
library(here)
library(lubridate)
library(tidyverse)
library(gghighlight)
library(sf)
library(leaflet)
library(plotly)


zone_strt <- read_csv(here::here("Data/zoneStrt.csv"))
All_Zone_avgs <- read_csv(here::here("Data/All_Zone_avgs.csv")) %>% mutate(zone_avg = zone_avg*(9/5)+32)

All_Zone_avgs_mon <- All_Zone_avgs%>% mutate(mon = month(t), yr = year(t)) %>%
  group_by(mon, yr, zone) %>% summarise(zone_avg = mean(zone_avg, na.rm = TRUE), .groups = "drop") %>% 
  mutate(t = as.Date(paste(yr, mon, "01", sep = "-"))) %>% dplyr::select(t, zone, zone_avg) %>% ungroup()

FVCOM_Zone_avgs <- read_csv(here::here("Data/FVCOM_Zone_avgs.csv")) %>% mutate(zone_avg = zone_avg*(9/5)+32)

surNbot <- FVCOM_Zone_avgs %>% mutate(mon = month(t), yr = year(t)) %>% dplyr::select(-t)

surNbot <- All_Zone_avgs_mon %>% mutate(mon = month(t), yr = year(t)) %>% dplyr::select(-t, "sur" = zone_avg, zone, mon, yr) %>% 
  left_join(., surNbot, by = c("mon", "yr", "zone")) %>% rename("bot" = zone_avg)

yr_lob_landings <- read_csv("Data/Yearly_lob_landings.csv")

lob_movement <- read_csv("Data/Lobster_movement.csv") %>% filter(`Common name` == "American lobster")

gom_strt <- read_csv("Data/pwrDstrt2.csv") %>% rename("Year" = year)
mon_landings <- read_csv("Data/Month_lob_price.csv")

forecasts <- read_csv("Data/Forecasts.csv")%>% 
  pivot_longer(., cols = c(-Year, -Forecast_Date, -Yrday), names_to = "strtbin", values_to = "Percent")

buoy_data <- read_csv("Data/Buoy_data.csv") %>% filter(Variable == "temp") %>% mutate(mon = month(Date), yr = year(Date)) %>%
  group_by(name, Type, Depth, mon, yr) %>% add_tally() %>% 
  ungroup() %>% filter(n >=22) %>%
  group_by(name, Type, Depth, mon, yr) %>% dplyr::summarise(Values = mean(Values, na.rm = TRUE), .groups = "drop") %>% 
  group_by(name, Type, Depth, yr) %>% add_tally() %>% ungroup() %>% filter(n>=9) %>%
  group_by(name, Type, Depth, yr) %>% dplyr::summarise(Values = mean(Values, na.rm=TRUE), .groups = "drop")
  

Lob_zone_sf_simple <- st_read(here::here("Data/Lob_zone_sf.shp")) %>% st_transform(crs = '+proj=longlat +datum=WGS84') 

zone_strt_temp_all <-  All_Zone_avgs %>% mutate(yr = year(t), yrday = yday(t)) %>% 
  left_join(., zone_strt, by = c("yr" = "year", "zone" = "zone")) %>%
  na.omit() %>% 
  rename("id" = zone)

lob_zone_landing <- read_csv("Data/lob_zone.csv")

lob_zone_landing <- lob_zone_landing %>% 
  mutate(Date = format(as.Date(paste(year,month,"01",sep="-")), "%Y-%m-%d"))

lob_zone <- left_join(lob_zone_landing, Lob_zone_sf_simple, 
                      by = c("zone"="ZONEID")) %>% 
  rename("id" = zone) %>% st_as_sf(crs = '+proj=longlat +datum=WGS84')

zone_strt_temp <-  All_Zone_avgs %>% mutate(yr = year(t), yrday = yday(t)) %>% 
  left_join(., zone_strt, by = c("yr" = "year", "zone" = "zone")) %>%
  na.omit() %>% group_by(yrday, zone) %>% nest() %>% 
  mutate(corr = purrr::map(data, ~cor(.x$zone_avg, .x$zoneStrtDay))) %>%
  dplyr::select(zone, yrday, corr) %>% unnest(corr) %>% 
  left_join(., Lob_zone_sf_simple, by = c("zone" = "ZONEID"))%>% 
  rename("id" = zone) %>% st_as_sf() %>% st_transform(crs = '+proj=longlat +datum=WGS84') 

zone_strt_temp <- st_as_sf(zone_strt_temp)

Zone_index <- c("Zone A" = "A", "Zone B" = "B", "Zone C" = "C", "Zone D" = "D",
                "Zone E" = "E", "Zone E" = "E", "Zone F" = "F", "Zone G" = "G")

coefs <- lob_zone_landing %>% group_by(month, zone) %>%
  summarise(avgs = mean(pounds), .groups = "drop") %>%
  group_by(month) %>% mutate(total = mean(avgs), wt = avgs/total)

gom_coast <- All_Zone_avgs %>% mutate(month = month(t)) %>% 
  left_join(coefs, by = c("month", "zone")) %>%
  group_by(t) %>% 
  summarise(Avg = weighted.mean(zone_avg, w = wt), .groups= "drop")

years <- seq(2002,2019,1)
names(years) <- seq(2002,2019,1)

source(here::here("Lobster_dashboard_functions.R"))

################# leaflet helpers ###########3

strt_day_pal <- colorNumeric(
  palette = "viridis",
  domain = c(-1,0), 
  reverse = TRUE)

factpal <- colorFactor(c("#25356f",
                         "#cb9d00",
                         "#961e1d",
                         "#b3875a",
                         "#ffd380",
                         "#76a1a0",
                         "#97c8f0"), Lob_zone_sf_simple$ZONEID)

move_pal <- colorNumeric(
  palette = "viridis",
  domain = c(min(lob_movement$Year),max(lob_movement$Year))
)

###################### app ####################

ui <- fluidPage(
  tags$head(tags$style(HTML(".selectize-input{background-color:#FFFFFF;
                              color:#003A54;
                              border-color:#E2E2E2;
                              border-style:none;
                            border-width:1px;
                            border-radius:20%;
                            font-size:18px;}
                            .selectize-dropdown{color:#003A54}
                            .container-fluid{
                            width: 100%;
                            margin: 0 auto;
                            padding: 0;
                            }
                           p{font-size: 16px}
                           ul{font-size: 16px}
                           h6{font-size: 14px}
                           @media screen and (min-width: 1300px){
                           .container-fluid{
                              width: 1300px;
                           }
                           }"))),
  titlePanel("Climate and Fisheries Dashboard"),
  fluidRow(
    column(
      width = 12,
      tags$h1("Purpose"),
      tags$p("This dashboard provides access to data on ocean temperatures and landings in Maine’s lobster fishery. 
             Use the prompts to explore the relationships between lobster landings and water temperature, 
             as well as how temperature can be used to project Maine’s lobster landings into the future.")
    )
  ),
  
  fluidRow(
    column(width = 12,
           tags$h1("Overview"),
           tags$p("The fishery for American lobster (Homarus americanus) is one of the most valuable in the United States, 
                     and the fishery is especially important in the state of Maine.  
                     80% of all US lobsters are landed in Maine, 
                     and the lobster fishery accounts for 78% of all fishery landings in the state.  
                     The high dependence of Maine’s coastal economy on this single fishery means that even small 
                     changes in the volume of lobster landings have a large impact in the region.  
                     Ocean temperatures have been trending upwards in the Gulf of Maine, and rising temperatures
                     affect lobster landings in multiple ways and at multiple time scales, including:"), 
           tags$ul(
             tags$li("Seasonal: The timing of spring warming influences when lobsters become active (and therefore more catchable) 
                           and cues their molting into harvestable size classes, both of which influence when the period of high landings begins in the fishery each year."),
             tags$li("Multi-year: Temperature has been associated with rates of post-larval settlement, 
                           and the success of settlement, as well as subsequent growth and mortality, 
                           influences the number of lobsters that will mature into harvestable sizes in any given year."),
             tags$li("Multi-decadal: Temperature also influences the recruitment of lobsters. 
                     Over multiple generations directional changes in recruitment affect the size of the lobster population and the associated sustainable catch levels.")),
           tags$p("Maine is divided into seven Lobster Management Zones (Figure 1). 
           Each zone has a set of regulations that control the commercial harvest of lobsters.",
                  tags$a(href="https://www.maine.gov/dmr/about/councils/lobzones/index.html", "Management Zone Councils"), 
                  " help tailor state-wide regulations to the conditions within each distinct zone."))),
  fluidRow(
    column(
      width = 12,
      leafletOutput(outputId = "Lob_zone_map", height = "600px", width = "600px")
    )
  ),
  fluidRow(
    column(
      width = 6,
      tags$h6("Figure 1: Maine Lobster Management Zones. There are seven zones lettered alphbetically from Zone A in the east to zone G in the west.")
    )
  ),
  
  tags$hr(style="border-color: black;"),
  
  fluidRow(
    column(
      width = 12,
      tags$h4("Explore historical data or future projections")
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      selectInput(inputId = "time_period", choices = c("Choose One" = "", "Historical" = "hist", "Projection" = "proj"), label = "Select a time period")
    )
  ),
  
  fluidRow(
    column(
      width = 12, 
      uiOutput(outputId = "time_period_ui"))),
  
  fluidRow(
    column(
      width = 12,
      uiOutput(outputId = "data_ui")
    )
  ),
  
  fluidRow(
    tags$br(),
    tags$br(),
    tags$hr(style="border-color: black;")
  )

  
)

server <- function(input, output, session) {
  
  theme_set(theme_bw()+ 
              theme(panel.grid.minor = element_blank(),
                    panel.grid = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    legend.background = element_blank(), 
                    legend.position = c(.15,.8), 
                    legend.key = element_rect(fill = NA), 
                    legend.direction = "horizontal",
                    axis.title = element_text(size = 18),
                    axis.text = element_text(size=14), 
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 12)))
  
  output$time_period_ui <- renderUI({
    if (is.null(input$time_period))
      return()
    
    switch(
      input$time_period,
      
      "hist" = fluidRow(
        column(
          width = 12,
          tags$h4("Are you interested in landings or temperature?"),
          selectInput(inputId = "indicators", label = "Indicator", choices = c("Choose One" = "", "Temperature" = "temp", "Landings" = "land"),
                      selected = NULL)
        )
      ),
      
      "proj" = fluidRow(
        column(
          width = 12,
          tags$h4("Which forecast do you want to explore?"),
          selectInput(inputId = "forecasts", label = "Forecast", choices = c("Choose One" = "", "Seasonal" = "season", "Multi-year" = "annual", "Muli-decadal" = "multi_decadal"),
                      selected = NULL)
        )
      )
    )
  })
  
  indicators_ui <- function(){
    if (is.null(input$indicators))
      return()

    switch(
      input$indicators,
          "temp" = column(
             width = 12,
             fluidRow(
               column(width = 12,
                      tags$h1("Temperature Explorer"))),
             fluidRow(
               column(width = 12,
                      tags$p("Ocean temperature in the Gulf of Maine has risen rapidly over the past twenty years. 
                      This increase in temperature has occurred both at the surface and at depth (Figure T1)."))),
             fluidRow(
               column(
                 width = 12,
                 plotOutput(outputId = "long_term_sst")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 tags$h6("Figure T1: Yearly averaged surface and bottom temperature for the coastal Gulf of Maine region. 
                         The horizonal line is the average temperature for the specified depth across the entire time series.")
               )
             ),
             fluidRow(
               column(width = 12,
                               tags$p("Ocean temperautre has a strong seasonality, with the coldest water occuring in March,
                               and the warmest water occuring in August (Figure T2).
                               Spring time ocean temperature, during the months of March and April, is a good 
                               predictor of the timing of the summer transition to high-volume landings in Maine’s lobster fishery. 
                               This lobster-landings transition period, when lobsters become more available to the fishery, 
                               occurs in the early summer as lobsters move more actively and molt into harvestable size classes.
                                       The interactive graphs below can be used to observe differences in sea 
                                       surface and bottom temperature in each of Maine’s lobster management zones 
                                       over time (top panel) and within the annual cycle of individual years (bottom panel).  
                                       Sea surface temperatures from March 8 through April 20 are the strongest predictors 
                                       of the timing of the lobster landings transition date ",
                        tags$a(href="https://www.frontiersin.org/articles/10.3389/fmars.2017.00337/full", "(Mills et al. 2017).")), 
                               tags$p("The graph below is interactive. Use the radio buttons to select surface or bottom temperature as 
                                       well as observed temperature (raw temperature) or temperature anomalies 
                                       (difference from long-term average from 1981-2011) for the full time series (top panel) or by years (bottom panel).
                                       The legend of the graph is interactive.  In the top panel, each of the lobster zones is selectable by clicking on 
                                       the zone in the legend to remove the zone or double clicking to isolate the zone.  
                                       For the bottom panel, the years are interactive in a similar manner, and lobster zones of interest can be selected 
                                       from the drop-down menu above the graph. Sea surface temperature data are from", 
                                       tags$a("https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.html", "NOAA OISST V2")," and 
                                      bottom temperature data are from ", tags$a("http://fvcom.smast.umassd.edu/necofs/","FVCOM NECOFS model")," ."))),
             
             fluidRow(
               column(width = 12,
                      fluidRow(
                        column(
                          width = 6,
                          title = "Surface and Bottom Temperature",
                          radioButtons("sur_bot", "Select Depth",
                                       choices = c("Surface Temperature" = "Yearday", "Bottom Temperature" = "Month"))
                        ),
                        column(
                          width = 6,
                          title = "Data type",
                          radioButtons("vis", "Select Data format",
                                       choices = c("Observed" = "raw_temp", "Anomalies" = "anom")))),
                      plotlyOutput(outputId = "plotly_lob_zone"))),
              fluidRow(
               column(width = 12,
                      fluidRow(
                        column(
                          width = 3,
                          title = "Zone",
                          selectInput("zones", "Select Lobster Zone",
                                      choices = Zone_index))),
                      plotlyOutput(outputId = "yrDay_plot")))),
           
           
           "land" = column(
             width = 12,
             fluidRow(
               column(
                 width = 12,
                 tags$h1("Lobster Landings Explorer"))),
             fluidRow(
               column(
                 width = 12,
                 tags$p("Lobster landings in the state of Maine have risen steadily since the mid-1980s (Figure L1, left panel). 
                        A large portion of this increase in lobster landings is attributed to 
                        the warming Gulf of Maine, which has caused a northward shift and enhanced productivity of the lobster population (Figure L1, right panel)."))),
             fluidRow(
               column(
                 width = 6,
                 plotOutput(outputId = "long_term_langings")
               ),
               column(
                 width = 6,
                 leafletOutput(outputId = "biomass")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 tags$h6("Figure L1: (Left) Yearly total lobster landings in Maine. 
                         (Right) Center of American lobster biomass, color indicates year.")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 tags$p("Lobster landings have seasonality (Figure L2). 
                        There is a low landings period during the winter months, and a high landings period from the summer into the fall.
                        The transition from the low landings season to the high landings season happens in the late spring or early summer depending
                        on the water temperatures for that location and year.")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 plotOutput(outputId = "transition")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 tags$h6("Figure L2: Seasonality and yearly differences in lobster landings from Lobster Management Zone C. The year 2016 had an earlier transition to the high summer
                         landings period than 2011 (indicated by the arrow).")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 tags$p("Use the interactive graph below to observe the change in landings for each zone through time. Notice how there is both 
                 a seasonal cycle and multi-year trend in landings. Use the radio buttons to select
                 the view as monthly or yearly averages for the full time series (on the top) or flip through the different lobster zones 
                 to see how the seasonal cycles compare between years (on the bottom). 
                 The legend of the graph is interactive and each of the lobster zones and years are selectable by clicking on the
                        zone in the legend to remove the zone or doubling clicking to isolate the zone. 
                        Landings data are from Maine DMR."))),
             fluidRow(
               column(
                 width = 6,
                 title = "Select Time Period",
                 radioButtons("time_input", "Select Monthly or Yearly Average",
                              choices = c("Monthly" = "Month", "Yearly" = "Year"),
                              selected = "Month"))),
             fluidRow(
               column(
                 width = 12,
                  plotlyOutput(outputId = "plotly_lob_landings"))),
             fluidRow(
               column(
                 width = 3,
                 selectInput("zone_bio", "Select Lobster Zone",
                             choices = Zone_index))),
             fluidRow(
               column(
                 width = 12,
                 plotlyOutput(outputId = "plotly_lob_landings_yday"))))
       
    )}
  
  forecasts_ui <- function(){
    if (is.null(input$forecasts))
      return()
    
    switch(
      input$forecasts,
      "season" = column(
        width = 12,
        fluidRow(
          column(width = 12,
                 tags$h1("Seasonal Forecast"),
                 tags$p("Lobster landings can increase dramatically in a short period of time, 
                    and the transition period from the low winter landings to the high summer landings 
                    is important for the lobster fishery (Figure S1). 
                    This rapid increase in landings results from 
                    a variety of conditions that are linked to the spring warming of coastal waters.  
                    As waters warm, lobsters become more active and move into traps more frequently.  
                    Warming also cues molting, which causes lobsters to grow into harvestable sizes.  
                    The volume of lobster landed increases as all these conditions align and 
                    lobsters become more readily harvested by the coastal fishery."), 
                 plotOutput(outputId = "transition_gom"),
                 tags$h6("Figure S1: Seasonality and yearly differences in lobsters landed in Maine. 
                 The vertical lines indicate the timing of the transition date for 2014 (red) and 
                 2016 (blue). 
                 The year 2016 had an earlier transition to the high summer
                         landings period than 2014 (indicated by the arrow)."),
                 tags$p("This transition to the high-landings summer period typically happens in late June 
                    or early July, but it can occur in late May through late July.  Water temperature 
                    plays a role in determining when this transition happens, and there is a lagged 
                    relationship between spring water temperatures and the timing of the transition date.  
                    Thus, early spring ocean temperatures can be used to calculate the timing of the uptick 
                    in landings associated with this transition."))),
        fluidRow(
          column(
            width = 12,
            tags$h3("How do the transition periods compare to springtime temperature?"),
            tags$p("Use the widget below to compare the water temperature and state-wide transition dates of different years. 
                   Notice how the patterns between the temperature (on the left) and lobster landings transition period (on the right) are similar
                   when new data points are added. 
                   Years with warmer temperatures also tend to have earlier transition dates.")
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            selectInput("Year_compare", label = "Select Year",
                        choices = c("Choose One" = "", years), multiple = TRUE)
          )
        ),
        fluidRow(
          column(
            width = 6,
            plotOutput("spring_temp")
          ),
          column(
            width = 6,
            plotOutput("transition_date")
          )
        ),
        fluidRow(
          column(width = 12,
                 fluidRow(
                   column(width = 12,
                       tags$h3("How do water temperature and landings compare in each lobster zone?"),
                       fluidRow(
                         column(
                           width = 12,
                           tags$p("Use this tool to investigate the relationship between sea surface temperature 
                                        and the timing of the transition between the low winter landings and 
                                        high summer landings periods for individual lobster zones."),
                           
                           tags$p("The top left graph shows the relationship between the average sea surface 
                                        temperature of a lobster zone on a specific date and the 
                                        date of the low-to-high landings transition. As you can see, 
                                        sea surface temperature in the spring can be used to predict the 
                                        transition period between the winter and summer landings.
                                        The date slider allows you to switch between different dates, and the lobster zone 
                                        selection input changes the lobster zone. 
                                        Each zone has its own relationship between springtime water temperature 
                                        and lobster landings. 
                                        "),
                           tags$p("The bottom graph shows the time series of the correlation between the average sea surface 
                                        temperature and the inflection point between the winter and summer landings period. In this graph, the 
                                        peak of the curve indicates the best spring date for predicting the landings transition date."),
                           tags$p("For most lobster zones, the correlation peaks between March 15th and March 25th."))))),
                 fluidRow(
                   column(
                     width = 4,
                     title = "Predictor Date Input",
                     sliderInput("strt_day_period", "Date",
                                 min = as.Date("2000-01-01"),
                                 max = as.Date("2000-04-30"),
                                 value = as.Date("2000-03-15"), 
                                 timeFormat = "%d-%B")),
                   column(
                     width = 4,
                     title = "Zone",
                     selectInput("zone_strt", "Select Lobster Zone",
                                 choices = Zone_index)),
                   column(
                     width = 4, 
                     title = "Legend",
                     checkboxInput("strt_day_legend", "Show legend", TRUE))),
                 fluidRow(
                   column(width = 6,
                          plotlyOutput("strt_day_plot", height = 250),
                          plotOutput("strt_day_plot2", height = 250)),
                   column(width = 6,
                          leafletOutput("strt_day_map")))))),
      
      "annual" = column(
        width = 12,
        fluidRow(
          column(
            width = 12,
            tags$h1("Multi-year Fishery Recruitment"))),
        fluidRow(
          column(
            width = 12,
            tags$p("Each year, approximately 85% of Maine’s lobster harvest is comprised of new recruits to the fishery. 
              Due to regional and interannual differences in temperature-dependent growth rates, 
              lobsters take anywhere from 5 to 10 years to enter the fishery after their larvae repopulate coastal nurseries.
              Annual surveys of newly settled lobsters conducted through the ",
            tags$a(href="https://umaine.edu/wahlelab/american-lobster-settlement-index-alsi/", "American Lobster Settlement Index"), 
            "offer the potential to give the industry and managers an early warning of the strength of upcoming year classes. 
              Observations of annual settlement and estimates of growth and mortality can be used to track the fate of each year 
              class and estimate the abundance of recruits entering the fishery each year."))),
        fluidRow(
          column(
            width = 12,
            imageOutput("multiyear")
          )
        )),
      
      "multi_decadal" = column(
        width = 12,
        fluidRow(
          column(
            width = 12,
            tags$h1("Multi-decadal Population Trends"))),
        fluidRow(
          column(
            width = 12,
            tags$p("Rising ocean temperatures can affect multiple biological processes in lobster but have a particularly strong effect on their survival through the first year of life.  
              Survival of during the first year is enhanced around optimal summer temperatures of approximately 16 C.  
              Recent temperatures in the Gulf of Maine near this optimum have boosted Gulf of Maine lobster abundance, 
              but as temperatures have exceeded this optimum in Southern New England, the lobster population has declined.  
              Future temperatures in Gulf of Maine are projected to exceed this preferred temperature and are expected to influence population trajectories for the lobster population."),
            tags$p("The interactive graph below can be used to explore projected estimates of the Gulf of Maine lobster 
                   population under a range of future temperatures"))),
        fluidRow(
          column(
            width = 12,
            imageOutput("multidecade")
          )
        ),
        fluidRow(
          column(
            width = 12,
            tags$p("Projections of American lobster abundance. Estimated abundance from 1985 to 2050 for the GoM")
          )
        )) 
    )}
  
  output$data_ui <- renderUI({
    if(is.null(input$time_period))
      return()
    switch(input$time_period,
           "hist" = indicators_ui(),
           "proj" = forecasts_ui())
  })
  
  
  ############################## LOBSTER ZONE MAP #############
  
  output$Lob_zone_map <- renderLeaflet({        
    leaflet() %>% addTiles() %>% fitBounds(-70.5, 42.5, -66, 45) %>%
      addPolygons(data = Lob_zone_sf_simple, fillColor = ~factpal(ZONEID), color = "transparent", fillOpacity = 1, layerId = ~ZONEID,
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2, bringToFront = TRUE),
                  label = ~paste("Zone", ZONEID, sep = " ")) %>% 
      addLegend(colors = c("#25356f","#cb9d00","#961e1d","#b3875a","#ffd380","#76a1a0","#97c8f0"), 
                labels = c("A","B","C", "D", "E", "F", "G"),
                title = "Lobster Zone") %>%
      leaflet.extras::suspendScroll()})
  
  ############################## LONG_TERM LANDINGS ##############
  
  output$long_term_langings <- renderPlot({
    yr_lob_landings %>% ggplot() + geom_line(aes(x = Year, y = `pounds(millions)`), size = 3) + 
      labs(x = "Year", y = "Landings (millions of pounds)")
  })
  
  
  ############################### CENTER OF BIOMASS ##############
  
  output$biomass <- renderLeaflet({
    leaflet() %>% addTiles() %>% 
      addCircleMarkers(data=lob_movement, lng = ~Longitude, lat = ~Latitude, color = ~move_pal(Year), label = ~Year) %>%
      addLegend(position = "topright",
                pal = move_pal, 
                values = c(min(lob_movement$Year), max(lob_movement$Year)),
                labFormat = labelFormat(big.mark = ""))
  })
  
  ############################### SPRING TRANSITION ###########
  
  output$transition <- renderPlot({
    lob_zone_landing %>% filter(zone == "C", year %in% c(2011, 2016)) %>% mutate(pounds = pounds/1000000) %>%
      ggplot() + geom_line(aes(x = month, y = pounds, color = as.factor(year)), size = 3) +
      labs(x = "Month", y = "Landings (millions of pounds)", color = "Year")  +
      scale_x_continuous(breaks = seq(1,12,1), 
                         labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
      geom_segment(aes(x = 7.2, y = 4.5, xend = 6.5, yend = 4.5, colour = "segment"),
                   arrow = arrow(), color = "black")
  })
  

  ############################### LONG_TERM_SST ########################
  
  output$long_term_sst <- renderPlot({
    surNbot %>% group_by(mon, zone) %>% mutate(surclim = mean(sur, na.rm = TRUE), botclim = mean(bot, na.rm = TRUE), 
                                               suranom = sur-surclim, botanom = bot-botclim) %>%
      group_by(yr) %>% dplyr::summarise(sur = mean(sur, na.rm = TRUE), bot = mean(bot, na.rm = TRUE), .groups = "drop") %>%
      filter(yr > 1981, yr<2019) %>% pivot_longer(cols = c(sur, bot), names_to = "Depth", values_to = "values") %>%
      group_by(Depth) %>% mutate(yint = mean(values, na.rm = TRUE)) %>%
      ggplot() + geom_line(aes(x = yr, y = values, color = Depth), size = 3) +
      geom_hline(aes(yintercept = yint, col = Depth)) +
      scale_x_continuous(breaks = seq(1985,2020,5)) +
      labs(x = "Year", y = "Temperature (deg F)") +
      scale_color_discrete(labels = c("Surface", "Bottom"))
  })
  
  
  ############################## LOBSTER ZONE PLOTLY PLOT SERVER LOGIC ####
  output$plotly_lob_zone <- renderPlotly({
    
    if(input$sur_bot == "Yearday"){
      plot1 <- anom_fun(input_Data = All_Zone_avgs_mon, zones = input$zones, 
                        vis = input$vis, sur_bot = input$sur_bot) + 
        theme(axis.title = element_text(size = 12),
              axis.text = element_text(size=10), 
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 8))}
    if(input$sur_bot == "Month"){
      plot1 <- anom_fun(input_Data = FVCOM_Zone_avgs, zones = input$zones, 
                        vis = input$vis, sur_bot = input$sur_bot) + 
        theme(axis.title = element_text(size = 12),
              axis.text = element_text(size=10), 
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 8))}
    
    return(plot1)
  })
  
  output$yrDay_plot <- renderPlotly({
    
    if(input$sur_bot == "Yearday"){
      plot1 <- yrday_fun(input_Data = All_Zone_avgs_mon, zones = input$zones, 
                         sur_bot = input$sur_bot, vis = input$vis) + 
        theme(axis.title = element_text(size = 12),
              axis.text = element_text(size=10), 
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 8))}
    if(input$sur_bot == "Month"){
      plot1 <- yrday_fun(input_Data = FVCOM_Zone_avgs, zones = input$zones, 
                         sur_bot = input$sur_bot, vis = input$vis) + 
        theme(axis.title = element_text(size = 12),
              axis.text = element_text(size=10), 
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 8))}
    
    return(plot1)
  })
  
  ############################## LOBSTER LANDINGS SERVER LOGIC ####
  
  output$plotly_lob_landings <- renderPlotly({
    
    if(input$time_input == "Year"){
      plot1 <- lob_zone %>% group_by(id, year) %>% summarise(total = sum(pounds, na.rm = TRUE)/1000000) %>% ungroup() %>%
        ggplot() + geom_line(aes(year, total, color = id)) +
        labs(title = "Total Catch", x= "Year", y = "Total Landings (millions of pounds)", color = "Lobster \n Zone") +
        scale_color_manual(values = c("#25356f",
                                      "#cb9d00",
                                      "#961e1d",
                                      "#b3875a",
                                      "#ffd380",
                                      "#76a1a0",
                                      "#97c8f0")) +
        theme(axis.title = element_text(size = 14),
              axis.text = element_text(size=12), 
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10))}
    
    if(input$time_input == "Month"){
      plot1 <- lob_zone %>% ungroup() %>% mutate(Date = as.Date(paste(year, month, "01", sep = "-")), pounds = pounds/1000000) %>% ungroup %>%
        ggplot() + geom_line(aes(Date, pounds, color = id)) +
        labs(title = "Total Catch", x= "Date", y = "Total Landings (millions of pounds)", color = "Lobster \n Zone")  +
        scale_color_manual(values = c("#25356f",
                                      "#cb9d00",
                                      "#961e1d",
                                      "#b3875a",
                                      "#ffd380",
                                      "#76a1a0",
                                      "#97c8f0")) +
        theme(axis.title = element_text(size = 14),
              axis.text = element_text(size=12), 
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10))
    }
    return(plot1)
    
  })
  
  output$plotly_lob_landings_yday <- renderPlotly({
    plot1 <- lob_zone %>% filter(id == input$zone_bio) %>% mutate(pounds = pounds/1000000) %>%
      ggplot() + geom_line(aes(month, pounds, color = as.factor(year))) + 
      labs(title = paste("Zone", input$zone_bio, "Catch", sep = " "), 
           x = "Month", y = "Landings (millions of pounds)") +
      scale_x_continuous(breaks = c(seq(1,12,1)), 
                         labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                    "Aug", "Sep", "Oct", "Nov", "Dec")) +
      scale_color_viridis_d(name = "Year") + 
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size=12), 
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10))
    return(plot1)
  })
  
  
  ############################## TRANSITION_PLOTS #############################
  
  output$spring_temp <- renderPlot({
    if(is.null(input$Year_compare))
      return()
    gom_coast %>% mutate(Year = year(t), yrday = yday(t)) %>% 
      filter(Year %in% input$Year_compare, yrday %in% c(60:121)) %>% 
      group_by(Year) %>% summarise(Avg = mean(Avg, na.rm =TRUE)) %>%
      ggplot() + geom_point(aes(y=Avg, group = Year, x = Year), size = 6) +
      labs(x = "Year", y = "Temperature (deg F)")
      
  })
  
  output$transition_date <- renderPlot({
    if(is.null(input$Year_compare))
      return()
    gom_strt %>% 
      dplyr::filter(Year %in% input$Year_compare) %>% 
      ggplot() + geom_point(aes(x = Year, y = pwrDstrt2), size = 6) + 
      labs(x = "Year", y = "Transition Date") +
      scale_y_reverse(breaks = c(seq(135,230,5)), 
                      labels = c("15-May","20-May","25-May","30-May","04-Jun","09-Jun","14-Jun","19-Jun", "24-Jun", 
                                 "29-Jun", "04-Jul", "09-Jul", "14-Jul", 
                                 "19-Jul", "24-Jul", "29-Jul", "03-Aug", "08-Aug", "13-Aug", "18-Aug"))
  })

  ############################### SPRING TRANSITION GOM ###########
  
  output$transition_gom <- renderPlot({
    mon_landings  %>%
      mutate(t = as.Date(paste(Year,Month,"01", sep="-")), 
             molen = days_in_month(t),
             Date = as.Date(paste(Year, Month, molen, sep="-")),
             yrday = yday(Date),
             Pounds = Pounds/1000000) %>% 
      filter(Year %in% c(2014, 2016)) %>%
      left_join(., gom_strt, by = c("Year")) %>%
      ggplot() + geom_line(aes(x = yrday, y = Pounds, color = as.factor(Year)), size = 3) +
      labs(x = "Month", y = "Landings (millions of pounds)", color = "Year")  +
      scale_x_continuous(breaks = c(31, 60,91,121,152,182,213,244,274,305,335,366), 
                         labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
      geom_vline(aes(xintercept = pwrDstrt2, col = as.factor(Year)), size = 3) +
      geom_segment(aes(x = 191, y = 5.5, xend = 166, yend = 5.5, colour = "segment"),
                   arrow = arrow(), color = "black")
  })
  
  
  
  ############################## START DAY SERVER LOGIC ####
  
  reactive_data_chrono <- reactive({
    req(input$strt_day_period)
    yrdays <- lubridate::yday(as.Date(input$strt_day_period))
    
    zone_strt_temp %>% filter(yrday == yrdays)
  })
  
  
  # static backround map
  output$strt_day_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      fitBounds(-70.5, 42.5, -66, 45)
  })  
  
  # reactive map
  observe({
    leafletProxy("strt_day_map", data = reactive_data_chrono()) %>%
      addPolygons(fillColor = ~strt_day_pal(corr), color = "transparent", fillOpacity = 1, layerId = ~id,
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2, bringToFront = TRUE),
                  label = ~paste("Zone", id, "Cor", round(corr, 2), sep = " "))
  })
  
  
  observe({
    click <- input$strt_day_map_shape_click
    if(is.null(click))
      return()
    
    updateSelectInput(session, "zone_strt", choices = Zone_index, selected = click$id)
  })
  
  observe({
    
    proxy <- leafletProxy("strt_day_map", data = reactive_data_chrono())
    proxy %>% clearControls()
    
    if (input$strt_day_legend) {
      proxy %>% addLegend(position = "topleft",
                          pal = strt_day_pal, 
                          values = ~c(-1,0), opacity = 1.0, 
                          labels = strt_day_pal,
                          title = "Landings-temperature correlation")}
  })
  
  
  observe({
    req(input$strt_day_period)
    mon <- format(as.Date(input$strt_day_period), "%d-%B")
    
    yrdays <- lubridate::yday(as.Date(input$strt_day_period))
    
    output$strt_day_plot <- renderPlotly({
      zone_strt_temp_all %>% filter(id == input$zone_strt, yrday == yrdays) %>% 
        dplyr::rename("SST" = zone_avg, "transition day" = zoneStrtDay, "year" = yr) %>%
        ggplot() +  geom_point(aes(`transition day`, SST, col = year)) + 
        geom_smooth(aes(`transition day`, SST), method = "lm", se = FALSE) +
        scale_x_continuous(breaks = c(seq(135,230,5)), 
                           labels = c("15-May","20-May","25-May","30-May","04-Jun","09-Jun","14-Jun","19-Jun", "24-Jun", 
                                      "29-Jun", "04-Jul", "09-Jul", "14-Jul", 
                                      "19-Jul", "24-Jul", "29-Jul", "03-Aug", "08-Aug", "13-Aug", "18-Aug"))+
        labs(y = paste(mon, "Average Zone SST"), x = "Landings Start Day of Year", title = paste("Zone", input$zone_strt, sep = " "))  +
        theme(legend.position = "none",
              axis.title = element_text(size = 12),
              axis.text = element_text(size=10), 
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 8))
      
    })
    
    output$strt_day_plot2 <- renderPlot({
      
      zone_strt_temp %>% data.frame() %>%
        ggplot() +  geom_smooth(aes(yrday, corr, color = id)) + 
        labs(x = "Day of Year", y = "Correlation", title = paste("Zone", input$zone_strt, sep = " ")) +
        scale_x_continuous(breaks = c(seq(1,140,20)), 
                           labels = c("1-Jan","21-Jan","10-Feb","02-Mar", "22-Mar", 
                                      "11-Apr", "19-May"), 
                           limits = c(1,141)) + 
        scale_y_reverse(limits = c(0,-1)) +
        gghighlight(id == input$zone_strt, label_key = id, keep_scales = TRUE, use_group_by = FALSE) + 
        geom_vline(xintercept = yrdays) + scale_color_manual(values = c("#25356f",
                                                                        "#cb9d00",
                                                                        "#961e1d",
                                                                        "#b3875a",
                                                                        "#ffd380",
                                                                        "#76a1a0",
                                                                        "#97c8f0"))
    })
  })
  
  ################################## image outputs #########
  output$multiyear <- renderImage({

      return(list(
        src = "Images/multiyear.png",
        contentType = "image/png",
        alt = "multi-year"
      ))
     
    
  }, deleteFile = FALSE)
  
  output$multidecade <- renderImage({
    
    return(list(
      src = "Images/multidecade.png",
      contentType = "image/png",
      alt = "multi-decade"
    ))
    
    
  }, deleteFile = FALSE)
}


shinyApp(ui, server)
