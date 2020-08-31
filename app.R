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


# Lobster landings start dates by zone calculated from defineStartDate.R
zone_strt <- read_csv(here::here("Data/zoneStrt.csv"))

# Average sst by lobster zone (OISST)
All_Zone_avgs <- read_csv(here::here("Data/All_Zone_avgs.csv")) %>% mutate(zone_avg = zone_avg*(9/5)+32)

All_Zone_avgs_mon <- All_Zone_avgs%>% mutate(mon = month(t), yr = year(t)) %>%
  group_by(mon, yr, zone) %>% summarise(zone_avg = mean(zone_avg, na.rm = TRUE), .groups = "drop") %>% 
  mutate(t = as.Date(paste(yr, mon, "01", sep = "-"))) %>% dplyr::select(t, zone, zone_avg) %>% ungroup()

cmipsst <- read_csv("Data/CMIPSSTProjections.csv") %>% 
  mutate(Mean = Mean*(9/5)+32,
         low = low*(9/5)+32,
         high = high*(9/5)+32) %>% mutate(Year = year(Date), mon = month(Date)) %>% 
  filter(mon %in% c(7,8,9)) %>% group_by(Year) %>% 
  dplyr::summarise(Mean = mean(Mean), 
                   low = mean(low),
                   high = mean(high),
                   .groups = "drop")

cmipsst_sne <- read_csv("Data/CMIPSSTProjections_sne.csv")%>% 
  mutate(Mean = Mean*(9/5)+32,
         low = low*(9/5)+32,
         high = high*(9/5)+32) %>% mutate(Year = year(Date), mon = month(Date)) %>% 
  filter(mon %in% c(7,8,9)) %>% group_by(Year) %>% 
  dplyr::summarise(Mean = mean(Mean), 
                   low = mean(low),
                   high = mean(high),
                   .groups = "drop")

# Multi decadal lobster population projection

gom_landings <- read_csv("Data/GoM_lob_landings.csv")
gom_landings_yr <- gom_landings  %>% group_by(Year) %>% 
  summarise(Landings = sum(Pounds, na.rm=TRUE)) %>% 
  mutate(Landings = Landings/1000000)

sne_landings <- read_csv("Data/sne_landings.csv")
sne_landings_yr <- sne_landings  %>% group_by(Year) %>% 
  summarise(Landings = sum(Pounds, na.rm=TRUE)) %>% 
  mutate(Landings = Landings/1000000)

gom_lob_data <- read_csv("Data/gom_lob_data.csv")

sne_lob_data <- read_csv("Data/sne_lob_data.csv")

gom_oisst_df <- read_csv("Data/gom_oisst.csv") %>% mutate(Year = year(Date), mon = month(Date)) %>% 
  filter(Year > 1981, Year < 2020, mon %in% c(7,8,9)) %>% group_by(Year) %>% 
  dplyr::summarise(Avg = mean(gom_oisst)*(9/5)+32, .groups = "drop")

sne_oisst_df <- read_csv("Data/sne_oisst.csv") %>% mutate(Year = year(Date), mon = month(Date)) %>% 
  filter(Year > 1981, Year < 2020, mon %in% c(7,8,9)) %>% group_by(Year) %>% 
  dplyr::summarise(Avg = mean(sne_oisst)*(9/5)+32, .groups = "drop")

# Average bottom temperature for lobster zone (FVCOM)
FVCOM_Zone_avgs <- read_csv(here::here("Data/FVCOM_Zone_avgs.csv")) %>% mutate(zone_avg = zone_avg*(9/5)+32)

surNbot <- FVCOM_Zone_avgs %>% mutate(mon = month(t), yr = year(t)) %>% dplyr::select(-t)

surNbot <- All_Zone_avgs_mon %>% mutate(mon = month(t), yr = year(t)) %>% dplyr::select(-t, "sur" = zone_avg, zone, mon, yr) %>% 
  left_join(., surNbot, by = c("mon", "yr", "zone")) %>% rename("bot" = zone_avg)

# yearly total lobster landings in Maine
yr_lob_landings <- read_csv("Data/Yearly_lob_landings.csv")

# Lobster center of biomass
lob_movement <- read_csv("Data/Lobster_movement.csv") %>% filter(`Common name` == "American lobster")

# Lobster landings start date for total maine landings
gom_strt <- read_csv("Data/pwrDstrt2.csv") %>% rename("Year" = year)

# monthly lobster landed in Maine
mon_landings <- read_csv("Data/Month_lob_price.csv")

# Seasonal forecasts
forecasts <- read_csv("Data/Forecasts_for_app.csv")%>% 
  pivot_longer(., cols = c(-Year, -Forecast_Date, -Yrday), names_to = "strtbin", values_to = "Percent")

#buoy_data <- read_csv("Data/Buoy_data.csv") %>% filter(Variable == "temp") %>% mutate(mon = month(Date), yr = year(Date)) %>%
#  group_by(name, Type, Depth, mon, yr) %>% add_tally() %>% 
#  ungroup() %>% filter(n >=22) %>%
#  group_by(name, Type, Depth, mon, yr) %>% dplyr::summarise(Values = mean(Values, na.rm = TRUE), .groups = "drop") %>% 
#  group_by(name, Type, Depth, yr) %>% add_tally() %>% ungroup() %>% filter(n>=9) %>%
#  group_by(name, Type, Depth, yr) %>% dplyr::summarise(Values = mean(Values, na.rm=TRUE), .groups = "drop")
  
# lobster zone shp file
Lob_zone_sf_simple <- st_read("Data/Lob_zone_sf.shp") %>% st_transform(crs = '+proj=longlat +datum=WGS84') 

zone_strt_temp_all <-  All_Zone_avgs %>% mutate(yr = year(t), yrday = yday(t)) %>% 
  left_join(., zone_strt, by = c("yr" = "year", "zone" = "zone")) %>%
  na.omit() %>% 
  rename("id" = zone)

# other shapefiles
gom_shp <- st_read("Data/GoM_combined_sp.shp") %>% st_transform(crs = '+proj=longlat +datum=WGS84') 
sne_shp <- st_read("Data/Southern_New_England_Management_Area.shp") %>% st_transform(crs = '+proj=longlat +datum=WGS84') 
multi_year_shp <- st_read("Data/Lob_zone_sf.shp") %>% 
  st_transform(crs = '+proj=longlat +datum=WGS84') %>% filter(ZONEID != "G")

# monthly lobster landings by zone
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
                "Zone E" = "E", "Zone F" = "F", "Zone G" = "G")

coefs <- lob_zone_landing %>% group_by(month, zone) %>%
  summarise(avgs = mean(pounds), .groups = "drop") %>%
  group_by(month) %>% mutate(total = mean(avgs), wt = avgs/total)

gom_coast <- All_Zone_avgs %>% mutate(month = month(t)) %>% 
  left_join(coefs, by = c("month", "zone")) %>%
  group_by(t) %>% 
  summarise(Avg = weighted.mean(zone_avg, w = wt), .groups= "drop")

# multi year forecast data
multi_year <- read_csv("Data/multi_year_forecast.csv") 
multi_year$`10th` <- multi_year$`10th`/1000000
multi_year$`25th` <- multi_year$`25th`/1000000
multi_year$`75th` <- multi_year$`75th`/1000000
multi_year$`90th` <- multi_year$`90th`/1000000
multi_year$Median <- multi_year$Median/1000000

# yearly landings by zone
yr_zone_landings <- read_csv("Data/Zone_Landings_year.csv")
yr_zone_landings$Landings <- yr_zone_landings$Landings/1000000

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

factpal2 <- colorFactor(c("#25356f",
                         "#cb9d00",
                         "#961e1d",
                         "#b3875a",
                         "#ffd380",
                         "#76a1a0",
                         "#97c8f0"), multi_year_shp$ZONEID)

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
                            font-size:20px;}
                            .selectize-dropdown{color:#003A54}
                            .container-fluid{
                            width: 100%;
                            margin: 0 auto;
                            padding: 0;
                            }
                           p{font-size: 18px}
                           ul{font-size: 18px}
                           h1{font-size: 34px}
                           h2{font-size: 30px}
                           h3{font-size: 28px}
                           h4{font-size: 20px}
                           h6{font-size: 18px}
                           @media screen and (min-width: 1300px){
                           .container-fluid{
                              width: 1300px;
                           }
                           }"))),
  titlePanel(tags$h1("Climate and Maine's lobster fishery"), windowTitle = "LobsterDashboard"),
  fluidRow(
    column(
      width = 12,
      tags$h2("Purpose"),
      tags$p("This dashboard provides access to data on ocean temperatures and landings in Maine’s lobster fishery. 
             Use the prompts to explore relationships between lobster landings and water temperature, 
             as well as how temperature changes may affect Maine's lobster landings in the future.")
    )
  ),
  
  fluidRow(
    column(width = 12,
           tags$h2("Overview"),
           tags$p("The fishery for American lobster (Homarus americanus) is one of the most valuable in the United States, 
                     and it is especially important in the state of Maine.  
                     Ninety percent of the volume of U.S. lobster landings are from the Gulf of Maine and 
                     80% from Maine alone. In Maine, the lobster fishery accounts for 78% of all fishery landings in the state.  
                     The high dependence of Maine’s coastal economy on this single fishery means that 
                     changes in the volume of lobster landings have a large impact in the state's coastal communities. 
                  Maine's lobster fishery is co-managed by the state's Department of Marine Resources and councils composed
           of lobster license holders that represent each of seven Lobster Management Zones (Figure 1). 
           Each zone has a set of regulations that controls the commercial harvest of lobsters.",
                  tags$a(href="https://www.maine.gov/dmr/about/councils/lobzones/index.html", "Lobster Zone Councils"), 
                  " help tailor state-wide regulations to the conditions within each distinct zone."),
           tags$p("Ocean temperatures have been trending upwards in the Gulf of Maine, and rising temperatures
                     affect lobster landings in multiple ways and at multiple time scales, including:"), 
           tags$ul(
             tags$li("Seasonal: The timing of spring warming influences when lobsters become active (and therefore more catchable) 
                           and cues their molting into harvestable size classes, both of which influence when the period of high landings begins in the fishery each year."),
             tags$li("Multi-year: Temperature has been associated with rates of post-larval settlement, 
                           and the success of settlement, as well as subsequent growth and mortality, 
                           influences the number of lobsters that will mature into harvestable sizes in any given year."),
             tags$li("Multi-decadal: Temperature also influences the recruitment of lobsters. 
                     Over multiple generations directional changes in recruitment affect the size of the lobster population and the associated sustainable catch levels.")),
           tags$p("In this dashboard, we provide opportunities to investigate past temperature and landings data 
                  as well as explore how temperature may affect Maine's lobster fishery at these three time scales in the future."))),
  fluidRow(
    column(
      width = 12,
      offset = 3,
      leafletOutput(outputId = "Lob_zone_map", height = "600px", width = "600px")
    )
  ),
  fluidRow(
    column(
      width = 6,
      offset = 3,
      tags$h6("Figure 1: Domains of the three studies. Seasonal: Maine Lobster Management Zones. 
              Multi-year: Southern New England to New Brunswick, Canada.
              Multi-decadal: Southern New England and the Gulf of Maine")
    )
  ),
  
  tags$hr(style="border-color: black;"),
  
  fluidRow(
    column(
      width = 12,
      tags$h2("Explore past and future data")
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      selectInput(inputId = "time_period", 
                  choices = c("Choose One" = "", "Past" = "hist", "Future" = "proj"), 
                  label = tags$h4("Select a time period"))
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
                    axis.text = element_text(size=16), 
                    legend.text = element_text(size = 14),
                    legend.title = element_text(size = 14)))
  
  output$time_period_ui <- renderUI({
    if (is.null(input$time_period))
      return()
    
    switch(
      input$time_period,
      
      "hist" = fluidRow(
        column(
          width = 12,
          tags$h2("Are you interested in temperature or landings?"),
          selectInput(inputId = "indicators", label = tags$h4("Select Variable"), 
                      choices = c("Choose One" = "", "Temperature" = "temp", "Landings" = "land"),
                      selected = NULL)
        )
      ),
      
      "proj" = fluidRow(
        column(
          width = 12,
          tags$h2("Which relationship do you want to explore?"),
          selectInput(inputId = "forecasts", label = tags$h4("Select Relationship"), 
                      choices = c("Choose One" = "", "Seasonal" = "season", "Multi-year" = "annual", "Multi-decadal" = "multi_decadal"),
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
                      tags$p("Ocean temperature in the Gulf of Maine has risen rapidly over the past 15 years. 
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
                 tags$h6("Figure T1: Yearly averaged surface and bottom temperature for Maine's lobster management zones. 
                         The horizonal line is the average temperature for the specified depth across the entire time series.")
               )
             ),
             fluidRow(
               column(width = 12,
                      tags$p("Ocean temperature has a strong seasonality, with the coldest water occurring in March,
                               and the warmest water occuring in August (Figure T2, Observed).
                               When temperature is viewed as anomalies (difference from average), yearly and multi-year patterns are more noticeable (Figure T2, Anomalies)."), 
                      tags$p("The interactive graphs below can be used to observe differences in sea 
                                       surface and bottom temperature in each of Maine’s lobster management zones 
                                       over time (top panel) and within the annual cycle of individual years (bottom panel).
                                       Use the radio buttons to select surface or bottom temperature as 
                                       as observed temperature or temperature anomalies 
                                       (difference from 1981-2011 long-term average). 
                                       Monthly and seasonal temperature averages are available.
                                       The legend of the graph is also interactive. In the top panel, each of the lobster zones is selectable by clicking on 
                                       the zone in the legend to remove the zone or double clicking to isolate the zone.  
                                       For the bottom panel, the years are interactive in a similar manner, and lobster zones of interest can be selected 
                                       from the drop-down menu above the graph."),
                      tags$p("Sea surface temperature data are from", 
                                       tags$a("https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.html", "NOAA OISST V2")," and 
                                      bottom temperature data are from ", tags$a("http://fvcom.smast.umassd.edu/necofs/","FVCOM NECOFS model")," ."),
                      tags$br())),
             
             fluidRow(
               column(width = 12,
                      fluidRow(
                        column(
                          width = 4,
                          radioButtons("sur_bot", "Depth",
                                       choices = c("Surface Temperature" = "Yearday", "Bottom Temperature" = "Month"))
                        ),
                        column(
                          width = 4,
                          radioButtons("vis", "Data format",
                                       choices = c("Observed" = "raw_temp", "Anomalies" = "anom"))),
                        column(
                          width = 4,
                          selectInput("season_sel", "Season", choices = c("Monthly" = "Monthly",
                                                                          "Winter" = "Winter",
                                                                          "Spring" = "Spring",
                                                                          "Summer" = "Summer",
                                                                          "Fall" = "Fall"))
                        )),
                      plotlyOutput(outputId = "plotly_lob_zone"))),
              fluidRow(
               column(width = 12,
                      fluidRow(
                        column(
                          width = 3,
                          title = "Zone",
                          selectInput("zones", "Select Lobster Zone",
                                      choices = Zone_index))),
                      plotlyOutput(outputId = "yrDay_plot"))),
             fluidRow(
               column(
                 width = 12,
                 tags$h6("Figure T2: Annual cycles of temperature in Maine's lobster management zones.")
               )
             )),
           
           
           "land" = column(
             width = 12,
             fluidRow(
               column(
                 width = 12,
                 tags$h1("Lobster Landings Explorer"))),
             fluidRow(
               column(
                 width = 12,
                 tags$p("Lobster 'landings' represent lobsters that are caught, brought to shore, and sold by the harvester. 
                 Lobster landings in the state of Maine have risen steadily since the mid-1980s (Figure L1). 
                        A large portion of this increase is attributed to 
                        the warming Gulf of Maine, which has 
                        enhanced productivity of the lobster population and contributed to a northward shift in the 
                        location of lobster in Northeast U.S. waters."))),
             fluidRow(
               column(
                 width = 6,
                 offset = 3,
                 plotOutput(outputId = "long_term_langings")
               ),
               column(
                 width = 6
               #  leafletOutput(outputId = "biomass")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align="center",
                 tags$h6("Figure L1: Yearly total lobster landings in Maine.")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 tags$br(),
                 tags$p("Lobster landings do not occur consistently over the course of a year, but instead display a strong seasonality (Figure L2). 
                        There is a low landings period during the winter months, and a high landings period from the summer into the fall.
                        The transition from the low landings season to the high landings season happens in the late spring or early summer, 
                        and the timing is influenced by water temperatures in that location and year.")
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
                 tags$h6("Figure L2: Yearly differences in seasonality of lobster landings from Lobster Management Zone C. The year 2016 had an earlier transition to the high summer
                         landings period than 2014 (indicated by the arrow).")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 tags$br(),
                 tags$p("Use the interactive graph below to observe the change in landings for each zone through time. Notice how there is both 
                 a seasonal cycle and multi-year trend in landings. Use the radio buttons to select
                 the view as monthly or yearly averages for the full time series (on the top) or flip through the different lobster zones 
                 to see how the seasonal cycles compare between years (on the bottom). 
                 The legend of the graph is interactive, and each of the lobster zones and years are selectable by clicking on the
                        zone in the legend to remove the zone or double clicking to isolate the zone. 
                        Landings data are from Maine Department of Maine Resources."))),
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
                 tags$h1("Seasonality of lobster landings"),
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
                 tags$h6("Figure S1: Seasonality and yearly differences in lobster landings in Maine. 
                 The vertical lines indicate the timing of the transition date for 2014 (gray) and 
                 2016 (black). 
                 This date is determined as a point when the landings have clearly started to increase for the season (for more details, see Mills et al. 2017).
                 The year 2016 had an earlier transition to the high summer
                         landings period than 2014 (indicated by the arrow)."),
                 tags$br(),
                 tags$p("This transition to the high-landings summer period typically happens in late June 
                    or early July, but it can occur in late May through late July.  Water temperature 
                    plays a role in determining when this transition happens, and there is a lagged 
                    relationship between spring water temperatures and the timing of the transition date.  
                    Thus, early spring ocean temperatures can be used to calculate the timing of the uptick 
                    in landings."))),
        fluidRow(
          column(
            width = 12,
            tags$h3("How do the transition periods compare to springtime temperature?"),
            tags$p("Use the widget below to compare the water temperature and statewide transition dates of different years. 
                   Notice how the patterns between the temperature (black line) and lobster landings transition period (red line) are similar. 
                   Years with warmer temperatures also tend to have earlier transition dates.")
          )
        ),
        fluidRow(
          column(
            width = 6,
            checkboxInput("transition_date_switch", "Show Transition Day", value = TRUE)
          ),
          column(
            width = 12,
            plotOutput("transition_date")
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            tags$h3("How do past forecasts of transition date compare to the actual transition period?"),
            tags$p("Use the widget below to see how the actual lobster landings transition date in the state of Maine aligns with the 
                   forecasted transition period, based on the temperature-transition day relationship shown above. 
                   The vertical red line is the observed transition date and the black dots are the monthly landings.
                   The forecast is run many times to create a range of possible transition dates. 
                   The color bar shows the percent of forecast runs that align on the same week. 
                   The darker the color, the higher chance the transition day will occur during that week. The tool allows you
                   to change the year and the date of the forecast. The forecast results might change depending on the forecast date. 
                   If the current year is shown, only the forecast is shown. 
                   Current year landings and actual transition date become available the following year.")
          )
        ),
        fluidRow(
          column(
            width = 4,
            radioButtons("forecast_date", "Select Forecast Date",
                        choices = c("March 1" = "03-01",
                                    "March 15" = "03-15",
                                    "April 1" = "04-01",
                                    "April 15" = "04-15",
                                    "May 1" = "05-01"),
                        inline = TRUE)),
          column(
            width = 4,
            sliderInput("Year", label = "Select Year",
                        min = 2004, max = 2020, value = 2014, sep = "")
          ),
          column(
            width = 4,
            checkboxInput("actual", label = "Actual Transition Date", value = TRUE),
            checkboxInput("forecast", label = "Forecasted Transition Date", value = TRUE)
          )
        ),
        fluidRow(
          column(
            width = 12,
            plotOutput("Strt_day_forecast")
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
                                        warmer spring sea surface temperatures are associated with earlier landings transition dates. 
                                        This relationship can be used to predict the 
                                        transition period between the winter and summer landings based on earlier spring temperatures.
                                        The date slider shows the temperature-transition date relationship for different days, 
                                        and the lobster zone selection input changes the lobster zone. 
                                        Each zone has its own relationship between springtime water temperature 
                                        and lobster landings."),
                           tags$p("The bottom graph shows the correlation between the average sea surface 
                                        temperature and the transition date for January to June. In this graph, the 
                                        peak of the curve indicates the best spring date for predicting the landings transition date. 
                                  For most lobster zones, the correlation peaks between March 15th and March 25th."))))),
                 fluidRow(
                   column(
                     width = 4,
                     title = "Predictor Date Input",
                     sliderInput("strt_day_period", "Forecast Date",
                                 min = as.Date("2000-01-01"),
                                 max = as.Date("2000-04-30"),
                                 value = as.Date("2000-03-15"), 
                                 timeFormat = "%d-%b")),
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
            tags$p("Each year, approximately 85% of Maine’s lobster harvest is comprised of new recruits to the fishery (",
            tags$a(href = "https://www.asmfc.org/uploads/file//55d61d73AmLobsterStockAssmt_PeerReviewReport_Aug2015_red2.pdf", "ASMFC 2015"),
            "). Due to regional and interannual differences in temperature-dependent growth rates, 
              lobsters take anywhere from 5 to 10 years to enter the fishery after their larvae populate coastal nurseries.
              Annual surveys of newly settled lobsters conducted through the ",
            tags$a(href="https://umaine.edu/wahlelab/american-lobster-settlement-index-alsi/", "American Lobster Settlement Index"), 
            "(ALSI) offer the potential to give the industry and managers an early indication of the strength of upcoming year classes. 
            The settlement monitoring that feeds into the ALSI has a large geographic range, 
                   stretching beyond Maine's lobster zones into southern New England, New Brunswick and Nova Scotia (Figure Y1).
              Observations of annual settlement and estimates of growth and mortality can be used to track the fate of each year 
              class and estimate the abundance of recruits entering the fishery each year. 
            The relationship between lobster recruitment into the fishery and landings can then be used 
            to project future landings (see ", 
            tags$a(href = "https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/eap.2006", "Oppenheim et al. 2019"), 
            " for details)."))),
        fluidRow(
          column(
            width = 12,
            img(src = "ALSI_map.png"),
            tags$h6("Figure Y1. ALSI sampling locations in New England and Atlantic Canada. Red dots indicate diver-based suction sampling and 
                    yellow dots indicate vessel-deployed bio-collector.")
          )
        ),
        fluidRow(
          column(
            width = 12,
            tags$h3("Projecting lobster landings six years into the future"),
            tags$p("In each plot in the section below, black dots connected by the line are the observed lobster landings for each year. 
                   The red line represents predicted recruitment values for each year derived from a model based on observed larval settlement 
                   and subsequent growth and mortality. 
                   The model is run 'backwards' (hindcast, blue shaded area) over the observed data to translate and scale 
                   the recruitment to landings. Using this information, the model is projected forward in time (forecast, orange shaded area). 
                   The dark shaded areas show the 25-75 percentiles, and the light shaded areas show the 10-90 percentile, 
                   meaning you would expect 50% of the data to fall within the dark shaded area and 80% of the data to fall within the light shaded area. 
                   You can use the drop down menus below to view the 2020-2025 forecast for lobster zones in which we have the ability to make a skillful forecast.")
          )
        ),
        fluidRow(
          column(
            width = 12,
            selectInput("study_area", "Study Area", c("Zone A (Jonesport, ME)" = "A",
                                                      "Zone B (Mount Desert Island, ME)" = "B",
                                                      "Zone C (Penobscot Bay East, ME)" = "C",
                                                      "Zone D (Penobscot Bay West, ME)" = "D",
                                                      "Zone E (Midcoast, ME)" = "E",
                                                      "Zone F (Casco Bay, ME)" = "F"),
                        selected = "A")
          )),
        fluidRow(
          column(
            width = 12,
            plotOutput("multiyear", height = "525px")
          )),
        fluidRow(
          column(
            width = 12,
            tags$h6("Predictive model forecasts for study areas with significant recruitment index-to-landings 
                   relationships using the best-fitting models. Observed (black line) and predicted (red line) lobster landings, hindcasts (blue), 
                   and forecasts from 2019 forward (orange) with 25–75% (dark shaded) and 10–90% (light shaded) quantiles (",
                    tags$a(href = "https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/eap.2006", "Oppenheim et al. 2019"), ").")
          )),
        fluidRow(
          column(
            width = 12,
            tags$h3("How does the model work?"),
            tags$p("Ocean temperature and other environmental variables, affect lobster growth and mortality.
                   Using those environmental relationships, we can predict how long it will take newly settled, 
                   young-of-year lobsters to grow into a harvestable size. 
                   Pairing this knowledge with the data from the American Lobster Settlement Index, 
                   we can calculate and project the recruitment index forward about six years."),
            tags$p("We can assess the predictive capabilities of the model by 
            selecting only the data before 2014 to use to develop a model of recruitment 
            based on settlement observations. 
            If the initial recruitment model fits the observed data, then we are able to make a prediction out about six years. 
            We can then compare these predictions to observed landings and evaluate how well the model performs, which we call its predictive 'skill'."),
            checkboxInput("ALSI_skill_select", label = "Show forecast", value = FALSE),
            plotOutput("ALSI_skill"),
            tags$h6("Assessing model skill by comparing model predictions of landings to actual observations."),
            tags$br(),
            tags$h3("How do the forecasts change through time?"),
            tags$p("These forecasts take in new data each year and adjust the predictions accordingly. 
                   Using the interactive graphs below, you can change the forecast date to see how well the models performed in forecasting future landings.")
          )
        ),
        fluidRow(
          column(
            width = 12,
            sliderInput("alsi_forecast_date", "Forecast Date", 
                        min = 2013, max = 2019, value = 2013, 
                        sep = "", step = 1)
          )
        ),
        fluidRow(
          column(
            width = 12,
            plotOutput("multi_year_assessment")
          )
        ),
        fluidRow(
          column(
            width = 12,
            tags$h6("Observed (black line and dots) and predicted (red line) lobster landings. Hindcasts (blue) 
                   and forecasts (orange) with 25–75% (dark shaded) and 10–90% (light shaded) 
                   quantiles are made starting from the selected date. 
                   Faded dots are not included in the forecast, as they were collected after the forecast was made. 
                    These observations can be used to assess the model's skill.")
          )
        )
        ),
      
      "multi_decadal" = column(
        width = 12,
        fluidRow(
          column(
            width = 12,
            tags$h1("Multi-decadal Trends"))),
        fluidRow(
          column(
            width = 12,
            tags$p("Rising ocean temperatures can affect multiple biological processes in lobster, 
            but they exert a particularly strong effect on lobster survival through the first year of life.  
              Survival during the first year is enhanced by optimal summer temperatures of approximately 61 degrees F.  
              Recent temperatures in the Gulf of Maine near this optimum have boosted Gulf of Maine lobster abundance, 
              but as temperatures have exceeded this optimum in Southern New England, the lobster population has declined.  
              Future temperatures in the Gulf of Maine are projected to exceed this preferred temperature and are expected to 
                   influence the trajectory of the lobster population."))),
        fluidRow(
          column(
            width = 4,
            checkboxInput("cmip_proj", "Summer Temperature Projections")),
          column(width = 6,
                 radioButtons("sne_select", label = "Location", choices = c("Gulf of Maine" = "gom", 
                                                       "Southern New England" = "sne"),
                             inline = TRUE))),
        fluidRow(
          column(
            width = 12,
            plotOutput("temp_landings_trends")
          )
        ),
        fluidRow(
          column(
            width = 12,
            tags$h6("Trends in Summertime sea surface temperature (dark blue line) in the Gulf of Maine 
            and southern New England from 
            1982 to 2020 and lobster landings (red dots) in Maine and southern New England states. 
                    The 'Summer Temperature Projection' button reveals the 
                    projected summertime sea surface temperature to 2050 (light blue, shaded region is 95% confidence interval) 
                    from teh CMIP-5 climate model ensemble run under RCP 8.5.")
          )
        ),
        tags$br(),
        fluidRow(
          column(
            width = 12,
            tags$p("Temperature can be used to predict the size of the lobster population. 
            Using temperature projections, we can estimate the lobster population through 2050. 
            We can then use the relationship between population and observed landings to convert the projected population abundance to landings.
            The interactive graph below can be used to explore projected estimates of the Gulf of Maine 
            and southern New England lobster 
                   landings and population under the 'business as usual' climate change scenario (RCP 8.5)"))),
        fluidRow(
          column(
            width = 4,
            radioButtons("pop_landings", "Modeled data", choices = c("Population" = "pop", "Landings" = "land"),
                         inline = TRUE)),
            column(width = 6,
                   radioButtons("gom_select", label = "Location", choices = c("Gulf of Maine" = "gom", 
                                                                        "Southern New England" = "sne"),
                                inline = TRUE))
        ),
        fluidRow(
          column(
            width = 12,
            plotOutput("multidecade")
          )
        ),
        fluidRow(
          column(
            width = 12,
            tags$h6("Projections of American lobster population and landings from 1985 to 2050 for the Gulf of Maine and southern New England. 
                    The red dots are observed population and landings. The shaded region shows the 95% confidence intervals.")
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
    leaflet() %>% addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      fitBounds(-72.5, 41.5, -66, 45) %>%
      addPolygons(data = sne_shp, 
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = FALSE),
                  label = "Southern New England",
                  color = "transparent",
                  fillColor = "red",
                  group = "Multi-decadal") %>%
      addPolygons(data = gom_shp, 
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = FALSE),
                  label = "Gulf of Maine",
                  color = "transparent",
                  fillColor = "blue",
                  group = "Multi-decadal") %>% 
      addPolygons(data = Lob_zone_sf_simple, fillColor = ~factpal(ZONEID), 
                  color = "transparent", fillOpacity = 1, layerId = ~ZONEID,
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2, bringToFront = TRUE),
                  label = ~paste("Zone", ZONEID, sep = " "),
                  group = "Seasonal") %>% 
      addPolygons(data = multi_year_shp, fillColor = ~factpal(ZONEID), 
                  color = "transparent", fillOpacity = 1,
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2, bringToFront = TRUE),
                  label = ~paste("Zone", ZONEID, sep = " "),
                  group = "Multi-year") %>% 
      addLegend(colors = c("#25356f","#cb9d00","#961e1d","#b3875a","#ffd380","#76a1a0","#97c8f0"), 
                labels = c("A","B","C", "D", "E", "F", "G"),
                title = "Management zones",
                position = "topleft",
                group = "Seasonal") %>% 
      addLegend(colors = c("blue","red"), 
                labels = c("Gulf of Maine","Southern New England"),
                title = "Study regions",
                position = "topleft",
                group = "Multi-decadal") %>% 
      addLayersControl(
        baseGroups = c("Seasonal", "Multi-year", "Multi-decadal"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      leaflet.extras::suspendScroll()})
  
  ############################## LONG_TERM LANDINGS ##############
  
  output$long_term_langings <- renderPlot({
    yr_lob_landings %>% ggplot() + geom_line(aes(x = Year, y = `pounds(millions)`), size = 3) + 
      labs(x = "Year", y = "Landings (millions of pounds)")
  })
  
  
  ############################### CENTER OF BIOMASS ##############
  
  #output$biomass <- renderLeaflet({
  # leaflet() %>% addTiles() %>% 
  #    addCircleMarkers(data=lob_movement, lng = ~Longitude, lat = ~Latitude, color = ~move_pal(Year), label = ~Year) %>%
  #    addLegend(position = "topright",
  #              pal = move_pal, 
  #              values = c(min(lob_movement$Year), max(lob_movement$Year)),
  #              labFormat = labelFormat(big.mark = ""))
  #})
  
  ############################### SPRING TRANSITION ###########
  
  output$transition <- renderPlot({
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
      geom_segment(aes(x = 217, y = 15, xend = 196, yend = 15, colour = "segment"),
                   arrow = arrow(), color = "red", size = 2) +
      scale_color_manual(values=c("#88A2AA","#0F1A20"))
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
      scale_color_manual(labels = c("Bottom", "Surface"),
                           values = c("#1A237E","#42B3D5")) +
      guides(color = guide_legend(reverse = TRUE))
  })
  
  
  ############################## LOBSTER ZONE PLOTLY PLOT SERVER LOGIC ####
  output$plotly_lob_zone <- renderPlotly({
    
    if(input$sur_bot == "Yearday"){
      plot1 <- anom_fun(input_Data = All_Zone_avgs_mon, zones = input$zones, 
                        vis = input$vis, sur_bot = input$sur_bot,
                        season_sel = input$season_sel) + 
        theme(axis.title = element_text(size = 12),
              axis.text = element_text(size=10), 
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 8))}
    if(input$sur_bot == "Month"){
      plot1 <- anom_fun(input_Data = FVCOM_Zone_avgs, zones = input$zones, 
                        vis = input$vis, sur_bot = input$sur_bot,
                        season_sel = input$season_sel) + 
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
        labs(title = "Total Landings", x= "Date", y = "Total Landings (millions of pounds)", color = "Lobster \n Zone")  +
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
      labs(title = paste("Zone", input$zone_bio, "Landings", sep = " "), 
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
  

  output$transition_date <- renderPlot({
    on_off <- paste(input$transition_date_switch)
    
    switch(on_off,
           
           "FALSE" =   gom_coast %>% mutate(Year = year(t), yrday = yday(t)) %>% 
             filter(yrday %in% c(60:121)) %>% 
             group_by(Year) %>% summarise(Avg = mean(Avg, na.rm =TRUE), .groups = "drop") %>% 
             left_join(gom_strt, by = "Year") %>% 
             ggplot() + geom_line(aes(x = Year, y = Avg), size = 4, color = "darkblue") +
             labs(y = "Sea Surface Temperature (deg F)") +
             scale_y_continuous(limits = c(37,43), 
                                sec.axis = sec_axis(~((43 - (.-23))*7), name = "Transition Day",
                                                    breaks = c(seq(135,230,5)), 
                                                    labels = c("15-May","20-May","25-May","30-May","04-Jun","09-Jun","14-Jun","19-Jun", "24-Jun", 
                                                               "29-Jun", "04-Jul", "09-Jul", "14-Jul", 
                                                               "19-Jul", "24-Jul", "29-Jul", "03-Aug", "08-Aug", "13-Aug", "18-Aug"))) +
             theme(axis.text.y.right = element_text(color = "transparent"),
                   axis.ticks.y.right = element_line(color = "transparent"),
                   axis.title.y.right = element_text(color = "transparent")) + 
             theme(axis.text.y.left = element_text(color = "darkblue"),
                   axis.ticks.y.left = element_line(color = "darkblue"),
                   axis.title.y.left = element_text(color = "darkblue"),
                   axis.line.y.left = element_line(color = "darkblue")),
           
           "TRUE" =   gom_coast %>% mutate(Year = year(t), yrday = yday(t)) %>% 
             filter(yrday %in% c(60:121)) %>% 
             group_by(Year) %>% summarise(Avg = mean(Avg, na.rm =TRUE), .groups = "drop") %>% 
             left_join(gom_strt, by = "Year") %>% 
             ggplot() + geom_line(aes(x = Year, y = Avg), size = 4, color = "darkblue") +
             geom_line(aes(x = Year, y = (43 - pwrDstrt2/7)+23), col = "darkred", size = 4) +
             scale_y_continuous(name = "Sea Surface Temperature (deg F)", 
                                limits = c(37,43),
                                sec.axis = sec_axis(~((43 - (.-23))*7), name = "Transition Day",
                                                    breaks = c(seq(135,230,5)), 
                                                    labels = c("15-May","20-May","25-May","30-May","04-Jun","09-Jun","14-Jun","19-Jun", "24-Jun", 
                                                               "29-Jun", "04-Jul", "09-Jul", "14-Jul", 
                                                               "19-Jul", "24-Jul", "29-Jul", "03-Aug", "08-Aug", "13-Aug", "18-Aug"))) +
             theme(axis.text.y.right = element_text(color = "darkred"),
                   axis.ticks.y.right = element_line(color = "darkred"),
                   axis.title.y.right = element_text(color = "darkred"),
                   axis.line.y.left = element_line(color = "darkred")) + 
             theme(axis.text.y.left = element_text(color = "darkblue"),
                   axis.ticks.y.left = element_line(color = "darkblue"),
                   axis.title.y.left = element_text(color = "darkblue"),
                   axis.line.y.left = element_line(color = "darkblue"))
           
    )
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
                   arrow = arrow(), color = "red", size = 2) +
      scale_color_manual(values=c("#88A2AA","#0F1A20"))
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
                          title = "Temp-Transition Date </br> correlation")}
  })
  
  
  observe({
    req(input$strt_day_period)
    mon <- format(as.Date(input$strt_day_period), "%d-%B")
    
    yrdays <- lubridate::yday(as.Date(input$strt_day_period))
    
    output$strt_day_plot <- renderPlotly({
      zone_strt_temp_all %>% filter(id == input$zone_strt, yrday == yrdays) %>% 
        mutate(zone_avg = round(zone_avg, 2),
               Yearday = round(zoneStrtDay),
               "Transition Date" = format(as.Date(Yearday, origin = paste(yr, "12-31", sep = "-")), "%d-%b-%Y")) %>% 
        dplyr::rename("SST" = zone_avg) %>%
        ggplot() +  geom_point(aes(Yearday, SST, group = `Transition Date`)) + 
        geom_smooth(aes(Yearday, SST), method = "lm", se = FALSE) +
        scale_x_continuous(breaks = c(seq(135,230,10)), 
                           labels = c("15-May","25-May","04-Jun","14-Jun", "24-Jun", 
                                      "04-Jul", "14-Jul", 
                                      "24-Jul", "03-Aug", "13-Aug"))+
        labs(y = paste(mon, "Average Zone SST"), x = "Landings Transition Day", title = paste("Zone", input$zone_strt, sep = " "))  +
        theme(legend.position = "none",
              axis.title = element_text(size = 12),
              axis.text = element_text(size=10), 
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 8))
      
    })
    
    output$strt_day_plot2 <- renderPlot({
      
      zone_strt_temp %>% data.frame() %>%
        ggplot() +  geom_smooth(aes(yrday, corr, color = id), se = FALSE, method = "gam") + 
        labs(x = "Day of Year", y = "Correlation", title = paste("Zone", input$zone_strt, sep = " ")) +
        scale_x_continuous(breaks = c(seq(1,140,20)), 
                           labels = c("01-Jan","21-Jan","10-Feb","02-Mar", "22-Mar", 
                                      "11-Apr", "01-May"), 
                           limits = c(1,130)) + 
        scale_y_reverse(limits = c(0,-1)) +
        gghighlight(id == input$zone_strt, label_key = id, 
                    keep_scales = TRUE, use_group_by = FALSE, unhighlighted_colour = "gray93") + 
        geom_vline(xintercept = yrdays) + scale_color_manual(values = c("#25356f",
                                                                        "#cb9d00",
                                                                        "#961e1d",
                                                                        "#b3875a",
                                                                        "#ffd380",
                                                                        "#76a1a0",
                                                                        "#97c8f0"), name = "") +
        theme(legend.position = c(.2,.9))
    })
  })
  
  
  ################################ START DAY YDAY plot #################3
  
  output$Strt_day_forecast <- renderPlot({
    plot1 <- 
      mon_landings %>%
      mutate(t = as.Date(paste(Year,Month,"01", sep="-")), 
             molen = days_in_month(t),
             Date = as.Date(paste(Year, Month, molen, sep="-")),
             yrday = yday(Date),
             Pounds = Pounds/1000000) %>%
      left_join(., gom_strt, by = c("Year")) %>%
      full_join(., forecasts, by = c("Year")) %>% 
      filter(Year == input$Year, Forecast_Date == as.Date(paste(input$Year, input$forecast_date, sep = "-"))) %>% 
      ggplot() + geom_point(aes(yrday, Pounds), size = 5) +
      scale_x_continuous(breaks = c(seq(75,250,25)), 
                         labels = c("16-Mar", "10-Apr","5-May", "30-May", "24-Jun",
                                    "19-Jul", "13-Aug",
                                    "7-Sep"), 
                         limits = c(50,250)) +
      scale_y_continuous(limits = c(-5,32), breaks = seq(0,30,5)) +
      labs(y = "Landings (millions of pounds)", x = "Day of year") +
      theme(legend.text = element_text(size = 14),
            legend.title = element_text(size = 14))

    if(input$forecast){
      plot1 <- plot1 + geom_tile(aes(x = as.numeric(strtbin), y = -2, fill = Percent), height = 6) +
        scale_fill_gradient(limits = c(0,100), low = "white", high = "black") +
        scale_alpha(range = c(0,1), guide = "none") 
    }
      
    if(input$actual){
      plot1 <-  plot1 + geom_vline(aes(xintercept = pwrDstrt2), col = "darkred", size = 3)
    }
    
    print(plot1)
    
  })
  
  
  ################################ POP_LANDINGS PLOT #################
  
  output$multidecade <- renderPlot({
    
    switch(input$gom_select,
           "gom" =     switch(input$pop_landings,
                              
                              "land" = gom_lob_data %>% 
                                filter(Model == "CMIP5_RCP 8.5_mean") %>% 
                                ggplot() + 
                                geom_point(aes(Year, Landings), size = 3, color = "darkred") + 
                                geom_line(aes(Year, Landings_pred), color = "#CD5C5C", size = 2) +
                                geom_ribbon(aes(x = Year, ymax = upr, ymin = lwr), fill = "#CD5C5C", alpha = .1) + 
                                theme(legend.position = c(.85,.85)) +
                                labs(y = "Landings (millions of pounds)") +
                                scale_x_continuous(limits = c(1982,2050)),
                              
                              "pop" = gom_lob_data %>% filter(Model == "CMIP5_RCP 8.5_mean") %>% ggplot() + 
                                geom_point(aes(Year, Abundance), size = 3, color = "darkred") +
                                geom_line(aes(Year, Average), size = 2, color = "#CD5C5C") +
                                geom_ribbon(aes(x = Year, ymax = Average + Std*2, ymin = Average - Std*2), fill = "#CD5C5C", alpha = .1) + 
                                theme(legend.position = c(.85,.85)) +
                                labs(y = "Population (millions of individuals)") +
                                scale_x_continuous(limits = c(1982,2050))
           ),
           
           "sne" =       switch(input$pop_landings,
                                
                                "land" = sne_lob_data %>% 
                                  filter(Model == "CMIP5_RCP 8.5_mean") %>% 
                                  ggplot() + 
                                  geom_point(aes(Year, Landings), size = 3, color = "darkred") + 
                                  geom_line(aes(Year, Landings_pred), color = "#CD5C5C", size = 2) +
                                  geom_ribbon(aes(x = Year, ymax = upr, ymin = lwr), fill = "#CD5C5C", alpha = .1) + 
                                  theme(legend.position = c(.85,.85)) +
                                  labs(y = "Landings (millions of pounds)") +
                                  scale_x_continuous(limits = c(1982,2050)),
                                
                                "pop" = sne_lob_data %>% filter(Model == "CMIP5_RCP 8.5_mean") %>% ggplot() + 
                                  geom_point(aes(Year, Abundance), size = 3, color = "darkred") +
                                  geom_line(aes(Year, Average), size = 2, color = "#CD5C5C") +
                                  geom_ribbon(aes(x = Year, ymax = Average + Std*2, ymin = Average - Std*2), fill = "#CD5C5C", alpha = .1) + 
                                  theme(legend.position = c(.85,.85)) +
                                  labs(y = "Population (millions of individuals)") +
                                  scale_x_continuous(limits = c(1982,2050))
           )
    )
  })
  
  #################################### temp_landings_trends #############
  
  output$temp_landings_trends <- renderPlot({
    
    on_off <- paste(input$cmip_proj)
    
    switch(input$sne_select,
           "gom" =     switch (on_off,
                               "FALSE" = gom_oisst_df %>%
                                 left_join(., gom_landings_yr, by = "Year") %>% 
                                 filter(Year >1981, Year < 2020) %>% 
                                 ggplot() + geom_line(aes(x = Year, y = Avg), size = 3, col = "darkblue") +
                                 geom_point(aes(x = Year, y = Landings/20+57.5), size = 3, color = "darkred") +
                                 scale_y_continuous(sec.axis = sec_axis(~(.-57.5)*20, breaks = c(25,50,75,100, 125, 150), 
                                                                        labels = c(25,50,75,100, 125, 150), name = "Landings (millions of pounds)")) +
                                 labs(y = "Sea Surface Temperature (deg F)") +
                                 theme(axis.title.y.left = element_text(color = "darkblue"),
                                       axis.ticks.y.left = element_line(color = "darkblue"),
                                       axis.text.y.left = element_text(color = "darkblue"),
                                       axis.line.y.left = element_line(color = "darkblue"),
                                       axis.title.y.right = element_text(color = "darkred"),
                                       axis.ticks.y.right = element_line(color = "darkred"),
                                       axis.text.y.right = element_text(color = "darkred"),
                                       axis.line.y.right = element_line(color = "darkred")),
                               
                               "TRUE" = gom_oisst_df %>% 
                                 left_join(., gom_landings_yr, by = "Year") %>% 
                                 right_join(cmipsst, by = "Year") %>% filter(Year < 2051) %>%  
                                 ggplot() + geom_line(aes(Year, Mean), size = 3, col = "deepskyblue") +
                                 geom_ribbon(aes(x = Year, ymin = low, ymax = high), fill = "deepskyblue", alpha = .15) +
                                 labs(y = "Sea Surface Temperature (deg F)") + geom_line(aes(x = Year, y = Avg), size = 3, col = "darkblue") +
                                 geom_point(aes(x = Year, y = Landings/20+57.5), size = 3, color = "darkred") +
                                 scale_y_continuous(sec.axis = sec_axis(~(.-57.5)*20, breaks = c(25,50,75,100, 125, 150), 
                                                                        labels = c(25,50,75,100, 125, 150), name = "Landings (millions of pounds)")) +
                                 labs(y = "Sea Surface Temperature (deg F)") +
                                 theme(axis.title.y.left = element_text(color = "darkblue"),
                                       axis.ticks.y.left = element_line(color = "darkblue"),
                                       axis.text.y.left = element_text(color = "darkblue"),
                                       axis.line.y.left = element_line(color = "darkblue"),
                                       axis.title.y.right = element_text(color = "darkred"),
                                       axis.ticks.y.right = element_line(color = "darkred"),
                                       axis.text.y.right = element_text(color = "darkred"),
                                       axis.line.y.right = element_line(color = "darkred"))
           ),
           "sne" =     switch (on_off,
                               "FALSE" = sne_oisst_df %>%
                                 left_join(., sne_landings_yr, by = "Year") %>% 
                                 filter(Year >1981, Year < 2020) %>% 
                                 ggplot() + geom_line(aes(x = Year, y = Avg), size = 3, col = "darkblue") +
                                 geom_point(aes(x = Year, y = Landings/5+66), size = 3, color = "darkred") +
                                 scale_y_continuous(sec.axis = sec_axis(~(.-66)*5, breaks = c(0,10,20,30,40,50), 
                                                                        labels = c(0,10,20,30,40,50), name = "Landings (millions of pounds)")) +
                                 labs(y = "Sea Surface Temperature (deg F)") +
                                 theme(axis.title.y.left = element_text(color = "darkblue"),
                                       axis.ticks.y.left = element_line(color = "darkblue"),
                                       axis.text.y.left = element_text(color = "darkblue"),
                                       axis.line.y.left = element_line(color = "darkblue"),
                                       axis.title.y.right = element_text(color = "darkred"),
                                       axis.ticks.y.right = element_line(color = "darkred"),
                                       axis.text.y.right = element_text(color = "darkred"),
                                       axis.line.y.right = element_line(color = "darkred")),
                               
                               "TRUE" = sne_oisst_df %>% 
                                 left_join(., sne_landings_yr, by = "Year") %>% 
                                 right_join(cmipsst_sne, by = "Year") %>% filter(Year < 2051) %>%  
                                 ggplot() + geom_line(aes(Year, Mean), size = 3, col = "deepskyblue") +
                                 geom_ribbon(aes(x = Year, ymin = low, ymax = high), fill = "deepskyblue", alpha = .15) +
                                 labs(y = "Sea Surface Temperature (deg F)") + geom_line(aes(x = Year, y = Avg), size = 3, col = "darkblue") +
                                 geom_point(aes(x = Year, y = Landings/5+66), size = 3, color = "darkred") +
                                 scale_y_continuous(sec.axis = sec_axis(~(.-66)*5, breaks = c(0,10,20,30,40,50), 
                                                                        labels = c(0,10,20,30,40,50), name = "Landings (millions of pounds)")) +
                                 labs(y = "Sea Surface Temperature (deg F)") +
                                 theme(axis.title.y.left = element_text(color = "darkblue"),
                                       axis.ticks.y.left = element_line(color = "darkblue"),
                                       axis.text.y.left = element_text(color = "darkblue"),
                                       axis.line.y.left = element_line(color = "darkblue"),
                                       axis.title.y.right = element_text(color = "darkred"),
                                       axis.ticks.y.right = element_line(color = "darkred"),
                                       axis.text.y.right = element_text(color = "darkred"),
                                       axis.line.y.right = element_line(color = "darkred"))
           ))
    

  })
  
  
  ################################## 2019 multi year forecast #########
  output$multiyear <- renderPlot({
    plot_forecast <- multi_year %>% left_join(yr_zone_landings, by = c("Year", "Zone")) %>% 
      filter(`forecast year` == 2019, Zone == input$study_area) %>% 
      ggplot() + 
      geom_ribbon(aes(x = Year, ymin = `10th`, ymax = `90th`, fill = run), alpha = .5) +
      geom_ribbon(aes(x = Year, ymin = `25th`, ymax = `75th`, fill = run)) +
      geom_line(aes(Year, Median), color = "red", size = 2) + 
      geom_point(aes(Year, Landings), size = 4) + 
      geom_line(aes(Year, Landings), size = 2) + 
      theme(legend.position = c(.9,.9)) +
      labs(x = "Year", y = "Millions of Pounds") + 
      scale_fill_manual(values = c("hindcast" = "deepskyblue", "forecast" = "darkorange"), name = "",
                        guide = guide_legend(reverse = TRUE))
    
    plot_inset <- ggplot() +
      geom_sf(data = Lob_zone_sf_simple, aes(fill = ZONEID, geometry = geometry)) +
      theme(axis.line = element_blank(), 
            axis.text = element_blank(),
            axis.ticks = element_blank()) +
      gghighlight(ZONEID == input$study_area, label_key = ZONEID, unhighlighted_colour = "gray93", 
                  keep_scales = TRUE) +
      scale_fill_manual(values = c("#25356f",
                                   "#cb9d00",
                                   "#961e1d",
                                   "#b3875a",
                                   "#ffd380",
                                   "#76a1a0",
                                   "#97c8f0"), guide = "none")
    
    vals <- multi_year %>% left_join(yr_zone_landings, by = c("Year", "Zone")) %>% 
      filter(`forecast year` == 2019, Zone == input$study_area) %>% 
      pivot_longer(cols = c(-Year, -`forecast year`, -run, -Zone), names_to = "name", values_to = "value") %>% 
      dplyr::summarise(max_val = max(value, na.rm = TRUE),
                       min_val = min(value, na.rm = TRUE))
    
    plot_together <- plot_forecast + annotation_custom(ggplotGrob(plot_inset),
                                                       xmax = 2005,
                                                       ymin = vals$max_val - (vals$max_val - vals$min_val)*.32)
    
    return(plot_together)

  })
  
  ######################### ALSI_skill
  
  output$ALSI_skill <- renderPlot({
    
    on_off <- paste(input$ALSI_skill_select)
    
    switch(on_off,
           
           "FALSE" = multi_year %>% left_join(yr_zone_landings, by = c("Year", "Zone")) %>% 
             filter(`forecast year` == 2013, Year %in% c(1999:2013), Zone == "B") %>% 
             ggplot() + 
             geom_ribbon(aes(x = Year, ymin = `10th`, ymax = `90th`, fill = run), alpha = .5) +
             geom_ribbon(aes(x = Year, ymin = `25th`, ymax = `75th`, fill = run)) +
             geom_line(aes(Year, Median), color = "red", size = 2) + 
             geom_point(aes(Year, Landings), size = 4) + 
             geom_line(aes(Year, Landings), size = 2) +
             scale_x_continuous(limits = c(2000, 2019)) + 
             annotate("text", x = 2004, y = 13, label = "Observed Landings", size = 10) + 
             annotate("text", x =2009, y = 20, label = "Hindcast", color = "deepskyblue", size = 10) +
             scale_fill_manual(values = c("hindcast" = "deepskyblue", "forecast" = "darkorange")) +
             theme(legend.position = "none") +
             labs(x = "Year", y = "Landings (millions of pounds)") + 
             scale_y_continuous(limits = c(0,25)),
           
           "TRUE" = multi_year %>% left_join(yr_zone_landings, by = c("Year", "Zone")) %>% 
             filter(`forecast year` == 2013, Zone == "B") %>% 
             ggplot() + 
             geom_ribbon(aes(x = Year, ymin = `10th`, ymax = `90th`, fill = run), alpha = .5) +
             geom_ribbon(aes(x = Year, ymin = `25th`, ymax = `75th`, fill = run)) +
             geom_line(aes(Year, Median), color = "red", size = 2) + 
             geom_point(aes(Year, Landings), size = 4) + 
             geom_line(aes(Year, Landings), size = 2) +
             scale_x_continuous(limits = c(2000, 2019)) + 
             annotate("text", x = 2015, y = 12, label = "Skill \n Assessment", size = 10) + 
             annotate("text", x =2018, y = 24, label = "Forecast", color = "darkorange", size = 10) +
             scale_fill_manual(values = c("hindcast" = "deepskyblue", "forecast" = "darkorange")) +
             theme(legend.position = "none") +
             labs(x = "Year", y = "Landings (millions of pounds)") + 
             scale_y_continuous(limits = c(0,25))
           
           )
  })
  
  ############################## Forecast Year skill assessment
  
  output$multi_year_assessment <- renderPlot({
    multi_year %>% left_join(yr_zone_landings, by = c("Year", "Zone")) %>% 
      filter(`forecast year` == input$alsi_forecast_date, Zone != "G") %>% 
      ggplot() + 
      geom_ribbon(aes(x = Year, ymin = `10th`, ymax = `90th`, fill = run), alpha = .5) +
      geom_ribbon(aes(x = Year, ymin = `25th`, ymax = `75th`, fill = run)) +
      geom_line(aes(Year, Median), color = "red", size = 2) + 
      geom_point(aes(Year, Landings, alpha = run), size = 4) + 
      geom_line(aes(Year, Landings, alpha = run), size = 2) + 
      theme(legend.position = c(.08,.95),
            strip.background = element_blank(),
            strip.text = element_text(size = 14)) +
      labs(x = "Year", y = "Landings (millions of pounds)") + 
      scale_fill_manual(values = c("hindcast" = "deepskyblue", "forecast" = "darkorange"), 
                        name = "", guide = guide_legend(reverse = TRUE)) +
      scale_alpha_discrete(range = c(.5,1), guide = "none") +
      facet_wrap(~Zone, scales = "free_y", labeller = labeller(Zone = c("A" = "Zone A", "B" = "Zone B",
                                                                        "C" = "Zone C","D"= "Zone D",
                                                                        "E" = "Zone E", "F" = "Zone F"))) +
      scale_x_continuous(limits = c(2000,2025))
  })

  
}


shinyApp(ui, server)
