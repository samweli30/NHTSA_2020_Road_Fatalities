# Initialise packages ###
library(dplyr)
library(rgeos)
library(maptools)
library(ggmap)
library(broom)
library(leaflet)
library(readr)
library(rgdal)
library(sf)
library(shiny)
library(htmlwidgets)
library(htmltools)
library(plotly)
library(shinyWidgets)
library(rsconnect)

# Initialize the data ###
crash <- read_csv("crashdata_2020.csv")

# Prepare the inputs ###
state_list <- unique(crash$State)
rural_urban <- unique(crash$`Rural/Urban (group)`)
weather_list <- unique(crash$Weather)
belt_list <- unique(crash$`Restraint Use (group)`)

# Define UI for application ###
shinyUI(fluidPage(
  style="color:black",
  
  titlePanel(h1("Uneven toll from US crashes in 2020",align="center")),
  
  sidebar_content <-sidebarPanel(
    shinyjs::useShinyjs(),
    id="side-panel",
    multiInput("stat_var",label="State",
               choices=c("All",state_list),
               selected = "All"),
    p(HTML("ℹ️ <em> Selected States are in right panel, default selection is <u>All</u>. Add State(s) by clicking them from the left panel and remove <u>All</u> selection by clicking it from right panel to view individual/multiple states</em>"),style="color:#000033",align="center"),
    selectInput("rural_var",label="Rural/Urban",
                choices=c("All",rural_urban),
                selected = "All",
                multiple = F),
    br(),
    selectInput("weather_var",label="Weather",
                choices=c("All",weather_list),
                selected = "All",
                multiple = F),
    br(),
    selectInput("belt_var",label="Seatbelts",
                choices=c("All",belt_list),
                selected = "All",
                multiple = F),
    br(),
    sliderInput("spd_lim", "Speed Limit",
                min = 0, max = 100,
                value = c(5,95), post="mph"),
    br(),
    sliderInput("rural_perc", "State Based Rural Population %",
                min = 0, max = 100,
                value = c(0,90), post="%"),
    br(),
    actionButton("reset_input", "Reset inputs"),
    width=3),
  
  main_content <- mainPanel(
    tabsetPanel(id="selected_tab",
      tabPanel("Overview",
               h3("Overview", align="center"),
               p(HTML("Road crashes are one of the leading causes of death in the US, this application aims to educate and show that not all US states are affected equally when it comes to road tolls. This application will enable the user to explore individual states as well as gain an understanding on some of the leading causes of Road Crash related deaths. In the US <strong>NHTSA (National Highway Traffic Safety Administration)</strong> has different branches that analyse crash data to inform legal requirements and policy that influence vehicle and infastructure design. The visualisation below contextualises the US crash statistics based on <strong>2020 Fatality Analysis Reporting System (FARS) data</strong>.")),
               br(),
               plotOutput("Plot2",width="100%", height="100"),
               br(),
               p(HTML("💡 <em> <strong>Tip</strong>: Select <u>Clear</u> from the <strong>Weather</strong> dropdown on the sidebar and see how the last 3 boxes change, the elements change based on user input.</em>"),style="color:#000033",align="center"),
               br(),
               p(HTML("In 2020, the national US population was 331.4 million and there were <strong>38353</strong> fatalities <em>(116 deaths per million population)</em> from vehicle crashes. There were more fatalities nationally in Rural Roads than in Urban Roads despite the rural population being <em>~13%</em> of the national US population; this is a significant discrepancy especially when compared to the national fatalities per capita. Contrary to expectation most fatal crashes <em>(~69%)</em> of National Fatalities happen in <em>Clear</em> weather suggesting that crash related deaths are potentially driven more by human/other factors than weather conditions.")),
               br(),
               plotOutput("Plot5",width="100%", height="100"),
               br(),
               p(HTML("➡️ <em> <strong>Action</strong>: Change the <strong>Speed Limit</strong> slide to have 5-50mph and compare the reactive visualisation when the speed limit range is 50-95mph. Repeat the Speed Limit study with the <strong>Seatbelt</strong> selection to <strong>No</strong>; does the trend stay the same? Repeat the Speed Limit study with the <strong>State Based Rural Population %</strong> slider to 50+ and <50; compare the unbelted percentage. </em>"),style="color:#000033",align="center"),
               br(),
               p(HTML("Road related fatalities are generally related with higher speed limits nationally as <em>56%</em> of crash related fatalities are on roads with speed limits between 50-95mph. <strong>Twice</strong> as many rural fatalities happen on Rural Roads when compared to Urban Roads in the 50-95mph speed limit range and the rural fatalities per rural population <strong>triples</strong> in the 50-95mph speed limit range when compared to the 5-50mph range. Seatbelt legislation varies significantly across different US states, the unbelted % is slightly higher with states with higher rural populations but the spatial map in the next tab is the best tool to understand the differences accross the US.")),
               br(),
               p(HTML("💡 <em> <strong>Tip</strong>: Click <strong>Reset Inputs</strong> button at any point to reset the application to default inputs </em>"),style="color:#000033",align="center"),
               p(HTML("ℹ️ <em> <strong>NHTSA Speed Limit</strong> data is coded to have <u>Unknown and Not Reported</u> speed limits at 98mph & 99mph (see the Speed Limit Slider Input); roads/driveways with <u>No Statutory Limit</u> are coded as 0mph. Select the full range of Speed Limits on the slider input to see total National fatalities in the US in 2020.</em>"),style="color:#000033",align="center"),
               br()),
      tabPanel("Fatalities by State",
               h3("Spatial Analysis of US Fatalities",align="center"),
               p("The visualisation below shows the different human contributions to vehicle crashes with Seatbelt use being the highest factor in crash fatalities nationally."),
               br(),
               plotOutput("Plot1",width="100%", height="100"),
               br(),
               p(HTML("💡 <em><strong>Tip</strong>: The <strong>Chloropleth Map</strong> below is interactive, to get specific details from each state hover on the spatial map.</em>"),style="color:#000033",align="center"),
               br(),
               p(HTML("➡️ <em><strong>Action</strong>: Select <strong>Texas</strong> and add another state to see the changes in the visualisations. What trends are observed when viewing <strong>Rural vs Urban</strong> Deaths or different <strong>Weather</strong> conditions? Change the map view to see what view provides the best insights.</em>"),style="color:#000033",align="center"),
               fluidRow(align="center",
                        radioButtons(
                        "map_sel","Select Map View",
                        inline=TRUE,
                        choiceNames = c("State Fatalities","State Fatilities per 1 Million People"),
                        choiceValues = c("sum_fatalities","sum_fatalities_1m"),
                        selected = "sum_fatalities_1m")),
               leafletOutput("map",width="100%",height = "400"),
               br(),
               p("States with higher Rural Population have higher crash fatalities per capita; the general trend shows that Rural Roads lead to a disproportoinate amount of crash related fatalities. Viewing deaths per capita enables easier analysis and demonstrates the differences between Rural and Urban deaths in terms of where they occur. Picking different states is informative in seeing whether there is a particular difference between the factors that influence crash related deaths."),
               br(),
               p(HTML("💡 <em><strong>Tip</strong>: Go to the <strong>Overview Tab</strong> to view more information especially when reviewing particular states.</em>"),style="color:#000033",align="center")
               ),
               #br(),
      tabPanel("Highway Spending & Infastructure",
               h3("Rural vs Urban State Highway Spending & Infastructure",align="center"),
               p(HTML("There are a myriad of potential reasons why there is a higher fatality rate on Rural roads compared to Urban roads; on this application the State Highway Funding is shown to inversly correlate with the State Rural Population %. Most Rural Crashes also happen on 2 lane roads when compared to Urban Crashes; the assumption is that more spending on highways would correlate with lower crash fatalities per capita.")),
               br(),
               p(HTML("💡 <em> <strong>Tip</strong>: Hover on the <strong>Scatter Plot</strong> to view more information especially when reviewing particular states, the information is reactive on user selection. </em>"),style="color:#000033",align="center"),
               #br(),
               p(HTML("➡️ <em> <strong>Action</strong>: Compare Rural and Urban by selecting each option on the <strong>Rural/Urban</strong> dropdown. Where do most fatal crashes happen and how does the chart on the left change? Does the trend change when the <strong>Seatbelt</strong> dropdown is set to Yes? Where do most crashes happen when the <strong>Speed Limit</strong> is set to 50-95mph? </em>"),style="color:#000033",align="center"),
               br(),
               fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("Plot3"), plotOutput("Plot4"))),
               br(),
               p(HTML("Lower funded states are typically the states with a higher proportion of Rural population; most fatalities happen on two lane roads in Rural Roads with higher speed limits even when occupants are using seatbelts. Increasing state highway spending on Rural Roads or reducing speed limits on Rural Roads may reduce overall fatalities on Rural Roads.")),
               br(),
               p(HTML("💡 <em><strong>Tip</strong>: Toggle the <strong>Overview & Fatalities by State</strong> tabs to view more information especially when exploring multiple factors.</em>"),style="color:#000033",align="center"),
               #br(),
               h3("Data Sources", align="center"),
               p("‘Fatality Analysis Reporting System | NHTSA’, viewed 23 October 2022, <https://www.nhtsa.gov/crash-data-systems/fatality-analysis-reporting-system>."),
               p("Boesen, U 2021, ‘How Are Your State’s Roads Funded?’, Tax Foundation, viewed 23 October 2022, <https://taxfoundation.org/state-infrastructure-spending/>."),
               p("‘List of States By Population, Based on 2020 US Census’ List of States by Population, viewed 23 October 2022, <https://state.1keydata.com/state-population.php>."),
               p("Bureau, UC ‘The District of Columbia Gained More Than 87,000 People in 10 years’, Census.gov, viewed 23 October 2022, <https://www.census.gov/library/stories/state-by-state/district-of-columbia-population-change-between-census-decade.html>."),
               br()
               ),
    selected = "Overview")
  )
  
))
