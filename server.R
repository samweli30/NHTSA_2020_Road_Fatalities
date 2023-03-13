# Load All the necessary packages ##### 
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
library(sp)
library(htmlwidgets)
library(htmltools)
library(plotly)
library(shinyWidgets)
library(rsconnect)

# Data loading and simplifying GeoSpatial ####
crash <- read_csv("crashdata_2020.csv")

##Geo-spatial map
states <- readShapeSpatial("cb_2018_us_state_500k.shp",delete_null_obj=TRUE)
states_simp <- gSimplify(states, tol = .001, topologyPreserve=TRUE)
states_simp <- SpatialPolygonsDataFrame(states_simp,data=states@data)
crash$NAME <- crash$State

national_fatalities <- sum(crash$Fatalities)

# Server Code Section ######
shinyServer(function(input, output) {
## Reactive filter elements #####-------------------------------------
  crash_state <- reactive({
    if(input$stat_var == "All") {
      crash
    }else{
      crash %>%
        filter(State== input$stat_var)
    }
  })
  
  crash_rural <- reactive({
    if(input$rural_var == "All") {
      crash_state()
    }else{
      crash_state() %>%
        filter(`Rural/Urban (group)`== input$rural_var)
    }
  })

  crash_weat <- reactive({
    if(input$weather_var == "All") {
      crash_rural()
    }else{
      crash_rural() %>%
        filter(Weather== input$weather_var)
    }
  })
  
  crash_belt <- reactive({
    if(input$belt_var == "All") {
      crash_weat()
    }else{
      crash_weat() %>%
        filter(`Restraint Use (group)`== input$belt_var)
    }
  })
  
  crash_spd_lim <- reactive({
      crash_belt()[crash_belt()["Speed Limit"]>= input$spd_lim[1] & crash_belt()["Speed Limit"]<= input$spd_lim[2], ]
  })
  
  crash_filter <- reactive({
    req(input$stat_var)
    req(input$rural_perc)
    req(input$spd_lim)
    crash_spd_lim()[crash_spd_lim()["Rural Population %"]>= input$rural_perc[1] & crash_spd_lim()["Rural Population %"]<= input$rural_perc[2], ]
  })
  
  crash_plotly <- reactive({
    crash_filter() %>%
      group_by(State) %>%
      mutate(sum_fatalities= sum(Fatalities)) %>% mutate(sum_fatalities_1m = sum(`Fatalities (per million)`)) %>%
      dplyr::select(State, NAME, Fatalities, sum_fatalities,sum_fatalities_1m,
                    `Rural Population %`,`Highway Spending (Millions)`) %>%
      unique() %>% ungroup()
  })
  
  df_merge <- reactive({
    req(input$stat_var)
    req(input$rural_perc)
    req(input$spd_lim)
    crash_filter() %>%
      group_by(State) %>%
      mutate(sum_fatalities= sum(Fatalities)) %>% mutate(sum_fatalities_1m = sum(`Fatalities (per million)`)) %>%
      dplyr::select(State, NAME, sum_fatalities,sum_fatalities_1m,`Rural Population %`) %>%
      unique() %>% ungroup() %>%
      sp::merge(states_simp,., by="NAME")
  })
  
  map_selection <- reactive({
    input$map_sel
  })
  
  colorpal <- reactive({
    colorNumeric("PuRd",df_merge()[[map_selection()]],na.color = NA)
  })

  # Plot 1: KPI Summary Reactive ####  
  output$Plot1 <- renderPlot({
      kpi1 <- paste("Total","Fatalities",sum(crash_filter()$Fatalities),sep="\n")
      kpi2 <- paste("% of National","Fatalities",round(sum(crash_filter()$Fatalities)/(national_fatalities*0.01),digits=1),sep="\n")
      kpi3 <- paste("Drug","Related %",round(sum(crash_filter()[which(crash_filter()$`Drugs Involved`=="Yes"),"Fatalities"])/(0.01*sum(crash_filter()$Fatalities)),digits=1),sep="\n")
      kpi4 <- paste("Alcohol","Related %",round(sum(crash_filter()[which(crash_filter()$`Driver Drinking`=="Yes"),"Fatalities"])/(0.01*sum(crash_filter()$Fatalities)),digits=1),sep="\n")
      kpi5 <- paste("Unbelted %",round(sum(crash_filter()[which(crash_filter()$`Restraint Use (group)`=="No"),"Fatalities"])/(0.01*sum(crash_filter()$Fatalities)),digits=1),sep="\n")
      kpi6 <- paste("Speeding","Related %",round(sum(crash_filter()[which(crash_filter()$`Speeding Related (group)`=="Yes"),"Fatalities"])/(0.01*sum(crash_filter()$Fatalities)),digits=1),sep="\n")
      
      rects <- data.frame(x = 1:6,colors = c("white", "white", "white", "white","white","white"),
                          text = c(kpi1,kpi2,kpi3,kpi4,kpi5,kpi6))
      
      ggplot(rects, aes(x, y = 0, fill = colors, label = text)) +
        geom_tile(width = 0.9, height = 0.5, color = "black",size=1) + geom_text(color = "black",fontface = "bold",size=5) + 
        scale_fill_identity(guide = "none") +   coord_fixed() + theme_void()
    })
  
  output$Plot2 <- renderPlot({
    kpi7 <- paste("National Population","Millions",round(sum(unique(crash$Population))*10^-6,digits = 1),sep="\n")
    kpi8 <- paste("National Rural","Population Millions",round(sum(unique(crash$`Rural Population (Est)`))*10^-6,digits = 1),sep="\n")
    kpi9 <- paste("National","Fatalities",sum(crash$Fatalities),sep="\n")
    kpi10 <- paste("Total Selected","Fatalities",sum(crash_filter()$Fatalities),sep="\n")
    kpi11 <- paste("Rural","Deaths",round(sum(crash_filter()[which(crash_filter()$`Rural/Urban (group)`=="Rural"),"Fatalities"]),digits=1),sep="\n")
    kpi12 <- paste("Urban", "Deaths",round(sum(crash_filter()[which(crash_filter()$`Rural/Urban (group)`=="Urban"),"Fatalities"]),digits=1),sep="\n")
    
    rects1 <- data.frame(x = 1:6,colors = c("white", "white", "white", "white","white","white"),
                        text1 = c(kpi7,kpi8,kpi9,kpi10,kpi11,kpi12))
    
    ggplot(rects1, aes(x, y = 0, fill = colors, label = text1)) +
      geom_tile(width = 0.9, height = 0.5, color = "black",size=1) + geom_text(color = "black",fontface = "bold",size=5) + 
      scale_fill_identity(guide = "none") +   coord_fixed() + theme_void()
  })
  
  output$Plot5 <- renderPlot({
    kpi13 <- paste("% of National","Fatalities",round(sum(crash_filter()$Fatalities)/(national_fatalities*0.01),digits=1),sep="\n")
    kpi14 <- paste("States Selected",n_distinct(crash_filter()$State),sep="\n")
    kpi15 <- paste("Total Fatalities","Per Million",round(sum(crash_filter()$Fatalities)/(10^-6*sum(unique(crash_filter()$Population))),digits=1),sep="\n")
    kpi16 <- paste("Rural Deaths","Per Million Rural",round(sum(crash_filter()[which(crash_filter()$`Rural/Urban (group)`=="Rural"),"Fatalities"])/(10^-6*sum(unique(crash_filter()$`Rural Population (Est)`))),digits=1),sep="\n")
    kpi17 <- paste("Urban Deaths","Per Million Urban",round(sum(crash_filter()[which(crash_filter()$`Rural/Urban (group)`=="Urban"),"Fatalities"])/(10^-6*(sum(unique(crash_filter()$Population))-sum(unique(crash_filter()$`Rural Population (Est)`)))),digits=1),sep="\n")
    kpi18 <- paste("Unbelted %",round(sum(crash_filter()[which(crash_filter()$`Restraint Use (group)`=="No"),"Fatalities"])/(0.01*sum(crash_filter()$Fatalities)),digits=1),sep="\n")
    
    rects2 <- data.frame(x = 1:6,colors = c("white", "white", "white", "white","white","white"),
                         text1 = c(kpi13,kpi14,kpi15,kpi16,kpi17,kpi18))
    
    ggplot(rects2, aes(x, y = 0, fill = colors, label = text1)) +
      geom_tile(width = 0.9, height = 0.5, color = "black",size=1) + geom_text(color = "black",fontface = "bold",size=5) + 
      scale_fill_identity(guide = "none") +   coord_fixed() + theme_void()
  })
  
  output$Plot3 <- renderPlotly({
    plot_ly(crash_plotly(), x=~`Highway Spending (Millions)`,y=~`Rural Population %`,
            text= ~paste("State: ",State,'<br>Fatalities:',round(sum_fatalities,digits=1),'<br>Fatalities/Million:',round(sum_fatalities_1m,digits = 1)),
            size= ~sum_fatalities_1m, type='scatter',mode ='markers',
            colors = "PuRd",
            marker=list(color=~sum_fatalities_1m,
                        line=list(width=0),
                        showscale = TRUE,
                        colorscale=list(c(0, 1), c("#E7E1EF", "#DD1C77")),
                        colorbar=list(x=0.9,y=0.9,len=0.5,title=~paste('Fatalities', '<br>per Million'))))%>%
      layout(#title="Highway Spending and Rural Populations",
             font=list(family="Helvetica Neue",size=12),
             yaxis=list(title="State Rural Population %"),
             xaxis=list(title="State Highway Spending US$ Millions")) 
  })
  
  output$Plot4 <- renderPlot({
    level_order <- c("One lane","Two lanes","Three lanes","Four lanes","Five lanes",
                     "Six lanes","Seven or more lanes","Non-Trafficway or Driveway Access",
                     "Not Reported","Reported as Unknown")
    
    ggplot(crash_filter(), aes(x=factor(`Total Lanes in Road`,level=level_order),y=Fatalities, fill=`Rural/Urban (group)`)) +
      geom_col() + coord_flip() + theme_minimal() +
      scale_y_continuous() +
      labs(y="Total Fatalities",x="Total Lanes in Road", fill="Rural/Urban") +
      theme(legend.position = c(0.9,0.8),
            legend.key.size = unit(1.0, "cm"),
            axis.text = element_text(family="Helvetica Neue",size=12),
            axis.title = element_text(family="Helvetica Neue",size=14),
            legend.text = element_text(family="Helvetica Neue",size=12),
            legend.title = element_text(family="Helvetica Neue",size=12)) +
      scale_fill_manual(values=c("#DF65B0","#DD1C77","#980043"))
  })
  
  #Plot 2: Map Object
  output$map <- renderLeaflet({
    leaflet(states_simp) %>%
      setView(lng = -100, lat = 41.5, zoom = 4) %>%
    addTiles()
  })
  observe({
    req(input$selected_tab == "Fatalities by State")
    pal <- colorpal()
    leafletProxy("map",data=df_merge()) %>%
      addPolygons(
        fillColor = ~pal(df_merge()[[map_selection()]]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = sprintf(
          "<strong>%s</strong><br/>Fatalities: %s </strong><br/>Fatalities/Million: %s",
          df_merge()$NAME,
          round((df_merge()$sum_fatalities),digits=1),
          round((df_merge()$sum_fatalities_1m),digits=1)) %>%  
          lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight"="normal",padding="3px 8px"),
          textsize = "15px",
          direction = "auto")
        ) 
  })
  observe({
    req(input$selected_tab == "Fatalities by State")
    pal <- colorpal()
    proxy <- leafletProxy("map",data=df_merge())
    proxy %>% clearControls()
    proxy %>% addLegend(pal = pal,
                        values=df_merge()[[map_selection()]],
                        title="Crash Fatalities",
                        position = "bottomright",
                        na.label = "")
  })
  
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
  })

  
  })
  

  
 
