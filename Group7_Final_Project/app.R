#Loading packages and data
library(tidyverse)
library(dbplyr)
library(ggplot2)
library(sf)
library(ggmap)
library(leaflet)
library(reshape2)
library(shiny)
library(shinythemes)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(readr)

#Load Data
census <- read_sf("2010_CensusData/2010_CensusData.shp")
abandoned <- st_read("Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp", stringsAsFactors = FALSE) 

# set up
census_area <- c("All","1","2","3.01","3.02","4","5","6","7","8","9",
                 "10","11","12","13","14","15","16","17","19",
                 "20","21","22","23","24","25","26","27","28","29",
                 "30","31","32","33","34","35",
                 "101","102","103","104","105","106","107","108","109",
                 "110","111","112.01","112.02",
                 "113.01","113.02","113.03","113.04","113.05","113.06",
                 "114.03","114.05","114.06",
                 "115.01","115.03","115.04","115.05","115.06",
                 "116.01","116.02","117.01","117.02","118.01","118.02",
                 "119","120","121","122","123","124")

police.points <- read.csv("PoliceForce/Police_Use_of_Force_Incidents.csv")

#Change the column name
colnames(police.points)[1] <- "X"
police.points <- police.points[complete.cases(police.points[ , 1:2]),]

#Create a column for the call counts
police.points<- police.points %>%
    dplyr::mutate(calls = 1)

#Make data spatial
police.spatial <- police.points %>% #projecting the table as an sf and setting the coordinate system
    st_as_sf(coords = c("X","Y")) %>% 
    st_set_crs(value = 4326) #code for coordinate system in EPSG

#Align the Census data
census.aligned <- census %>% st_transform(crs = 4326)

#Join the two datasets
census_police_agg <- st_join(x = census.aligned, y = police.spatial)

#Get call counts by Census Zone
calls_agg <- aggregate(calls ~ NAME, data = census_police_agg, FUN = sum)
census_calls <- left_join(census.aligned, calls_agg)

pal <- colorNumeric(
    palette = "YlGnBu",
    domain = census_calls$calls,
    na.color="transparent")

#Create a column for the call counts
abandoned<- abandoned %>%
    dplyr::mutate(houses = 1, short_code_enfor = substr(Code_Enfor,1,15))

#Join the two datasets
census_abandoned_agg <- st_join(x = census.aligned, y = abandoned)

#Get call counts by Census Zone
calls_agg <- aggregate(houses ~ Outcome_St, data = census_abandoned_agg, FUN = sum)
census_abandon <- dplyr::left_join(calls_agg, abandoned, by=c("Outcome_St"))
census_abandon_final <- dplyr::left_join(census_abandoned_agg,census_abandon, 
                                         by=c("Outcome_St", "Direction", "Street_Nam", "Suffix", "Code_Enfor", "Property_S", "Program_De", "Date_of_Ou", "Council_Di", "Zip_Code", "ShapeSTAre", "ShapeSTLen", "OBJECTID", "County_Tax", "State_Parc", "Address_Nu", "Structures")
)

#TODO:
#pal <- colorNumeric( palette = "YlGnBu", domain = census_abandon_final$houses.y, na.color="transparent") 
#color brewer: Bars = Set2 

outcome <- factor(unique(census_abandon$Outcome_St))
structure <- factor(unique(census_abandon$Structures))
code_enfor <- factor(unique(census_abandon$Code_Enfor))

age_axis <- c("Under 5", "5-9", "10-14", "15-17", "18-24", "25-34", 
              "35-44", "45-54", "55-64", "65-74", "75-84", "Over 85")
race_axis <- c("White", "Black", "American Indian", "Asian", "Native Hawaiian", "Other", 
               "2+ Races")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = "litera.css",
                
                
                # Application title
                titlePanel("Rebuilding our neighborhoods block by block"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                    sidebarPanel(
                        selectInput(inputId = "area",
                                    
                                    label = "Choose Area",
                                    choices = census_area,
                                    selected = "All"),
                        width=2,
                        fluid=FALSE
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel(title = "About",
                                             titlePanel("Welcome!"),
                                             includeMarkdown("about.md"),
                                             fixedRow(
                                                 column(width = 3,
                                                        align = "center",
                                                        style = "background-color: #c8e6c9;",
                                                        titlePanel(icon("phone","fa-2x")),
                                                        htmlOutput("phone")
                                                 ),
                                                 column(width = 3, offset = 1,
                                                        align = "center",
                                                        style = "background-color: #fff9c4;",
                                                        titlePanel(icon("city","fa-2x")),
                                                        htmlOutput("abandon")
                                                 ),
                                                 column(width = 3, offset = 1,
                                                        align = "center",
                                                        style = "background-color: #bbdefb;",
                                                        titlePanel(icon("users","fa-2x")),
                                                        htmlOutput("cen")
                                                 ))
                                    ),
                                    tabPanel(title = "Police Response Activity",
                                             leafletOutput(outputId = "areaPlot"),
                                             fluidRow(
                                                 column(12,
                                                        align = "center",
                                                        htmlOutput("text")
                                                 )),
                                             fixedRow(
                                                 column(5,
                                                        plotOutput(outputId ="plot1")
                                                 ),
                                                 column(7,
                                                        plotOutput(outputId ="plot2"),
                                                        theme = "litera.css"
                                                 ))
                                    ),
                                    tabPanel(title = "Abandon Properties",
                                             leafletOutput(outputId = "mymap"),
                                             fluidRow(
                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1a"), plotOutput("plot3"))
                                             )
                                    ),
                                    tabPanel(title = "Demographics",
                                             #titlePanel("Demographics"),)
                                             fluidRow(
                                                 leafletOutput(outputId = "mapPlot"),
                                                 column(2,
                                                        radioButtons(inputId = 'demo',
                                                                     label = 'Choose Demographic',
                                                                     choices = c("Age", "Race"))
                                                 ),
                                                 column(10, 
                                                        plotOutput(outputId = "demoPlot")
                                                 ))
                                    )
                        )
                        
                        
                    )
                    
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # POLICE FORCE
    
    dat <-reactive({
        if(input$area == "All"){
            rtrn <- census_calls
            
        }else{
            
            rtrn <- census_calls %>% 
                dplyr::filter(census_calls$NAME == input$area)
        }
        return(rtrn)
    })
    
    gbdat <-reactive({
        if(input$area == "All"){
            rtrn <- census_police_agg %>%
                st_set_geometry(NULL) %>%
                group_by(Type_of_Resistance) %>%
                tally  %>% 
                na.omit()%>%
                arrange(desc(n))%>%
                top_n(10,n)
            
            
            
        }else{
            
            rtrn <- census_police_agg %>% 
                st_set_geometry(NULL) %>%
                dplyr::filter(NAME == input$area) %>%
                group_by(Type_of_Resistance) %>%
                tally  %>% 
                na.omit()%>%
                arrange(desc(n))%>%
                top_n(10,n)
        }
        return(rtrn)
    })
    
    gpdat <-reactive({
        if(input$area == "All"){
            rtrn <- census_police_agg %>%
                st_set_geometry(NULL) %>%
                group_by(Call_in_Response_To) %>%
                tally %>% 
                na.omit()%>%
                arrange(desc(n))%>%
                top_n(10, n)
            
        }else{
            
            rtrn <- census_police_agg %>% 
                st_set_geometry(NULL) %>%
                dplyr::filter(NAME == input$area) %>%
                group_by(Call_in_Response_To) %>%
                tally  %>% 
                na.omit()%>%
                arrange(desc(n))%>%
                top_n(10, n)
        }
        return(rtrn)
    })
    
    output$areaPlot <- renderLeaflet({
        
        mytext <- paste(
            "Area: ", dat()$NAME, "<br/>", 
            "<strong>Calls: ", dat()$calls,"</strong>",
            sep="")%>%
            lapply(htmltools::HTML)
        
        leaflet(dat())  %>%
            addTiles(group = "Basic")  %>%
            addPolygons(data = dat(),
                        fillColor = ~pal(dat()$calls), 
                        fillOpacity = 0.8, 
                        color = "blue", 
                        weight = 1,
                        label = mytext)%>%
            addLegend(position = "bottomleft",pal = pal, values = ~dat()$calls, title = "<strong>Calls</strong><br>2017-2019") 
        
        
        
    })
    
    output$plot1 <- renderPlot({
        
        theme_set(theme_classic())
        
        ggplot(gbdat(), aes(x=reorder(Type_of_Resistance,-n), y=n, fill=Type_of_Resistance)) + 
            geom_bar(aposition="dodge", stat="identity", width = 0.5)  +
            #scale_fill_viridis(discrete = T, option = "E") +
            
            theme(axis.text.x = element_text(angle = 60,hjust=1),legend.position = "none") + 
            labs(title="Resistance to Police Response", 
                 subtitle="Type of Resistance")+
            ylab("")+
            xlab("")+
            scale_fill_brewer(palette="Set3")+
            theme(plot.title=element_text(size=15, 
                                          face="bold", 
                                          family="Helvetica",
                                          
                                          hjust=0.5,
                                          lineheight=1.2),  # title
                  plot.subtitle=element_text(size=12, 
                                             family="Helvetica",
                                             face="italic",
                                             hjust=0.5),  # subtitle
                  plot.caption=element_text(size=10,
                                            
                                            family="Helvetica"),
                  axis.text.x=element_text(size=10,
                                           face="bold"))
        
        
    })
    
    output$plot2 <- renderPlot({
        
        
        
        ggplot(gpdat(), aes(x=reorder(Call_in_Response_To,-n), y=n, fill=Call_in_Response_To)) + 
            geom_bar(position="dodge", stat="identity")  +
            #scale_fill_viridis(discrete = T, option = "E") +
            
            theme(axis.text.x = element_text(angle = 60, hjust=1),legend.position = "none")+
            labs(title="Police Response Reason", 
                 subtitle="Reason for call", 
                 caption="Source: South Bend Open Data Portal 'Police Use of Force'")+
            ylab("")+
            xlab("")+
            scale_fill_brewer(palette="Set2") +
            theme(plot.title=element_text(size=15, 
                                          face="bold", 
                                          family="Helvetica",
                                          
                                          hjust=0.5,
                                          lineheight=1.2),  # title
                  plot.subtitle=element_text(size=12, 
                                             family="Helvetica",
                                             face="italic",
                                             hjust=0.5),  # subtitle
                  plot.caption=element_text(size=10,
                                            
                                            family="Helvetica"),
                  axis.text.x=element_text(size=10,
                                           face="bold"))   
        
    })
    
    output$text <- renderText({
        paste0("<br><div class='jumbotron' style='
                padding: 10px'>
                <h4><strong>Police Force Characteristics for Area:</strong>
                <p>", input$area, "</p></h4>")
    })
    output$phone <- renderText({
        # paste0("<div class='card text-wite bg-primary mb-3'>
        #         <div class='card-header'>Calls to Police</div>
        #         <div class'card-body'>
        #         <h4 class='card-title'>Total Calls</h4>
        #         <p class='card-text'>203 </p>
        #         </div>
        #         </div>")
        paste0('
        <div class="card text-white green lighten-4" style="padding: 5px">
        <div class="card-body">
        <h4 class="card-title"><strong><center><u>Calls to Police</u></center></strong></h4>
        <h5 class="card-text" ><strong><center>Total Calls</center></strong></h5>
        <p class="card-text" ><center>203</center></p> 
        <h5 class="card-text" ><strong><center>Most Calls for</center></strong></h5>
        <p class="card-text" ><center>Domestic: 26</center></p>
        <h5 class="card-text" ><strong><center>Freq. Resist Type</center></strong></h5>
        <p class="card-text" ><center>Physical: 187</center></p>
              </div> </div>')
    })
    
    output$cen <- renderText({
        
        paste0('
        <div class="card text-white blue lighten-4" style="padding: 5px">
        <div class="card-body">
        <h4 class="card-title"><strong><center><u>Census</u></center></strong></h4>
        <h5 class="card-text" ><strong><center>Population</center></strong></h6>
        <p class="card-text" ><center>266,931</center></p> 
        <h5 class="card-text" ><strong><center>Area</center></strong></h6>
        <p class="card-text" ><center>461.39 sq. miles</center></p>
        <h5 class="card-text" ><strong><center>Census Tracts</center></strong></h6>
        <p class="card-text" ><center>75</center></p>
              </div> </div>')
    })
    
    output$abandon <- renderText({
        
        paste0('
        <div class="card text-white yellow lighten-4" style="padding: 5px">
        <div class="card-body">
        <h4 class="card-title"><strong><center><u>Abandon</u></center></strong></h4>
        <h5 class="card-text" ><strong><center>Properties</center></strong></h5>
        <p class="card-text" ><center>1,511</center></p> 
        <h5 class="card-text" ><strong><center>Zip Codes</center></strong></h5>
        <p class="card-text" ><center>13</center></p>
        <h5 class="card-text" ><strong><center>Time Frame</center></strong></h5>
        <p class="card-text" ><center>2013-2016</center></p>
              </div> </div>')
    })
    
    
    # ABANDON
    # **Total Population:**
    #     
    #     266,931
    # 
    # **Total Area:**
    #     
    #     461.3855 sq. miles
    # 
    # **Number of Census Tracts:**
    #     
    #     75
    
    mapDat <-reactive({
        if(input$area == "All"){
            rtrn <- census
        }else{
            rtrn <- census %>% 
                dplyr::filter(NAME == input$area)
        }
        return(rtrn)
    })
    
    histDat1 <-reactive({
        if(input$area == "All"){
            #rtrn <- census_abandon_final
            
            rtrn <- census_abandon_final %>%
                st_set_geometry(NULL) %>%
                group_by(Outcome_St) %>%
                tally %>% 
                na.omit()%>%
                arrange(desc(n))%>%
                top_n(10, n)
        }else{
            #rtrn <- census_abandon_final %>% 
            #   dplyr::filter(NAME == input$area)
            
            rtrn <- census_abandon_final %>%
                dplyr::filter(NAME == input$area) %>%
                st_set_geometry(NULL) %>%
                group_by(Outcome_St) %>%
                tally %>% 
                na.omit()%>%
                arrange(desc(n))%>%
                top_n(10, n)
        }
        return(rtrn)
    })
    
    histDat3 <-reactive({
        if(input$area == "All"){
            #rtrn <- census_abandon_final
            
            rtrn <- census_abandon_final %>%
                st_set_geometry(NULL) %>%
                group_by(short_code_enfor.y) %>%
                tally %>% 
                na.omit()%>%
                arrange(desc(n))%>%
                top_n(10, n)
        }else{
            #rtrn <- census_abandon_final %>% 
            #   dplyr::filter(NAME == input$area)
            
            rtrn <- census_abandon_final %>%
                dplyr::filter(NAME == input$area) %>%
                st_set_geometry(NULL) %>%
                group_by(short_code_enfor.y) %>%
                tally %>% 
                na.omit()%>%
                arrange(desc(n))%>%
                top_n(10, n)
        }
        return(rtrn)
    })
    
    
    output$mymap <- renderLeaflet({
        
        leaflet()  %>%
            addTiles(group = "Basic")  %>%
            setView( -86.2520, 41.6764, zoom = 11) %>% 
            addPolygons(data = mapDat(), color = "orange", fill = "light grey") %>%
            addPolygons(data=abandoned, fillOpacity = 1,weight = 2)
        
    })
    
    output$plot1a <- renderPlot({
        
        #barplot(prop.table(table(histDat()$Outcome_St)),col="#69b3a2" ,
        #        horiz=T, las=1
        #)
        ggplot(histDat1(), aes(x=reorder(Outcome_St,-n), y=n, fill=Outcome_St)) + 
            geom_bar(aposition="dodge", stat="identity", width = 0.5)  +
            theme(axis.text.x = element_text(angle = 60,hjust=1),legend.position = "none") + 
            labs(title="Outcome", 
                 subtitle="Type of Abandoned Property")+
            ylab("")+
            xlab("")+
            scale_fill_brewer(palette="Set3")+
            theme(plot.title=element_text(size=15, 
                                          face="bold", 
                                          family="Helvetica",
                                          
                                          hjust=0.5,
                                          lineheight=1.2),  # title
                  plot.subtitle=element_text(size=12, 
                                             family="Helvetica",
                                             face="italic",
                                             hjust=0.5),  # subtitle
                  plot.caption=element_text(size=10,
                                            
                                            family="Helvetica"),
                  axis.text.x=element_text(size=10,
                                           face="bold"))
        
        
    })
    
    # output$plot2 <- renderPlot({
    #     barplot(prop.table(table(histDat()$Structures)), col="#69b3a2",
    #             horiz=T, las=1
    #     )
    # })
    
    output$plot3 <- renderPlot({
        #barplot(prop.table(table(histDat()$Code_Enfor)), col="#90a4e5")
        ggplot(histDat3(), aes(x=reorder(short_code_enfor.y,-n), y=n, fill=short_code_enfor.y)) + 
            geom_bar(position="dodge", stat="identity")  +
            theme(axis.text.x = element_text(angle = 60, hjust=1),legend.position = "none")+
            labs(title="Code Enforment", 
                 caption="Source: South Bend Open Data Portal 'Abandoned Properties'")+
            ylab("")+
            xlab("")+
            scale_fill_brewer(palette="Set2") +
            theme(plot.title=element_text(size=15, 
                                          face="bold", 
                                          family="Helvetica",
                                          
                                          hjust=0.5,
                                          lineheight=1.2),  # title
                  plot.subtitle=element_text(size=12, 
                                             family="Helvetica",
                                             face="italic",
                                             hjust=0.5),  # subtitle
                  plot.caption=element_text(size=10,
                                            
                                            family="Helvetica"),
                  axis.text.x=element_text(size=10,
                                           face="bold"))   
    })
    
    # DEMOGRAPHICS
    
    # react to area selection
    mapDat <-reactive({
        if(input$area == "All"){
            rtrn <- census
        }else{
            rtrn <- census %>% 
                dplyr::filter(NAME == input$area)
        }
        return(rtrn)
    })
    
    # react to demographics radio buttons
    demoDat <- reactive({
        if(input$demo == "Age"){
            if(input$area == "All"){
                rtrn <- census %>%
                    st_set_geometry(NULL) %>%
                    dplyr::select("SE_T008_01":"SE_T008_12") %>%
                    melt() %>%
                    group_by(variable) %>%
                    summarize(count = sum(value)) %>%
                    mutate(range = factor(age_axis, levels=age_axis))
            }else{
                rtrn <- census %>%
                    st_set_geometry(NULL) %>%
                    dplyr::select("SE_T008_01":"SE_T008_12","NAME") %>%
                    filter(NAME == input$area) %>%
                    melt(value.name = "count") %>%
                    mutate(range = factor(age_axis, levels=age_axis))
            }
        }else{
            if(input$area == "All"){
                rtrn <- census %>%
                    st_set_geometry(NULL) %>%
                    dplyr::select("SE_T054_01":"SE_T054_07") %>%
                    melt() %>%
                    group_by(variable) %>%
                    summarize(count = sum(value)) %>%
                    mutate(range = factor(race_axis, levels=race_axis))
            }else{
                rtrn <- census %>%
                    st_set_geometry(NULL) %>%
                    dplyr::select("SE_T054_01":"SE_T054_07","NAME") %>%
                    filter(NAME == input$area) %>%
                    melt(value.name = "count") %>%
                    mutate(range = factor(race_axis, levels=race_axis))
            }
        }
        return(rtrn)
    })
    demoTitle <- reactive({
        if(input$demo == "Age"){
            if(input$area == "All"){
                rtrn <- "Ages in All Areas"
            }else{
                rtrn <- paste("Ages in Area", input$area, sep=" ")
            }
        }else{
            if(input$area == "All"){
                rtrn <- "Races in All Areas"
            }else{
                rtrn <- paste("Races in Area", input$area, sep=" ")
            }
        }
        return(rtrn)
    })
    
    # create plots for demographic data
    output$mapPlot <- renderLeaflet({
        # set up pop up
        popup <- paste("<b>","Area: ",mapDat()$NAME,"</b><br>",
                       "Population: ",mapDat()$SE_T001_00,"<br>",
                       "Households: ",mapDat()$SE_T058_00,sep ="")%>%
            lapply(htmltools::HTML)
        # generate map and color selected area
        leaflet(mapDat())  %>%
            setView(.,lng = -86.2520, lat = 41.6764, zoom = 11) %>%
            addTiles(group = "Basic")  %>%
            addPolygons(data = mapDat(),
                        label = popup,
                        color = "#225EA8", 
                        weight = 3) 
    })
    output$demoPlot <- renderPlot({
        # generate bar chart of ages based on area
        ggplot(demoDat(), aes(x=range, y=count, fill=range)) +
            geom_col() +
            # geom_bar(position="dodge", stat="identity") +
            scale_y_continuous(expand = c(0, 0)) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(color = 'black'),
                  plot.title = element_text(size=22),
                  axis.title.y = element_text(size=15),
                  axis.text.x = element_text(size=10),
                  axis.text.y = element_text(size=10),
                  legend.position = "none") +
            labs(title=demoTitle(),
                 x = "",
                 y = "Number of People") +
            scale_fill_brewer(palette="Set3")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

