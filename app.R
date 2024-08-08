library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(DT)
library(sf)
library(rvest)
library(viridis)
library(ggthemes)
library(rgdal)
library(countrycode)
library(rnaturalearthdata)
library(rnaturalearth)
library(RColorBrewer)

data <- read.csv("GrainProduction.csv")

crctd_data <- na.omit(data)

ui <- dashboardPage(
  dashboardHeader(title = "Grain Production"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("World Grain Production Map", tabName = "Map"),
      menuItem("Country wise Production", tabName = "Bar"),
      menuItem("Correlation Plot", tabName = "Dot"),
      menuItem("Grain Production Proportion", tabName = "Pie"),
      menuItem("Time Series Analysis", tabName = "Line"),
      menuItem("Data", tabName = "Search")
    )
  ),
  dashboardBody(
    theme = shinytheme("yeti"),
    
    tabItems(
      tabItem(tabName = "Map",
              tabsetPanel(
                tabPanel("World Grain Production Map",
                         h3("Map"),
                         selectInput("mvar", "Variable", colnames(crctd_data)[5:7]),
                         plotOutput("map"),
                         h4("Summary Statistics"),
                         tableOutput("summstat")
                )
              )
      ),
      
      tabItem(tabName = "Bar",
              tabsetPanel(
                tabPanel("Country wise Production",
                         h3("Grain Production of specific Country"),
                         selectInput("bvar", "Country", choices = unique(crctd_data$Country_name)),
                         plotOutput("bar")
                )
              )
      ),
      
      tabItem(tabName = "Dot",
              tabsetPanel(
                tabPanel("Correlation Plot",
                         h3("Correlation Analysis"),
                         selectInput("dvar1", "Variable 1", colnames(crctd_data)[5:7]),
                         selectInput("dvar2", "Variable 2", colnames(crctd_data)[5:7]),
                         plotOutput("dot")
                )
              )
      ),
      
      tabItem(tabName = "Pie",
              tabPanel("Grain Production Proportion",
                       selectInput("pvar", "Grain", choices = unique(crctd_data$Grain_name)),
                       plotOutput("pie")
              )
      ),
      
      tabItem(tabName = "Line",
              tabsetPanel(
                tabPanel("Time Series Analysis",
                         h3("Year wise production of Major Grains in specific Continent"),
                         selectInput("lvar", "Continent", choices = unique(crctd_data$Continent)),
                         plotOutput("line")
                )
              )
      ),
      
      tabItem(tabName = "Search",
              tabsetPanel(
                tabPanel("Data Head",
                         tableOutput("tablehead")
                ),
                tabPanel("Data Select",
                         checkboxGroupInput("selvars", "Add/Remove Data Variables", colnames(crctd_data)),
                         textInput("selrows", "Select Rows (vector expression)", "1:15"),
                         tableOutput("tableselect")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  rv <- reactiveValues()
  
  output$summstat <- renderTable({
    fac <- input$mvar
    if (is.numeric(crctd_data[[fac]])) {
      df <- data.frame(
        Min = min(crctd_data[[fac]], na.rm = TRUE),
        Median = median(crctd_data[[fac]], na.rm = TRUE),
        Mean = mean(crctd_data[[fac]], na.rm = TRUE),
        Max = max(crctd_data[[fac]], na.rm = TRUE)
      )
      return(df)
    } else {
      return(NULL)
    }
  })
  
  
  
  output$map <- renderPlot({
    fac <- input$mvar
    Country_GP <- summarise(group_by(crctd_data, Country_name), factor = sum(!!sym(fac)))
    Country_GP$Country_name <- recode(Country_GP$Country_name,
                                      "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
                                      "Congo - Kinshasa" = "Democratic Republic of the Congo",
                                      "Congo - Brazzaville" = "Republic of Congo",
                                      "Czechia" = "Czech Republic",
                                      "Eswatini" = "Swaziland",
                                      "Myanmar (Burma)" = "Myanmar",
                                      "North Macedonia" = "Macedonia",
                                      "St. Vincent & Grenadines" = "Saint Vincent and the Grenadines",
                                      "São Tomé & Príncipe" = "Sao Tome and Principe",
                                      "Trinidad & Tobago" = "Trinidad and Tobago")
    Country_GP$Country_name <- countrycode(Country_GP$Country_name, "country.name", "country.name")
    world <- ne_countries(scale = "medium", returnclass = "sf")
    world <- left_join(world, Country_GP, by = c("name" = "Country_name"))
    ggplot(data = world) +
      geom_sf(aes(fill = factor)) +
      scale_fill_continuous(name = paste(fac), low = "pink", high = "darkblue", na.value = "lightgrey") +
      theme_minimal() +
      labs(title = paste("World Map: ", fac)) +
      theme(
        panel.background = element_rect(fill = "white"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
      )
  })
  
  output$bar <- renderPlot({
    country <- input$bvar
    slt_data <- crctd_data[crctd_data$Country_name == country,]
    ggplot(slt_data, aes(x = factor(Year_of_production), y = Total_production, fill = Grain_name)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = paste("Total Grain Production in", country, "by Year"),
           x = "Year of Production",
           y = "Total Production in 1000 tons",
           fill = "Grain Name") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_fill_brewer(palette = "Set3")
  })
  
  output$dot <- renderPlot({
    var1 <- input$dvar1
    var2 <- input$dvar2
    selected_data <- data.frame(Value1 = crctd_data[[var1]], Value2 = crctd_data[[var2]])
    ggplot(selected_data, aes(x = Value1, y = Value2)) +
      geom_point() +
      labs(x = var1, y = var2, title = paste("Correlation Plot:", var1, "vs", var2))
  })
  
  output$pie <- renderPlot({
    grain <- input$pvar
    G = crctd_data[crctd_data$Grain_name == grain,]
    GCo = summarise(group_by(G, Continent), Production = sum(Total_production))
    r_production <- GCo[GCo$Continent == "Europe/Asia",]$Production
    t_production <- GCo[GCo$Continent == "Asia/Europe",]$Production
    GCo <- GCo[GCo$Continent != "Europe/Asia",]
    GCo <- GCo[GCo$Continent != "Asia/Europe",]
    GCo[GCo$Continent == "Asia", "Production"] <- GCo[GCo$Continent == "Asia", "Production"] + 0.77*r_production + 0.97*t_production
    GCo[GCo$Continent == "Europe", "Production"] <- GCo[GCo$Continent == "Europe", "Production"] + 0.23*r_production + 0.03*t_production
    ggplot(GCo, aes(x = "", y = Production, fill = Continent)) +
      geom_bar(stat = "identity") +
      coord_polar("y", start = 0) +
      labs(title = "Production of Corn",
           fill = "Continents",
           x = NULL, y = NULL) +
      theme_void() +
      theme(legend.position = "right") +
      geom_text(aes(label = paste0(round(Production/sum(Production)*100,2), "%")),
                position = position_stack(vjust = 0.5),
                color = "black", size = 4) +
      scale_fill_manual(values = c('#FBF8CC', '#FFCFD2', '#B9FBC0','#957DAD','#B7D3DF','#DEAAAD'))
  })
  
  output$line <- renderPlot({
    continent <- input$lvar
    GPnew <- crctd_data[crctd_data$Continent == continent,]
    agg_data <- GPnew %>% 
      filter(Grain_name %in% c("Corn","Wheat","Rice")) %>%
      group_by(Year_of_production, Grain_name) %>%
      summarise(Total_production = sum(Total_production)) %>%
      ungroup()
    ggplot(agg_data, aes(x = Year_of_production, y = Total_production, color = Grain_name)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Production of Major Grains over Years in", continent),
           x = "Year of Production",
           y = "Total Production") +
      theme_minimal()
  })
  
  output$tablehead <- renderTable({
    head(crctd_data)
  })
  
  output$tableselect <- renderTable({
    selvars <- input$selvars
    selrows <- eval(parse(text = input$selrows))
    crctd_data[selrows, selvars, drop = FALSE]
  })
}

shinyApp(ui, server)
