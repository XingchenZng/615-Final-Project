library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(ggplot2)


world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = st_crs("+proj=longlat +datum=WGS84"))
stkitts_nevis <- world[world$admin %in% c("Saint Kitts and Nevis"), ]


ui <- dashboardPage(
  
  dashboardHeader(title = "Saint Kitts and Nevis Overview"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("General description", tabName = "general", icon = icon("info-circle")),
      menuItem("Key Demographics", tabName = "demo", icon = icon("users")),
      menuItem("Comparison", tabName = "comp", icon = icon("exchange-alt"),
               menuSubItem("Fact", tabName = "fact"),
               menuSubItem("Plot", tabName = "plot")),
      menuItem("SWOT analysis", tabName = "SWOT", icon = icon("tasks")),
      menuItem("References", tabName = "Ref", icon = icon("book"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # General tab content
      tabItem(tabName = "general",
              fluidRow(
                box(
                  title = "Map of Saint Kitts and Nevis",
                  status = "primary",
                  solidHeader = TRUE,
                  leafletOutput("map_stkitts_nevis", height = "500px")
                ),
                box(
                  title = "Saint Kitts and Nevis in the World",
                  status = "primary",
                  solidHeader = TRUE,
                  leafletOutput("map_world_stkitts_nevis", height = "500px")
                ),
                box(
                  title = "General Description",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  background = "light-blue",
                  tags$p(
                    HTML(
                      "<strong>Description:</strong> Saint Kitts and Nevis is a dual-island nation located in the Caribbean. The country consists of two volcanic islands, Saint Kitts and Nevis, known for their stunning landscapes and rich history."
                    )
                  )
                ),
                box(
                  title = "Key Facts",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  background = "light-blue",
                  tags$p(
                    HTML(
                      "<strong>Population:</strong> Approximately 53,000<br>",
                      "<strong>Economic Dependency:</strong> The economy relies on tourism and services.</span><br>",
                      "<strong>Official Language:</strong> English</span><br>",
                      "<strong>Capital:</strong> Basseterre</span><br>",
                      "<strong>Area:</strong> 261 sq km<br>",
                      "<strong>Currency:</strong> Eastern Caribbean Dollar (XCD)</span>"
                    )
                  )
                )
              )
      ),
     
      tabItem(tabName = "demo",
              fluidRow(
                box(
                  title = "Population",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  background = "light-blue",
                  plotOutput("ageDistributionPlot"),
                  tags$p(
                    HTML(
                      "<strong>Country Name:</strong> Saint Kitts and Nevis<br>",
                      "<strong>Population:</strong> 54,817<br>",
                      "<strong>Median Age:</strong> 25.4<br>",
                      "<strong>Age 0-14 years (%):</strong> 19.4<br>",
                      "<strong>Age 15-64 years (%):</strong> 68.54<br>",
                      "<strong>Age 65 years and over (%):</strong> 12.05<br>",
                      "<strong>Growth Rate (%):</strong> 1.2<br>",
                      "<strong>Gender Ratio:</strong> 1.02 (male to female)<br>",
                      "<strong>Date of Data Collection:</strong> January 1, 2023"
                    )
                  )
                ),
                box(
                  title = "Race",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  background = "light-blue",
                  plotOutput("Ethnicity_plot"),
                  tags$p(
                    HTML(
                      "<strong>African Descent (%):</strong> 92.5<br>",
                      "<strong>Mixed (%):</strong> 2.1<br>",
                      "<strong>East Indian (%):</strong> 1.5<br>",
                      "<strong>Other (%):</strong> 0.6<br>",
                      "<strong>Unspecified (%):</strong> 0.3<br>"
                    )
                  )
                )
              )
      ),
      
      tabItem(tabName = "fact",
              fluidRow(
                # Add content specific to Saint Kitts and Nevis for comparison
                box(
                  title = "Key Facts - Saint Kitts and Nevis",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  background = "light-blue",
                  tags$p(
                    HTML(
                      "<strong>Population:</strong> 54,817<br>",
                      "<strong>Official Language:</strong> English<br>",
                      "<strong>Capital:</strong> Basseterre<br>",
                      "<strong>Area:</strong> 261 sq km<br>",
                      "<strong>Currency:</strong> Eastern Caribbean Dollar (XCD)<br>",
                      "<strong>GDP:</strong> 1.8 billion"
                    )
                  )
                )
              ),
              fluidRow(
                
                box(
                  title = "Key Facts - Antigua and Barbuda",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  background = "light-blue",
                  tags$p(
                    HTML(
                      "<strong>Population:</strong> 93,219<br>",
                      "<strong>Official Language:</strong> English<br>",
                      "<strong>Capital:</strong> St. John's<br>",
                      "<strong>Area:</strong> 440 sq km<br>",
                      "<strong>Currency:</strong> Eastern Caribbean Dollar (XCD)<br>",
                      "<strong>GDP:</strong> 2.6 billion"
                    )
                  )
                ),
              
                box(
                  title = "Key Facts - Barbados",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  background = "light-blue",
                  tags$p(
                    HTML(
                      "<strong>Population:</strong> 267,800<br>",
                      "<strong>Official Language:</strong> English<br>",
                      "<strong>Capital:</strong> Bridgetown<br>",
                      "<strong>Area:</strong> 439 sq km<br>",
                      "<strong>Currency:</strong> Barbados dollar(BBD)<br>",
                      "<strong>GDP:</strong> 5.436 billion"
                    )
                  )
                ),
                
                box(
                  title = "Key Facts - Dominica",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  background = "light-blue",
                  tags$p(
                    HTML(
                      "<strong>Population:</strong> 72,412<br>",
                      "<strong>Official Language:</strong> English<br>",
                      "<strong>Capital:</strong> Roseau<br>",
                      "<strong>Area:</strong> 750 sq km<br>",
                      "<strong>Currency:</strong> East Carbbean dollar(XCD)<br>",
                      "<strong>GDP:</strong> 1 billion"
                    )
                  )
                )
              )
      ),
     
      tabItem(tabName = "plot",
              fluidRow(
          
                box(
                  title = "Population Comparison",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("population_plot")
                )
              ),
              fluidRow(
             
                box(
                  title = "Area Comparison",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("Area_plot")
                )
              ),
              fluidRow(
              
                box(
                  title = "GDP Comparison",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("GDP_plot")
                )
              ),
      ),
    
      tabItem(tabName = "SWOT",
              tags$p(
                HTML("<strong>SWOT analysis of Saint Kitts and Nevis:</strong>")
              ),
              tags$div(
                class = "swot-section",
                tags$h4("Strengths:"),
                tags$ul(
                  tags$li("Beautiful natural scenery."),
                  tags$li("Growing tourism industry.")
                )
              ),
              tags$div(
                class = "swot-section",
                tags$h4("Weaknesses:"),
                tags$ul(
                  tags$li("Vulnerability to natural disasters.")
                )
              ),
              tags$div(
                class = "swot-section",
                tags$h4("Opportunities:"),
                tags$ul(
                  tags$li("Diversification of the tourism sector.")
                )
              ),
              tags$div(
                class = "swot-section",
                tags$h4("Threats:"),
                tags$ul(
                  tags$li("Climate change impacts.")
                )
              ),
              tags$p(
                class = "additional-info",
                HTML("<strong>Detailed Description:</strong>")
              ),
              tags$div(
                class = "additional-info",
                tags$p(
                  HTML("<strong>Strengths:</strong> Saint Kitts and Nevis is known for its natural landscapes, pristine beaches, and plenty of greenery. The natural beauty of the islands makes it a tourist destination and enhances the overall quality of life for residents.")
                ),
                tags$p(
                  HTML("<strong>Weaknesses:</strong> Saint Kitts and Nevis is easy to expose to natural disasters, including hurricanes, earthquakes, and volcanic activity. The geographic location of the islands exposes them to these hazards, which leads to a challenge for its future development and long-term sustainability.")
                ),
                tags$p(
                  HTML("<strong>Opportunities:</strong> There is an opportunity to diversify the tourism by introducing new attractions, activities, and experiences. This could include promoting eco-tourism, cultural events, and adventure tourism to attract a broader range of visitors and reduce dependency on traditional tourism offerings.
")
                ),
                tags$p(
                  HTML("<strong>Threats:</strong> The effects of climate change, such as rising sea levels, extreme weather events, and changing weather patterns, leads to a threat to the islands. These impacts can affect tourism, agriculture, and overall economic stability. Thus, planning stratgeis need to be given for future plans.")
                )
              )
      ),
      
      tabItem(tabName = "Ref",
              tags$p(HTML("<strong>References:</strong>")),
              p("https://en.wikipedia.org/wiki/Saint_Kitts_and_Nevis"),
              p("https://data.worldbank.org/indicator/SP.POP.TOTL.MA.ZS?end=2022&start=2022&view=bar"),
              p("https://www.cia.gov/the-world-factbook/countries/saint-kitts-and-nevis/"),
              
      )
    )
  )
)



server <- function(input, output) {
  
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  
 
  output$map_stkitts_nevis <- renderLeaflet({
    leaflet(data = stkitts_nevis) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lng = -62.7228, lat = 17.3026, zoom = 12) %>%
      addCircleMarkers(
        lng = -62.7228,
        lat = 17.3026,
        radius = 8,
        fillOpacity = 1,
        color = "red",
        fillColor = "red",
        popup = "Saint Kitts and Nevis"
      ) %>%
      addLabelOnlyMarkers(
        lng = -62.7228,
        lat = 17.3026,
        label = "Saint Kitts and Nevis",
        labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE)
      ) %>%
      addLabelOnlyMarkers(
        lng = -62.7177,
        lat = 17.2942,
        label = "Basseterre - Capital City",
        labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE)
      )
  })
  
  
  output$map_world_stkitts_nevis <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = world, fillColor = "white", weight = 0.5, color = "white", fillOpacity = 0.5) %>%
      addPolygons(data = stkitts_nevis, fillColor = "blue", weight = 1, color = "blue", fillOpacity = 0.8) %>%
      setView(lng = -62.7228, lat = 17.3026, zoom = 3) %>%
      addLegend(
        position = "bottomright",
        colors = "blue",
        labels = "Saint Kitts and Nevis"
      ) %>%
      addLabelOnlyMarkers(
        lng = -62.7177,
        lat = 17.2942,
        label = "Basseterre - Capital City",
        labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE)
      )
  })

  output$ageDistributionPlot <- renderPlot({
    age_data <- data.frame(
      Age = c("0-14 years", "15-64 years", "65 years and over"),
      Percentage = c(19.4, 68.54, 12.05)
    )
    
    ggplot(age_data, aes(x = Age, y = Percentage, fill = Age)) +
      geom_bar(stat = "identity", width = 0.5, color = "black", alpha = 0.7) +
      labs(title = "Age Distribution for Saint Kitts and Nevis",
           x = "Age Group",
           y = "Percentage (%)",
           fill = "Age Group") +
      theme_minimal()
  })
  
  
  output$Ethnicity_plot <- renderPlot({
    ethnicity_data <- data.frame(
      Ethnicity = c("African Descent", "Mixed", "White", "East Indian", "Other", "Unspecified"),
      Percentage = c(92.5, 3, 2.1, 1.5, 0.6, 0.3)
    )
    
    ggplot(ethnicity_data, aes(x = Ethnicity, y = Percentage, fill = Ethnicity)) +
      geom_bar(stat = "identity", width = 0.5, color = "black", alpha = 0.7) +
      labs(title = "Race Distribution for Saint Kitts and Nevis",
           x = "Ethnicity Group",
           y = "Percentage (%)",
           fill = "Ethnicity Group") +
      theme_minimal()
  })
  
  
  

  output$population_plot <- renderPlot({
    
    countries <- c("Saint Kitts and Nevis", "Antigua and Barbuda", "Barbados", "Dominica")
    populations <- c(54817, 93219, 267800, 72412)
    
    barplot(populations, names.arg = countries, col = "black", main = "Population Comparison", ylab = "Population")
  })
  
  output$Area_plot <- renderPlot({
    
    countries <- c("Saint Kitts and Nevis", "Antigua and Barbuda", "Barbados", "Dominica")
    area <- c(261, 440, 439, 750)
    
    barplot(area, names.arg = countries, col = "black", main = "Area Comparison", ylab = "Area(km^2)")
  })
  
  output$GDP_plot <- renderPlot({
   
    countries <- c("Saint Kitts and Nevis", "Antigua and Barbuda", "Barbados", "Dominica")
    GDP <- c(1.8, 2.6, 5.436, 1)
    
    barplot(GDP, names.arg = countries, col = "black", main = "GDP Comparison", ylab = "GDP(in billion)")
  })

  
}

shinyApp(ui, server)


