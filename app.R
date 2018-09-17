source("global.R")

library(shiny)
library(shinythemes)

# turn warning messages off
options(warn=-1)

degree_choices <-  c("PhD" = "doctorate", "MA/MS" = "masters", "BA/BS" = "bachelors")
science_choices <- c("anatomy", "biology", "botany", "chemistry", "geology", "mathematics", "morphology", 
                     "paleontology", "pathology", "physics", "physiology", "zoology")
mc_choices <- c("major", "career", "both", "all records")

iconSet <- awesomeIconList(
      biology = makeAwesomeIcon(icon='leaf', library='fa', markerColor = 'black', iconColor = '#ffffff'),
      chemistry = makeAwesomeIcon(icon='flask', library='fa', markerColor = 'black', iconColor = '#ffffff'),
      geology = makeAwesomeIcon(icon='diamond', library='fa', markerColor = 'black', iconColor = '#ffffff'),
      anatomy = makeAwesomeIcon(icon='male', library='fa', markerColor = 'black', iconColor = '#ffffff'),
      botany = makeAwesomeIcon(icon='leaf', library='fa', markerColor = 'black', iconColor = '#ffffff'),
      pathology = makeAwesomeIcon(icon='medkit', library='fa', markerColor = 'black', iconColor = '#ffffff'),
      physiology = makeAwesomeIcon(icon='heart', library='fa', markerColor = 'black', iconColor = '#ffffff'),
      zoology = makeAwesomeIcon(icon='paw', library='fa', markerColor = 'black', iconColor = '#ffffff'),
      morphology = makeAwesomeIcon(icon='house', library='fa', markerColor = 'black', iconColor = '#ffffff'),
      paleontology = makeAwesomeIcon(icon='university', library='fa', markerColor = 'black', iconColor = '#ffffff'),
      physics = makeAwesomeIcon(icon='magnet', library='fa', markerColor = 'black', iconColor = '#ffffff'),
      mathematics = makeAwesomeIcon(icon='cube', library='fa', markerColor = 'black', iconColor = '#ffffff'),
      interdisciplinary = makeAwesomeIcon(icon='star', library='fa', markerColor = 'black', iconColor = '#ffffff')
)

# set map tile options
tileOptions(minZoom = 3, maxZoom = 10)

# set map marker options
markerOptions(riseOnHover = TRUE)

# set map cluster options
markerClusterOptions(showCoverageOnHover = FALSE, zoomToBoundsOnClick = TRUE,
                     spiderfyOnMaxZoom = TRUE, removeOutsideVisibleBounds = TRUE,
                     spiderLegPolylineOptions = list(weight = 1.0, color = "#222", opacity = 0.5), freezeAtZoom = 4)


ui <- navbarPage("History of Bryn Mawr Women in Science - Interactive Map and Data Explorer", theme = shinytheme("spacelab"), selected = "About",
            tags$head(
                 includeCSS("styles.css")
            ),
            
            tabPanel("About",
                     h1("Welcome"),
                     p("This interactive map and data explorer forms part of an ongoing collaborative project led by Dr. Jessica Linker, 
                        the CLIR Digital Scholarship & Humanities Postdoctoral Fellow at Bryn Mawr College. The app was developed
                       by ", a(href="http://rachelstarry.org", "Dr. Rachel Starry"),
                      " during the pilot phase (2017-2018) of the Bryn Mawr History of Women in Science Project."),
                     br(),
                     h2("History of the Project"),
                     p("The ",
                       a(href="http://digitalscholarship.blogs.brynmawr.edu/2017/11/28/dalton-hall-and-the-history-of-bryn-mawr-women-in-science/", "pilot phase"),
                       " of the Bryn Mawr 'HoWis' Project has focused on the 3D reconstruction of Dalton Hall's historical 
                       laboratory space. The 3D models of Dalton Hall created by Dr. Linker's undergraduate Digital Scholarship Research Assistants 
                       aim to contextualize the reconstructed spaces and archival objects displayed within them, by allowing users to navigate through the spaces and 
                       engage with original research and pedagogical exercises from the sciences, social sciences, and humanities."), 
                     br(),
                     p("This interactive map acts as a second interface with the project. It visualizes a datataset focused on women 
                       who studied STEM fields while at Bryn Mawr College in the late 19th and early 20th centuries, and encourages users 
                       to explore the career paths and global impact of women in science after they left Bryn Mawr."),
                     br(),
                     h2("About the Data"),
                     p("The dataset consists of information about 392 women who attended Bryn Mawr College between the years 1895 and 1920, 
                       whose undergraduate majors or graduate degrees included one or more STEM disciplines. The data was obtained from 
                       the register of alumnae and former students published in the 1920 Bryn Mawr Calendar, which is  ",
                       a(href="https://repository.brynmawr.edu/bmc_calendars/50/", "available online"), 
                       " from the open-access repository of Scholarship, Research, and Creative Work 
                       at Bryn Mawr College. The original document was processed through the text scanning software ABBYY FineReader12 to 
                       produce a plain-text document of the 1920 alumnae register, which was then parsed into a tabular dataset using scripts written in R. 
                       OpenRefine was utilized both for data clean-up and in combination with the ",
                       a(href="https://dandelion.eu", "Dandelion API"), " (a named entity extraction tool) and the ",
                       a(href="https://developers.google.com/maps/documentation/geocoding/intro", "Google Geocoding API"), 
                       " (used to translate named location entities into geographical coordinates)."), 
                     br(),
                     h2("About the Map"),
                     p("The interactive map displays markers for each individual in the dataset whose place of residence or occupation was extracted 
                       from the 1920 alumnae register. The data can be filtered by scientific disciplines studied, highest degree earned, whether an individual 
                       studied a STEM discipline as a major as opposed to pursuing a career in the sciences (or both), and the year of an individual's most current 
                       recorded occupation. See the 'Help' tab for more detailed instructions on how to explore the interactive map."), br(),
                     p("While the map permits a dynamic overview of the geographical distribution of women in scientific disciplines at the beginning of the 20th 
                       century, we encourage visitors to move between the map and the fully searchable Data Explorer, as more detailed information is available for  
                       each individual's record in the dataset than what is immediately visible within the map interface."),
                     br(),
                     h2("Code"),
                     p("Code for this Shiny app is ", a(href="https://github.com/rachelstarry/howis_map", "available on github"), ".")
                     
            ),
            
            tabPanel("Interactive Map",
                     div(class="outer",
                         tags$head(
                               includeCSS("styles.css")
                         ),
                         leafletOutput("map", width = "100%", height = "100%"),
                         absolutePanel(id = "totals", fixed = TRUE, draggable = TRUE, 
                                       top = 120, right = "auto", left = 60, bottom = "auto",
                                       width = 200, height = "auto",
                                       
                                       conditionalPanel(
                                             condition = "input.update == 0",
                                             h4("Current number of markers on map: "),
                                             p("388")
                                       ),
                                       conditionalPanel(
                                             condition = "input.update != 0",
                                             h4("Current number of markers on map: "),
                                             textOutput("current_n")
                                       )
                                       ),
                         absolutePanel(id = "controls", fixed = FALSE, draggable = TRUE, 
                                       top = 120, left = "auto", right = 20, bottom = "auto",
                                       width = "auto", height = "auto",
                                       
                                       h2("Map & Data Options"),
                                       radioButtons("major_career", label = h4("Filter by scientific majors vs. careers"), 
                                                    mc_choices, selected = "all records"),
                                       br(),
                                       checkboxGroupInput("filter_science", label = h4("Filter by scientific discipline(s)"), science_choices),
                                       br(),
                                       checkboxInput("filter_degree", label = h4("Filter by highest degree earned"), value = FALSE),
                                       conditionalPanel(
                                             condition = "input.filter_degree == true",
                                             selectInput("degree", label = "Degree", degree_choices, selected = "doctorate")
                                       ),
                                       br(),
                                       sliderInput("date_range", label = h4("Filter by year of current occupation"), sep="", min = 1895, max = 1920,
                                                   value = c(1895, 1920)),
                                       br(),
                                       actionButton("update", label = "Update Map"),
                                       actionButton("reset", label = "Reset Defaults"),
                                       br()
                         )
                     )
            ),
            
            tabPanel("Data Explorer",
                  DT::dataTableOutput("table")
            ),
            
            tabPanel("Help",
                     h1("How to use this app"),
                     br(),
                     h2("Interactive Map Controls"),
                     p("Map markers are grouped into clusters: red clusters indicate a large percentage of markers, yellow a moderate percentage, and green 
                       a small number of markers at that geographical location."),
                     p("Click on a marker to display more information about the individual represented."),
                     p("Hover over a marker to display the scienctific discipline(s) studied by each individual."),
                     p("Use the checkboxes to filter the dataset by scientific discipline(s), STEM majors vs. careers, highest degree earned, and years of 
                       individuals' current occupation (as recorded in the 1920 Bryn Mawr alumnae register)."),
                     p("Click on the 'Update Map' button to apply filters or the 'Reset Defaults' button to remove all filters and view all records."),
                     br(),
                     h2("Data Explorer"),
                     p("Type in the search box to search any field in the dataset."),
                     p("Click on the arrows next to any column name to sort the data by the values in that column.")
            )
            
      )


server <- function(input, output, session) {
      
      # render dataset
      output$table <- DT::renderDataTable(clean_db)
      
      # reactive expression run whenever dataset needs to be filtered according to user input
      filteredData <- reactive({
            
            
            # filter by science first (if necessary)
            if(length(input$filter_science) == 1) {
                  # if there is exactly one science box checked...
                  filtered_data <- db %>% filter(science1 == input$filter_science | science2 == input$filter_science | 
                                                 science3 == input$filter_science | science4 == input$filter_science | 
                                                 science5 == input$filter_science)
                  
                  
            } else if(length(input$filter_science) > 1) {
                  # if there are more than one science boxes checked...
                  # for the first type of science, initialize filtered_data
                  filtered_data <- db %>% filter(science1 == input$filter_science[1] | science2 == input$filter_science[1] | 
                                           science3 == input$filter_science[1] | science4 == input$filter_science[1] | 
                                           science5 == input$filter_science[1])
                  
                  # for each subsequent type of science, add rows to filtered_data
                  s <- seq.int(2, length(input$filter_science))
                  for (i in s) {
                        temp_db <- db %>% filter(science1 == input$filter_science[i] | science2 == input$filter_science[i] | 
                                                 science3 == input$filter_science[i] | science4 == input$filter_science[i] | 
                                                 science5 == input$filter_science[i])
                        
                        filtered_data <- bind_rows(filtered_data, temp_db)
                  }
                  filtered_data <- distinct(filtered_data, full_name, .keep_all = TRUE)
            } else { 
                  #don't filter by science so filtered_data = db
                  filtered_data <- db }
            
            # filter by degree
            if (input$filter_degree) {
                  if (input$degree == "doctorate") {
                       filtered_data <-  db[!is.na(db$d_degree),]
                  } else if (input$degree == "masters") {
                       filtered_data <-  filtered_data %>% filter(!is.na(m_degree)) %>% filter(is.na(d_degree))
                  } else if (input$degree == "bachelors") {
                       filtered_data <-  filtered_data %>% filter(is.na(d_degree)) %>% filter(is.na(m_degree))
                  } 
            } else { #filter_degree == FALSE
                  # don't filter by degree
            }
            
            # filter by major/career
            if (input$major_career == "major") {
                  filtered_data <- filtered_data %>% filter(major_career == "major")
            } else if (input$major_career == "career") {
                  filtered_data <- filtered_data %>% filter(major_career == "career")
            } else if (input$major_career == "both") {
                  filtered_data <- filtered_data %>% filter(major_career == "both")
            } else {
                  #don't filter by major/career
            }
            
            # filter by occupation year
            filtered_data <- filtered_data %>% filter(pos_year >= input$date_range[1] & pos_year <= input$date_range[2])
            
            return(filtered_data)
                  
      })
      
      # create original leaflet map with full dataset
      output$map <- renderLeaflet({
            
            map_popup = paste0("<b>Name: </b>",
                               db$full_name,
                               "<br><b>Occupation: </b>",
                               db$current_position, 
                               ". ", db$pos_year)
            
            leaflet(db) %>%
                  addProviderTiles(providers$OpenStreetMap.BlackAndWhite, options = tileOptions()) %>% 
                  setView(lat = 40.02, lng = -45.31, zoom = 3) %>%
                  addAwesomeMarkers(icon = ~iconSet[db$science_icon], options = markerOptions(riseOnHover=TRUE), 
                                    clusterOptions = markerClusterOptions(), label = ~htmlEscape(db$science), popup = map_popup)
                  
      }) 
      
      # observer for action button; on click, recreate map with filtered dataset
      observeEvent(input$update, {
            
            local_db <- filteredData()
          
            map_popup = paste0("<b>Name: </b>",
                               local_db$full_name,
                               "<br><b>Occupation: </b>",
                               local_db$current_position, 
                               ". ", local_db$pos_year)
            
            leafletProxy("map", data = local_db) %>% clearMarkerClusters() %>% 
                        addAwesomeMarkers(lat = local_db$latitude, lng = local_db$longitude, icon = ~iconSet[local_db$science_icon], 
                                          options = markerOptions(riseOnHover=TRUE), 
                                          clusterOptions = markerClusterOptions(), label = ~htmlEscape(local_db$science), popup = map_popup)
            
            output$current_n <- renderText({ isolate(nrow(filteredData())) })
            
      })
      
      
      # observer for reset button; on click, recreate map with complete dataset and reset all inputs
      observeEvent(input$reset, {
            
            map_popup = paste0("<b>Name: </b>",
                               db$full_name,
                               "<br><b>Occupation: </b>",
                               db$current_position, 
                               ". ", db$pos_year)
            
            leafletProxy("map", data = db) %>% clearMarkerClusters() %>%
                  addAwesomeMarkers(icon = ~iconSet[db$science_icon], options = markerOptions(riseOnHover=TRUE), 
                                    clusterOptions = markerClusterOptions(), label = ~htmlEscape(db$science), popup = map_popup)
                  
            updateRadioButtons(session, "major_career", "Filter by scientific majors vs. careers", choices=mc_choices, selected = "all records")
            updateCheckboxGroupInput(session, "filter_science", "Filter by scientific discipline(s)", choices=science_choices)
            updateCheckboxInput(session, "filter_degree", value = FALSE)
            updateSliderInput(session, "date_range", value = c(1895, 1920))
            
            output$current_n <- renderText({ nrow(db) })
            
      })

      
}

shinyApp(ui, server)