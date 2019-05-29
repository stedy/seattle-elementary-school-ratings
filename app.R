library(shinydashboard)
library(leaflet)
library(dplyr)

es_schools <- rgdal::readOGR("Seattle_ES_locations.geojson")
es_schools$school_name <- as.character(es_schools$PROPERTY_L)
es_area <- rgdal::readOGR("Seattle_attendance_ES.geojson")
es_area$school_name <- as.character(es_area$ES_ZONE)
es_stats <- read.csv("es_stats.csv")

ui <- dashboardPage(
                    dashboardHeader(title = "Seattle Elementary school ratings", titleWidth = 350),
                    dashboardSidebar(
                      uiOutput("schoolSelect")
                    ),
                    dashboardBody(
                                  fluidRow(
                                           box(leafletOutput("seattlePlot", height = 600)),
                                           box(uiOutput("school_name", status = "info", height=100)),
                                           box(title = "Math Test Pass Rate", background = "light-blue", height = 40),
                                           box(uiOutput("mathResultsTable", height= 500, style="font-size:130%")),
                                           box(title = "ELA Test Pass Rate", background = "olive", height = 40),
                                           box(uiOutput("elaResultsTable", height= 500, style="font-size:130%")),
                                           box(title = "Student/Teacher Statistics", background = "fuchsia", height = 40),
                                           box(uiOutput("strResultsTable", height= 300, style="font-size:130%"))
                                               )
                                           )
                                  )

server <- function(input, output) {

   output$seattlePlot <- renderLeaflet({
    test <- subset(es_area, school_name == input$school)
    school_temp <- subset(es_schools, school_name == input$school)

    map = leaflet() %>%
      addProviderTiles('OpenStreetMap.Mapnik') %>%
      addPolygons(data = test, color = "#444444",
                  weight = 1, smoothFactor = 0.8, fillOpacity = 0.5) %>%
      addCircleMarkers(data = school_temp, lat = ~ school_temp@coords[,2], lng = ~ school_temp@coords[,1],
                       radius = 10,
                       color = "#F66955", weight = 4, fillOpacity=0.9)
    map

  })

	output$schoolSelect <- renderUI({
    school_choices <- sort(es_schools$school_name)
    selectInput("school", "Select school", choices = school_choices)
	})

	output$school_name <- renderUI({
	  tags$h1(paste0(" ", input$school, " Elementary"))
	})

	output$mathResultsTable <- renderUI({
	  selected_math_table <- subset(es_stats, ES_ZONE == input$school) %>%
	          select(math3, math4, math5, math_overall, math_rank)

	  tags$table(class = "table",
	             tags$thead(tags$tr(
	               tags$th("Grade 3"),
	               tags$th("Grade 4"),
	               tags$th("Grade 5"),
	               tags$th("Overall"),
	               tags$th("Rank")
	             )),
	             tags$tbody(
	               tags$tr(
	                 tags$td(selected_math_table[1,1]),
	                 tags$td(selected_math_table[1,2]),
	                 tags$td(selected_math_table[1,3]),
	                 tags$td(selected_math_table[1,4]),
	                 tags$td(selected_math_table[1,5])

	             ))
    )
	})

	output$elaResultsTable <- renderUI({
	  selected_ela_table <- subset(es_stats, ES_ZONE == input$school) %>%
	          select(ela3, ela4, ela5, ela_overall, ela_rank)

	  tags$table(class = "table",
	             tags$thead(tags$tr(
	               tags$th("Grade 3"),
	               tags$th("Grade 4"),
	               tags$th("Grade 5"),
	               tags$th("Overall"),
	               tags$th("Rank")
	             )),
	             tags$tbody(
	               tags$tr(
	                 tags$td(selected_ela_table[1,1]),
	                 tags$td(selected_ela_table[1,2]),
	                 tags$td(selected_ela_table[1,3]),
	                 tags$td(selected_ela_table[1,4]),
	                 tags$td(selected_ela_table[1,5])

	             ))
    )
	})

  output$strResultsTable <- renderUI({
    selected_str_table <- subset(es_stats, ES_ZONE == input$school) %>%
      select(student_count, teacher_count, student_teacher_ratio, student_teacher_ratio_rank)

    tags$table(class = "table",
               tags$thead(tags$tr(
                tags$th("Student Count"),
                tags$th("Teacher Count"),
                tags$th("Student-Teacher Ratio"),
                tags$th("Rank")
                )),
               tags$tbody(
                tags$tr(
                        tags$td(selected_str_table[1,1]),
                        tags$td(selected_str_table[1,2]),
                        tags$td(selected_str_table[1,3]),
                        tags$td(selected_str_table[1,4])
                        ))
               )
  })


}
shinyApp(ui, server)
