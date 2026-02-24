```{r}
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
```

```{r}
df2 = read_csv("https://uwmadison.box.com/shared/static/0ff6z2hx6bgkxjvx5masd3tdsobtdzpw.csv")
df2 = df2 %>% select(study_hours_per_day, social_media_hours, netflix_hours, sleep_hours, exercise_frequency, exam_score)
```


```{r}
scatter <- function(df, selected) {
  
  palette <- c("#d4d4d4", "steelblue") 
  
  df %>%
    mutate(selected_ = selected) %>%
    ggplot(aes(x = study_hours_per_day,
               y = exam_score,
               color = as.factor(selected_))) +
    geom_point(size = 3) +
    scale_color_manual(values = palette, guide = "none") +
    labs(
      title = "Study Hours vs Exam Scores",
      x = "Study Hours per Day",
      y = "Exam Score"
    ) +
    theme_minimal(base_size = 14)
}

ui <- fluidPage(
  
  fluidRow(
  column(
    12,
    h2("Exploring the Relationship Between Lifestyle factors and Academic Performance"),
    h4("Academic performance is often influenced by more than just study time. Daily habits such as sleep, media consumption, and exercise may shape how effectively students translate effort into exam outcomes. This application explores how lifestyle factors relate to the relationship between study hours and exam performance, allowing users to investigate patterns across different behavioral conditions. Adjust lifestyle filters to examine how behavioral patterns relate to the relationship between study effort and exam performance. Drag across the scatterplot to inspect specific groups of students.",
       style = "color: gray; margin-top: -10px;")
  )
),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Lifestyle Factors"),
      
      sliderInput("socialRange",
                  "Daily Social Media Usage (Hours)",
                  min = min(df2$social_media_hours),
                  max = max(df2$social_media_hours),
                  value = range(df2$social_media_hours)),
      
      sliderInput("netflixRange",
                  "Daily Netflix Streaming Time (Hours)",
                  min = min(df2$netflix_hours),
                  max = max(df2$netflix_hours),
                  value = range(df2$netflix_hours)),
      
      sliderInput("sleepRange",
                  "Sleep Hours per Night:",
                  min = min(df2$sleep_hours),
                  max = max(df2$sleep_hours),
                  value = range(df2$sleep_hours)),
      
      sliderInput("exerciseRange",
                  "Exercise Frequency (Days):",
                  min = min(df2$exercise_frequency),
                  max = max(df2$exercise_frequency),
                  value = range(df2$exercise_frequency))
    ),
    
    mainPanel(
      
      plotOutput("plot", brush = "scatter_brush"),
      
      br(),
      h4("Data of Selected Students"),
      DTOutput("table")
    )
  )
)

server <- function(input, output) {
  
  # Reactive filtering
  filtered_data <- reactive({
    df2 %>%
      filter(
        social_media_hours >= input$socialRange[1],
        social_media_hours <= input$socialRange[2],
        netflix_hours >= input$netflixRange[1],
        netflix_hours <= input$netflixRange[2],
        sleep_hours >= input$sleepRange[1],
        sleep_hours <= input$sleepRange[2],
        exercise_frequency >= input$exerciseRange[1],
        exercise_frequency <= input$exerciseRange[2]
      )
  })
  
  selected <- reactiveVal(NULL)
  
  observe({
    selected(rep(FALSE, nrow(filtered_data())))
  })
  
  observeEvent(input$scatter_brush, {
    
    brushed_points <- brushedPoints(
      filtered_data(),
      input$scatter_brush,
      allRows = TRUE
    )
    
    selected(brushed_points$selected_)
    
  })
  
  output$plot <- renderPlot({
    req(filtered_data())
    scatter(filtered_data(), selected())
  })
  
  output$table <- renderDT({
    
    if (is.null(input$scatter_brush) || all(!selected())) {
      filtered_data()
    } else {
      filtered_data()[selected(), ]
    }
    
  })
}

shinyApp(ui, server)
```
