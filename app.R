# load libraries

#list of packages required
list.of.packages <- c("shiny","scholar","dplyr","ggplot2","DT","plotly","bslib")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

### Load packages
for (i in list.of.packages) {
  print(i)
  library(i, character.only = TRUE)
}



ui <- page_navbar(
  title = "Scholar Assistant",
  id = "navbar_id",  
  
  nav_panel("Scholar publication",
            sidebarLayout(
              sidebarPanel(
                textInput("scholarId", "Enter Google Scholar ID (extract from link)",
                          value = "9rOOivYAAAAJ", placeholder = "Your Scholar ID"),
                actionButton("go", "Go"),
                sliderInput("years", "Years Range",
                            min = 2000, max = as.integer(format(Sys.Date(), "%Y")),
                            value = c(2015, as.integer(format(Sys.Date(), "%Y"))),
                            step = 1)
              ),
              
              mainPanel(
                # One tab only: plot + table stacked vertically
                tabsetPanel(
                  tabPanel("Scholar Publication Overview",
                           plotOutput("scholarPlot", height = 400),
                           br(),
                           h4("Top Impact Publications"),
                           DTOutput("pubTable")
                  )
                )
              )
            )
  ),
  
  nav_panel("Journal selection",
          sidebarLayout(
              sidebarPanel(
              fileInput("file", "Upload CAS journal database",
                        accept = c(".csv", ".tsv", ".txt")),
              tags$h6("Download example：",
                      tags$a(href = "https://github.com/zx0223winner/Scholar_Assistant/tree/main/journals/CAS_citation_2024.txt",
                             "中科院分区表2024",
                             target = "_blank"),
                      "."),
              radioButtons("sep", "Delimiter",
                           choices = c("Tab" = "\t",
                                       "Comma" = ",",
                                       "Semicolon" = ";"),
                           selected = "\t"),
              
              uiOutput("var_select"),
              uiOutput("yaxis_slider"),
              
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("Table", DTOutput("datatable"),width = 8),
                tabPanel("Dot Plot", plotlyOutput("dotplot",height = 400)),
              )
            )
  )
),
  

  
  nav_spacer(), # Adds space between navigation links and items
  nav_item(a("GitHub Link", href = "https://github.com/zx0223winner/Scholar_Assistant")) # Add external links
)






server <- function(input, output, session) {
  
  scholar_id <- eventReactive(input$go, { input$scholarId })
  
  citations <- reactive({
    req(scholar_id())
    get_citation_history(scholar_id())
  })
  
  publications <- reactive({
    req(scholar_id())
    get_publications(scholar_id())
  })
  
  dat <- reactive({
    ct <- citations()
    pubs <- publications()
    pubs <- pubs[complete.cases(pubs$year), ]
    
    papersPerYear <- as.data.frame(table(pubs$year), stringsAsFactors = FALSE)
    papersPerYear <- dplyr::rename(papersPerYear, year = Var1, Freq = Freq)
    papersPerYear$year <- as.numeric(as.character(papersPerYear$year))
    
    df <- full_join(ct, papersPerYear, by = "year")
    df[is.na(df)] <- 0
    df <- arrange(df, year)
    df
  })
  
  # ScholarPlot style plot
  output$scholarPlot <- renderPlot({
    req(dat())
    plotdata <- dat() %>%
      filter(year >= input$years[1], year <= input$years[2])
    
    transform <- round(max(plotdata$cites) / max(plotdata$Freq) * (2/3), 0)
    
    ggplot(plotdata) +
      geom_col(aes(x = year, y = Freq * transform),
               fill = "white", colour = "blue", size = 1) +
      geom_line(aes(x = year, y = cites), linewidth = 2, colour = "grey") +
      geom_point(aes(x = year, y = cites), size = 3, colour = "black") +
      theme_bw() +
      labs(x = "Year",
           y = "Annual Citations (grey line)",
           title = "ScholarPlot Style: Citations vs Publications") +
      scale_x_continuous(breaks = seq(min(plotdata$year), max(plotdata$year), 1)) +
      scale_y_continuous(
        sec.axis = sec_axis(~ . / transform, name = "Annual Papers (blue bars)")
      )
  })
  
  # Top impact publications table
  output$pubTable <- renderDT({
    req(publications())
    pubs <- publications()
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    
    pubs <- pubs %>%
      mutate(citesPerYear = ifelse(year != current_year,
                                   cites / (current_year - year),
                                   cites)) %>%
      arrange(desc(citesPerYear)) %>%
      select(title, author, journal, year, cites, citesPerYear)
    
    datatable(pubs, options = list(pageLength = 10))
  })
  
 
  

  
  
  # Reactive: read uploaded file
  data <- reactive({
    req(input$file)
    read.table(input$file$datapath,
               sep = input$sep,
               header = TRUE,
               stringsAsFactors = FALSE)
  })
  
  # Dynamically generate variable selectors
  output$var_select <- renderUI({
    req(data())
    df <- data()
    tagList(
      selectInput("xvar", "X-axis Variable", choices = names(df)),
      selectInput("yvar", "Y-axis Variable", choices = names(df)),
      selectInput("filtervar", "Filter Column", choices = names(df)),
      uiOutput("filter_values")
    )
  })
  
  # Filter values based on chosen column
  output$filter_values <- renderUI({
    req(data(), input$filtervar)
    df <- data()
    selectInput("filterval", "Select Values",
                choices = unique(df[[input$filtervar]]),
                multiple = TRUE)
  })
  
  # Filtered data
  filtered_data <- reactive({
    df <- data()
    if (!is.null(input$filterval)) {
      df <- df[df[[input$filtervar]] %in% input$filterval, ]
    }
    df
  })
  
  # Y-axis slider based on selected variable
  output$yaxis_slider <- renderUI({
    req(filtered_data(), input$yvar)
    df <- filtered_data()
    rng <- range(df[[input$yvar]], na.rm = TRUE)
    sliderInput("ylim", "Adjust Y-axis Range",
                min = floor(rng[1]),
                max = ceiling(rng[2]),
                value = rng,
                step = 1)
  })
  
  # Interactive dot plot with plotly
  output$dotplot <- renderPlotly({
    req(filtered_data(), input$xvar, input$yvar)
    p <- ggplot(filtered_data(), aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(color = "blue", size = 3) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      ) +
      coord_cartesian(ylim = input$ylim)   # <-- apply slider limits
    ggplotly(p)
  })
  
  # Data table
  output$datatable <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  

  }
  
  

shinyApp(ui, server)
