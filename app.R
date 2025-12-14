# load libraries


## call functions
source(file.path("Desktop/Scholar_Assistant/functions", "setBackgroundColor.R"), local=T)
source(file.path("Desktop/Scholar_Assistant/functions", "jRnkBootFunc.R"), local=T)
fpath <- "Desktop/Scholar_Assistant/journals/"

#list of packages required
list.of.packages <- c("DT","bootstrap","plotly","Hmisc","shinybusy","tidyverse", "scholar", "NLP", "RColorBrewer", "tm", "wordcloud", "qdap", "readtext", "xml2", "rvest", "bslib","scholar")


#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

### Load packages
for (i in list.of.packages) {
  print(i)
  library(i, character.only = TRUE)
}

library(shiny)
library(bslib)

require(shinyjs)
require(graphics)
require(methods)
require(scholar)
require(igraph)
# require(network)
require(stringdist)
require(networkD3)
library(ggplot2)


# load libraries
library(shiny)
library(shinybusy)
library(Hmisc)
library(cluster)
library(bootstrap)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(forcats)
library(dplyr)

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
              fileInput("file", "Upload CAS journal database File (from gtihub)",
                        accept = c(".csv", ".tsv", ".txt")),
              
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
  
  
  
  
  nav_panel("Scholar network",
            #h1("Find who you collaborated most"),
            #p("Please input your google scholar page link here"),
  shinyjs::useShinyjs(),
  titlePanel("Find who you collaboratd most via google scholar LINK"),
  sidebarPanel(
    helpText("see how it works"),
    HTML('<iframe width="350" height="200" src="https://www.youtube.com/embed/NZ5WdBnZ-CE" frameborder="0" allowfullscreen></iframe>'),
    helpText("Copy the link to your person of interest Google Scholar profiles (example: 'https://scholar.google.com/citations?user=zufgVroAAAAJ&hl=en') below and see the co-authorships network", style = "color:green"),
      
    textInput("scholarid",'google scholar profile link',value = ""),
    actionButton("submit", "Submit"),
    helpText("Reference: data for co-authorships and publications are being extracted from google scholar with 'scholar' package for R"),
    helpText("**: It is the first and earliest version, some issues are knwon and in the process to be solved, some not, I will be happy to hear: akbaritabar[at] gmail.com")
      
      
    ),
    mainPanel(
      h4("author's data",align="top",align="center"),
      tableOutput("sctable"),
      h4("Raw co-authorships Graph",align="top",align="center"),
      helpText("use mouse scroll to zoom in and out; further details (including main component) below", style="color:green"),
      simpleNetworkOutput("evalPlot1"),
      h4("Main component",align="bottom",align="center"),
      
      plotOutput("evalPlot2"),
      h4("author name's inconsistencies",align="top",align="center"),
      helpText("if you see similar name with different spellings, means google scholar data has inconsistencies that need to be resolved", style="color:green"),
      textOutput("sctext")
      
    )
  ),

  

  
  nav_panel("Rank Journal",
            titlePanel("JournalRankShiny: a multi-index bootstrap function to rank a sample of peer-reviewed journals"),
            
            wellPanel(style = "background: azure",
                      tags$a(href="https://github.com/cjabradshaw/JournalRankShiny", tags$img(height = 150, src = "race.png", style="float:right")),
                      tags$p(style="font-family:Avenir", tags$i(class="fab fa-r-project", title="R Project"),"Shiny App by", tags$a(href="https://globalecologyflinders.com/people/#CJAB", "Corey Bradshaw "),
                             tags$a(href = "mailto:corey.bradshaw@flinders.edu.au","(",tags$i(class="far fa-envelope"),"e-mail"),";",
                             tags$a(href = "https://github.com/cjabradshaw", tags$i(class="fab fa-github",title="Github"),"Github)")),
                      tags$h4(style="font-family:Avenir", "Preamble"),
                      tags$p(style="font-family:Avenir", "There are many methods available to assess the relative citation performance of peer-reviewed journals.
           Regardless of their individual faults and advantages, citation-based metrics are used by researchers to maximise the citation potential
           of their articles, and by employers to rank academic track records. The absolute value of any particular index is arguably meaningless
           unless compared to other journals, and different metrics result in divergent rankings. To provide a simple yet more objective way to rank
           journals within and among disciplines, this app provides a κ-resampled composite journal rank incorporating six user-supplied citation indices:",
                             tags$a(href="http://help.incites.clarivate.com/incitesLiveJCR/glossaryAZgroup/g8/4346-TRS.html", "Impact Factor"), "(IF),",
                             tags$a(href="http://help.incites.clarivate.com/incitesLiveJCR/glossaryAZgroup/g7/7751-TRS.html", "Immediacy Index"), "(IM),",
                             tags$a(href="https://scholar.google.com/intl/en/scholar/metrics.html#metrics", "Google 5-year h-index"), "(h5),",
                             tags$a(href="https://service.elsevier.com/app/answers/detail/a_id/30562/supporthub/scopus/", "CiteScore"), "(CS),",
                             tags$a(href="https://blog.scopus.com/posts/journal-metrics-in-scopus-source-normalized-impact-per-paper-snip", "Source-Normalized Impact Per Paper"),
                             "(SNIP), and", tags$a(href="https://www.scopusjournals.com/2019/02/scimago-journal-rank.html", "SCImago Journal Rank"), "(SJR).
           The output gives an index of relative rank uncertainty for all sample journals provided by the user. This", tags$i(class="fab fa-github"),
                             "Github ", tags$a(href = "https://github.com/cjabradshaw/JournalRankShiny", "repository"),
                             "provides all the 'under-the-bonnet'",tags$i(class="fab fa-r-project"),"code for the app. Read the related",
                             tags$a(href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0149852", "paper", tags$i(class="far fa-file")),
                             " and/or", tags$a(href="https://conservationbytes.com/2016/02/18/how-to-rank-journals/",
                                               "blog post", tags$i(class="fas fa-blog"), ".")),
                      tags$h4(style="font-family:Avenir", "Instructions"),
                      tags$ol(tags$li(tags$p(style="font-family:Avenir", "Create a delimited text file of", tags$strong("exactly the same format"), "as the example file in this",
                                             tags$a(href="https://github.com/cjabradshaw/JournalRankShiny/blob/main/Jsamp2019.csv","repository", tags$i(class="far fa-file")), ",
           although you can specify the delimit character (", tags$em("comma"),", ", tags$em("space"),", ", tags$em("tab"),").")),
                              tags$li(tags$p(style="font-family:Avenir", "Load your delimited text file in the app by clicking the",tags$i(class="fas fa-file-import"),
                                             tags$strong("choose file"), "button.")),
                              
                              
                              tags$li(tags$p(style="font-family:Avenir", "Select the number of bootstrap iterations", tags$i(class="fas fa-sort-amount-down-alt"), 
                                             "you desire (1000, 10000, or 100000) — more iterations will provide better estimates of the uncertainty bounds, but will take longer to calculate.")),
                              tags$a(href="https://globalecologyflinders.com/", tags$img(height = 100, src = "GEL Logo Kaurna transparent.png", style="float:right",
                                                                                         title="Global Ecology @ Flinders University")),
                              tags$li(tags$p(style="font-family:Avenir", "Set the", tags$em("κ"), "'clipping'",tags$i(class="fas fa-cut"), "limitation and number of repeats",
                                             tags$em("n"), tags$i(class="fas fa-redo"), "to reduce the influence of outliers on the  uncertainty bounds.")),
                              tags$li(tags$p(style="font-family:Avenir", "Click the", tags$i(class="fas fa-calculator"), tags$strong("calculate ranks"), "button.")),
                              tags$li(tags$p(style="font-family:Avenir", "Download the results table as a", tags$i(class="fas fa-file-csv"), "file by clicking the", tags$i(class="fas fa-download"),
                                             tags$strong("download"), "button.")))
            ), # end wellPanel
            
            tabsetPanel(id="tabs",
                        tabPanel(value="tab1", title="user-collated journal metrics",
                                 
                                 sidebarLayout(
                                   sidebarPanel(
                                     wellPanel(style = "background: LightCyan",
                                               fileInput("file1", label=tags$p(tags$i(class='fas fa-file-import'),"choose delimited file with index data (9 columns)"),
                                                         multiple=F, buttonLabel="choose file", placeholder="no file selected"),
                                              
                                               
                                                                                              
#                                               selectInput("site_choice", "Choose one:",
#                                                           choices = list("Jsamp2022" = "https://github.com/cjabradshaw/JournalRankShiny/tree/main/Jsamp2022.csv",
#                                                                          "Jsamp2021" = "https://github.com/cjabradshaw/JournalRankShiny/tree/main/Jsamp2021.csv")),
#                                               uiOutput("dynamic_link"),
#                                               #downloadButton("downloadData", "Download"),

                                               
                                               tags$hr(),                        
                                               radioButtons("sep",label=tags$p(tags$i(class="fas fa-file-csv"),"separator"),choices=c(comma=',',space="",tab="\t"), inline=T),
                                               checkboxInput("header1", "header?", TRUE),
                                               tags$hr(),
                                               radioButtons("iter", label=tags$p(tags$i(class='fas fa-sort-amount-down-alt'), "select number of bootstrap iterations"), inline=T,
                                                            choiceNames = list("1000", "10000", "100000"), choiceValues = list(1000,10000,100000)),
                                               tags$hr(),
                                               radioButtons("kappa", label=tags$p(tags$i(class='fas fa-cut'), "choose clipping value", tags$em("κ")), inline=T,
                                                            c("1.5"="1.5","2"="2","3"="3","4"="4"), selected="2"),
                                               tags$hr(),
                                               radioButtons("nK", label=tags$p(tags$i(class='fas fa-redo'), "choose number of clipping repeats", tags$em("n")), inline=T,
                                                            c("2"="2","5"="5","10"="10"), selected="5"),
                                               tags$hr(),
                                               actionButton("calcButton", label="calculate ranks",icon=shiny::icon("fas fa-calculator")),
                                               br(),
                                               tags$small(style="font-family:Avenir", "(refresh page to clear data)"),
                                               tags$hr(),
                                               downloadButton('downloadData', 'download',icon = shiny::icon("download"))
                                     ),
                                   ),
                                   
                                   # open main panel
                                   mainPanel(style = "background: MintCream",
                                             
                                             fluidRow(
                                               tags$div(id = "firstOutput", 
                                                        h3("input data"),
                                                        add_busy_spinner(spin="fading-circle", color="blue", timeout=500, position="bottom-right", height = 250, width = 250),
                                                        dataTableOutput("table1")) 
                                             ),
                                             
                                             fluidRow(
                                               add_busy_spinner(spin="fading-circle", color="blue", timeout=500, position="bottom-right", height = 250, width = 250),
                                               tags$div(id = "placeholder") # the dynamic UI will be inserted relative to this placeholder
                                             ),
                                             
                                   ) # close mainPanel
                                   
                                 ) # sidebarLayout
                        ), # end tab1

tabPanel(value="tab0", title="Explore journal databse",
         sidebarPanel(
           selectInput('selectfile','Select File',choice = list.files(fpath)),
           textOutput('fileselected'),width = 40, hight = 50,
           mainPanel("Main Panel",dataTableOutput("txtout"),style = "font-size:100%", width = 40, hight = 50)
         )
),




                        tabPanel(value="tab2", title=tags$strong("rank plots"), style = "background: MintCream",
                                 
                                 tags$br(),
                                 tags$p(style="font-family:Avenir", "The following plot show the relative ranks (± 95% bootstrapped confidence intervals)
                              for the sample of journals provided according to:"),
                                 tags$ul(tags$li(tags$p(style="font-family:Avenir", tags$strong("A.", tags$em("κ"),"-clipped bootstrap rank"))),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("B. bootstrap rank"))),
                                 ), # end ul
                                 
                                 mainPanel(
                                   tags$br(),
                                   add_busy_spinner(spin="fading-circle", color="blue", timeout=500, position="bottom-right", height = 250, width = 250),
                                   plotOutput(height="1200px", width="150%","rankPlots")
                                 ) # end mainPanel
                                 
                        ), # end tab2
                        
                        tabPanel(value="tab3", title=tags$strong("Impact Factor", tags$em("vs."), "rank"), style = "background: MintCream",
                                 
                                 tags$br(),
                                 tags$p(style="font-family:Avenir", "The following plot shows the relationship between log",tags$sub(tags$em("e")), " journal Impact Factor and
                              its relative",tags$em("κ"),"-clipped rank:"),
                                 
                                 mainPanel(
                                   tags$br(),
                                   tags$p(style="font-family:Avenir","The loess trend is indicated by the blue line."),
                                   tags$br(),
                                   add_busy_spinner(spin="fading-circle", color="blue", timeout=500, position="bottom-right", height = 250, width = 250),
                                   plotOutput(height="800px", width="150%", "cMYPlots")
                                 ) # end mainPanel
                                 
                        ), # end tab3
                        
                        tabPanel(value="tab4", title=tags$strong("input/output column descriptors"), style = "background: MintCream",
                                 tags$h2(style="font-family:Avenir", "Column descriptors"),
                                 tags$a(href="https://flinders.edu.au/", tags$img(height = 100, src = "F_V_CMYK.png", style="float:right",title="Flinders University")),
                                 tags$h3(style="font-family:Avenir", "Input data file requirements"),
                                 tags$ol(tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 1"),": ", tags$em("Journal")," — abbreviated journal name (see abbreviations
                                              list", tags$a(href="https://www.library.caltech.edu/journal-title-abbreviations", "here"),")")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 2"),": ", tags$em("n")," — number of citable items that year (available
                                              from Web of Science™", tags$a(href="https://clarivate.com/webofsciencegroup/solutions/journal-citation-reports/",
                                                                            "Journal Citation Reports"),")")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 3"),": ", tags$em("cites")," — total number of citations that year
                                              (available from Web of Science™", tags$a(href="https://clarivate.com/webofsciencegroup/solutions/journal-citation-reports/",
                                                                                       "Journal Citation Reports"),")")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 4"),": ", tags$em("h5")," — five-year h-index
                                              (available from", tags$a(href="https://scholar.google.com.au/citations?view_op=top_venues&hl=en",
                                                                       "Google Scholar"),")")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 5"),": ", tags$em("IF")," — journal Impact Factor
                                              (available from Web of Science™", tags$a(href="https://clarivate.com/webofsciencegroup/solutions/journal-citation-reports/",
                                                                                       "Journal Citation Reports"),")")),
                                         tags$a(href="https://epicaustralia.org.au/", tags$img(height = 150, src = "CABAHlogo.png",
                                                                                               style="float:right", title="ARC Centre of Excellence for Australian Biodiversity and Heritage")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 6"),": ", tags$em("IM")," — Immediacy Index
                                              (available from Web of Science™", tags$a(href="https://clarivate.com/webofsciencegroup/solutions/journal-citation-reports/",
                                                                                       "Journal Citation Reports"),")")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 7"),": ", tags$em("CS")," — CiteScore
                                              (available from Elsevier™", tags$a(href="https://www.scopus.com/sources",
                                                                                 "Scopus"),")")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 8"),": ", tags$em("SNIP")," — Source-Normalized Impact Per Paper
                                              (available from Elsevier™", tags$a(href="https://www.scopus.com/sources",
                                                                                 "Scopus"),")")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 9"),": ", tags$em("SJR")," — SCImago Journal Rank
                                              (available from Elsevier™", tags$a(href="https://www.scopus.com/sources",
                                                                                 "Scopus"),")"))),
                                 
                                 tags$h3(style="font-family:Avenir", "rank output"),
                                 tags$ol(tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 1"),": ", tags$em("journal")," — abbreviated journal name (see abbreviations
                                              list", tags$a(href="https://www.library.caltech.edu/journal-title-abbreviations", "here"),")")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 2"),": ", tags$em("IF")," — journal Impact Factor")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 3"),": ", tags$em("avgObsRnk")," — average rank across 6 metrics (not bootstrapped)")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 4"),": ", tags$em("rnkLo")," — lower 95% confidence bound of bootstrapped rank")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 5"),": ", tags$em("rnkMed")," — median bootstrapped rank")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 6"),": ", tags$em("rnkUp")," — upper 95% confidence bound of bootstrapped rank")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 7"),": ", tags$em("rnkKaplo")," —", tags$em("κ"), "-clipped lower 95% confidence bound of bootstrapped rank")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 8"),": ", tags$em("rnkKapmed")," —", tags$em("κ"), "-clipped median bootstrapped rank")),
                                         tags$a(href="https://github.com/cjabradshaw/JournalRankShiny/blob/main/LICENSE", tags$img(height = 50, src = "GNU GPL3.png", style="float:right", title="GNU General Public Licence v3.0")),
                                         tags$li(tags$p(style="font-family:Avenir", tags$strong("COLUMN 9"),": ", tags$em("rnkKapup")," —", tags$em("κ"), "-clipped upper 95% confidence bound of bootstrapped rank"))),
                                 tags$br()
                        ) # end tab4
                        
            ) # end tabsetPanel
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
      geom_line(aes(x = year, y = cites), size = 2, colour = "grey") +
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
  
 
  

  
  # Server logic can access the currently selected tab value using the 'id'
  observe({
    cat("Current navbar selection:", input$navbar_id, "\n")
  })
  
  observeEvent(input$submit, {
    citid <- strsplit((strsplit(input$scholarid,"&",fixed = TRUE)[[1]][1]),"=",fixed = TRUE)[[1]][2]
    infosc <- get_profile(citid)
    pub <- get_publications(citid, flush=TRUE)
    authnum_articles <- get_num_articles(citid)
    authdistinc_journal <- get_num_distinct_journals(citid)
    autholdest_article <- get_oldest_article(citid)
    authnum_topjournals <- get_num_top_journals(citid)
    ## tolower does character conversion, and remove the trailing "..."
    coauthors <- sub('[ ,.]+$', '', tolower(pub$author))
    coauthors <- coauthors[nzchar(coauthors)]  # only keep entries that aren't blank
    ## Add self-loops for single-author entries
    adjlist <- strsplit(coauthors, '\\s*,\\s*')
    lens <- lengths(adjlist)
    adjlist[lens==1L] <- lapply(adjlist[lens==1L], rep, times=2)  # repeat single author entries
    
    edgelist <- cbind(
      unlist(lapply(adjlist, tail, -1L)),                        # col1
      rep(sapply(adjlist, `[`, 1L), times=lengths(adjlist)-1L)   # col2
    )
    
    charr.list <- list()
    charr.list2 <- list()
    for (j in seq_along(edgelist[,1])) {
      charr <- edgelist[,1][j]
      charr<-unlist(strsplit(charr, '\\s* \\s*'))
      charr <- charr[max(seq_along(charr))]
      charr.list[[j]] <- charr
      
      charr2 <- edgelist[,2][j]
      charr2<-unlist(strsplit(charr2, '\\s* \\s*'))
      charr2 <- charr2[max(seq_along(charr2))]
      charr.list2[[j]] <- charr2
      
    }
    edgelist.clean <- cbind(as.character(charr.list),as.character(charr.list2))
    
    coauthorgraph <- graph_from_edgelist(edgelist.clean,directed = TRUE)
    # report of author name inconsistencies
    name.author<-unlist(strsplit(infosc$name, '\\s* \\s*'))
    name.author <- tolower(name.author[max(seq_along(name.author))])
    comparisons <- stringdist(name.author,names(V(coauthorgraph)),method = "jw")
    node.comparisons <- V(coauthorgraph)[0<comparisons & comparisons<0.3]
    results <- list(node.comparisons=node.comparisons)
    
    # author information to be shown on app
    authordatadownloaded <- cbind.data.frame(
      "number of articles" = as.character(authnum_articles),
      "unique journals" = as.character(authdistinc_journal),
      "oldest article" = as.character(autholdest_article),
      "number of top journals" = as.character(authnum_topjournals),
      "h-index" = as.character(infosc$h_index)
    )
    networkData <- data.frame(edgelist.clean)
    
    shinyjs::disable("submit")
    shinyjs::disable("scholarid")
    shinyjs::disable("scholarid2")
    output$evalPlot1 <- renderSimpleNetwork({
      set.seed(333)
      # using networkD3 to have zoom possibility
      simpleNetwork(networkData,zoom = TRUE)
    })
    
    output$evalPlot2 <- renderPlot({
      set.seed(333)
      ord <- V(coauthorgraph)                                               # node order
      theta <- seq(0, 2*pi-2*pi/length(ord), 2*pi/length(ord))  # angle
      theta[theta>pi] <- -(2*pi - theta[theta>pi])              # convert to [0, pi]
      dists <- rep(c(1, 0.7), length.out=length(ord))           # alternate distance
      
      ## Plot
      plot(decompose.graph(coauthorgraph)[[order(clusters(coauthorgraph)$csize,decreasing = TRUE)[1]]], layout=layout.auto, vertex.label.degree=-theta, main = paste("main component of:",infosc$name),
           vertex.label.dist=dists, vertex.label.cex=1.1,
           vertex.size=5, vertex.color='#FFFFCC', edge.color='#E25822')
    })
    
    output$sctable <- renderTable(authordatadownloaded)
    output$sctext <- renderPrint(results)
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
  
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$file1, {
  
    if(input$tabs == "tab1"){
      
      output$table1 <- renderDataTable({
        file_to_read = input$file1
        if(is.null(file_to_read)){
          return()
        }
        read.table(file_to_read$datapath, sep=input$sep, header=input$header1, quote=NULL)
      }) # end output table1
      
      datin <- reactive({
        fileinp <- input$file1
        if(is.null(fileinp)){return()}
        inpdat <- data.frame(read.table(fileinp$datapath, sep=input$sep, header = input$header1, quote=NULL))
        return(inpdat)
      }) # end datin
      
      KappaDat <- reactiveValues()
      observe({
        KappaDat$iter <- as.numeric(input$iter)
        KappaDat$kappa <- as.numeric(input$kappa)
        KappaDat$nK <- as.numeric(input$nK)
      })
      
      # when action button pressed ...
      observeEvent(input$calcButton, {
        removeUI("div:has(>#firstOutput)")
        insertUI(
          selector = "#placeholder",
          where = "afterEnd", # inserts UI after the end of the placeholder element
          ui = fluidRow(
            h3("calculating bootstrapped ranks ... (this can take some time depending on the number of journals in your sample and the bootstrap settings)"),
            output$ranktable <- renderDataTable({
              if(is.null(datin())){return ()}
              results <<- jRnkBootFunc(datsamp=(datin()), iter=KappaDat$iter, kap=KappaDat$kappa, kN=KappaDat$nK)
            })))
      }) # end observeEvent
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("journalRanksOut", "csv", sep = ".")
        },
        
        content = function(file) {
          sep <- ","
          
          write.table(results, file, sep=sep, row.names = F)
        }
      )
    } # end if for tab1
    
  }) # end Events
  
  observeEvent(input$tabs, {
    
    if(input$tabs == "tab2"){
      
      output$rankPlots <- renderPlot({
        input$rankPlots
        
        Ctheme1 = theme(
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 8, face="italic")
        )
        
        kappaRnk <- results %>%
          mutate(journal = fct_reorder(journal, desc(rnkKapmed))) %>%
          ggplot( aes(x=rnkKapmed, y=journal, xmin = rnkKaplo, xmax = rnkKapup)) +
          geom_point(size=1) +
          xlim(1, max(results$rnkKapup)) +
          geom_errorbarh() +
          labs(x="(← higher)      rank      (lower →)", y=NULL) +
          Ctheme1
        
        bootRnk <- results %>%
          mutate(journal = fct_reorder(journal, desc(rnkMed))) %>%
          ggplot( aes(x=rnkKapmed, y=journal, xmin = rnkLo, xmax = rnkUp)) +
          geom_point(size=1) +
          xlim(1, max(results$rnkUp)) +
          geom_errorbarh() +
          labs(x=NULL, y=NULL) +
          Ctheme1
        
        ggarrange(kappaRnk, bootRnk,
                  labels=c("A. κ-clipped bootstrap rank", "B. bootstrap rank"),
                  hjust=c(-1,-1.6),
                  label.y=c(0.11, 0.08),
                  ncol=1, nrow=2)
      })
      
    } # end if for tab2
    
    if(input$tabs == "tab3"){
      
      output$cMYPlots <- renderPlot({
        input$cMYPlots
        
        Ctheme = theme(
          axis.title.x = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 16),
          axis.text.y = element_text(size = 14))
        
        IFkapRnk <- ggplot(data=results, aes(x=rnkKapmed, y=log(IF), xmin=rnkKaplo, xmax=rnkKapup)) + 
          geom_point() +
          geom_errorbar(linetype=2, position=position_dodge(0.4)) +
          geom_smooth() +
          labs(x="κ rank", y="log Impact Factor") +
          geom_label_repel(aes(label = journal),
                           box.padding = 0.35, 
                           point.padding = 0.5,
                           segment.color = 'grey50',
                           segment.alpha = 0.7,
                           show.legend = F,
                           alpha=0.7) +
          Ctheme
        IFkapRnk
        
        ggarrange(IFkapRnk,
                  labels=NULL,
                  ncol=1, nrow=1)
      })
      
    } # end if for tab3
    
  })
 
  
  
#  output$dynamic_link <- renderUI({
#    tags$a("Download the journal database", href = input$site_choice, target = "_blank")
    
#  })
  
  
#  data <- reactive({
#    get(input$site_choice)
#  })
#  
#  output$downloadData <- downloadHandler(
#    filename = function() {
#      # Use the selected dataset as the suggested file name
#      paste0(input$site_choice)
#    },
#    content = function(file) {
#      # Write the dataset to the `file` that will be downloaded
#      write.csv(data(), file)
#    }
#  )
  

    # Also, I moved the brackets from around reactive to up to the server
    output$fileselected<-renderText({
      paste0('You have selected: ', input$selectfile)
    })
    
    # reactive({#I was told it's not a good idea to put outputs within reactives, so I blocked this out
    # fullpath <- file.path(fpath,input$selectfile)
    # req(fullpath)
    # df <- read.csv(fullpath, header = TRUE,  sep = ",")})
    # output$fileselected <- renderText({
    #   paste0('You have selected: ', input$selectfile)
    # })
    # req(df)
    output$txtout <- renderDataTable({
      req(input$selectfile)
      fullpath <- file.path(fpath,input$selectfile)
      df <- read.csv(fullpath, header = TRUE,  sep = ",")
      head(df)
    }, options =list(pageLength = 10))
    
  }
  
  

shinyApp(ui, server)