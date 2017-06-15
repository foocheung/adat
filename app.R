options(warn = -1)
options(shiny.maxRequestSize = 30 * 1024 ^ 2)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
##library(RMySQL)
library(d3heatmap)
library(ggfortify)

library(readr)
library("genefilter")
source("chooser.R")

library(stats)
library(shinyjs)

#Version 0.01
#Added more Comments

# Help Pages url

vidurl <-
  paste0("https://www.youtube.com/embed/GdhxiC2Nxfc?rel=0&amp;controls=0&amp;showinfo=")
vidurl2 <-
  paste0("https://www.youtube.com/embed/Z-g_Yv4RCmQ?rel=0&amp;controls=0&amp;showinfo=")
vidurl3 <-
  paste0("https://www.youtube.com/embed/YPDTyuQ5o9E?rel=0&amp;controls=0&amp;showinfo=")
vidurl4 <-
  paste0("https://www.youtube.com/embed/sRaO7_oBDK4?rel=0&amp;controls=0&amp;showinfo=")

ui <- dashboardPage(
  dashboardHeader(
    title = "CHI",
    
    tags$li(
      class = "dropdown",
      tags$a(href = "mailto:?Subject=https://foocheung.shinyapps.io/soma_stats/ ",
             tags$img(height = "18px",
                      src = "email.png"))
    ),
    
    # Twitter Sharing Link
    tags$li(
      class = "dropdown",
      tags$a(
        href = "http://twitter.com/share?url=https://foocheung.shinyapps.io/soma_stats/&text=Web Tool For Navigating and Plotting ADAT files",
        target = "_blank",
        tags$img(height = "18px",
                 src = "twitter.png")
      )
    ),
    
    # Facebook Sharing link
    tags$li(
      class = "dropdown",
      tags$a(
        href = "http://www.facebook.com/sharer.php?u=https://foocheung.shinyapps.io/soma_stats/",
        target = "_blank",
        tags$img(height = "18px",
                 src = "facebook.png")
      )
    ),
    
    # LinkedIn Sharing link
    tags$li(
      class = "dropdown",
      tags$a(
        href = "http://www.linkedin.com/shareArticle?mini=true&url=https://foocheung.shinyapps.io/soma_stats/",
        target = "_blank",
        tags$img(height = "18px",
                 src = "linkedin.png")
      )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      # ADAT
        menuItem(
        "Upload ADAT file",
        tabName = "adat",
        icon = shiny::icon("file-text")
      ),
      
      menuItem(
        # BoxPlots 
          "BoxPlots",
        tabName = "BoxPlots",
        icon = shiny::icon("line-chart")
      ),
      
      
      
      menuItem(
        # Heatmap and stats
        "Heatmap And Statistics",
        tabName = "Heatmap",
        icon = shiny::icon("table")
      ),
      #  PCA
      menuItem("PCA", tabName = "PCA", icon = shiny::icon("braille")),
     
       # Download
      menuItem(
        "Download",
        tabName = "DownloadData",
        icon = shiny::icon("download")
      ),
      menuItem("Help", tabName = "Help", icon = shiny::icon("info"))
      
      ,
      hr(),
      menuItem(
        "Paper",
         icon = shiny::icon("file-pdf-o"),
        href = "http://openresearchsoftware.metajnl.com"),
      menuItem(
        "Source code",
        icon = icon("file-code-o"),
        href = "https://github.com/foocheung/adat"
      ),
      menuItem("Bug Reports", icon = icon("bug"),
               href = "https://github.com/foocheung/adat/issues"),
      menuItem("How To Guide", icon = icon("file-pdf-o"),
               href = "howto.pdf")
      
    
      
      ),
    HTML('<br><br><br><br>'),
    HTML('<p><center>Further Help ? <br>Contact the developer at <font color="cyan"><br> foo.cheung @ nih . gov </font></center>')
    
  ),
  
  dashboardBody(
    includeCSS("www/custom.css"),
    useShinyjs(),
    fluidRow(
      tabItems(
        # First tab content
        tabItem(
          tabName = "adat",
          column(
            8,
            offset = 1,
            img(src = 'https://chi.nhlbi.nih.gov/web/sites/all/themes/irptheme/logo.png', align = "center")
          ),
          column(
            10,
            offset = 1,
            h1("Web Tool For Navigating and Plotting ADAT files", style = "font-family: 'Source Sans Pro';")
          ),
          
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          
          # Upload ADAT UI
          column(
            12,
            offset = 1,
            box(
              fileInput('file1', h3("Step 1. Upload ADAT File"),
                        accept = c('adat')),
              actionButton("show", "Help", icon("question-circle"),
                           style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              tags$a(
                href = 'PLASMA.1.3k.HybNorm.MedNorm.Cal.20151030.adat',
                target = "_blank",
                class = "btn",
                icon("download"),
                'Download Test ADAT file'
              )
              
            ),
            br(),
            br(),
            br(),
            br()
            
            
          )
          
          
          
        ),
        
        
        # UI calls for sample filtering, boxplots, heatmaps,ttest and dawnload datasets
        
        tabItem(tabName = "filter"),
        
        tabItem(tabName = "BoxPlots",
                uiOutput('moreControls2')),
        tabItem(
          tabName = "Heatmap",
          
          
          uiOutput('filter_sample'),
          
          uiOutput('hmap'),
          
          uiOutput('moreControls_stat'),
          
          
          box(
            width = 7,
            solidHeader = TRUE,
            dataTableOutput('ttestout')
          )
          
          
          
        ),
        
        
        tabItem(tabName = "PCA"
                ,
                uiOutput('moreControls3')),
        
        tabItem(
          tabName = "Help"
          ,
          column(10,
                 offset = 1,
                 h1("Help")),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          column(8, offset = 1, htmlOutput("frame"))
          
        ),
        
        
        tabItem(tabName = "DownloadData",
                
                uiOutput('moreControls4'))
        
        
        
      )
    )
  )
)





shinyApp(
  ui,
  server = function(input, output) {
  
    
    
    # Hide sidebar until ADAT file is uploaded
    addClass(selector = "body", class = "sidebar-collapse")

    # iframes for youtube 
    
    output$frame <- renderUI({
      help1 <- tags$iframe(src = vidurl,
                           width = 560,
                           height = 315)
      help2 <- tags$iframe(src = vidurl2,
                           width = 560,
                           height = 315)
      help3 <- tags$iframe(src = vidurl3,
                           width = 560,
                           height = 315)
      help4 <- tags$iframe(src = vidurl4,
                           width = 560,
                           height = 315)
      
      
      box(
        width = 12,
        h4("Step 1: Upload ADAT Files"),
        print(help1),
        br(),
        h4("BoxPlots"),
        print(help2),
        br(),
        h4("Heatmap and Statistics"),
        print(help3),
        br(),
        h4("PCA"),
        print(help4)
        
        
      )
      
    })
    
    
    # read in files containing GO process and Disease ID <-> somamer mapping 
    gotable1 <- read_csv("./process", col_names =  TRUE)
    gotable <- read_csv("./disease", col_names =  TRUE)
    gotable2 <- rbind(gotable, gotable1)
    
    
    observe({
      if (is.null(input$file1)) {
        
      }
      else{
        removeClass(selector = "body", class = "sidebar-collapse")
        
      }
      
      
    })
    
    
    
    
    # Parse ADAT file
    
    filedata <- reactive({
      req(input$file1)
      
      inFile <- input$file1
      is.null(inFile)
      
      
      
      withProgress(message = 'Step 1 Loading File and Plotting Data in progress',
                   detail = 'This may take a while...',
                   value = 0,
                   {
                     for (i in 1:5) {
                       incProgress(1 / 15)
                       Sys.sleep(0.25)
                     }
                   })
      
      param2.indic <- "^\\s+EntrezGeneID"
      param3.indic <- "^PlateId"
      param4.indic <- "^\\s+SomaId"
      param5.indic <- "^\\s+Organism"
      param6.indic <- "\\^TABLE_BEGIN"
      
      
      lines <- readLines(inFile$datapath)
      p2.start <- grep(param2.indic, lines)
      p3.start <- grep(param3.indic, lines)
      
      p4.start <- grep(param4.indic, lines)
      p4.stop <- grep(param5.indic, lines)
      p6.start <- grep(param6.indic, lines)
      
      myData <-
        read.table(
          inFile$datapath,
          header = T,
          fill = TRUE,
          skip = p2.start ,
          sep = "\t"
        )
      
      
      b <- myData %>% collect()
      
      
      myData2 <-
        read.table(
          inFile$datapath,
          header = T,
          fill = TRUE,
          skip = p3.start - 1 ,
          sep = "\t"
        )
      
      
      
      c <- myData2 %>% collect()
      
      d <- c[,!grepl("X\\.|^X$", colnames(c))]
      e <- b[,!grepl("X\\.|^X$", colnames(b))]
      aa <- c(colnames(d), colnames(e))
      
      myData3a <-
        read.table(
          inFile$datapath,
          fill = TRUE,
          skip = p3.start ,
          col.names = aa,
          sep = "\t"
        )
      myData3 <- myData3a
      
      
      
      
      enterez_GS_col <- grep("EntrezGeneSymbol", colnames(myData3))
      enterez_GS_col  <- enterez_GS_col + 1
      length_col <- ncol(myData3)
      
      matrix_data <- myData3[, enterez_GS_col:length_col]
      
      rownames(matrix_data) <-
        (make.names(myData3$SampleId, unique = TRUE))
      
      
      return(
        list(
          "myData3" = myData3,
          "matrix_data" = matrix_data,
          "enterez_GS_col" = enterez_GS_col,
          "length_col" = length_col
        )
      )
    })
    
    
    # Generat Heatmap
    
    output$hmap <- renderUI({
      matrix_data <- filedata()$matrix_data
      
      if (nrow(matrix_data) * 25 < 400)
      {
        he = 400
        
      }
      else {
        he =  25 * nrow(matrix_data)
        
      }
      
      
      tagList(
        box(
          title = "Heatmap",
          width = 12,
          d3heatmapOutput("heatmap", height = paste0(he, "px")),
          status = "success",
          solidHeader = TRUE,
          footer = "Hover for details. May take several seconds to load."
          
          
          
          
        )
      )
      
      
      
    })
    
    
    # Generate PCA page 
    
    output$moreControls3 <- renderUI({
      matrix_data <- filedata()$matrix_data
      
      matrix_data <- matrix_data[, order(colnames(matrix_data))]
      
      myData3 <- filedata()$myData3
      
      myData3 <- myData3[order(myData3$SampleId), ]
      
      
      tagList(
        fluidRow(
          width = 12,
          
          
          box(
            title = "Options",
            status = "primary",
            width = 2,
            solidHeader = TRUE,
            checkboxInput("showcolor2", "Add Colors", TRUE),
            conditionalPanel(
              "input.showcolor2 > 0",
              selectInput(
                inputId = "color2",
                label = "Color",
                names(myData3)[0:29],
                names(myData3)[18]
              )
            ),
            checkboxInput("showshape2", "Add Shapes", FALSE) ,
            conditionalPanel(
              "input.showshape2 > 0",
              selectInput(
                inputId = "shape2",
                label = "Shape",
                names(myData3)[0:29],
                names(myData3)[15]
              )
            ),
            checkboxInput("showcustom", "Predefined Sets/User Defined", TRUE),
            checkboxInput("label", "Apply labelling"),
            
            actionButton("show4", "Help", icon("question-circle"),
                         style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          conditionalPanel(
            "input.showcustom > 0 ",
            box(
              title = "GO Ontology And Disease",
              width = 6,
              status = "primary",
              solidHeader = TRUE,
              selectInput(
                "go_fun2",
                "GO Ontology And Disease",
                selected = "All Proteins",
                list(
                  "GO Process" =  c(gotable$name),
                  "Disease ID" = c(gotable2$name),
                  "Custom" = c("All Proteins", "")
                )
              )
            )
          ),
          conditionalPanel(
            "input.showcustom == 0 ",
            
            box(
              width = 6,
              title = "Select Proteins",
              status = "primary",
              solidHeader = TRUE,
              chooserInput(
                "mychooser33",
                "Available frobs",
                "Selected frobs",
                colnames(matrix_data),
                colnames(matrix_data[50:55]),
                size = 10,
                multiple = TRUE
              )
              
            )
          ),
          
          box(
            width = 4,
            title = "Select Samples",
            status = "primary",
            solidHeader = TRUE,
            chooserInput(
              "mychooser3",
              "Available frobs",
              "Selected frobs",
              myData3$SampleId,
              c(),
              size = 10,
              multiple = TRUE
            )
          )
          
          
          
        ),
        
        box(
          title = "PCA",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("pca1", height = 300)
        ),
        box(
          title = "Proportion of Variance Explained",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotOutput("pca2", height = 300)
        )
        
      )
      
    })
    
    
    
    # Generate t-test UI 
    
    output$moreControls_stat <- renderUI({
      req(input$file1)
    
      myData3 <- filedata()$myData3
    
      matrix_data <- selection()$go_samp_matrix_data
      
      
      
      
      
      if (length(input$mychooser$right) < 2)
        samp_myData3 <-
        myData3[grep(paste("", collapse = '|'), myData3$SampleId, ignore.case =
                       TRUE),]
      
      
      else if (input$rowlabel == "SampleId") {
        rownames(myData3) <-
          (make.names(paste(myData3$SampleId, sep = "_"), unique = TRUE))
        
        samp_myData3 <-
          myData3[grep(
            paste0("^", input$mychooser$right, "$",  collapse = '|'),
            rownames(myData3),
            ignore.case = TRUE
          ),]
      }
      
      else if (input$rowlabel == "SampleGroup") {
        rownames(myData3) <-
          (make.names(
            paste(myData3$SampleId, myData3$SampleGroup, sep = "_"),
            unique = TRUE
          ))
        
        samp_myData3 <-
          myData3[grep(
            paste0("^", input$mychooser$right, "$",  collapse = '|'),
            rownames(myData3),
            ignore.case = TRUE
          ),]
      }
      
      
      
      rnmat <- rownames(matrix_data)
      matrix_data <-
        matrix_data[order(row.names(matrix_data)), ]
      
      
      if (is.null(input$lock) || input$lock == 'FALSE') {
   
        
        tagList(
          box(
            title = "t-test: Select 2 Groups",
            width = 4,
            status = "success",
            solidHeader = TRUE,
            chooserInput(
              "mychooser2",
              "Available frobs2",
              "Selected frobs2",
              rownames(matrix_data) ,
              c(),
              
              size = 7,
              multiple = TRUE
            ),
            br(),
            checkboxInput("lock", "Lock", FALSE),
            downloadButton('downloadData2', 'Download')
            
            
          )
        )
      }
      
      
      else{
        input$mychooser$right
        
        
        tagList(
          box(
            title = "t-test: Select 2 Groups",
            width = 4,
            status = "success",
            solidHeader = TRUE,
            chooserInput(
              "mychooser2",
              "Available frobs2",
              "Selected frobs2",
              input$mychooser2$left,
              input$mychooser2$right,
              size = 7,
              multiple = TRUE
            ),
            br(),
            checkboxInput("lock", "Lock", TRUE),
            downloadButton('downloadData2', 'Download')
            
            
          )
        )
        
        
      }
      
    })
    
    
    
    
    
    
    # Help dialogue Upload ADAT
    
    observeEvent(input$show, {
      showModal(modalDialog(title = "Step1 Upload ADAT File",
                            tags$iframe(
                              src = vidurl,
                              width = 560,
                              height = 315
                            )))
    })
    
    # Help dialogue Box Plots
    
    observeEvent(input$show2, {
      showModal(modalDialog(title = "Box Plots",
                            tags$iframe(
                              src = vidurl2,
                              width = 560,
                              height = 315
                            )))
    })
    
    
    # Help dialogue Heatmap and Statistics
    
    observeEvent(input$show3, {
      showModal(modalDialog(title = "Heatmap and Statistics",
                            tags$iframe(
                              src = vidurl3,
                              width = 560,
                              height = 315
                            )))
    })
    
    
    # Help dialogue PCA 
    
    observeEvent(input$show4, {
      showModal(modalDialog(title = "PCA",
                            tags$iframe(
                              src = vidurl4,
                              width = 560,
                              height = 315
                            )))
    })
    
    
    # Function to download data file
    
    output$downloadData2 <- downloadHandler(
      filename = function() {
        paste(input$go_fun  , '.csv', sep = '')
      },
      content = function(file) {
        write.csv(ttest()$ttestmat, file)
      }
    )
    
    
    
   # Generate Heatmap and statistics page
    
    output$filter_sample <- renderUI({
      myData3 <- filedata()$myData3
      matrix_data <- filedata()$matrix_data
      matrix_data <- matrix_data[, order(colnames(matrix_data))]
      myData3 <- myData3[order(myData3$SampleId), ]
      
      tagList(
        fluidRow(
          width = 12,
          box(
            title = "Options",
            status = "success",
            width = 2,
            solidHeader = TRUE,
            selectInput(
              "palette",
              "Palette",
              c("YlOrRd", "RdYlBu", "Greens", "Blues")
            ),
            selectInput(
              "rowlabel",
              "LabelRowsBy",
              c(
                "SampleId",
                "SampleGroup",
                "TimePoint",
                "SampleDescription"
              )
            ) ,
            
            
            checkboxInput("cluster", "Apply clustering"),
            checkboxInput("showcustomhm", "Predefined Sets/User Defined", TRUE),
            actionButton("goButton", "Go!"),
            actionButton("show3", "Help" , icon("question-circle"),
                         style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
            
            
            
            
          ) ,
          
          conditionalPanel(
            "input.showcustomhm > 0 ",
            box(
              width = 6,
              title = "GO Ontology And Disease",
              status = "success",
              solidHeader = TRUE,
              selectInput(
                "go_fun",
                "GO Ontology And Disease",
                selected = "immune_response",
                list(
                  "GO Process" =  c(gotable$name),
                  "Disease ID" = c(gotable2$name),
                  "Custom" = c("All Proteins", "")
                )
                
                
              )
            )
          ),
          conditionalPanel(
            "input.showcustomhm == 0 ",
            box(
              width = 6,
              title = "Select Proteins",
              status = "success",
              solidHeader = TRUE,
              chooserInput(
                "mychooser66",
                "Available frobs",
                "Selected frobs",
                colnames(matrix_data)  ,
                colnames(matrix_data[50:55]),
                size = 10,
                multiple = TRUE
              )
              
            )
          ),
          conditionalPanel(
            condition = "input.rowlabel == 'SampleId'" ,
            box(
              width = 4,
              title = "Select Samples",
              status = "success",
              solidHeader = TRUE,
              chooserInput(
                "mychooser",
                "Available frobs",
                "Selected frobs",
                
                make.names(paste(myData3$SampleId, sep = "_"), unique = TRUE),
                c(),
                size = 10,
                multiple = TRUE
              )
            )
            
          ),
          conditionalPanel(
            condition = "input.rowlabel == 'SampleGroup'" ,
            box(
              width = 4,
              title = "Select Samples",
              status = "success",
              solidHeader = TRUE,
              chooserInput(
                "mychooser10",
                "Available frobs",
                "Selected frobs",
                make.names(
                  paste(myData3$SampleId, myData3$SampleGroup, sep = "_"),
                  unique = TRUE
                ),
                c(),
                size = 8,
                multiple = TRUE
              )
            )
            
          ),
          conditionalPanel(
            condition = "input.rowlabel == 'TimePoint'" ,
            box(
              width = 4,
              title = "Select Samples",
              status = "success",
              solidHeader = TRUE,
              chooserInput(
                "mychooser11",
                "Available frobs",
                "Selected frobs",
                make.names(
                  paste(myData3$SampleId, myData3$TimePoint, sep = "_"),
                  unique = TRUE
                ),
                c(),
                size = 8,
                multiple = TRUE
              )
            )
            
          ),
          conditionalPanel(
            condition = "input.rowlabel == 'SampleDescription'" ,
            box(
              width = 4,
              title = "Select Samples",
              status = "success",
              solidHeader = TRUE,
              chooserInput(
                "mychooser12",
                "Available frobs",
                "Selected frobs",
                make.names(
                  paste(myData3$SampleId, myData3$SampleDescription, sep = "_"),
                  unique = TRUE
                ),
                c(),
                size = 8,
                multiple = TRUE
              )
            )
            
          )
         
          
        )
        
        
      )
      
      
    })
    
    
    
    
    # t-test and Mann-Whitney calculations
    
    output$ttestout <- renderDataTable(ttest()$ttestmat,
                                       options = list(pageLength = 5))
    
    
    ttest <- reactive({
      req(input$file1)
      
      go_samp_matrix_data <- selection()$go_samp_matrix_data
      
      m1 <-
        go_samp_matrix_data[grep(
          paste(input$mychooser2$left, collapse = '|'),
          rownames(go_samp_matrix_data),
          ignore.case = TRUE
        ),]
      m2 <-
        go_samp_matrix_data[grep(
          paste(input$mychooser2$right, collapse = '|'),
          rownames(go_samp_matrix_data),
          ignore.case = TRUE
        ),]
      
      ttestmat1 <-
        sapply(seq(ncol(m1)), function(x)
          f(m1[, x], m2[, x]))
      
      ttestmat1 <- unlist(ttestmat1)
      
      wilcoxmat1 <-
        sapply(seq(ncol(m1)), function(x)
          w(m1[, x], m2[, x]))
      
      wilcoxmat1 <- unlist(wilcoxmat1)
      wilpadj <-
        p.adjust(sapply(wilcoxmat1, function(x)
          paste(unlist(x), collapse = "")), method = "fdr")
      
      
      padj <-
        p.adjust(sapply(ttestmat1, function(x)
          paste(unlist(x), collapse = "")), method = "fdr")
      
      somamers <- unlist(colnames(m1))
      
      
      
    #Generate statistics table  
      foo <-
        list(
          df2 = data_frame(ttestmat1),
          df4 = data_frame(wilcoxmat1),
          df1 = data.frame(padj),
          df5 = data.frame(wilpadj),
          ff3 = data.frame(somamers)
        )
      
      
      ttestmat <- do.call("cbind", foo)
      colnames(ttestmat) <-
        c(
          't-test',
          'Wilcoxin Mann-Whitney' ,
          't-test (fdr)',
          'Wilcoxin Mann-Whitney (fdr)',
          'id'
        )
      return(list("ttestmat" = ttestmat))
      
    })
    
    # Parse out the user selection from Heatmap and Statistics page
    selection <- reactive({
      req(input$go_fun)
      
      myData3 <- filedata()$myData3
      enterez_GS_col <- filedata()$enterez_GS_col
      length_col <- filedata()$length_col
      
      
      if (input$rowlabel == "SampleId") {
        rownames(myData3) <-
          (make.names(paste(myData3$SampleId, sep = "_"), unique = TRUE))
        
        
        
        samp_myData3 <-
          myData3[grep(
            paste0("^", input$mychooser$right, "$",  collapse = '|'),
            rownames(myData3),
            ignore.case = TRUE
          ),]
      }
      else if (input$rowlabel == "SampleGroup") {
        rownames(myData3) <-
          (make.names(
            paste(myData3$SampleId, myData3$SampleGroup, sep = "_"),
            unique = TRUE
          ))
        
        samp_myData3 <-
          myData3[grep(
            paste0("^", input$mychooser10$right, "$",  collapse = '|'),
            rownames(myData3),
            ignore.case = TRUE
          ),]
        
      }
      else if (input$rowlabel == "TimePoint") {
        rownames(myData3) <-
          (make.names(
            paste(myData3$SampleId, myData3$TimePoint, sep = "_"),
            unique = TRUE
          ))
        samp_myData3 <-
          myData3[grep(
            paste0("^", input$mychooser11$right, "$",  collapse = '|'),
            rownames(myData3),
            ignore.case = TRUE
          ),]
      } else if (input$rowlabel == "SampleDescription") {
        rownames(myData3) <-
          (make.names(
            paste(myData3$SampleId, myData3$SampleDescription, sep = "_"),
            unique = TRUE
          ))
        
        samp_myData3 <-
          myData3[grep(
            paste0("^", input$mychooser12$right, "$",  collapse = '|'),
            rownames(myData3),
            ignore.case = TRUE
          ),]
      }
      
      
      
      if ((length(input$mychooser$right) < 2) &
          (input$rowlabel == "SampleId"))
        samp_myData3 <-
        myData3[grep(paste("", collapse = '|'), myData3$SampleId, ignore.case =
                       TRUE),]
      
      
      if ((length(input$mychooser10$right) < 2) &
          (input$rowlabel == "SampleGroup"))
        samp_myData3 <-
        myData3[grep(paste("", collapse = '|'),
                     myData3$SampleGroup,
                     ignore.case =
                       TRUE),]
      
      if ((length(input$mychooser11$right) < 2) &
          (input$rowlabel == "TimePoint"))
        samp_myData3 <-
        myData3[grep(paste("", collapse = '|'), myData3$TimePoint, ignore.case =
                       TRUE),]
      
      if ((length(input$mychooser12$right) < 2) &
          (input$rowlabel == "SampleDescription"))
        samp_myData3 <-
        myData3[grep(paste("", collapse = '|'),
                     myData3$SampleDescription,
                     ignore.case =
                       TRUE),]
      
      
      enterez_GS_col <-
        grep("EntrezGeneSymbol", colnames(samp_myData3))
      enterez_GS_col  <- enterez_GS_col + 1
      length_col <- ncol(samp_myData3)
      
      
      samp_matrix_data <- samp_myData3[, enterez_GS_col:length_col]
      
      
      
      goselected_i <- input$go_fun
      
      
      if (input$showcustomhm == 0) {
        go_samp_matrix_data <-
          samp_matrix_data[, grepl(
            paste0("^", input$mychooser66$right, "$",  collapse = '|'),
            colnames(samp_matrix_data),
            ignore.case = TRUE
          ),]
        
      }
      else if (goselected_i == 'All Proteins') {
        go_samp_matrix_data <- samp_matrix_data[,-1]
        
      }
      else{
        goselected <-
          gotable2 %>% filter(name == goselected_i) %>% select(somamer) %>% collect()
        go_samp_matrix_data <-
          samp_matrix_data[, grepl(goselected,  colnames(samp_matrix_data))]
        
      }
      
      
      cnames <- colnames(go_samp_matrix_data)
      
     
      return(
        list(
          "cnames" = cnames,
          "samp_matrix_data" = samp_matrix_data,
          "myData3" = myData3,
          "go_samp_matrix_data" = go_samp_matrix_data,
          "enterez_GS_col" = enterez_GS_col,
          "length_col" = length_col
        )
      )
      
    })
    
    
    #Functions for t-test and wilcoxin test
    
    w <- function(x, y) {
      test <- wilcox.test(x, y, paired = FALSE)
      out <- data.frame(pval = as.numeric(test$p.value))
      
      
      
      return(out)
    }
    
    f <- function(x, y) {
      test <- t.test(x, y, paired = FALSE)
      out <-
        data.frame(pval = as.numeric(sprintf("%.3f", test$p.value)))
      
      
      
      return(out)
    }
    
    
    
    # Generate Box Plot page
    
    output$moreControls2 <- renderUI({
      df <- filedata()$matrix_data
      myData3 <- filedata()$myData3
      if (is.null(df))
        return(NULL)
      
      
      
      withProgress(message = 'Step Parsing Data',
                   detail = 'This may take a while...',
                   value = 0,
                   {
                     for (i in 1:5) {
                       incProgress(1 / 15)
                       Sys.sleep(0.25)
                     }
                   })
      
      
      myData3 <- myData3[order(myData3$SampleId), ]
      
      tagList(fluidRow(
        width = 12,
        box(
          width = 5,
          title = "Box Plots",
          plotlyOutput("plot"),
          height = 820,
          status = "info",
          solidHeader = TRUE
        ),
        
        box(
          width = 2,
          height = 820,
          solidHeader = TRUE,
          title = "Options",
          status = "info",
          selectInput(
            inputId = "x",
            label = "X",
            names(myData3),
            names(myData3)[[1]],
            selectize = FALSE
          ) ,
          selectInput(
            inputId = "y",
            label = "Select Protein",
            names(myData3[, 29:ncol(myData3)]),
            names(myData3)[[32]],
            selectize = FALSE
          ) ,
          checkboxInput("showcolor", "Add Colors", TRUE),
          conditionalPanel(
            "input.showcolor > 0",
            selectInput(
              inputId = "color",
              label = "color",
              names(myData3),
              names(myData3)[[1]],
              selectize = FALSE
            )
          ) ,
          checkboxInput("showshape", "Add Shapes") ,
          conditionalPanel(
            "input.showshape > 0",
            selectInput(
              inputId = "shape",
              label = "shape",
              names(myData3[, 0:29]),
              names(myData3)[[1]],
              selectize = FALSE
            )
          ) ,
          selectInput(
            "ggplot_scaletype",
            "Scale type",
            c(
              "normal" = "normal",
              "Reverse direction of y axis " = "reverse",
              "Plot y on log10 scale" = "log10",
              "Plot y on log2 scale)" = "log2",
              "log10 (coord_trans())" = "log10_trans",
              "log2 (coord_trans())" = "log2_trans",
              "coord_cartesian()" = "coord_cartesian",
              "coord_flip()" = "coord_flip",
              "coord_polar() (doesn't work)" = "coord_polar",
              "x factor" = "x_factor",
              "date and time" = "datetime"
            ),
            selectize = FALSE
          ),
          
          
          
          textInput('title', 'title', "Title")  ,
          textInput('xlab', 'x-axis label', "X"),
          textInput('ylab', 'y-axis label', "Y"),
          checkboxInput("joindp", "Join Datapoints"),
          sliderInput(
            'jitter',
            'Jitter',
            min = 0,
            max = 2,
            value = 0,
            step = 0.1
          ),
          
          actionButton("show2", "Help", icon("question-circle"),
                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
        ),
        
        box(
          title = "Annotation",
          height = 600,
          width = 2,
          (DT::dataTableOutput("db")),
          status = "info",
          solidHeader = TRUE
        ) ,
        box(
          width = 3
          ,
          height = 400,
          title = "Select Samples",
          status = "info",
          solidHeader = TRUE,
          chooserInput(
            "mychooser1",
            "Available frobs",
            "Selected frobs",
            myData3$SampleId,
            c(),
            size = 10,
            multiple = TRUE
          )
        )
        
      ))
      
    })
    
    # Plot the BoxPlot
    
    output$plot <- renderPlotly({
      myData3 <- filedata()$myData3
      matrix_data <- filedata()$matrix_data
      
      
      
      if (length(input$mychooser1$right) < 2)
        plot_myData3 <-
        myData3[grep(paste("", collapse = '|'), myData3$SampleId, ignore.case =
                       TRUE),]
      else{
        plot_myData3 <-
          myData3[grep(
            paste0("^", input$mychooser1$right, "$",  collapse = '|'),
            myData3$SampleId,
            ignore.case = TRUE
          ),]
        
      }
      
      myData3 <- plot_myData3
      
      
      
      if (is.null(df))
        return(NULL)
      
      if (input$showcolor && input$showshape) {
        gp = geom_point(data = myData3,
                        aes(
                          x = factor(get(input$x)),
                          y = as.numeric(as.character(get(input$y))) ,
                          color = factor(get(input$color)),
                          shape = as.factor(get(input$shape))
                        ))
      }
      
      else if (input$showcolor && !input$showshape) {
        gp = geom_point(data = myData3, aes(
          x = factor(get(input$x)),
          y = as.numeric(as.character(get(input$y)))  ,
          color = factor(get(input$color))
        ))
        
      }
      else if (!input$showcolor && input$showshape) {
        gp = geom_point(data = myData3, aes(
          x = factor(get(input$x)),
          y = as.numeric(as.character(get(input$y))) ,
          shape = as.factor(get(input$shape))
        ))
      }
      else{
        gp = geom_point(data = myData3, aes(x = factor(get(input$x)), y = as.numeric(as.character(get(
          input$y
        )))))
        
      }
      
      
      
      
      if (input$joindp) {
        p <-
          ggplot(data = myData3, aes(
            text = paste(
              "SampleId:",
              myData3$SampleId,
              "<BR>",
              "SampleDescription:",
              myData3$SampleDescription,
              "<BR>",
              "SampleGroup:",
              myData3$SampleGroup,
              "<BR>",
              "Value:",
              as.numeric(as.character(get(input$y))),
              "<BR>"
            ),
            x = factor(get(input$x)),
            y = as.numeric(as.character(get(input$y)))
          )) +
          geom_line(data = myData3,
                    aes(
                      x = factor(get(input$x)),
                      y = as.numeric(as.character(get(input$y)))  ,
                      color = get(input$color),
                      group = deparse(substitute(get(input$color)))
                      
                    ))   +
          
          geom_point(data = myData3,
                     aes(color = get(input$color), shape = as.factor(get(input$shape)))) +
          
          theme(
            text = element_text(size = 10),
            axis.text.x = element_text(angle = 90, hjust = 1),
            legend.title = element_blank()
          )  +  ggtitle(input$title) +  xlab(input$xlab) +
          ylab(input$ylab)
      }
      else{
        p <-
          ggplot(data = myData3, aes(
            x = factor(get(input$x)),
            y = as.numeric(as.character(get(input$y))),
            text = paste(
              "SampleId:",
              myData3$SampleId,
              "<BR>",
              "SampleDescription:",
              myData3$SampleDescription,
              "<BR>",
              "SampleGroup:",
              myData3$SampleGroup,
              "<BR>",
              "Value:",
              as.numeric(as.character(get(input$y))),
              "<BR>"
            )
          ))     +
          gp +
    
          theme(
            text = element_text(size = 10),
            axis.text.x = element_text(angle = 90, hjust = 1),
            legend.title = element_blank()
          )  +  ggtitle(input$title) +  xlab(input$xlab) +
          ylab(input$ylab)  + geom_boxplot()
      }
      
      
      
      
      if (input$jitter) {
        p  <-
          p +  geom_jitter(data = myData3,
                           aes(
                             x = factor(get(input$x)),
                             y = as.numeric(as.character(get(input$y)))  ,
                             color = get(input$color)
                           ))
      }
      else{
        
      }
      
      # p<- p + theme(legend.title = element_blank() )
      ## p<- p + theme(legend.title = element_text(paste(input$color) ) )
      
      p <- switch(
        input$ggplot_scaletype,
        normal =
          p,
        reverse =
          p + scale_y_reverse(),
        log10 =
          p + scale_y_log10(),
        log2 =
          p +
          scale_y_continuous(trans = scales::log2_trans()),
        log10_trans =
          p + coord_trans(y = "log10"),
        log2_trans =
          p + coord_trans(y = "log2"),
        coord_cartesian =
          p + coord_cartesian(xlim = c(2, 4), ylim = c(0, 50)),
        coord_flip =
          p + coord_flip(),
        coord_polar =
          p + coord_polar(),
        # Discrete x, continuous y
        x_factor =
          p,
        # Datetime x, Date y
        datetime =
          p
      )
      
      
      
      
      
      p <- ggplotly(p, tooltip = "text")
      
      
      
    })
    
    
    # Parse out ADAT and generate Annotation table
    output$db <- DT::renderDataTable({
      myData3 <- filedata()
      
      inFile <- input$file1
      is.null(inFile)
      
      lines <- readLines(inFile$datapath)
      param2.indic <- "^\\s+EntrezGeneID"
      param3.indic <- "^PlateId"
      
      param6.indic <- "\\^TABLE_BEGIN"
      p6.start <- grep(param6.indic, lines)
      p2.start <- grep(param2.indic, lines)
      p3.start <- grep(param3.indic, lines)
      
      myData <-
        read.table(
          inFile$datapath,
          header = T,
          fill = TRUE,
          skip = p2.start ,
          sep = "\t"
        )
      b <- myData %>% collect()
      myData2 <-
        read.table(
          inFile$datapath,
          header = T,
          fill = TRUE,
          skip = p3.start - 1 ,
          sep = "\t"
        )
      c <- myData2 %>% collect()
      
      
      
      enterez_GS_col <- grep("EntrezGeneSymbol", colnames(myData3))
      enterez_GS_col  <- enterez_GS_col + 1
      length_col <- ncol(myData3)
      ss <- p6.start
      
      d <- c[,!grepl("X\\.|^X$", colnames(c))]
      e <- b[,!grepl("X\\.|^X$", colnames(b))]
      
      
      
      myData4 <-
        read.csv(
          inFile$datapath,
          blank.lines.skip = TRUE,
          skip = ss,
          nrows = 10,
          sep = "\t",
          col.names = c(colnames(d), colnames(e))
        )
      
      
      
      DT::datatable(myData4[, input$y, drop = FALSE], options = list(dom = 't', pageLength =
                                                                       10))
      
      
      
    })
    
    #Render Heatmap
    output$heatmap <- renderD3heatmap({
      if (is.null(input$goButton))
        return()

      
      isolate({
        matrix_data <- selection()$go_samp_matrix_data
        
        myData3 <- selection()$myData3
        
        
        
        
        
        
        d3heatmap(
          scale(matrix_data),
          colors = input$palette,
          dendrogram = if (input$cluster)
            "both"
          else
            "none"
          
        )
        
        
        
      })
    })
    
    
    
    
    
    #Parse out user requirements from PCA
    pca_selection <- reactive({
      myData3 <- filedata()$myData3
      enterez_GS_col <- filedata()$enterez_GS_col
      length_col <- filedata()$length_col
      matrix_data <- filedata()$matrix_data
      
      
      rownames(myData3) <-
        (make.names(paste(myData3$SampleId), unique = TRUE))
      
      
      if (length(input$mychooser3$right) < 2)   {
        pca_samp_myData3 <-
          myData3[grep(paste("", collapse = '|'), myData3$SampleId, ignore.case =
                         TRUE),]
      }
      else{
        pca_samp_myData3 <-
          myData3[grep(
            paste0("^", input$mychooser3$right, "$", collapse = '|'),
            myData3$SampleId,
            ignore.case = TRUE
          ),]
        
        
      }
      
      
      enterez_GS_col <-
        grep("EntrezGeneSymbol", colnames(pca_samp_myData3))
      enterez_GS_col  <- enterez_GS_col + 1
      length_col <- ncol(pca_samp_myData3)
      
      
      pca_samp_matrix_data <-
        pca_samp_myData3[, enterez_GS_col:length_col]
      
   
      if (input$showcustom == 0) {
        chooser <- input$mychooser33$right
        
        h <-
          pca_samp_matrix_data[, grepl(
            paste0("^", input$mychooser33$right, "$",  collapse = '|'),
            colnames(pca_samp_matrix_data),
            ignore.case = TRUE
          ),]
        
        
      }
      else if (input$go_fun2 == 'All Proteins') {
        h <- pca_samp_matrix_data[-1]
        
      }
      else{
        rr <- input$go_fun2
        soms2 <-
          gotable2 %>% filter(name == rr) %>% select(somamer) %>% collect()
        h <-
          pca_samp_matrix_data[, grepl(soms2, colnames(pca_samp_matrix_data))]
      }
      
      
      return(list("h" = h, "pca_samp_myData3" = pca_samp_myData3))
      
    })
    
    
    # Render PCA plots
    output$pca1 <- renderPlotly({
      h <- pca_selection()$h
      pca_samp_myData3 <- pca_selection()$pca_samp_myData3
      
      
      if (input$label) {
        p <-
          autoplot(
            prcomp(h, scale = TRUE),
            data = pca_samp_myData3,
            colour = as.character(input$color2),
            label = TRUE,
            label.size = 3,
            text = paste("ID:", myData3$SampleID)
          )
      }
      
      else if (input$showshape2) {
        p <-
          autoplot(
            prcomp(h, scale = TRUE),
            data = pca_samp_myData3,
            colour = as.character(input$color2),
            shape = as.character(input$shape2),
            text = paste("ID:", myData3$SampleID)
          )
      }
      else{
        p <-
          autoplot(
            prcomp(h, scale = TRUE),
            data = pca_samp_myData3,
            colour = as.character(input$color2),
            text = paste("ID:", myData3$SampleID)
          )
      }
      
      
      
      
      if (input$showcustom == 0) {
        p + ggtitle("User-Defined")
        
      }
      else{
        p + ggtitle(gsub("_", " ", input$go_fun2))
        
      }
      
      
      
      
    })
    
    
    
    output$pca2 <- renderPlot({
      h <- pca_selection()$h
      
      pca_samp_myData3 <- pca_selection()$pca_samp_myData3
      
      
      
      pca <- prcomp(h,
                    
                    center = TRUE,
                    
                    scale. = TRUE)
      
      std_dev <- pca$sdev
      
      pr_var <- std_dev ^ 2
      
      prop_varex <- pr_var / sum(pr_var)
      
      prop_varex[1:29]
      
      plot(prop_varex,
           xlab = "Principal Component",
           
           
           ylab = "Proportion of Variance Explained",
           
           
           type = "b")
      
      
      
    })
    
    
    
    
    # Generate Download dataset UI
    output$moreControls4 <- renderUI({
      df <- filedata()$matrix_data
      myData3 <- filedata()$myData3
      if (is.null(df))
        return(NULL)
      
      columns <- rbind("All Proteins", gotable2[, 1])
      
      tagList(box(
        width = 8,
        title = "Download By GO or Disease",
        selectInput(inputId = "Download", label = "Download", columns),
        
        downloadButton('downloadData', 'Download')
      ))
    })
    
    
    #Process download files once user pressed button for download
    
    dload <- function() {
      matrix_data <- filedata()$myData3
      
      
      
      
      
      rr <- input$Download
      if (rr == "All Proteins") {
        h <- matrix_data
        
      }
      else{
        rownames(matrix_data) <-
          (make.names(paste(matrix_data$SampleId, sep = "\t"), unique = TRUE))
        
        soms2 <-
          gotable2 %>% filter(name == rr) %>% select(somamer) %>% collect()
        h <- matrix_data[, grepl(soms2, colnames(matrix_data))]
      }
      
      
      list("rr" = rr, "h" = h)
      
    }
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(dload()$rr, '.csv', sep = '')
      },
      
      
      content = function(file) {
        write.csv(dload()$h, file)
      }
    )
    
    
    
    
  }
)
