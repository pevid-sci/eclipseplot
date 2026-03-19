library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggforce)
library(cowplot)
library(eclipseplot)
library(rhandsontable)

if (file.exists("eclipseplot.R")) {
  source("eclipseplot.R")
}

# Template de uma única linha para o editor
single_row_template <- data.frame(
  pmid = "00001",
  study = "Aaaaaa (2000)",
  item1_step1 = "Probably Yes",  item1_step2 = "Probably Low", 
  item2_step1 = "Probably Yes", item2_step2 = "Probably Low",
  item3_step1 = "Probably Yes", item3_step2 = "Probably Low",
  item4_step1 = "Probably Yes", item4_step2 = "Probably Low",
  item5_step1 = "Probably Yes", item5_step2 = "Probably Low",
  item6_step2 = "Probably Low",
  stringsAsFactors = FALSE
)

ui <- navbarPage(
  # Título com logo integrado
  title = div(
    tags$img(src = "assets/eclipse_icon.ico", style = "margin-top: -5px; padding-right: 5px;", height = "30px"),
    "Eclipse Plot"
  ),
  
  tags$head(
    # Mantém o favicon da aba do navegador
    tags$link(rel = "icon", type = "image/x-icon", href = "assets/eclipse_icon.ico"),
    tags$style(HTML("
      .nav-tabs { font-weight: bold; }
      .well { background-color: #f8f9fa; border-radius: 8px; }
      .handsontable td { vertical-align: middle; }
      /* Garante que o texto do logo na navbar fique alinhado */
      .navbar-brand { display: flex; align-items: center; }
    "))
  ),
  
  
  # --- ABA 1: GERADOR ---
  tabPanel("Create a plot",
           sidebarLayout(
             sidebarPanel(
               fileInput("file", "1. Upload ROBUST-RCT CSV", accept = ".csv"),
               textInput("title", "2. Plot title", placeholder = "Default title"),
               radioButtons("design", "3. Layout", 
                            choices = list("Horizontal (Standard)" = "horizontal", 
                                           "Vertical (Full Page)" = "vertical"),
                            selected = "horizontal"),
               checkboxInput("standard", "Show two-step assessment", value = TRUE),
               checkboxInput("optionals", "Include optional items (if available)", value = FALSE),
               hr(),
               checkboxInput("show_adv", "Advanced options", value = FALSE),
               conditionalPanel(
                 condition = "input.show_adv == true",
                 wellPanel(
                   selectInput("plot_part", "Display part", 
                               choices = c("Full plot" = "full", "Eclipses only" = "eclipses", "Legend only" = "legend"))
                   #,
#                   sliderInput("prop_main", "Plot proportion", min = 0.5, max = 0.9, value = 0.75, step = 0.05)
                 )
               ),
               downloadButton("downloadPlot", "Download PNG", class = "btn-success")
             ),
             mainPanel(
               plotOutput("distPlot", height = "700px")
             )
           )
  ),
  
  # --- ABA 2: COMO USAR ---
  tabPanel("How to use",
           fluidPage(
             column(8, offset = 2,
                    h2("How to use"),
                    p("Your CSV should follow a specific data format."),
                    
                    h3("Required Columns"),
                    tags$ul(
                      tags$li(strong("pmid:"), "the identification number (e.g., PubMed ID) of the article."),
                      tags$li(strong("study:"), " the label for the article (e.g., 'Schmidt, 2026')."),
                      tags$li(strong("item1_step1 to item6_step2:"), " the rating for each item and step. Remember that item 6 only has step 2 as a multiple-choice value.")
                    ),
                    
                    # --- ABA RETRÁTIL (Details/Summary) ---
                    tags$details(
                      tags$summary(style = "cursor: pointer; color: #2c3e50; font-weight: bold; margin-bottom: 10px;", 
                                   "Click here to see the example table"),
                      div(style = "overflow-x: auto;", tableOutput("template_preview"))
                    ),
                    
                    hr(),
                    
                    h2("Example datasets"),
                    p("To understand the file format or test the app, you can download the tables below."),
                    
                    fluidRow(
                      column(6,
                             wellPanel(
                               h4("Standard"),
                               p("A dataset similar to most systematic reviews, without optional items."),
                               downloadButton("downloadBrief", "Download Brief CSV")
                             )
                      ),
                      column(6,
                             wellPanel(
                               h4("Extended"),
                               p("A large dataset including 3 optional items to test additional features of the app."),
                               downloadButton("downloadLong", "Download Long CSV")
                             )
                      )
                    )
             )
           )
  ),
  
  # --- ABA 3: RATINGS EDITOR ---
  tabPanel("Data Editor",
           fluidPage(
             h2("Data Editor"),
             p("Use this tab to configure your dataset into the 'eclipseplot' standard."),
             wellPanel(
               fluidRow(
                 column(3, fileInput("file_editor", "Upload data (.xlsx/.csv)", accept = c(".csv", ".xlsx"))),
                 column(3, br(), actionButton("add_row", "Add Row", icon = icon("plus"), class = "btn-info")),
                 column(3, br(), actionButton("reset_table", "Clear", icon = icon("trash"))),
                 column(3, br(), downloadButton("download_edited", "Download CSV", class = "btn-primary"))
               )
             ),
             rHandsontableOutput("hot_table")
           )
  ),
  
  # --- ABA 4: ABOUT ROBUST-RCT ---
  tabPanel("About ROBUST-RCT",
           fluidPage(
             tags$head(
               tags$style(HTML("
                 .def-yes-low { background-color: #4C72B0; color: white; padding: 2px 5px; border-radius: 3px; font-weight: bold; }
                 .prob-yes-low { background-color: #c3d0e4; color: black; padding: 2px 5px; border-radius: 3px; font-weight: bold; }
                 .prob-no-high { background-color: #f5c7c7; color: black; padding: 2px 5px; border-radius: 3px; font-weight: bold; }
                 .def-no-high { background-color: #E15759; color: white; padding: 2px 5px; border-radius: 3px; font-weight: bold; }
                 .citation-box { background-color: #f4f4f4; border-left: 5px solid #ccc; padding: 15px; font-family: monospace; white-space: pre-wrap; display: block; margin: 10px 0; }
                 summary { cursor: pointer; font-weight: bold; color: #2c3e50; margin-bottom: 5px; }
               "))
             ),
             column(8, offset = 2,
                    h2("About the ROBUST-RCT"),
                    p("ROBUST-RCT is a tool for risk-of-bias assessments of randomized controlled trials that aims to balance rigor and simplicity. If you are using the tool in your systematic review, we suggest citing the BMJ article."),
                    
                    h4("NLM reference"),
                    div(class = "citation-box",
                        "Wang Y, Keitz S, Briel M, Glasziou P, Brignardello-Petersen R, Siemieniuk RAC, Zeraatkar D, Akl EA, Armijo-Olivo S, Bassler D, Gamble C, Gluud LL, Hutton JL, Letelier LM, Ravaud P, Schulz KF, Torgerson DJ, Guyatt GH. Development of ROBUST-RCT: Risk Of Bias instrument for Use in SysTematic reviews-for Randomised Controlled Trials. BMJ. 2025 Mar 25;388:e081199. doi: 10.1136/bmj-2024-081199. PMID: 40132800."
                    ),
                    
                    p("The assessment through the ROBUST-RCT tool is a two-step framework."),
                    tags$ul(
                      tags$li(strong("Step 1: Evaluate what happened.")),
                      tags$li(strong("Step 2: Judge the risk of bias associated with it."))
                    ),
                    
                    p("This applies to the six core items, with additional optional items included when appropriate. With the exception of the first step of the sixth core item, the answers can be one of these:"),
                    
                    p(strong("Answers for Step 1:"), 
                      span(class="def-yes-low", "“Definitely Yes”"), ", ", 
                      span(class="prob-yes-low", "“Probably Yes”"), ", ", 
                      span(class="prob-no-high", "“Probably No”"), ", or ", 
                      span(class="def-no-high", "“Definitely No”"), "."),
                    
                    p(strong("Answers for Step 2:"), 
                      span(class="def-yes-low", "“Definitely Low”"), ", ", 
                      span(class="prob-yes-low", "“Probably Low”"), ", ", 
                      span(class="prob-no-high", "“Probably High”"), ", or ", 
                      span(class="def-no-high", "“Definitely High”"), "."),
                    
                    p("Below is a brief summary of the answers for each core item. This is not a guidance document: you should carefully read the tool’s manual, available on its article, to ensure appropriate ratings."),
                    
                    hr(),
                    
                    h3("Core item 1: Random sequence generation"),
                    tags$details(
                      tags$summary("[Click here to expand]"),
                      p("For this core item, there is no additional judgment. Choosing a rating for the first step means you should assign the equivalent rating for the second step."),
                      tags$ul(
                        tags$li(span(class="def-yes-low", "“Definitely Yes” and “Definitely Low”"), ": Trial explicitly stated use of an adequate method of generating the random allocation sequence (e.g., random number table/generator, throwing dice, drawing of lots, minimization)."),
                        tags$li(span(class="prob-yes-low", "“Probably Yes” and “Probably Low”"), ": Trial described as \"randomized\" without further details, AND either mentioned simple/block/stratified randomization, or described allocation concealment (e.g., central allocation, drug containers, envelopes)."),
                        tags$li(span(class="prob-no-high", "“Probably No” and “Probably High”"), ": Trial described as \"randomized\" without further details, and does not meet \"Probably Yes/Low\" criteria."),
                        tags$li(span(class="def-no-high", "“Definitely No” and “Definitely High”"), ": Trial used a recognized \"quasi-randomization\" method (e.g., allocation based on date of birth/admission, patient record number, alteration/rotation, clinician/participant decision, laboratory test results, or availability of the intervention).")
                      )
                    ),
                    
                    h3("Core item 2: Allocation concealment"),
                    tags$details(
                      tags$summary("[Click here to expand]"),
                      p("For this core item, there is no additional judgment. Choosing a rating for the first step means you should assign the equivalent rating for the second step."),
                      tags$ul(
                        tags$li(span(class="def-yes-low", "“Definitely Yes” and “Definitely Low”"), ": Trial used a clearly satisfactory allocation concealment method (e.g., telephone/web-based central allocation, pharmacy-controlled randomization with sequentially numbered, sealed drug containers, or explicitly stated sequentially numbered opaque sealed envelopes with evidence of sequential opening)."),
                        tags$li(span(class="prob-yes-low", "“Probably Yes” and “Probably Low”"), ": Trial explicitly stated using sequentially numbered, opaque, sealed envelopes without further details; or for drug trials where participants and healthcare providers were blinded: no further information on allocation concealment; or stated using envelopes/drug containers, but clarity on sequential numbering, opacity, and sealing is lacking."),
                        tags$li(span(class="prob-no-high", "“Probably No” and “Probably High”"), ": For unblinded drug trials or non-drug trials: no further information on allocation concealment; or stated using envelopes/drug containers, but clarity on sequential numbering, opacity, and sealing is lacking."),
                        tags$li(span(class="def-no-high", "“Definitely No” and “Definitely High”"), ": Trial used an open random allocation schedule or a \"quasi-randomization\" allocation sequence.")
                      )
                    ),
                    
                    h3("Core item 3: Blinding of participants"),
                    tags$details(
                      tags$summary("[Click here to expand]"),
                      p("From this core item, rate step 1, and only then assess the risk of bias in step 2."),
                      h4("Step 1: Were participants blinded?"),
                      tags$ul(
                        tags$li(span(class="def-yes-low", "“Definitely Yes”"), ": Trial explicitly stated that participants were blinded."),
                        tags$li(span(class="prob-yes-low", "“Probably Yes”"), ": No explicit statement but it's a placebo-controlled drug trial; or an active control drug trial (A vs. B) with \"double dummy\" or identical/matched medications mentioned; or described as \"single/double/triple blinded\" and judgment suggests participants were one of the blinded groups; or participants incapable of distinguishing intervention (e.g., neonates, severely demented)."),
                        tags$li(span(class="prob-no-high", "“Probably No”"), ": No explicit statement, and it's an active control drug trial (A vs. B) without \"double dummy\" or matched medications; or a non-drug trial; or \"single-blinded\" and judgment suggests the single-blinded group was not the participants."),
                        tags$li(span(class="def-no-high", "“Definitely No”"), ": Trial explicitly stated participants were not blinded, or described as \"open-label\" or \"unblinded\".")
                      ),
                      h4("Step 2: Judge the risk of bias related to participant blinding."),
                      tags$ul(
                        tags$li(span(class="def-yes-low", "“Definitely Low”"), ": Participants definitely blinded; OR unblinding very unlikely to have influenced outcome (e.g., participant expectations/co-interventions very unlikely to influence outcome)."),
                        tags$li(span(class="prob-yes-low", "“Probably Low”"), ": Participants probably blinded; OR unblinding unlikely to have influenced outcome (e.g., participant expectations/co-interventions unlikely to influence outcome)."),
                        tags$li(span(class="prob-no-high", "“Probably High”"), ": Participants definitely or probably not blinded, AND unblinding likely influenced outcome (e.g., participant expectations/co-interventions likely influenced outcome)."),
                        tags$li(span(class="def-no-high", "“Definitely High”"), ": Participants definitely or probably not blinded, AND unblinding very likely influenced outcome (e.g., participant expectations/co-interventions very likely influenced outcome).")
                      )
                    ),
                    
                    h3("Core item 4: Blinding of healthcare providers"),
                    tags$details(
                      tags$summary("[Click here to expand]"),
                      p("Rate step 1, and only then assess the risk of bias in step 2."),
                      h4("Step 1: Were healthcare providers blinded?"),
                      tags$ul(
                        tags$li(span(class="def-yes-low", "“Definitely Yes”"), ": The trial explicitly stated that healthcare providers were blinded."),
                        tags$li(span(class="prob-yes-low", "“Probably Yes”"), ": No explicit statement, but it's a placebo-controlled drug trial; or an active control drug trial (A vs. B) with \"double dummy\" or identical/matched medications mentioned; or described as \"single/double/triple blinded\" and judgment suggests healthcare providers were one of the blinded groups."),
                        tags$li(span(class="prob-no-high", "“Probably No”"), ": No explicit statement, and it's an active control drug trial (A vs. B) without \"double dummy\" or matched medications; or a non-drug trial; or \"single-blinded\" and judgment suggests single blinded group was not healthcare providers."),
                        tags$li(span(class="def-no-high", "“Definitely No”"), ": Trial explicitly stated healthcare providers were not blinded, or described as \"open-label\" or \"unblinded\".")
                      ),
                      h4("Step 2: Judge the risk of bias related to blinding of healthcare providers."),
                      tags$ul(
                        tags$li(span(class="def-yes-low", "“Definitely Low”"), ": Healthcare providers are definitely blinded."),
                        tags$li(span(class="prob-yes-low", "“Probably Low”"), ": Healthcare providers probably blinded; or unblinding unlikely to have influenced outcome because: unlikely any healthcare provider-initiated co-intervention could influence outcome; or investigators documented all co-interventions and demonstrated similarity between groups."),
                        tags$li(span(class="prob-no-high", "“Probably High”"), ": Healthcare providers were definitely or probably not blinded, and unblinding likely influenced the outcome because healthcare provider-initiated co-interventions could have influenced the outcome."),
                        tags$li(span(class="def-no-high", "“Definitely High”"), ": Healthcare providers were definitely or probably not blinded, and unblinding was very likely to have influenced the outcome because healthcare provider-initiated co-interventions could have affected the outcome, and investigators documented differences in these co-interventions between groups.")
                      )
                    ),
                    
                    h3("Core item 5: Blinding of outcome assessors"),
                    tags$details(
                      tags$summary("[Click here to expand]"),
                      p("Rate step 1, and only then assess the risk of bias in step 2. Remember that when the outcome is participants' self-report (e.g., questionnaire), participants are the outcome assessors."),
                      h4("Step 1: Were outcome assessors blinded?"),
                      tags$ul(
                        tags$li(span(class="def-yes-low", "“Definitely Yes”"), ": Trial explicitly stated outcome assessors or adjudicators were blinded."),
                        tags$li(span(class="prob-yes-low", "“Probably Yes”"), ": No explicit statement, but it's a placebo-controlled drug trial; or an active control drug trial (A vs. B) with \"double dummy\" or identical/matched medications mentioned; or described as \"single/double/triple blinded\" and judgment suggests outcome assessors were one of the blinded groups."),
                        tags$li(span(class="prob-no-high", "“Probably No”"), ": No explicit statement, and it's an active control drug trial (A vs. B) without \"double dummy\" or matched medications; or a non-drug trial; or \"single-blinded\" and judgment suggests the single-blinded group was not the outcome assessors."),
                        tags$li(span(class="def-no-high", "“Definitely No”"), ": Trial explicitly stated outcome assessors or adjudicators were not blinded, or described as \"open-label\" or \"unblinded\".")
                      ),
                      h4("Step 2: Judge the risk of bias related to blinding of outcome assessors."),
                      tags$ul(
                        tags$li(span(class="def-yes-low", "“Definitely Low”"), ": Outcome assessors definitely blinded; or outcome is all-cause mortality."),
                        tags$li(span(class="prob-yes-low", "“Probably Low”"), ": Outcome assessors probably blinded; or unblinding unlikely to have influenced outcome assessment because assessment involves minimal judgment (e.g., laboratory measurement, hospital admission, mechanical ventilation)."),
                        tags$li(span(class="prob-no-high", "“Probably High”"), ": Outcome assessors definitely or probably not blinded, and unblinding likely influenced outcome assessment because assessment involves some judgment (e.g., cause-specific mortality)."),
                        tags$li(span(class="def-no-high", "“Definitely High”"), ": Outcome assessors definitely or probably not blinded, AND unblinding could have influenced outcome assessment because assessment involves considerable judgment by participant or adjudicator (e.g., symptoms/symptom scores, quality of life, seizure occurrence).")
                      )
                    ),
                    
                    h3("Core item 6: Outcome data not included in analysis"),
                    tags$details(
                      tags$summary("[Click here to expand]"),
                      p("For this core item, step 1 is data extraction (which is why there is no multiple-choice answer). Step 2 is the rating derived from this extraction. Suggested rules for specific cases (e.g., time-to-event outcomes) are provided in the manual."),
                      p("An option is to set pre-specified thresholds of participants not included in the analysis. The thresholds below are just an example, and the authors should adjust them to their field of investigation."),
                      tags$ul(
                        tags$li(span(class="def-yes-low", "“Definitely low”"), ": Missing data < 5%"),
                        tags$li(span(class="prob-yes-low", "“Probably low”"), ": 5% ≤ Missing data < 10%"),
                        tags$li(span(class="prob-no-high", "“Probably high”"), ": 10% ≤ Missing data < 15%"),
                        tags$li(span(class="def-no-high", "“Definitely high”"), ": Missing data ≥ 15%")
                      )
                    ),
                    br(), br()
             )
           )
  ),
  
  # --- ABA 5: ABOUT ECLIPSE PLOT ---
  tabPanel("About Eclipse Plot",
           fluidPage(
             column(8, offset = 2,
                    h2("About Eclipse Plot"),
                    p("Developed specifically for two-step risk-of-bias assessments in the ROBUST-RCT framework, Eclipse Plot is a novel visualization tool that displays both the factual evaluation and the judgment. Its design aims to empower readers of systematic reviews not only to passively accept the authors' judgments but also to form their own judgments about the study’s design and conduct."),
                    
                    p("If you will use the plot in your work, please consider citing the reference."),
                    
                    h4("Reference"),
                    div(class = "citation-box",
                        "Vidor et al., 2026. Eclipse plot: a graphical visualization for risk of bias assessments in two steps. MetaArxiv. https://doi.org/10.31222/osf.io/58yda_v1 [Preprint]"
                    ),
                    
                    hr(),
                    
                    h3("Tutorial Videos"),
                    fluidRow(
                      column(6, h4("R Package Guide"), tags$video(src = "assets/guide_r.mp4", type = "video/mp4", controls = "controls", width = "100%")),
                      column(6, h4("Web Tool Guide"), tags$video(src = "assets/guide_web.mp4", type = "video/mp4", controls = "controls", width = "100%"))
                    )
             )
           )
  )
)

server <- function(input, output, session) {
    
    # --- 1. LÓGICA DO PREVIEW (Aba How to use) ---
    output$template_preview <- renderTable({
      # Tenta ler o arquivo local. Se não existir, cria um fallback seguro.
      tryCatch({
        df_preview <- read.csv("sample_brief.csv", check.names = FALSE)
        head(df_preview, 2)
      }, error = function(e) {
        # Fallback caso os arquivos .csv não estejam na pasta raiz do app
        data.frame(
          pmid = "40001", 
          study = "Albert, 2024", 
          item1_step1 = "Definitely Yes", 
          item1_step2 = "Definitely Low",
          stringsAsFactors = FALSE
        )
      })
    })
    
    # --- 2. LÓGICA DOS DOWNLOADS DE EXEMPLO (Aba How to use) ---
    output$downloadBrief <- downloadHandler(
      filename = function() { "sample_brief.csv" },
      content = function(file) { file.copy("sample_brief.csv", file) }
    )
    
    output$downloadLong <- downloadHandler(
      filename = function() { "sample_long.csv" },
      content = function(file) { file.copy("sample_long.csv", file) }
    )
    
    # --- 3. LÓGICA DO EDITOR (Data Editor) ---
    empty_template <- single_row_template[0, ]
    values <- reactiveValues(data = single_row_template)
    
    observeEvent(input$add_row, {
      if(!is.null(input$hot_table)){
        values$data <- hot_to_r(input$hot_table)
      }
      values$data <- rbind(values$data, single_row_template[1, ])
    })
    
    observeEvent(input$reset_table, {
      values$data <- empty_template
    })
    
    observeEvent(input$file_editor, {
      ext <- tools::file_ext(input$file_editor$name)
      if (ext == "csv") {
        values$data <- read.csv(input$file_editor$datapath, check.names = FALSE)
      } else {
        values$data <- eclipseplot::from_xlsx(input$file_editor$datapath)
      }
    })
    
    output$hot_table <- renderRHandsontable({
      df <- values$data
      rhandsontable(
        df,
        stretchH = "all",
        rowHeaders = FALSE,
        height = 500,
        manualColumnResize = TRUE
      ) %>%
        hot_cols(colWidths = 130) %>%
        hot_table(
          highlightCol = TRUE,
          highlightRow = TRUE,
          cells = htmlwidgets::JS("
    function(row, col, prop) {
      var cellProperties = {};
      cellProperties.renderer = function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        var colors = {
          'Definitely Yes': '#4C72B0', 'Probably Yes': '#c3d0e4',
          'Probably No': '#f5c7c7', 'Definitely No': '#E15759',
          'Definitely Low': '#4C72B0', 'Probably Low': '#c3d0e4',
          'Probably High': '#f5c7c7', 'Definitely High': '#E15759',
          'Not applicable': '#D3D3D3'
        };
        if (value && colors[value]) {
          td.style.background = colors[value];
          td.style.color = value.includes('Definitely') ? 'white' : 'black';
        } else {
          td.style.background = '';
          td.style.color = '';
        }
        return td;
      };
      return cellProperties;
    }
  ")
        )
    })
    
    # --- 4. LÓGICA DO PLOT (Create a plot) ---
    dataset <- reactive({
      req(input$file)
      read.csv(input$file$datapath, check.names = FALSE)
    })
    
    output$distPlot <- renderPlot({
      req(dataset())
      eclipseplot(
        robust_data = dataset(),
        standard    = input$standard,
        optionals   = input$optionals,
        title       = if(input$title == "") NULL else input$title,
        design      = input$design,
        plot        = if(input$show_adv) input$plot_part else "full"
#        proportions = if(input$show_adv) input$prop_main else 0.75
      )
    })
    
    # --- 5. DOWNLOAD DO CSV EDITADO ---
    output$download_edited <- downloadHandler(
      filename = function() { paste0("edited_data_", Sys.Date(), ".csv") },
      content = function(file) {
        df <- rhandsontable::hot_to_r(input$hot_table)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    # --- 6. DOWNLOAD DO PLOT ---
    output$downloadPlot <- downloadHandler(
      filename = function() { paste0("eclipseplot_", Sys.Date(), ".png") },
      content = function(file) {
        # Recriar o plot para o ggsave
        p <- eclipseplot(
          robust_data = dataset(),
          standard    = input$standard,
          optionals   = input$optionals,
          title       = if(input$title == "") NULL else input$title,
          design      = input$design,
          plot        = if(input$show_adv) input$plot_part else "full"
#          proportions = if(input$show_adv) input$prop_main else 0.75
        )
        ggplot2::ggsave(file, plot = p, width = 14, height = 9.8, dpi = 300)
      }
    )
}

shinyApp(ui = ui, server = server)