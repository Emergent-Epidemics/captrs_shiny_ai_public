#SVScarpino

shinyUI(fluidPage(
    mainPanel(
      img(src="CAPTRS_HorizonalLogo_DarkBlue.jpg", width = 300, height = 150)
    ),
    tabsetPanel(
      tabPanel("Threat generation",fluidRow(
        column(2,
               selectInput(inputId = "ai_model", label = "Select AI", choices = c("gpt-4o", "gpt-4.5-preview-2025-02-27"), selected = "gpt-4o"),
               selectInput(inputId = "ai_rag", label = "Use RAG?", choices = c("No", "Yes"), selected = "No"),
               textInput(inputId = "text_pathogen", label = "Pathogen", value = "Ebola"),
               textInput(inputId = "text_severity", label = "Severity", value = "Highly lethal"),
               selectizeInput(inputId = "text_location", label = "Geo-political context", choices= locations, selected = "Sub-Saharan Africa", multiple = FALSE),
               textAreaInput(inputId = "text_system_prompt", label = "System Prompt", value = "You are generating threats for use in a professional pandemic response wargame. The threat should combine risks associated with the pathogen and the geo-political context.", width = 300, height = 130),
               textAreaInput(inputId = "text_prompt", label = "Prompt", value = "Generate a threat by combining the following information, the pathogen is PATHOGEN_HERE the severity is SEVERITY_HERE and the geopolitical context is LOCATION_HERE as a location with HEALTH_IND as health indicators and DEV_IND as development indicators. Be sure to include the pathogen, the pathogen severity, the health indicators, and development indicators in the threat you create.", width = 300, height = 250),
               actionButton("Submit_threat", "Submit")
        ),
        column(6,
               tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
               htmlOutput("results_text_threat") %>% withSpinner(color="#0dc5c1")
        ),
        column(4,
               tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
               htmlOutput("results_text_threat_realism")
        )
      )
      ),
      tabPanel("Ontology Evaluation",fluidRow(
        column(4,
               textAreaInput("text_ontology", "Scenario to evaluate:", "Date: Fri 7 Jun 2024
Source: Haqqin [in Russian, machine trans., edited]
https://m.haqqin.az/news/318729


Three residents of the village of Faldarly, Zagatala region of Azerbaijan, were hospitalized today at the Clinical Medical Center (CMC) in Baku with suspected anthrax. This is stated in a joint statement by the Ministry of Health and TƏBIB [Administration of the Regional Medical Divisions - TABIB].

One of them had a confirmed diagnosis and continues treatment in accordance with the medical protocol. The other 2 had samples taken for laboratory testing for suspected anthrax and were sent to the Center for Disease Control.

The statement notes that anthrax is a zoonotic infection and is not transmitted from person to person.
                             
The report doesn't note the source, but presumably the 3 individuals were exposed through a common source, likely via exposure to raw meat during butchery and/or exposure to infected animals. As noted in previous posts, the best way to prevent the spillover of anthrax into humans is via aggressive vaccination of livestock in affected areas. Unfortunately such programs require long-term commitments due to the prolonged survival of anthrax spores in the environment. Any additional information on these cases and/or the source(s) would be greatly appreciated.", width = 400, height = 600),
        actionButton("Submit_ontology","Submit")
      ),
      column(4,
             tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
             htmlOutput("results_text_ontology_realism_assess") %>% withSpinner(color="#0dc5c1"),
             htmlOutput("results_text_ontology")
      ),
      column(4,
             tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
             htmlOutput("results_text_ontology_realism")
      )
    )
    ),
    tabPanel("Ontology Evaluation Geo",fluidRow(
      column(4,
             textAreaInput("text_ontology", "Scenario to evaluate:", "Date: Fri 7 Jun 2024
Source: Haqqin [in Russian, machine trans., edited]
https://m.haqqin.az/news/318729


Three residents of the village of Faldarly, Zagatala region of Azerbaijan, were hospitalized today at the Clinical Medical Center (CMC) in Baku with suspected anthrax. This is stated in a joint statement by the Ministry of Health and TƏBIB [Administration of the Regional Medical Divisions - TABIB].

One of them had a confirmed diagnosis and continues treatment in accordance with the medical protocol. The other 2 had samples taken for laboratory testing for suspected anthrax and were sent to the Center for Disease Control.

The statement notes that anthrax is a zoonotic infection and is not transmitted from person to person.
                             
The report doesn't note the source, but presumably the 3 individuals were exposed through a common source, likely via exposure to raw meat during butchery and/or exposure to infected animals. As noted in previous posts, the best way to prevent the spillover of anthrax into humans is via aggressive vaccination of livestock in affected areas. Unfortunately such programs require long-term commitments due to the prolonged survival of anthrax spores in the environment. Any additional information on these cases and/or the source(s) would be greatly appreciated.", width = 400, height = 600),
             actionButton("Submit_ontology","Submit")
      ),
      # column(4,
      #        tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
      #        htmlOutput("results_text_ontology_promed")
      # ),
      column(4,
             tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
             htmlOutput("results_geo_ontology") %>% withSpinner(color="#0dc5c1")
      )
    )
    ),
    tabPanel("PubMed Interface",fluidRow(
      column(3,
             textInput("text_pubmed", "Search Terms", "Ebola"),
             actionButton("Submit_pubmed","Submit")
      ),
      column(8,
             tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
             htmlOutput("results_text_pubmed") %>% withSpinner(color="#0dc5c1")
      )
    )
    ),
    tabPanel("ProMED Look-up",fluidRow(
      column(3,
             textInput("text_promed", "ProMed ID", "20160616.4292080"),
             actionButton("Submit_promed","Submit")
      ),
      column(8,
             tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
             htmlOutput("results_text_promed") %>% withSpinner(color="#0dc5c1")
      )
    )
    ),
    tabPanel("Bulk scenario evaluation",fluidRow(
      column(3,
             fileInput('bulk_data_excel_file', 'Upload xlsx file',
                      accept = c(".xlsx")),
             uiOutput('ui.action.cols'),
             uiOutput('ui.action.rows'),
             uiOutput('ui.action.id'),
             uiOutput('ui.action.run')
      ),
      column(4,
             tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
             DTOutput("bulk_output"),
             downloadButton("dl_bulk", "Download")
      )
      )
    )
)))