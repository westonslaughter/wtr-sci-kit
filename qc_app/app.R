library(shiny)
# runApp("C:/Users/wslaught/OneDrive/UMD/science/Support/ABDann/qc_app")

# libraries
if(!require("dataRetrieval")) install.packages("dataRetrival")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("plotly")) install.packages("plotly")

# default info
site_info <- c('pb' = '01649190', 'hr' = '01651770', 'rc' = '01648010', 'slb' = '01589290',
               'lf' = '01646500', 'ac' = '01651827', 'sc' = '01650800')

target_variables <- c('Cu' = '01040', 'Cl' = '00940', 'EColi' = '50468', 'P' = '00666', 
                      'TN' = '62854', 'TotalColiform' = '50569', 'Nitrite' = '00613', 'Nitrate' = '00618',
                      'NH3_NH4' = '00608', 'SPC' = '00095', 'ParticulateN' = '49570',
                      'ON' = '00607', 'Cd' = '01025', 'Pb' = '01049', 'Zn' = '01090', 'OC' = '00681',
                      'TW' = '00010')

siteNo <- as.character(site_info)
pCode = target_variables[['Cl']]
start.date <- "2022-01-01"
end.date <- "2023-01-01"

site_q <- readNWISdata(
  siteNumbers = siteNo,
  service = "dv",
  parameterCd = "00060",
  startDate = start.date,
  endDate = end.date) %>%
  mutate(sitecode = paste0(agency_cd, '-', site_no), 
         var = 'discharge_dv_cfs') %>%
  select(sitecode, date = dateTime, var, val = 'X_00060_00003')

site_c <- readWQPqw(
  siteNumbers = paste0("USGS-", siteNo),
  parameterCd = as.character(target_variables),
  startDate = start.date,
  endDate = end.date) %>% 
  mutate(name = names(target_variables[match(USGSPCode, target_variables)])) %>%
  select(sitecode = MonitoringLocationIdentifier, 
         date = ActivityStartDate, 
         var = name, 
         val = ResultMeasureValue) %>%
  mutate(var = gsub(" ", "", var))

site_cq <- rbind(site_q, site_c) %>%
  tidyr::pivot_wider(id_cols = c('sitecode', 'date'), values_from = val, names_from = var, values_fn = {mean})

if(!exists("site_cq_x")) {
  site_cq_x <- site_cq %>%
    filter(sitecode ==  paste0('USGS-', as.character(site_info[[1]])))
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("WQ!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("sites", "select site", choices = paste0('USGS-', as.character(site_info))),
      
      selectInput("yvars", "select Y axis var", choices = c("")),
      
      selectInput("xvars", "select X axis var", choices = c(""))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  site_data_input <- reactive({
    site_cq %>% filter(sitecode == input$sites)
  })
    
  output$distPlot <- renderPlot({
    site_cq_x <- site_data_input()
    
    cq_plot <- ggplot(data = site_cq_x,
                 aes_string(x = input$xvars, y = input$yvars)) +
      geom_point() +
      geom_smooth(method = 'lm', se = FALSE, formula = 'y ~ x', color = 'grey20', alpha = 0.4) +
      scale_x_log10() +
      scale_y_log10() +
      ggtitle(paste(input$sites))

      cq_plot
  })
  
  observeEvent(input$sites, {
    site_cq_x <- site_data_input()
      
    site_vars <- colnames(site_cq_x[,colSums(is.na(site_cq_x))<nrow(site_cq_x)])[3:ncol(site_cq_x)]
    print(site_vars)
    
    updateSelectInput(inputId = 'xvars',
                      choices = site_vars
                      # choices = c(colnames(site_cq_x)[colnames(site_cq_x) %in% names(target_variables)], "discharge_dv_cfs")
                      )
    
    updateSelectInput(inputId = 'yvars',
                      # choices = c(colnames(site_cq_x)[colnames(site_cq_x) %in% names(target_variables)], "discharge_dv_cfs"))
                      choices = site_vars
                      )
  })
  
}

shinyApp(ui = ui, server = server)
