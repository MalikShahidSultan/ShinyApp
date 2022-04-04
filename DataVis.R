library(shiny)
library(dplyr)
library(rlang)
library(ggplot2)
library(PerformanceAnalytics)
library(corrplot)
library(psych)


scatter_plot <- function(dataset, xvar, yvar,c,s) {
  
  x <- rlang::sym(xvar)
  y <- rlang::sym(yvar)
  c <- rlang::sym(c)
  s <- rlang::sym(s)
  
  
  p <- ggplot(dataset, aes(x = !!x, y = !!y)) +
    geom_point(aes(color= !!c,size = !!s)) +
    theme(axis.title = element_text(size = rel(1.2)),
          axis.text = element_text(size = rel(1.1)))
  
  
  return(p)
  
}

importUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file1"), "Choose File"),
    checkboxInput(ns("header"), "Header", TRUE)
  )
  
}

importSE <- function(id) {
  moduleServer(id, 
               function(input, output, session) {
                 
                 dtreact <- reactive({
                   file <- input$file1
                   if (is.null(file))
                     return(NULL)
                   read.csv(file$datapath, header = input$header)
                 })
                 
                 
                 
                 return(dtreact)
               }
  )
  
}

varselect_ui <- function(id) {
  ns <- NS(id)
  var_choices <-"" 
  tagList(selectInput(ns("xvar"), "Select Independent Variable", choices = var_choices, selected = NULL),
          selectInput(ns("yvar"), "Select Dependent Variable", choices = var_choices, selected = NULL),
          selectInput(ns("cvar"), "Select Color Variable", choices = var_choices, selected = NULL),
          selectInput(ns("svar"), "Select Size Variable", choices = var_choices, selected = NULL)
          
  )
}

dist_varselect_ui <- function(id) {
  ns <- NS(id)
  var_choices <-"" 
  tagList(selectInput(ns("distvar"), "Select Independent Variable", choices = var_choices, selected = NULL))
  
}





varselect_server <- function(id, dataset) {
  moduleServer(id,
               function(input, output, session) {
                 observeEvent(dataset(), {
                   updateSelectInput(session,
                                     "xvar",
                                     choices = names(dataset()))
                   updateSelectInput(session,
                                     "yvar",
                                     choices = names(dataset()))
                   updateSelectInput(session,
                                     "cvar",
                                     choices = names(dataset()))
                   updateSelectInput(session,
                                     "svar",
                                     choices = names(dataset()))
                   
                   
                 })
                 
                 return(
                   list(
                     xvar = reactive({input$xvar}),
                     yvar = reactive({input$yvar}),
                     cvar = reactive({input$cvar}),
                     svar = reactive({input$svar})
                     
                     
                   )
                 )
               }
  )
}


varselect_server_dist <- function(id, dataset) {
  moduleServer(id,
               function(input, output, session) {
                 observeEvent(dataset(), {
                   updateSelectInput(session,
                                     "distvar",
                                     choices = names(dataset()))
                   
                 })
                 
                 return(
                   list(
                     distvar = reactive({input$distvar})
                     
                   )
                 )
               }
  )
}






scatterplot_ui <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot1"))
  
}

scatterplot_server <- function(id, dataset, plot1vars, plot2vars,plot3vars,plot4vars) {
  moduleServer(id, 
               function(input, output, session) {
                 
                 plot1_obj <- reactive({
                   req(dataset())
                   p <- scatter_plot(dataset(), xvar = plot1vars(), yvar = plot2vars(),c=plot3vars(),s=plot4vars())
                   return(p)
                 })
                 
                 output$plot1 <- renderPlot({
                   plot1_obj()
                 })
               }
  )
}






ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "minty"),
  titlePanel("Correlation & Data Visualization Application"),
  tabsetPanel(
    
    
    tabPanel("Discriptive Statistics",
             fluidRow(
               
               column(12,
                      
                      importUI("importy")
                      
               )),
             fluidRow(
               
               column(12,
                      
                      dataTableOutput("cor")
               ))),
    tabPanel("Visualize Distribution",
             fluidRow(
               
               column(12,
                      plotOutput("histo")
               ))),
    
    tabPanel("Correlation",
             fluidRow(
               
               column(12,
                      plotOutput("hist")
               ))),
    
    
    
    tabPanel("Scatter Plot",
             fluidRow(
               
               column(4,
                      varselect_ui("select")),
               column(8,
                      
                      scatterplot_ui("scatter")
               )))
  )
)






server <- function(input, output, session) {
  
  dataset <- importSE("importy")
  output$cor <- renderDataTable({summary(dataset())})
  
  
  
  
  plotvars <- varselect_server("select", dataset = dataset)
  scatterplot_server("scatter", dataset = dataset, plot1vars = plotvars$xvar,
                     plot2vars = plotvars$yvar,plot3vars =plotvars$cvar,plot4vars = plotvars$svar )
  histplotvars <- varselect_server_dist("selects", dataset = dataset)
  X <- reactive(input$X)
  Y <-reactive(input$Y)
  output$hist = renderPlot({
    par(pin=c(input$X,input$Y))              ##  (width, height) in inches    
    #par(omi=c(0,4,4,2))
    corrplot(cor(dataset(), use="pairwise", method="spearman"))})
  
  
  
  histoplotvars <- varselect_server_dist("selects", dataset = dataset)
  v<- function() {
    return(as.numeric(histplotvars$distvar))  
  }
  output$histo = renderPlot({
    
    pairs.panels(dataset(), 
                 method = "pearson", # correlation method
                 hist.col = "#00AFBB",
                 density = TRUE,  # show density plots
                 ellipses = TRUE # show correlation ellipses
    )
    
    
  })
}

shinyApp(ui, server)




