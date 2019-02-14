
# HEADER
header <- dashboardHeader(title = HTML(paste(icon('connectdevelop'), 'MBToolbox')))

# SIDEBAR
sidebar <- dashboardSidebar(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    tags$script("
    
                Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                  Shiny.onInputChange(variableName, null);
                });
                
                "),
    
    tags$script("

function removeLearner(obj) {
  $(obj).closest('.lrnrbox').remove();
}
                
                ")
  ),
  
  sidebarMenu(
    
    selectInput('dataset',label = 'Choose dataset (test only):',
                choices = names(datasets), selected = 'iris'), 
    
    convertMenuItem(
      
      menuItem("Step 1:  Create Task", tabName = "task_setup", icon = icon("binoculars"),
               
               br(),
               
               tags$div(class = "surv", 
                        
                        uiOutput("target")
                        
                        ),
               
               materialSwitch("survival", label = "Survival analysis", 
                              value = F, 
                              status = "primary"),
               
               shinyWidgets::pickerInput("predictors", "Select predictors:", choices = character(0), 
                           multiple = T, 
                           options = pickerOptions(actionsBox = T,
                                                   liveSearchStyle = "startsWidth")),
               
               bsButton("create_task", label = "Create Task", style = "primary"), 
               
               br()
               
               ), tabName = "task_setup"),
    
    menuItem("Step 2:  Model Fitting", tabName = "model_setup", icon = icon("flask"), 
             
             menuSubItem("Decision Trees", tabName = "trees"),
             
             menuSubItem("Linear Methods", tabName = "linear"),
             
             menuSubItem("Boosting", tabName = "boosting"),
             
             menuSubItem("Neural Networks", tabName = "nn"),
             
             menuSubItem("Benchmark Experiments", tabName = "mlr")
             
    ),
    
    menuItem("Step 3:  Model Validation", tabName = "model_perf", icon = icon("chart-bar")), # validation set\test set perf
    
    br(),
    
    menuItem("Interpretability Measures", tabName = "interp"),
    
    menuItem("Predict New Data", tabName = "predict")
    
    
  )
  
  
  
)

# BODY
body <- dashboardBody(shinyjs::useShinyjs(),
  
  tabItems(
    tabItem(tabName = "task_setup",
            
            column(width = 5,
                   
                   tabBox(height = 680, width = 12,
                          
                          tabPanel(title = "Task Summary",
                                   
                                   tags$form(class = "form-horizontal",
                                             
                                             tags$div(class = "task form-group",
                                                             
                                                             tags$label(class = "col-sm-5 control-label", `for` = "task_type", "Task type:"),
                                                             textOutput("task_type"),
                                                             hr(), 
                                                             
                                                             tags$label(class = "col-sm-5 control-label", `for` = "task_target", "Response:"),
                                                             textOutput("task_target"),
                                                             hr(),
                                                             
                                                             tags$label(class = "col-sm-5 control-label", `for` = "task_formula", "Formula:"),
                                                             verbatimTextOutput("task_formula"),
                                                             hr(), 
                                                             
                                                             tags$label(class = "col-sm-5 control-label", `for` = "task_nobs", "No. of samples:"),
                                                             textOutput("task_nobs"),
                                                             hr(), 
                                                             
                                                             tags$label(class = "col-sm-5 control-label", `for` = "task_npredictors", "No. of predictors:"),
                                                             textOutput("task_npredictors"),
                                                             hr(),
                                                             
                                                             tags$label(class = "col-sm-5 control-label", `for` = "task_count", "Count data:"),
                                                             textOutput("task_count"),
                                                             hr(),
                                                             
                                                             tags$label(class = "col-sm-5 control-label", `for` = "task_complete", "Complete cases:"),
                                                             textOutput("task_complete"),
                                                             hr(), 
                                                             
                                                             tags$label(class = "col-sm-5 control-label", `for` = "task_missings", "Missing values:"),
                                                             textOutput("task_missings"),
                                                             hr(), 
                                                             
                                                             tags$label(class = "col-sm-5 control-label", `for` = "task_weights", "Has weights:"),
                                                             textOutput("task_weights"),
                                                             hr()
                                                      )
                                             ),
                                   
                                   tags$div(class = "src-footer", 
                                            bsButton("show_createTask", "Show code", icon = icon("code"))
                                            )
                                   
                                   ), 
                          
                          tabPanel(title = "Data", 
                                   DT::dataTableOutput('x1'))
                   )
                   
            ),
            
            column(width = 7,
                     
                     tabBox(title = "Exploratory Analysis", height = 680, width = 12,
                            tabPanel(title = "Plots", 
                                     plotOutput('x2')), 
                            tabPanel(title = "Correlogram", 
                                     plotOutput('corrPlot'))
                     )
                   
                   
            )
    ), 
    
    tabItem(tabName = "trees", 
            
            column(width = 2, 
                   
                     box(title = "Model Setup", status = "primary", width = 12, solidHeader = T, background = "navy",
                         
                         tags$div(id = 'add'),
                         
                         tags$div(class = "pull-right",
                                  bsButton("add_learner", "Add Learner", style = "success")
                                  )
                         
                     )
                   
                   
                   
            ), # eo column
            
            column(width = 2,
                   
                     box(title = "Resampling Strategy", status = "primary", width = 12, solidHeader = TRUE, background = "navy",
                         
                         tags$div(class = "innerbox", 
                                  box(width = 12, 
                                      solidHeader = FALSE,
                                      title = div(h4(class = "inner-title", "Inner")),
                                      selectizeInput("tree_resample", "", choices = list("CART", "C5.0", "Random Forest"), 
                                                     multiple = T, options = list(placeholder = 'None selected'))
                                  )
                         ),
                         
                         tags$div(class = "outerbox", 
                                  box(width = 12, 
                                      solidHeader = FALSE,
                                      title = div(h4(class = "outer-title", "Outer"))
                                  )
                         ), 
                         
                         tags$div(class = "pull-right", 
                                  bsButton("train", "Train Learners", style = "danger")
                         )
                     )
                   
                   ), 
            
            column(width = 8, 
                   
                   tabBox(title = "Visualisations", height = 680, width = 12, side = "right", selected = "Performance",
                          tabPanel(title = "Tuning"), 
                          tabPanel(title = "Prediction Boundaries"), 
                          tabPanel(title = "Tree Plot?"),
                          tabPanel(title = "Performance",
                                   fluidRow(
                                     box(
                                       title = "CV Performance",
                                       status = "primary",
                                       width = 12,
                                       solidHeader = FALSE,
                                       collapsible = TRUE,
                                       plotOutput("cvPlot")
                                     ) # end of box
                                   ), # end of fluidRow
                                   fluidRow(
                                     box(
                                       title = "Quick Summary",
                                       status = "primary",
                                       width = 4,
                                       solidHeader = FALSE,
                                       collapsible = TRUE
                                     ), # eo box
                                     
                                     box(
                                       title = "ROC Curves",
                                       status = "primary",
                                       width = 8,
                                       solidHeader = FALSE,
                                       collapsible = TRUE
                                     ) # end of box
                                     
                                   ) # end of fluidRow
                          ) # eo tabPanel
                   )
                   
            )
            
    ),
    
    tabItem(tabName = "interp",
            tabBox(height = 680, width = 12, side = "right", selected = "Performance", 
                   tabPanel(title = "Parial Dependence"),
                   tabPanel(title = "Variable Importance",
                            
                            box(
                              title = "CV Performance",
                              status = "primary",
                              width = 12,
                              solidHeader = FALSE,
                              collapsible = TRUE,
                              plotOutput("impPlot")
                            )
                            
                   )
            )
            
    )
    
  )
  
)


ui = dashboardPage(header, sidebar, body, shinyFeedback::useShinyFeedback())