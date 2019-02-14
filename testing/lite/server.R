server = function(input, output, session) {
  
  # Loading dataset (for test only - not needed once integrated into Lite)
  data <- reactive({  
    
    datasets[[input$dataset]]
    
  })
  
  colnames <- reactive({
    
    sort(names(data()))
    
  })
  
  ############################################ START Create Task
  
  # Rendering UI for target variable
  
  output$target <- renderUI({
    
    if(input$survival) {
      
      tagList(
        
        selectizeInput("status", "Status variable:", choices = colnames(),
                       selected = character(0),
                       options = list(placeholder = "Search by name")),
        
        selectizeInput("time", "Time variable:", choices = colnames(),
                       selected = character(0),
                       options = list(placeholder = "Search by name"))
        
      )
      
    } else {
      
      selectizeInput("response", "Response variable:", choices = colnames(),
                     selected = character(0),
                     options = list(placeholder = "Search by name"))
      
    }
    
    
  })
  
  
  # To reset target panel selections if survival analysis switch is toggled
  
  observe({
    
    if(input$survival) {
      
      updateSelectizeInput(session, "response", selected = character(0))
      
      session$sendCustomMessage(type = "resetValue", message = "response")
      
    } else {
      
      lapply(c("status", "time"), function(x) {
        
        updateSelectizeInput(session, x, selected = character(0))
        
        session$sendCustomMessage(type = "resetValue", message = x)
        
      })
      
    }

    # Not sure why I decided to put the pciker update in this observer
    
    updatePickerInput(session, "predictors",
                      choices = setdiff(colnames(), c(input$response, input$status, input$time))) # The setdiff with all three values is ok because of the reset above - so if surv analysis switch is on then value for response is NULL and if surv switch is off then value for status and time are NULL 
    
    # Check line 90 of createTask.R - need to incorporate weights

  })
  
  values <- reactiveValues(task = NULL) # Why I chose observeEvent + reactiveValues over observeEvent + reactive here: the "task" obj is only created when the createTask button is clicked - it is not invalidated if the value of one of its depedencies (e.g. predictors or response) changes. If I wanted it to respond to changes in these dependecies independent of the create task button being clicked then I would have chosen reactive(). Not sure if this is correct. 
  
  observeEvent(input$create_task, { # Why I chose observeEvent over eventReactive? WHAT IS THE DIFFERENCE BETWEEN eventReactive vs observeEvent + reactiveValues vs observeEvent + reactive
    
    
    if(input$survival) {
      
      # Checking if inputs are null
      feedbackDanger("status", is.null(input$status), "Please specify status variable.")
      feedbackDanger("time", is.null(input$time), "Please specify time variable.")
      feedbackDanger("predictors", is.null(input$predictors), "Please specify at least one predictor.") # No shinyFeedback support for pickerInput yet - may have to resort to validate(need) with a renderText - but is it ok to use render inside an observer? https://stackoverflow.com/questions/31760717/rendering-inside-observeevent-dynamic-plot-generation
      
      
      condition = ((!is.null(input$status) & !is.null(input$time)) && (input$status == input$time))
      # Checking if status = time
      feedbackDanger("status", condition, text = "")
      feedbackDanger("time", condition, text = "'status' and 'time' cannot be the same.")
      
      req(input$status, input$time, input$predictors, input$status != input$time)
      
      values$task <- createTask(data(),
                                target = list(time = input$time, 
                                              status = input$status),
                                predictors = input$predictors)
      
    } else {
      
      # Checking if inputs are null
      feedbackDanger("response", is.null(input$response), "Please specify response variable.")
      feedbackDanger("predictors", is.null(input$predictors), "Please specify at least one predictor.") # No shinyFeedback support for pickerInput yet - may have to resort to validate(need) with a renderText - but is it ok to use render inside an observer? https://stackoverflow.com/questions/31760717/rendering-inside-observeevent-dynamic-plot-generation
      
      req(input$response, input$predictors)
      
      values$task <- createTask(data(), target = input$response, predictors = input$predictors)
      
      values$task.desc <- values$task$task.desc
      
    }
    
    updateButton(session, "create_task", label = "Refresh", style = "success", icon = icon("refresh"))
    
    values$learners <- getLearners(values$task)
    
    values$learners.short <- lapply(values$learners, "[[", "short.name")
    
    names(values$learners) <- values$learners.short
    
    values$learner.choices <- setNames(values$learners.short, lapply(values$learners, "[[", "name"))
    
    # Updating dropdown and param values for learner 1
    updateSelectInput(session, "tree_learner1", choices = values$learner.choices)
    
  })
  
  observeEvent(input$tree_learner1, {
    
    if(input$tree_learner1 == "rpart") {
      
      updateNumericInput(session, "tree_learner1_param_cart_cp", value = values$learners$rpart$param$cp$default)
      updateNumericInput(session, "tree_learner1_param_cart_minsplit", value = values$learners$rpart$param$minsplit$default)
      
    }
    
  })
  
  # #Create task
  # task <- eventReactive(input$create_task, {
  # 
  #   if(input$survival) {
  #     
  #     createTask(data(),
  #                target = list(time = req(input$time), 
  #                              status = req(input$status)),
  #                predictors = req(input$predictors))
  #     
  #     
  #   } else {  # req doesn't seem to be working?
  #     
  #     createTask(data(), target = req(input$response), predictors = req(input$predictors))
  #     
  #   }
  #   
  # })
  
  
  
  # STUFF BELOW IS WAY TOO WORDY FOR MY LIKING BUT I DON'T KNOW HOW TO LOOP IT YET
 
  output$task_type <- renderText({ if(is.null(values$task)) "- - -" else  switch(values$task.desc$task.type, 
                                                                                 "classif" = "Classifcation", 
                                                                                 "regr" = "Regression", 
                                                                                 "surv" = "Survival") })
  
  output$task_target <- renderText({ if(is.null(values$task)) "- - -" else values$task.desc$target })
  
  output$task_formula <- renderPrint({ if(is.null(values$task)) "- - -" else  print(values$task.desc$formula) }) # fix this - doesn't print
  
  output$task_nobs <- renderText({ if(is.null(values$task)) "- - -" else values$task.desc$n.obs })
  
  output$task_npredictors <- renderText({ if(is.null(values$task)) "- - -" else  values$task.desc$n.predictors }) # fix this - shows up as vector in the UI
  
  output$task_count <- renderText({ if(is.null(values$task)) "- - -" else values$task.desc$target.is.count })
  
  output$task_complete <- renderText({ if(is.null(values$task)) "- - -" else  values$task.desc$complete.cases })
  
  output$task_missings <- renderText({ if(is.null(values$task)) "- - -" else values$task.desc$has.missings })
  
  output$task_weights <- renderText({ if(is.null(values$task)) "- - -" else values$task.desc$has.weights })
  
  # Modal for createTask code + download option
 
  observeEvent(input$show_createTask, {
    
    showModal(
      modalDialog(
        title = tags$div(class = "rightAlign", downloadButton("downloadCreateTask", "Download")),
        renderUI(tags$iframe(src='createTask.html',width="100%",frameBorder="0",height="1000px")),
        footer = tagList(modalButton("Close")), 
        easyClose = T
    ))
    
    output$downloadCreateTask <- downloadHandler(
      
      filename <- function() {
        paste("createTask", "html", sep=".")
      },
      
      content = function(file) {
        file.copy("./wwww/createTask.html", file, overwrite = T)
      },
      
      contentType = "html"
      
    )
    
  })
  
  observeEvent(input$dataset, {
    
    updateButton(session, "create_task", label = "Create Task", icon = character(0), style = "primary")
    
  })
   
  

  #put weights in DT
  
  # two columns of the mtcars data
  mtcars2 = mtcars[, c('hp', 'mpg')]
  
  # render the table (with row names)
  output$x1 = DT::renderDataTable({
    
    DT::datatable(mtcars2, options = list(pageLength = 10), filter = "top")
    
  }, server = FALSE)
  
  # a scatterplot with certain points highlighted
  output$x2 = renderPlot({
    
    s1 = input$x1_rows_current  # rows on the current page
    s2 = input$x1_rows_all      # rows on all pages (after being filtered)
    
    par(mar = c(4, 4, 1, .1))
    plot(mtcars2, pch = 21)
    
    # solid dots (pch = 19) for current page
    if (length(s1)) {
      points(mtcars2[s1, , drop = FALSE], pch = 19, cex = 2)
    }
    
    # show red circles when performing searching
    if (length(s2) > 0 && length(s2) < nrow(mtcars2)) {
      points(mtcars2[s2, , drop = FALSE], pch = 21, cex = 3, col = 'red')
    }
    
    # dynamically change the legend text
    s = input$x1_search
    txt = if (is.null(s) || s == '') 'Filtered data' else {
      sprintf('Data matching "%s"', s)
    }
    
    legend(
      'topright', c('Original data', 'Data on current page', txt),
      pch = c(21, 19, 21), pt.cex = c(1, 2, 3), col = c(1, 1, 2),
      y.intersp = 2, bty = 'n'
    )
    
  })
  
  # Correlogram
  output$corrPlot = renderPlot({
    corr = cor(mtcars)
    corrplot(corr, order = "hclust", method = "color")
  })

  ############################################ END Create Task
  
  ############################################ START Model Fitting
  
  
  values$counter <- NULL
  
  
  values$counter <- reactive({ input$add_learner + 1 })

    # Original layout had just one select learner dropdown - this wasn't very flexible because the user cannot compare the results of different parameter values for the same learner. 
  # New layout has an add learner button which adds a new learner panel which allows you to compare different param values for the same learner, e.g. random forest with ntree = 1 vs random forest with ntree = 500.
  
  
  observeEvent(values$counter(), {
    id = paste0('tree_learner', values$counter())
    insertUI(
      selector = '#add',
      ## wrap element in a div with id for ease of removal
      ui = tags$div(class = "lrnrbox",
                    id = paste0("learner", id),
                    box(width = 12,
                        solidHeader = T,
                        title = div(h4(class = "learner-title", paste("Learner", values$counter())),
                                    div(class = "box-tools",
                                        tags$button(class = paste0("btn btn-box-tool"),
                                                    onclick = "removeLearner(this)",
                                                    `data-widget` = "remove",
                                                    shiny::icon("remove")
                                                    )
                                        )
                                    ),
                        
                        selectizeInput(id, "Select learner:", choices = values$learner.choices),
                        
                        tableOutput( paste0(id, "_paramtbl") ),
                        
                        # tags$div(class = "paramset", 
                        #          
                        #          fluidRow(
                        #            column(width = 4, 
                        #                   h4("Parameters")), 
                        #            
                        #            column(width = 8, 
                        #                   hr())
                        #            ),
                        #          
                        #          #textOutput(paste0(id, "param_cart_cp"))
                        #          
                        #          #uiOutput(paste0(id, "paramset")), 
                        #          
                        #          tableOutput( paste0(id, "_paramtbl") )
                        #          
                        #          ),
                        
                        tags$div(class = "pull-right",
                                 actionBttn(paste0(id, "_edit_param"), "Edit",
                                            style = "bordered", color = "success", size = "sm")
                                 ),
                        
                        bsModal(paste0(id, "_param_modal"), 
                                title = "Parameter Settings", 
                                trigger = paste0(id, "_edit_param"),
                                footer = bsButton(paste0(id, "_reset"), "Reset", style = "primary"),
                                
                                conditionalPanel(condition = paste0("input.", id, " == 'rpart'"), 
                                                 numericInput(paste0(id, "_param_cart_cp"), "cp:", value = values$learners$rpart$param$cp$default, min = 0, max = 1, step = 0.001),
                                                 numericInput(paste0(id, "_param_cart_minsplit"), "minsplt:", value = values$learners$rpart$param$minsplit$default, min = 1, step = 1))
                                
                                )
                        
                        
                    )
      ) #eo tags$div
    )
    
    values[[ paste0(id, "_paramuser") ]] <- reactive({ # is this allowed? Without the reactive({}) the value doesn't update!
      
      req(input[[id]])
      
      
      if(input[[id]] == "rpart") {
        
        return(list(cp = input[[paste0(id, "_param_cart_cp")]], minsplit = input[[paste0(id, "_param_cart_minsplit")]]))
        
        
        }
        
      
      
    })
    
    
    output[[paste0(id, "_paramtbl")]] <- renderTable({ 
      
      req(values[[ paste0(id, "_paramuser") ]]())
      
      setNames(
        data.frame(names(values[[ paste0(id, "_paramuser") ]]()), as.character(unlist(values[[ paste0(id, "_paramuser") ]]()))), 
        c("Parameters", "")
        )
      
      }, 
      
      align = 'lr',
      
      colnames = T
      
      )
    
    observeEvent(input[[paste0(id, "_reset")]], {
      
      reset(paste0(id, "_param_modal"))
      
    })
    
    
    # output[[paste0(id, "paramset")]] <- renderUI({
    #   
    #   if(input[[id]] == "rpart") {
    #     
    #     column(width = 6, 
    #            
    #            tags$form(class = "form-horizontal", 
    #                      
    #                      tags$div(class = "paramset form-group", 
    #                               tags$label(class = "col-sm-4 control-label", `for` = paste0(id, "cp"), "CP:"),
    #                               tags$p(id = paste0(id, "cp"), input[[paste0(id, "_param_cart_cp")]])
    #                               )
    #                      
    #                      )
    #            
    #            )
    #     
    #   }
    #   
    # })
    
    #output[[paste0(id, "param_cart_cp")]] <- renderText({ paste("CP: ", input[[paste0(id, "param_cart_cp")]]) })
    
    
  })
  
  # paramModal <- function() {
  #   
  #   modalDialog(
  #     
  #     "Param values",
  #     
  #     title = "Parameter Settings",
  #     footer = tagList(
  #       modalButton("Cancel"),
  #       bsButton("reset", "Reset", style = "primary")),
  #     easyClose = T
  #     
  #   )
  #   
  # }
  # 
  # observeEvent(input$edit_param, {
  #   showModal(paramModal())
  # })
  
  # Mock plots
  ### Create a list of learners
  lrns = list(
    makeLearner("classif.rpart", id = "rpart"),
    makeLearner("classif.C50", id = "C50"),
    makeLearner("classif.randomForest", id = "randomForest")
  )  
  
  
  tasks = list(iris.task)
  rdesc = makeResampleDesc("CV", iters = 10)
  meas = list(ber, acc)
  bmr = benchmark(lrns, tasks, rdesc, meas, show.info = FALSE)
  
  output$cvPlot = renderPlot({
    
    gridExtra::grid.arrange(
      
      plotBMRBoxplots(bmr, measure = ber) + 
        aes(colour = learner.id) + 
        theme(strip.text.x = element_text(size = 8), 
              legend.position = "none",
              axis.text.x = element_text(angle = 0, hjust = 0.5), 
              axis.title.x = element_text(size = 8)) +
        labs(x = "Learner",
             y = "BER") +
        coord_flip(expand = T),
      
      plotBMRBoxplots(bmr, measure = acc) + 
        aes(colour = learner.id) +
        theme(strip.text.x = element_text(size = 8), 
              axis.text.x = element_text(angle = 0, hjust = 0.5), 
              axis.title.x = element_text(size = 8)) +
        labs(x = "",
             y = "Accuracy") + 
        coord_flip(expand = T),
      
      nrow = 1
      
    )
    
  }) 
  
  # Variable importance plots
  imp.rpart = getFeatureImportance(getBMRModels(bmr, learner.ids = "rpart")$`iris-example`$rpart[[1]])$res
  #imp.C50 = getFeatureImportance(getBMRModels(bmr, learner.ids = "C50")$`iris-example`$C50[[1]])$res
  imp.randomForest = getFeatureImportance(getBMRModels(bmr, learner.ids = "randomForest")$`iris-example`$randomForest[[1]])$res
  #OR USE SAPPLY TO EXTRACT ELEMENT OF CERTAIN CLASS
  imp.rpart <- imp.rpart %>%
    gather(variable, importance, seq_len(ncol(imp.rpart))) #%>
  imp.randomForest <- imp.randomForest %>%
    gather(variable, importance, seq_len(ncol(imp.rpart))) #%>%
  
  output$impPlot = renderPlot({
    
    gridExtra::grid.arrange(
      
      #arrange(desc(importance))
      ggplot(imp.rpart, aes(x = reorder(variable, importance), y = importance)) +
        geom_bar(stat = "identity") +
        labs(title = "CART", x = "Variable", y = "Importance") +
        coord_flip(),
      
      ggplot(imp.randomForest, aes(x = reorder(variable, importance), y = importance)) +
        geom_bar(stat = "identity") +
        labs(title = "Random Forest", x = "Variable", y = "Importance") +
        coord_flip(),
      
      nrow = 1
      
    )
    
  })
}