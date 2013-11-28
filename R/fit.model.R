fit.model <-
function(y, x, data, family = 'gaussian',
             design = 'simple', svydes = NA, ...) {
        
      ##################################################################
      # This function takes input from iNZight software, and perpares
      # it for iNZightRegression package.
      # y            character string representing the response,
      # x            character string of the explanatory variables,
      # family       gaussian, binomial, poisson (so far, no others
      #              will be added)
      # design       data design specification. one of 'simple',
      #              'survey' or 'experiment'
      # data         name of the object containing the data.
      # svydes       a vector of arguments to be passed to the svydesign
      #              function, excluding data (defined above)
      # ...          further arguments to be passed to lm, glm, svyglm,
      #              such as offset, etc.
      #
      # Value: a fitted model object, either an lm, glm, or svyglm,
      # depending on the design and family of the model.
      #
      # Details: 
      ##################################################################

        Formula <- paste(y, x, sep = ' ~ ')
        dat <- paste("data", data, sep = ' = ')
        fam <- paste("family", family, sep = ' = ')

      # Deal with extra arguments (eg. weights, offset ...)
        xarg <- list(...)
        xargs <- paste(names(xarg), xarg, sep = ' = ', collapse = ', ')
        
        if (design == 'simple') {
            ## simple IID data:
            if (family == 'gaussian') {
                ## Simple linear regression model:
                args <- paste(Formula, dat, xargs, sep = ', ')
                call <- paste('lm(', args, ')', sep = '')
            } else {
                ## general linear model:
                args <- paste(Formula, dat, fam, xargs, sep = ', ')
                call <- paste('glm(', args, ')', sep = '')
            }
        } else if (design == 'survey') {
            ## complex survey design:

          # set up the survey design
            svy.des <- paste('svydesign(',
                             paste(svydes, collapse = ', '),
                             ', data = data)', sep = '')
            svy.design <- eval(parse(text = svy.des))

          # set up the svyglm function call
            args <- paste(Formula, fam, "design = svy.design",
                          xargs, sep = ', ')
            call <- paste('svyglm(', args, ')', sep = '')
        } else if (design == 'experiment') {
            ## experimental design:
            stop('Experiments are not yet implemented. \n')
        }

      # save the call to the history, and return 
        #histScript <<- c(histScript, call)  # this *will* change
        fit <- cHist(call)
        fit
    }
