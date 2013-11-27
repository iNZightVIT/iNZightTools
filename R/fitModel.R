fit.model <-
    function(y, x, family = 'gaussian', design = 'simple',
             data = NULL, svydes = NA, ...) {
      # This function takes input from iNZight software, and perpares
      # it for iNZightRegression package.
      # y            character string representing the response,
      # x            character string of the explanatory variables,
      # family       gaussian, binomial, poisson (so far, no others
      #              will be added)
      # design       data design specification. one of 'simple',
      #              'survey' or 'experiment'
      # data         if NULL, data assumed to be available in R session.
      #              Otherwise, name of the object containing the data.
      # svydes       a list of arguments to be passed to the svydesign
      #              function, excluding data (defined above)
      # ...          further arguments to be passed to lm, glm, svyglm,
      #              such as offset, etc.
      #
      # Value: a fitted model object, either an lm, glm, or svyglm,
      # depending on the design and family of the model.

        Formula <- paste(y, x, sep = ' ~ ')

        if (design == 'simple') {
          # simple IID data:
            if (family == 'gaussian') {
              # Simple linear regression model:
                fit <- lm(Formula, data = data, ...)
            } else {
              # general linear model:
                fit <- glm(Formula, data = data, family = family,
                           ...)
            }
        } else if (design == 'survey') {
          # complex survey design:
            require(survey)
            svy.des <- paste('svydesign(',
                             paste(svydes, collapse = ', '),
                             ', data = data)', sep = '')
            svy.design <- eval(parse(text = svy.des))
            fit <- svyglm(Formula, design = svy.design,
                          family = family, ...)
        } else if (design == 'experiment') {
          # experimental design:
            stop('Experiments are not yet implemented. \n')
        }
        
        fit
    }



