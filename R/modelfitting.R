##' Fit models 
##'
##' wrapper function
##' 
##' @title Fit a Model
##' 
##' @param y character string representing the response,
##' @param x character string of the explanatory variables,
##' @param data name of the object containing the data.
##' @param family gaussian, binomial, poisson (so far, no others will be added)
##' @param design data design specification. one of 'simple', 'survey' or 'experiment'
##' @param svydes  a vector of arguments to be passed to the svydesign function, excluding data (defined above)
##' @param ... further arguments to be passed to lm, glm, svyglm, such as offset, etc.
##' 
##' @return A fit object (lm, glm, or svyglm)
##' 
##' @author Tom Elliott
##' 
##' @export
fitModel <-
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
       # simple IID data:
        if (family == 'gaussian') {
          # Simple linear regression model:
            args <- paste(Formula, dat, sep = ', ')
            if (xargs != "")
                args <- paste(args, xargs, sep = ', ')
            call <- paste('lm(', args, ')', sep = '')
        } else {
          # general linear model:
            args <- paste(Formula, dat, fam, sep = ', ')
            if (xargs != "")
                args <- paste(args, xargs, sep = ', ')
            call <- paste('glm(', args, ')', sep = '')
        }
    } else if (design == 'survey') {
      # complex survey design:
      # set up the svyglm function call
        args <- paste(Formula, fam, "design = svy.design", sep = ', ')
        if (xargs != "")
            args <- paste(args, xargs, sep = ', ')
        call <- paste('svyglm(', args, ')', sep = '')
    } else if (design == 'experiment') {
      # experimental design:
        stop('Experiments are not yet implemented. \n')
    }

    # at this stage we just return the call
      call
  }


##' Fit a survey design
##'
##' Fit a survey design to an object
##' 
##' @title Fit Survey Design
##' 
##' @param svydes a design
##' @param dataset.name a dataset name
##' 
##' @return a survey object
##' 
##' @author Tom Elliott
##'
##' @export
fitDesign <-
    function(svydes, dataset.name) {
        if(all(svydes==""))
            return()
        svy.des <- paste0('survey::svydesign(',
                          paste(svydes, collapse = ', '),
                          ', data = ', dataset.name, ')')

        eval(parse(text = svy.des), .GlobalEnv)
    }
