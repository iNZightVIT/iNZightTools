#' Read CSV with iNZight metadata
#' 
#' This function will read a CSV file with iNZight metadata in the header.
#' This allows plain text CSV files to be supplied with additional comments
#' that describe the structure of the data to make import and data handling easier.
#' 
#' The main example is to define factor levels for an integer variable in large data sets.
#' 
#' @param file the plain text file with metadata 
#' @return a data frame
#' @author Tom Elliott
#' @export
readMetadata2 <- function(file) {
    if (!is.character(file)) stop('file must be a string')
    if (!file.exists(file)) stop('That file doesn\'t seem to exists.')

    ## fetch the metadata
    meta <- readMetaComments(file)
    
    ## fetch the first few rows of the data ... 
    suppressMessages({
        dtop <- readr::read_csv(file, comment = '#', n_max = 5, progress = FALSE)
    })
    
    ## columns in meta not in dataset:
    mvars <- sapply(meta$columns, getname)
    dvars <- names(dtop)
    if (any(!mvars %in% dvars)) {
        stop('Some variables defined in metadata not in dataset: ',
            paste(mvars[!mvars %in% dvars], collapse = ', '))
    }

    ## use metadata to determine column types
    mtypes <- sapply(meta$columns, gettype, abbr = TRUE)
    names(mtypes) <- mvars
    ctypes <- paste(ifelse(dvars %in% mvars, mtypes[dvars], '?'), collapse = "")
    
    ## read the data (strings remain as strings)
    data <- readr::read_csv(file, col_types = ctypes, comment = '#')

    ## convert factors appropriately
    lapply(meta$columns, function(c) {
        switch(gettype(c),
            'factor' = {
                n <- getname(c)
                data[[n]] <<- c$fun(data[[n]])
            },
            {
                ## do nothing otherwise (for now?)
            })
    })

    ## convert any remaining character columns to factors
    lapply(names(data)[sapply(data, is.character)], function(vn) {
        data[[vn]] <<- as.factor(data[[vn]])
    })

    as.data.frame(data)
}


readMetaComments <- function(file) {
    con <- file(file, open = 'r')
    meta <- character()
    while (grepl("^#'", x <- readLines(con, n = 1)))
        meta <- c(meta, trimws(gsub("^#'", "", x)))
    close(con)

    if (length(meta) == 0) 
        return(NULL)

    md <- list(title = file, desc = NULL, columns = list())

    ## Grab the title, if present - always the first line of metadata
    if (!grepl('^@', meta[1])) {
        md$title = trimws(gsub('^@', '', meta[1]))
        meta <- meta[-1]
    }

    ## Remove any empty lines between title description
    while (meta[1] == "") 
        meta <- meta[-1]

    ## Grab the description - all content between the title and first @
    desc.end <- grep('^@', meta)[1] - 1
    if (desc.end > 0) {
        desc <- meta[1:desc.end]
        ## drop off the last empty line
        if (desc[length(desc)] == "")
            desc <- desc[-length(desc)]
        ## add paragraph breaks
        desc[desc == ""] <- "\n\n"
        ## add space at END of each line, but collapse with no space so new lines are flush
        md$desc <- trimws(paste(trimws(desc), " ", collapse = ""))
    }

    ## check remaining lines commence with a @; drop any that don't
    meta <- meta[-(1:desc.end)]
    if (any(!grepl('^@', meta))) {
        warning('All lines after title and description should start with a @. Skipping those that don\'t')
        meta <- meta[grepl('^@', meta)]
    }

    ## process each column
    md$columns <- processCols(meta)

    md
}

processCols <- function(metadata) {
    lapply(metadata, processCol)
}

processCol <- function(x) {
    txt <- cleanstring(x)
    txt <- strsplit(x, ' ')[[1]]

    ## now x is a vector of components
    if (length(txt) == 1) 
        stop('Invalid metadata: ', x, '. Needs at least a type and a column name')

    type <- txt[1]
    txt <- txt[-1]

    ## use s3 methods to provide extensible method for processing metadata
    ## drop the type as its redundant from here ...
    class(txt) <- sprintf('inz.meta.%s', gsub('^@', '', type))
    .processCol(txt)
}

cleanstring <- function(x) {
    ## return a cleaned meta data string 
    
    ## how about dealing with quotes?
    
    ## removing additional spaces around: '=', '@', '+', '-', '\', ','
    x <- gsub('\\s*([,@\\[\\=\\+\\-\\\\])\\s*','\\1', x, perl = TRUE)

    x
}


.processCol <- function(x) UseMethod('.processCol')

#' unknown, guess it
.processCol.default <- function(x) {
    warning('Unknown type: ', class(x))
    return(NULL)
}

.processCol.inz.meta.numeric <- function(x) {
    ## The name of the variable
    vname <- x[1]

    ## some other calculations? perhaps ...
    
    ## and return a meta object
    metaFun(type = 'numeric', name = vname, 
        fun = numeric)
}

.processCol.inz.meta.factor <- function(x) {
    vname <- x[1]

    ## extract levels=labels (level in output, label in dataset)
    if (grepl('\\[', vname)) {
        f <- strsplit(vname, '\\[')[[1]]
        vname <- f[1]
        lvlstr <- trimws(strsplit(gsub('\\]', '', f[2]), ',')[[1]])
        labels <- levels <- character(length(lvlstr))
        for(i in 1:length(lvlstr)) {
            lvl <- strsplit(lvlstr[i], '=')[[1]]
            levels[i] <- lvl[1]
            labels[i] <- lvl[length(lvl)]
        }        
    } else {
        levels <- labels <- NULL
    }
    if (is.null(levels)) {
        fun <- "factor"
    } else {
        fun = sprintf("function(x) { 
                lvls <- %s
                labs <- %s
                for (i in 1:length(lvls)) {
                    if (lvls[i] != labs[i]) {
                        x[ x %s strsplit(labs[i], '|', fixed = TRUE)[[1]] ] <- lvls[i]
                    }
                }
                factor(x, levels = lvls)
            }", deparse(levels), deparse(labels), "%in%")
    }
    metaFun(type = 'factor', name = vname, fun = eval(parse(text = fun)))
        
}