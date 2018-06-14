metaFun <- function(type, name, fun) {
    if (!is.character(type) || length(type) != 1) 
        stop('type must be a character string of length 1')
    if (!is.character(name) || length(name) != 1)
        stop('name must be a character string of length 1')
    if (!is.function(fun))
        stop('fun should be, well, a function ...')

    structure(list(type = type, name = name, fun = fun), class = 'inzmetafun')
}

print.inzmetafun <- function(x, ...) {
    cat(sprintf('%s: a %s variable', x$name, x$type))
}

## Accessors:
getname <- function(x) x$name
gettype <- function(x, abbr = FALSE) {
    if (abbr) {
        return(switch(x$type, 
            'integer' = 'i', 
            'numeric' = 'n', 
            'factor' = 'c'))
    }
    x$type
}