
##' Convert mathematical expression into its component parts
##'
##' Right now just splits on + and -
##' @title expression_to_text
##' @param string 
##' @return a vector of characters representing an equation
##' @author richie
expression_to_text <- function(string) {
    res <- stringr::str_split(string, "\\+|-")
}

setClass("Polynomial", slots=list(coefficient="integer",
                                           variable="character",
                                  exponent="integer"))
##' Convert a string to a polynomial
##'
##' Code assumes strings of the form 20x, 10^2 or 2xy^2
##' @title to_polynomial
##' @param string 
##' @return a polynomial object
##' @author richie
to_polynomial <- function(string) {
    var <- stringr::str_extract(string, "([A-Za-z]+)")
    coeff <- stringr::str_extract(string, "([0-9]+)")
    message("var is: ", var, "\n", "coeff is: ", coeff)
    if(grepl("\\^", x=string)) {
        exp <- stringr::str_extract(string, "([0-9]+)$")
    }
    else {
        exp <- 0
    }
    exp <- new("Polynomial", coefficient=as.integer(coeff),
               variable=var,
               exponent=as.integer(exp))
}

diff_poly <- function(expression) {
    res <- with(expression,
                new("Polynomial", coefficient=exponent*coefficient,
                    variable=variable,
                    exponent = exponent-1))
    
}
setGeneric("differentiate", def=diff_poly)
