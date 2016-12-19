
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
setGeneric("differentiate", function(object) {
    standardGeneric("differentiate")
})
setMethod("differentiate", signature(object="Polynomial"),
          definition=diff_poly)

setClass("Equation", representation = list(text="character", members="list"))
as_equation <- function(string) {
    textlist <- unlist(expression_to_text(string))
    polylist <- sapply(textlist, to_polynomial)
    eq <- new("Equation", text=string, members=polylist)
    return(eq)
}
diff_equation <- function(eq) {
    #todo
}

setGeneric("exponent", function(object) {
    standardGeneric("exponent")
})

setGeneric("var", function(object) {
    standardGeneric("var")
})

exponent <- function(obj) {
    standardGeneric("exponent", fdef=exp_poly)
}
exp_poly <- function(polynomial) {
    exp <- polynomial@exponent

}
var_poly <- function(polynomial) {
    polynomial@var
}

setMethod("coef",
    signature(object = "Polynomial"),
    definition=coef_poly
)
coef_poly <- function(object, ...) {
    object@coefficient
}
