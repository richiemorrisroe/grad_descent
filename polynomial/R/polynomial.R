
##' Convert mathematical expression stored as string  into its component parts
##'
##' Right now just splits on + and -
##' @title expression_to_text
##' @param string 
##' @return a vector of characters representing an equation
##' @author richie
##' @export
expression_to_text <- function(string) {
    res <- stringr::str_split(string, "\\+|-")
}

##' @export
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
##' @export
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

##' differentiate a polynomial object
##'
##' returns a new polynomial
##' @title diff_poly
##' @param expression 
##' @return a new polynomial
##' @author richie
##' @export
diff_poly <- function(expression) {
    res <- with(expression,
                new("Polynomial", coefficient=exponent*coefficient,
                    variable=variable,
                    exponent = exponent-1))
    
}
##' @export
setGeneric("differentiate", function(object) {
    standardGeneric("differentiate")
})
##' @export
setMethod("differentiate", signature(object="Polynomial"),
          definition=diff_poly)

##' @export
setClass("Equation", representation = list(text="character", members="list"))
##' convert a string in polynomial form to an Equation object
##'
##' I really need to rename some of this stuff
##' @title as_equation
##' @param string an equation of the form cx^n+/-cx^n.., c
##' @return an equation object representing the 
##' @author richie
##' @export
as_equation <- function(string) {
    textlist <- unlist(expression_to_text(string))
    polylist <- sapply(textlist, to_polynomial)
    eq <- new("Equation", text=string, members=polylist)
    return(eq)
}
diff_equation <- function(eq) {
    #todo
}

##' @export
setGeneric("exponent", function(object, ...) {
    standardGeneric("exponent")
})
##' @export
setGeneric("variable", function(object, ...) {
    standardGeneric("variable")
})
##' @export
exponent <- function(obj, ...) {
    standardGeneric("exponent", fdef=exp_poly)
}
##' @export
exp_poly <- function(polynomial) {
    exp <- polynomial@exponent

}
##' @export
var_poly <- function(polynomial) {
    polynomial@var
}
##' @export
coef_poly <- function(object, ...) {
    object@coefficient
}
##' @export
setMethod("coef",
    signature(object = "Polynomial"),
    definition=coef_poly
)

##' convert a polynomial object to a function over the variable(s)
##'
##' Right now only works for one variable functions
##' @title polynomial_to_function
##' @param polynomial 
##' @return a function which takes an argument x and computes the value of the function
##' @author richie
##' @export
polynomial_to_function <- function(polynomial) {
    return(function(x) {
        res <-   coef(polynomial)  * x ^(exponent(polynomial))
    })}
