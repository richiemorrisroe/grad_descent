
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

##' An S4 class for Expression objects 
##' @slot coefficient - an integer representing the coefficient
##' @slot variable - the indeterminate in the equation
##' @slot exponent the exponent portion of the object
##' See above
##' @export
setClass("Exp", slots=list(coefficient="numeric",
                                           variable="character",
                                 exponent="numeric"))
##' Convert a string to a polynomial
##'
##' Code assumes strings of the form 20x, 10^2 or 2xy^2
##' @title to_expression
##' @param string 
##' @return a polynomial object
##' @author richie
##' @export
to_expression <- function(string) {
    var <- stringr::str_extract(string, "([A-Za-z]+)")
    coeff <- stringr::str_extract(string, "([0-9]+)")
    message("var is: ", var, "\n", "coeff is: ", coeff)
    if(grepl("\\^", x=string)) {
        exp <- stringr::str_extract(string, "([0-9]+)$")
    }
    else {
        exp <- 0
    }
    exp <- methods::new("Exp", coefficient=as.integer(coeff),
               variable=var,
               exponent=as.integer(exp))
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
exponent_expression <- function(object, ...) {
    exp <- object@exponent

}
##' @export
exponent <- function(object, ...) {
    standardGeneric("exponent")
}

setMethod("exponent", signature(object="Exp"),
          definition=exponent_expression)

##' @export
variable_expression <- function(object, ...) {
    object@variable
}
setMethod("variable", signature(object="Exp"),
          definition=variable_expression)

##'Get the coefficients of an expression
##'
##'
##' @param object an expression object
##' @param ...
##' @export
coef_expression <- function(object, ...) {
    object@coefficient
}
##' A coefficient method for Expression objects
##'
##' As above
##' @export
setMethod("coef",
    signature(object = "Exp"),
    definition=coef_expression
)

##' differentiate a expression object
##'
##' returns a new expression object
##' @title diff_expression
##' @param expression 
##' @return a new expression
##' @author richie
##' @export
diff_expression <- function(object, ...) {
    newxp <- object@exponent - 1
    newcoeff <- object@exponent * coef(object)
    var <- variable(object)
    res <-  methods::new("Exp",
                         coefficient=newcoeff,
                         variable=var,
                         exponent = newxp)

}
##' A generic to perform differentiation
##'
##' Works for expression objects right now
##' @export
setGeneric("differentiate", function(object, ...) {
    standardGeneric("differentiate")
})
##' @export
setMethod("differentiate", signature(object="Exp"),
          definition=diff_expression)

##' An S4 class representing an Polynomial object
##' @slot text a character object containing an equation
##' @slot members a list of polynomial objects
##'
##' See above
##' @export
setClass("Polynomial", representation = list(text="character", members="list"))

##' convert a string in polynomial form to an Equation object
##'
##' I really need to rename some of this stuff
##' @title as_polynomial
##' @param string an equation of the form cx^n+/-cx^n.., c
##' @return an equation object representing the 
##' @author richie
##' @export
polynomial <- function(string) {
    textlist <- unlist(expression_to_text(string))
    ops <- unlist(stringr::str_extract_all(string, "\\+|\\-"))
    polylist <- sapply(textlist, to_expression)
    eq <- methods::new("Polynomial", text=string, members=polylist,operators=ops)
    return(eq)
}
diff_polynomial <- function(eq) {
    #todo
}

##' convert an expression object to a function over the variable(s)
##'
##' Right now only works for one variable functions
##' @title polynomial_to_function
##' @param polynomial 
##' @return a function which takes an argument x and computes the value of the function
##' @author richie
##' @export
expression_to_function <- function(expression) {
    if(exponent(expression)>=1) {
        return(function(x) {
            res <-   coef(expression)  * x ^(exponent(expression))
        })}
    else {
            return(function()) {
                res <-  coef(expression) * 1 ^(exponent(expression))
            }

        }
}

##' An S4 class representing an Polynomial object
##' @slot text a character object containing an equation
##' @slot members a list of polynomial objects
##'@slot operators a vector of additions/subtractions
##' See above
##' @export
setClass("Polynomial", representation = list(text="character", members="list", operators="character"))

##' convert a string in polynomial form to an Equation object
##'
##' I really need to rename some of this stuff
##' @title as_polynomial
##' @param string an equation of the form cx^n+/-cx^n.., c
##' @return an equation object representing the 
##' @author richie
##' @export
polynomial <- function(string) {
    textlist <- unlist(expression_to_text(string))
    ops <- unlist(stringr::str_extract_all(string, "\\+|\\-"))
    polylist <- sapply(textlist, to_expression)
    eq <- methods::new("Polynomial",
                       text=string,
                       members=polylist,
                       operators=ops)
    return(eq)
}
