#########################
#-RELATIVE FRECUENCY ACUM-#
#########################

#' Relative Accumulated Frequency Function
#'
#' This function calculates the relative accumulated frequency of a value in a numeric vector.
#'
#' @param v Optional numeric vector (not needed for interactive mode)
#' @param x Optional numeric value to count (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The relative accumulated frequency of x in v (for non-interactive mode)
#' @importFrom crayon bold green blue red yellow
#'
#' @examples
#' data <- c(1,4,3,3,2,5,7,12,1,2,3,12)
#' value <- 12
#'
#' # Simple calculation
#' relative_acum_frequency(data, value)
#'
#' # Learning mode
#' relative_acum_frequency(data, value, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' relative_acum_frequency(interactive = TRUE)
#' }
#'
#' @export
relative_acum_frequency <- function(v = NULL, x = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && (is.null(v) || is.null(x))) {
    stop("data vector and value are required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("relative_acum_frequency.jpeg")
    cont = 0

    message("\nInsert your data set:\n")
    buffer = getUserAction()

    #show data sorted
    buffer = sort(buffer)
    message("\nData sorted : ")

    drawVector(buffer)

    size = length(buffer)

    message("\nOK! Next Move !!\n")
    rand <- sample(buffer,1)
    flag <- 1

    while(flag == 1) {
      message("Please, insert the relative acumulated frecuency of the data '", blue(rand) ,"' : ")
      message("\n(remember your data) -> ", buffer , "\n")
      message("If the number has decimals, round to the 3rd\n")

      resp <- as.numeric(readline(prompt = ""))
      if(resp == round(relative_acum_frequency(buffer,rand), 3)) {
        message(bold("\n\nWell done !\n\n"))
        flag <- 0
      } else {
        cont <- cont + 1
        message("Ups, that might not be correct... Try again")
        if(cont == 1) {
          message(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        } else if(cont > 1) {
          message(yellow("\nHint 2 -> Hey! remember that the maximum value for the relative acumulated frecuency is 1!\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    message(bold("\n__RELATIVE ACUMULATED FRECUENCY CALCULUS__ \n"))
    data <- sort(as.vector(v))
    size <- length(v)
    message("\nThe relative acumulated frequency is the quotient between the sum of the absolute frequency of the values minors or equals than the value we want to examine, and the total number of data\n")
    message(green("\nFormula -> (Summation(abs_frecuency <= X) / N ) -> Where 'X' is the element we want to examine\n"))
    message(bold("\n__Use Example__\n"))
    message("\nStep 1: count the number of times that the elements minors or equals than ", blue(x) ," appears in our data set\n")
    message("\nOur data set: ")

    drawVector(data)

    count = relative_acum_frequency(data,x)
    message("\n\nNumber of times that elements minors or equals to ", blue(x) ," appears = ", blue(count), "\n")
    for(i in 1:size) {
      if(i == size) {
        if(v[i] == x || v[i] < x) {
          message(red(v[i]))
        } else{
          message(v[i])
        }
      } else{
        if(v[i] == x || v[i] < x) {
          message(red(v[i],","))
        } else{
          message(v[i], ",")
        }
      }
    }
    message("\nStep 2: divide it by the length of the data set\n")
    rel_frec_acum = relative_acum_frequency(data,x)
    message("\nSolution --> relative_frecuency_acum = (Summation(abs_frecuency <= X) / length(data)) = ", count, " / ", size, " = ", bold(rel_frec_acum), ".\n")

    message("\nNow try by your own! :D\n")
    message("\nUse relative_acum_frequency(interactive = TRUE) function to practice.\n")
    return(rel_frec_acum)
  }

  # Simple calculation mode
  if(is.element(x,v)) {
    v <- sort(as.vector(v))
    x <- as.integer(x)
    count = 0

    for(i in 1:x) {
      if(is.element(i,v)) {
        count = count + absolute_frequency(v,i)
      }
    }

    res = count / length(v)
    return(res)
  } else {
    message("Not found element [",x,"]\n")
  }
}
