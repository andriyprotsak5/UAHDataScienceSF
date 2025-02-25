######################
#-RELATIVE FRECUENCY-#
######################

#' Relative Frequency Function
#'
#' This function calculates the relative frequency of a value in a numeric vector.
#'
#' @param v Optional numeric vector (not needed for interactive mode)
#' @param x Optional numeric value to count (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The relative frequency of x in v (for non-interactive mode)
#' @importFrom crayon bold green blue red yellow
#'
#' @examples
#' data <- c(1,4,3,3,2,5,7,12,1,2,3,12)
#' value <- 12
#'
#' # Simple calculation
#' relative_frequency(data, value)
#'
#' # Learning mode
#' relative_frequency(data, value, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' relative_frequency(interactive = TRUE)
#' }
#'
#' @export
relative_frequency <- function(v = NULL, x = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && (is.null(v) || is.null(x))) {
    stop("data vector and value are required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("relative-frequency.jpeg")
    cont = 0

    message("\nInsert your data set:\n")
    buffer = getUserAction()

    #show data sorted
    buffer_sort = sort(buffer)
    message("\nData sorted : ")

    drawVector(buffer_sort)

    size = length(buffer)

    message("\nOK! Next Move !!\n")
    rand <- sample(buffer,1)
    flag <- 1

    while(flag == 1) {
      message("Please, insert the relative frecuency of the data '", rand ,"' : ")
      message("\n(remember your data) -> ", buffer , "\n")
      message("If the number has decimals, round to the 3rd\n")
      resp <- as.numeric(readline(prompt = ""))
      if(resp == round(relative_frequency(buffer,rand), 3)) {
        message(bold("\n\nWell done !\n\n"))
        flag <- 0
      } else {
        cont <- cont + 1
        message("Ups, that might not be correct... Try again")
        if(cont >= 1) {
          message(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    message(bold("\n__RELATIVE FRECUENCY CALCULUS__ \n"))
    data <- sort(as.vector(v))
    size <- length(v)
    message("\nThe relative frequency is the quotient between the absolute frequency of a certain value and the total number of data\n")
    message(green("\nFormula -> (Abs_frec(X) / N ) -> Where 'X' is the element we want to examine\n"))
    message(bold("\n__Use Example__\n"))
    message("\nStep 1: count the number of times that the element ", blue(x) ," appears in our data set\n")
    message("\nOur data set: ")

    drawVector(data)

    count = absolute_frequency(v,x)
    message("\n\nNow count the number of times that the element ", blue(x) ," appears: ", blue(count), "\n")
    for(i in 1:size) {
      if(i == size) {
        if(v[i] == x) {
          message(red(v[i]))
        } else{
          message(v[i])
        }
      } else{
        if(v[i] == x) {
          message(red(v[i],","))
        } else{
          message(v[i], ",")
        }
      }
    }
    message("\nStep 2: divide it by the length of the data set\n")
    rel_frec = relative_frequency(v,x)
    message("\nSolution --> relative_frecuency = (absolute_frecuency(x) / length(data)) = ", count, " / ", size, " = ", bold(rel_frec), ".\n")

    message("\nNow try by your own! :D\n")
    message("\nUse relative_frequency(interactive = TRUE) function to practice.\n")
    return(rel_frec)
  }

  # Simple calculation mode
  v <- as.vector(v)
  x <- as.integer(x)
  f_abs = absolute_frequency(v,x)
  f_rel = (f_abs/length(v))
  return(f_rel)
}
