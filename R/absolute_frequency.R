######################
#-ABSOLUTE FRECUENCY-#
######################

#' Absolute Frequency Function
#'
#' This function calculates the absolute frequency of a value in a numeric vector.
#'
#' @param v Optional numeric vector (not needed for interactive mode)
#' @param x Optional numeric value to count (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The absolute frequency of x in v (for non-interactive mode)
#' @importFrom crayon bold green blue red yellow
#'
#' @examples
#' data <- c(1,4,3,3,2,5,7,12,1,2,3,12)
#' value <- 12
#'
#' # Simple calculation
#' absolute_frequency(data, value)
#'
#' # Learning mode
#' absolute_frequency(data, value, learn = TRUE)
#'
#' # Interactive mode
#' \dontrun{
#' absolute_frequency(interactive = TRUE)
#' }
#'
#' @export
absolute_frequency <- function(v = NULL, x = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && (is.null(v) || is.null(x))) {
    stop("data vector and value are required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("absolute_frequency.jpeg")
    cont = 0

    cat("\nInsert your data set:\n")
    buffer = getUserAction()

    #show data sorted
    buffer_sort = sort(buffer)
    cat("\nData sorted : ")

    drawVector(buffer_sort)

    size = length(buffer)

    cat("\nOK! Next Move !!\n")
    rand <- sample(buffer,1)
    flag <- 1

    while(flag == 1) {
      cat("Please, insert the absolute frecuency (Fi) of the data '", rand ,"' : ")
      cat("\n(remember your data) -> ", buffer , "\n")
      resp <- as.numeric(readline(prompt = ""))
      if(resp == absolute_frequency(buffer,rand)) {
        cat(bold("\n\nWell done !\n\n"))
        flag <- 0
      } else {
        cont <- cont + 1
        cat("Ups, that might not be correct... Try again")
        if(cont >= 1) {
          cat(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    cat(bold("\n__ABSOLUTE FRECUENCY CALCULUS__ \n"))
    data <- sort(as.vector(v))
    size <- length(v)
    cat("\nThe absolute frequency (Ni) of a value Xi is the number of times the value is in the set (X1, X2, ..., XN)\n")
    cat(green("\nFormula -> N1 + N2 + N3 + ... + Nk -> Nk = X (Where 'X' is the element we want to examine)\n"))
    cat(bold("\n__Use Example__\n"))
    cat("\nAll we need to do is count the number of times that the element ", x ," appears in our data set\n")
    cat("\nOur data set: ")

    drawVector(data)

    count = absolute_frequency(v,x)
    cat("\n\nNow count the number of times that the element ", blue(x) ," appears: ", bold(count), "\n")
    for(i in 1:size) {
      if(i == size) {
        if(v[i] == x) {
          cat(red(v[i]))
        } else{
          cat(v[i])
        }
      } else{
        if(v[i] == x) {
          cat(red(v[i],","))
        } else{
          cat(v[i], ",")
        }
      }
    }

    cat("\nNow try by your own! :D\n")
    cat("\nUse absolute_frequency(interactive = TRUE) function to practice.\n")
    return(count)
  }

  # Simple calculation mode
  v <- as.vector(v)
  x <- as.integer(x)
  count = 0
  for(i in 1:length(v)) {
    if(v[i] == x) {
      count = count + 1
    }
  }
  return(count)
}
