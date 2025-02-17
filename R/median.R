######################
#######-MEDIAN-#######
######################

#' Median Function
#'
#' This function calculates the median of a numbers vector.
#'
#' @param x Optional numeric vector (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The median of the vector (for non-interactive mode)
#' @importFrom crayon bold green blue yellow
#' @examples
#' data <- c(1,3,2,5,12,4,4,2,9)
#'
#' # Simple calculation
#' median_(data)
#'
#' # Learning mode
#' median_(data, learn = TRUE)
#'
#' # Interactive mode
#' if(interactive()){
#' median_(interactive = TRUE)
#' }
#'
#' @export
median_ <- function(x = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && is.null(x)) {
    stop("data vector is required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("median.png")
    cont_aux <- 0
    message("\nInsert your data set:\n")
    buffer = getUserAction()

    message("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      message("Please, insert the result of the median calculus for your data : ")
      usr_resp <- as.numeric(readline(prompt = ""))
      if(usr_resp == round(median_(buffer), 3)) {
        flag <- 0
        message(bold("\n\nWell done !\n\n"))
      } else {
        cont_aux <- cont_aux + 1
        message("Ups, that might not be correct...")
        if(cont_aux == 1) {
          message(yellow("\nHint -> Psst!... Look at the formula on the plot panel at your side -->\n\n"))
        }
        else if(cont_aux == 2 ) {
          message(yellow("\nHint 2 -> The element at the middle of the dataset\n\n"))
        }
        else if(cont_aux > 2) {
          message(yellow("\nHint 3 -> Check if it has a pair number of elements or not...\n\n"))
        }
      }
    }
    return(invisible(NULL))
  }

  # Learning mode
  if (learn) {
    v <- as.vector(x)
    message(bold("\n__MEDIAN CALCULUS__ \n"))
    message("\nThe median of a dataset is the value in the middle of the sorted data. It's important to know that the data must be sorted. If the dataset has a pair number of elements, we should select both in the middle to add each other and get divided by two. If the dataset has a no pair number of elements, we should select the one in the middle.\n")
    message(green("\nFormula -> 1/2(n+1) where n -> vector size\n"))
    message(bold("\n__Use Example__\n"))
    message("\nFirst step : identify if the vector has a pair number of elements\n")
    message("\nThe content of the vector is: ")
    v <- sort(v)

    drawVector(v)

    message("\n")
    message("\nSecond step: depending of the number of elements\n")
    if(length(v) %% 2 == 0) {
      message("\nIt has a PAIR number of elements (", blue(length(v)), ")\n")
      aux <- v[(length(v) / 2)]
      message("\nWe take the 'n/2' element -> ", blue(aux))
      aux2 <- v[((length(v) / 2) + 1)]
      message("\nWe take the '(n/2)+1' element -> ", blue(aux2))
      res <- ((aux + aux2) / 2)
      message("\nNow we add each other and divided it by two")
      message("\n(",aux," + ", aux2, ") / 2")
      message("\nThe result is : ", bold(res))
    } else {
      message("\nIt has a ODD number of elements (", blue(length(v)), ")\n")
      res <- v[(length(v) / 2) + 0.5]
      message("\nWe take the 'n/2' approaching up element")
      message("\nThe result is : ", bold(res))
    }

    message("\nNow try by your own! :D\n")
    message("\nUse median_(interactive = TRUE) function to practice.\n")
    return(res)
  }

  # Simple calculation mode
  x <- as.vector(x)
  message("\nSorted vector: ")
  x_sorted <- sort(x)
  message(x_sorted, "\n")
  pair <- as.logical()
  pair <- FALSE
  if(length(x_sorted) %% 2 == 0) {
    pair <- TRUE
  }
  if(pair) {
    aux <- x_sorted[(length(x) / 2)]
    aux2 <- x_sorted[((length(x) / 2) + 1)]
    res <- ((aux + aux2) / 2)
  } else {
    res <- x_sorted[(length(x_sorted) / 2) + 0.5]
  }
  message("\n")
  return(res)
}
