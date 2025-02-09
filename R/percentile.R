######################
#####-PERCENTILES-####
######################

#' Percentile Function
#'
#' This function calculates the percentiles of a numeric vector.
#'
#' @param x Optional numeric vector (not needed for interactive mode)
#' @param p Numeric value between 0 and 1 for percentile calculation (not needed for interactive mode)
#' @param learn Logical, if TRUE shows step-by-step explanation
#' @param interactive Logical, if TRUE enables interactive practice mode
#' @return The percentile value (for non-interactive mode)
#' @examples
#' data <- c(1,4,3,3,2,5,7,12)
#'
#' # Simple calculation
#' percentile(data, 0.3)
#'
#' percentile(data, 0.3, learn = TRUE)
#'
#' \dontrun{
#' percentile(interactive = TRUE)
#' }
#'
#' @importFrom crayon bold green blue red yellow
#' @export
percentile <- function(x = NULL, p = NULL, learn = FALSE, interactive = FALSE) {
  # Validate parameters
  if (learn && interactive) {
    stop("learn and interactive modes cannot be enabled simultaneously")
  }

  if (!interactive && (is.null(x) || is.null(p))) {
    stop("data vector and percentile value are required when not in interactive mode")
  }

  # Interactive mode
  if (interactive) {
    initImages("percentile.svg")
    cont = 0

    cat("\nInsert your data set:\n")
    buffer = getUserAction()

    #show data sorted
    buffer = sort(buffer)
    cat("\nData sorted : ")

    drawVector(buffer)

    rand_percentile = sample(1:100,1)
    perc = rand_percentile/100

    cat("\nOK! Next Move !!\n")
    flag <- 1

    while(flag == 1) {
      cat("Please, insert the result of the ", rand_percentile ,"% percentile for your data : ")
      cat("\n(remember your data) -> ", buffer , "\n")

      resp_percPos <- as.numeric(readline(prompt = ""))
      if(resp_percPos == percentile(buffer, perc)) {
        cat("\nCorrect!\n")
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
    cat(bold("\n__PERCENTILES CALCULUS__ \n"))
    data <- sort(as.vector(x))
    size <- length(data)
    cat("\nThe percentile divides the dataset in 100 parts.\nThe percentile indicates, once the data is ordered from least to greatest, the value of the variable below which a given percentage is located on the data\n")
    cat(green("\nFormula x -> (k * N ) / 100 where k -> [1-100] and N -> vector size\n"))
    cat(green("\nIf rest of x is diference to 0, the value of its percentile will be the position of the quotient of the previous operation.  \n"))
    cat(green("\nIn the opposite case and being 0 will be the sum of the elements whose value is the quotient and following, less in the case of the 100% percentile that will be the last element.  \n"))

    cat(bold("\n__Use Example__\n"))
    cat("\nStep 1: The vector must be sorted.\n")

    drawVector(data)

    cat("\n")
    cat("\nStep 2: Apply the formula (k * N) / 100 where 'k' is [1-100]\n")
    cat("\nWe will calculate the percentiles 1,25,37,50,92 in this example\n")

    perc_array <- array(data = NA, dim = 100) #the percentil in our data
    perc_pos_array <- array(data = NA, dim = 100) #the real value of the percentil
    perc_posRound_array <- array(data = NA, dim = 100) #the value rounded up for locate it

    #function calculates percentiles [1-100]
    for(i in 1:100) {
      perc_pos_array[i] = ((size * i) / 100)
      perc_posRound_array[i] = ceiling(perc_pos_array[i])
      perc_array[i] = data[perc_posRound_array[i]]
    }

    for(i in c(1,25,37,50,92)) {
      cat("\nPercentile ", i, " -> (", i, " * ", size , ") / 100 = ", perc_pos_array[i] , "\n")
      cat("\t.Round up the value to locate it in the vector -> ", perc_pos_array[i], " ~ ", perc_posRound_array[i],"\n")
      cat("\t..In our data, the value is = ")
      for(j in 1:size) {
        if(j == size) {
          if(data[j]==perc_array[i]) {
            cat(red(data[j]))
          } else {
            cat(data[j])
          }
        } else {
          if(data[j]==perc_array[i]) {
            cat(red(data[j]), ",")
          } else {
            cat(data[j], ",")
          }
        }
      }
      cat("\n")
    }

    cat("\nNow try by your own! :D\n")
    cat("\nUse percentile(interactive = TRUE) function to practice.\n")
    return(perc_array[p*100])
  }

  # Simple calculation mode
  if( p <= 1 ) {
    data <- as.vector(sort(x))
    size <- length(x)

    perc_pos <- (size * p)
    int_div <- (perc_pos %% 1)
    perc_posRound = ceiling(perc_pos)

    if (int_div  != 0 ) {
      perc_sol=data[ceiling(perc_pos)]
    }else {
      if( perc_pos == size ) {
        perc_sol=data[ceiling(perc_pos)]
      }else {
        perc_sol= (data[perc_posRound] + data[perc_posRound + 1]) / 2
      }
    }

    cat("Percentile ",p*100,"% = ",perc_sol, "\n")
    return(perc_sol)
  }else {
    cat("Error, the percentile has to be less o equal than 1")
  }
}
