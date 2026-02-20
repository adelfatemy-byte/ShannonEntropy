#' Calculate frequency of each unique value
#'
#' This function takes a vector and returns the frequency count of each unique value.
#'
#' @param data A vector (numeric, character, or factor)
#' @return A table of frequencies for each unique value
#' @export
#'
#' @examples
#' freq_table(c(1, 1, 2, 3, 3, 3, 4))
freq_table <- function(data) {
  freq <- table(data)
  return(freq)
}
#' Calculate probability of each unique value
#'
#' This function takes a vector and returns the probability (relative frequency)
#' of each unique value. Probability = frequency / total count
#'
#' @param data A vector (numeric, character, or factor)
#' @return A table of probabilities for each unique value
#' @export
#'
#' @examples
#' prob_table(c(1, 1, 2, 3, 3, 3, 4))
prob_table <- function(data) {
  freq <- table(data)
  prob <- freq / sum(freq)
  return(prob)
}
#' Calculate Shannon Entropy
#'
#' This function calculates the Shannon entropy of a vector.
#' Shannon entropy = -sum(p * log(p)) with convention 0*log(0)=0
#'
#' @param data A vector (numeric, character, or factor)
#' @param base Logarithm base (default = exp(1) for natural log)
#' @return Shannon entropy value
#' @export
#'
#' @examples
#' shannon_entropy(c(1, 1, 2, 3, 3, 3, 4))
#' shannon_entropy(c(1, 1, 2, 3, 3, 3, 4), base = 2)
shannon_entropy <- function(data, base = exp(1)) {
  # Step 1: Calculate probabilities
  freq <- table(data)
  prob <- freq / sum(freq)

  # Step 2: Calculate p * log(p) with special handling for zero
  # For zero probabilities, p*log(p) is defined as 0
  p_log_p <- ifelse(prob == 0, 0, prob * log(prob, base = base))

  # Step 3: Calculate Shannon entropy
  entropy <- -sum(p_log_p)

  return(entropy)
}
