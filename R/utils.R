
retrieve_quosure <- function(x){
  rlang::quo_text(rlang::enquo(x))
}
