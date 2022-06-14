#' Iterations per Round (data.table)
#' 
#' @param dataset a dataset
#' @param cluster_var a string
#' @param Id_Patient a string
#' @param total_cont_per_case a string
#' @param case_control a string
#' @param mat_per_case a numeric indicating number of matches per case with default of \code{NULL}
#' @keywords internal
iterations_per_round_dt <- function(dataset, cluster_var, 
                                 Id_Patient, total_cont_per_case, 
                                 case_control, mat_per_case = NULL) {
  cluster_var <- retrieve_quosure(cluster_var)
  Id_Patient <- retrieve_quosure(Id_Patient)
  total_cont_per_case <- retrieve_quosure(total_cont_per_case)
  
  # Data table way
  data.table::setDT(dataset)
  
  one_to_one = dat[order(get(cluster_var)),row_id := data,table::rowid(get(cluster_var))][row_id <= 2]
  
  dup_con = unique(one_to_one)
  
  dup_con = dup_con[, if(.N>1) .SD, by=get(Id_Patient)][
    order(get(Id_Patient), get(total_cont_per_case))][
      ,head(.SD, 1), by = list(get(Id_Patient), get(total_cont_per_case))
    ]
  
  if (nrow(dup_con) > 0) {
    one_to_one <- one_to_one[!dup_con, on = get(Id_Patient)]
    
    
    one_to_one <- bind_rows(one_to_one, dup_con) %>% arrange((!!cluster_var), case_control)
    one_to_one <- one_to_one %>% group_by((!!cluster_var)) %>% mutate(mat_per_case = n() - 1)
    case_cntrl_1st_wave <- one_to_one %>% filter(mat_per_case == 1) %>% select(-mat_per_case)
    dataset <- anti_join(dataset, case_cntrl_1st_wave, by = quo_name(cluster_var))
    dataset <- anti_join(dataset, case_cntrl_1st_wave, by = quo_name(Id_Patient))
  } else {
    one_to_one <- one_to_one %>% group_by((!!cluster_var)) %>% mutate(mat_per_case = n() - 1)
    case_cntrl_1st_wave <- one_to_one %>% filter(mat_per_case == 1) %>% select(-mat_per_case)
    dataset <- NULL
  }
  return(list(case_cntrl_1st_wave = case_cntrl_1st_wave, dataset = dataset, dup_con = dup_con))
  
}