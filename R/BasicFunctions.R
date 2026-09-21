#' Basic functions
#'
create_vaja <- function(age, weight, id, alive=TRUE, year=0, wcalf=FALSE, slaughtered=FALSE) {
  vaja = NULL
  vaja$age = age
  vaja$weight = weight
  vaja$id = id
  vaja$alive = alive
  vaja$year = year
  vaja$wcalf = wcalf
  vaja$slaughtered = slaughtered
  return(vaja)
}

#Function used to initiate the herd and to simulate new calves
sim_weight<- function(age, mean_weight, sd_weight = rep(10,11)){
  return(rnorm(1, mean_weight[age+1], sd_weight[age+1]))
}

get_weights <- function(vajor) sapply(vajor, function(x){as.numeric(x[2])})
get_ages <- function(vajor) sapply(vajor, function(x){as.numeric(x[1])})
get_ids <- function(vajor) sapply(vajor, function(x){as.numeric(x[3])})
get_living <- function(vajor) sapply(vajor, function(x) as.logical(x[4]))
get_with_calf <- function(vajor) sapply(vajor, function(x) as.logical(x[6]))
get_calves <- function(vajor) sapply(vajor, function(x) as.logical(x[1]==0))

#Basic functions test example
run.example = FALSE
if (run.example) {
  test=NULL
  test[[1]] = create_vaja(1, 40, 1001)
  test[[2]] = create_vaja(1, 40, 1002, alive=FALSE)
  get_living(test)
}

make_variables_global <- function(list_in) {
  var_names = names(list_in)
  for (name in var_names) {
    assign(name, as.numeric(unlist(list_in[name])), envir = .GlobalEnv)
  }
}