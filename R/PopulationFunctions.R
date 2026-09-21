#' Population functions
#' 
initiate_herd <- function(age_structure = rep(0:10, each=50)) {
  vajor = NULL
  for (i in 1:length(age_structure)) {
    vajor[[i]] = create_vaja(age=age_structure[i], weight=sim_weight(age_structure[i], mean.weight), id=i)
  }
  return(vajor)
}
create_calves <- function(n.calves.born, n, mother_weights=NULL ) {
  calves = NULL
  if (is.null(mother_weights)) {
    for (i in 1:n.calves.born) {
      calves[[i]] = create_vaja(age=0, weight=sim_weight(0, mean.weight), id=n+i)
    }
  } else {
    for (i in 1:n.calves.born) {
      calf.weight = mean.weight
      calf.weight[1] = mean.weight[1]+ b_weight*(mother_weights[i]-standard_vaj_weight)
      calves[[i]] = create_vaja(age=0, weight=sim_weight(0, calf.weight), id=n+i)
    }
  }
  
  return(calves)
}
calving <- function(vajor, calving.rate, weight.dependent = FALSE) {
  n = length(vajor)
  n.calves.born = 0
  for (i in 1:n) {
    if (vajor[[i]]$alive) {
      if (!weight.dependent) {
        calved = rbinom(1, 1, calving.rate[ vajor[[i]]$age ]/2)
      } else {
        cr = calving.rate[ vajor[[i]]$age ]/2
        a = log(cr/(1-cr))
        b = b_calving
        c.rate = exp(a+b*(vajor[[i]]$weight-standard_vaj_weight))/(1+exp(a+b*(vajor[[i]]$weight-standard_vaj_weight)))
        calved = rbinom(1, 1, c.rate)
      }
      
      if (calved == 1) {
        vajor[[i]]$wcalf = TRUE
        n.calves.born = n.calves.born + 1
      } else {
        vajor[[i]]$wcalf = FALSE
      }
    }
    
  }
  mothers=vajor[get_with_calf(vajor) & get_living(vajor)]
  mother_weights = get_weights(mothers)
  cat("The average weight of mothers is:",round(mean(mother_weights),1), "kg \n")
  new.calves  = create_calves(n.calves.born, n, mother_weights)
  return( c(vajor, new.calves) )
}

slaughter_adult <- function(vajor, slaughter_ages, weight.dependent = FALSE) {
  n = length(vajor)
  for (i in 1:n) {
    if (vajor[[i]]$alive & vajor[[i]]$age>0) {
      vajor[[i]]$alive <- rbinom(1, 1, 1-slaughter_ages[vajor[[i]]$age])==1
      vajor[[i]]$slaughtered <- !vajor[[i]]$alive
    }
  }
  return(vajor)
}
slaughter_calves <- function(vajor, slaughter, weight.dependent = FALSE) {
  n = length(vajor)
  calves = vajor[get_calves(vajor) & get_living(vajor)]
  threshold = sort(get_weights(calves))[round(length(get_weights(calves))*slaughter)]
  cat("The threshold for calf slaughter is:", round(threshold,1), "kg \n")
  for (i in 1:n) {
    if (vajor[[i]]$alive & vajor[[i]]$age==0 ) {
      if (!weight.dependent) {
        vajor[[i]]$alive <- rbinom(1, 1, 1-slaughter)==1
      } else {
        vajor[[i]]$alive <- vajor[[i]]$weight >=threshold
      }
      vajor[[i]]$slaughtered <- !vajor[[i]]$alive
      
    }
  }
  return(vajor)
}
survival_adults <- function(vajor, survival_ages, weight.dependent = FALSE) {
  n = length(vajor)
  for (i in 1:n) {
    if (vajor[[i]]$alive & vajor[[i]]$age>0) {
      vajor[[i]]$alive <- rbinom(1, 1, survival_ages[vajor[[i]]$age])==1
    }
  }
  return(vajor)
}
survival_calves <- function(vajor, survival, weight.dependent = FALSE) {
  n = length(vajor)
  for (i in 1:n) {
    if (vajor[[i]]$alive & vajor[[i]]$age==0) {
      vajor[[i]]$alive <- rbinom(1, 1, survival)==1
    }
  }
  return(vajor)
}
aging <- function(vajor, mean_weight) {
  n = length(vajor)
  for (i in 1:n) {
    if (vajor[[i]]$alive) {
      vajor[[i]]$age = vajor[[i]]$age + 1
      vajor[[i]]$year = vajor[[i]]$year + 1
      vajor[[i]]$weight = vajor[[i]]$weight + rnorm(1, mean_weight[vajor[[i]]$age+1] - mean_weight[vajor[[i]]$age], sd.repeatability)
    }
  }
  return(vajor)
}