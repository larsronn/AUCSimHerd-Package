#' Function to simulate a reindeer herd
#' 
#' This function simulates the female part of a herd for 50 years
#' @param calving.rate The proportion of females calving per age class
#' @param winter.survival The probability of winter survival for non-calf females per age class 
#' @param winter.survival_calves The probability of winter survival for calves
#' @param summer.survival The probability of summer survival for non-calf females per age class 
#' @param summer.survival_calves The probability of summer survival for calves
#' @param autumn.survival The probability of autumn survival for non-calf females per age class 
#' @param autumn.survival_calves The probability of autumn survival for calves 
#' @param slaughter.adults Proportion slaughtered females per age class
#' @param mean.weight Expected mean weight (kg) per age class
#' @param slaughter.calves Proportion slaughtered females calves. 
#' @param standard_vaj_weight Standard weight for an adult female that the weight dependence has as baseline.
#' @param b_weight Regression parameter relating a calf's body weight to its mother's body weight
#' @param b_calving Regression parameter relating a female's probability of calving to her body weight
#' @param sd.repeatability The amount of noise added to the body weight each year
#' @param weight.dependent_calving_probability Specifies if the reproduction and calf weight should depend on the mother's weight
#' @param weight.dependent_calf_sluaghter Specifies if selection on calf weights is applied or not
#' @param n_years The number of years to simulate. Default is 50.
#' @param plot_output Specifies whether results should be plotted every year
#' @returns A list consisting of 7 values for each individual: age, weight, id, alive, year, wcalf, slaughtered.
#' @export
#' @details
#' The default input values correspond to a herd in very good condition and hardly any predation.
#' The parameter slaughter.calves is used to tune the growth of the herd. 
#' The following parameters are used to model weight dependence: standard_vaj_weight, b_weight, and b_calving.
#' The parameter standard_vaj_weight is used in the calving() and create_calves() functions
#' The parameter b_weight is used in the create_calves() function
#' The parameter b_calving is used in the calving() function. The relationship is given by a logit function.
#' The parameter sd.repeatability is used in the aging() function
#' @examples
#' Herd1 <- SimulateHerd() 
#' 


SimulateHerd <- function(
  calving.rate = c(0.05, 0.6, 0.75, 0.85, 0.95, 0.97, 0.97, 0.95, 0.9, 0.9, 0.9),
  winter.survival = rep(0.96, 10),
  winter.survival_calves = 0.85,
  summer.survival = rep(0.99, 11),
  summer.survival_calves=0.96,
  autumn.survival = rep(0.99, 11),
  autumn.survival_calves=0.96,
  slaughter.adults =c(0.01, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 1),
  mean.weight = c(40, 60 , 70, 75, rep(80, 5), 77, 75, 75 ),
  slaughter.calves = 0.56,
  standard_vaj_weight = 75, 
  b_weight=0.25, 
  b_calving = 0.01, 
  sd.repeatability = 2,
  weight.dependent_calving_probability = TRUE,
  weight.dependent_calf_sluaghter = TRUE,
  n_years = 50,
  plot_output = FALSE
) {
  make_variables_global(as.list(environment())[1:14])
  vajor = initiate_herd()
  for (cohort in 1:n_years) {
    #Matrices as defined in Petersson & Danell (1992)
    #P1 Winter survival
    #Aging
    #P2 calving
    #P3 summer survival
    #P5 autumn survival
    #P6 slaughter
    
    ############################
    #P1 Winter survival
    vajor=survival_adults(vajor, winter.survival, weight.dependent = FALSE)
    vajor=survival_calves(vajor, winter.survival_calves, weight.dependent = FALSE)
    
    #Aging
    vajor = aging(vajor, mean.weight)
    #P2 calving
    vajor = calving(vajor, calving.rate, 
                    weight.dependent = weight.dependent_calving_probability)
    
    #P3 summer survival
    vajor=survival_adults(vajor, summer.survival)
    vajor=survival_calves(vajor, summer.survival_calves)
    
    #P5 autumn survival
    vajor=survival_adults(vajor, autumn.survival)
    vajor=survival_calves(vajor, autumn.survival_calves)
    
    #P6 slaughter
    vajor=slaughter_adult(vajor, slaughter.adults, weight.dependent = FALSE)
    vajor=slaughter_calves(vajor, slaughter.calves, 
                           weight.dependent = weight.dependent_calf_sluaghter)
    
    
    #plot(get_ages(vajor), get_weights(vajor))
    print(sum(get_living(vajor)))
    #hist(get_ages(vajor[get_living(vajor)]), breaks=20)
  }
  if (plot_output) {
    plot(get_ages(vajor[get_living(vajor)]), get_weights(vajor[get_living(vajor)]), xlab="Age", ylab="Body weight (kg)")
    hist(get_ages(vajor[get_living(vajor)]), breaks=-1:11, xlab="Age", labels=0:10, main="")
  }
  return(vajor)
}






