cost_sampler <- function(end.NOAC.after.bleeding = FALSE, discount = TRUE, prog.bar = TRUE, rate = NULL, af_rate = NULL, policy = NULL, severity = NULL, qaly = NULL, months = 120, size = 10000, base_qaly = NULL, initial_state = NULL, seed = NULL, costs.df = NULL, monthly.cost.of.treatment = 70) {
  #policy: the policy to medicate patients
  #severity: proportions of different severities of the outcome
  #qaly: quality adjusted life years for different observed outcomes
  #months: maximum time in months to simulate
  #size: number of simulations
  #base_qaly: quality adjusted life without any outcomes
  #initial_state: the initial health state to start the simulation
  #seed: integer seed for RNG
  #costs.df: data frame for costs of different health events
  #################################################
  # Costs of treatment of different health events #
  #################################################
  costs.first.year <- as.matrix(costs.df[[1]]) #costs[morbidity, observed_state]
  costs.subsequent.years <- as.matrix(costs.df[[2]]) #costs[morbidity, observed_state]

  ## other parameters ##
  if (discount) {
    power <- (0:(months-1))/12 # this is for discount factor
  } else {
    power <- rep(0,months)
  }
  
states <- 1:(dim(rate)[1]) # get health states
  af_states <- 1:(length(af_rate)) #get clinical af health states
  actions <- 1:(dim(policy)[1])# get actions
  observed_state <- initial_state
  Time <- 1:months #timeline to simulate over to
  output <- list() #temporal working variable
  death_rates_coefficients_after_disability <- c(rate[1,1], rate[1,1], -log(1-0.14)/12, -log(1-0.16)/12, -log(1-0.16)/12, rate[1,1], rate[1,1]) / rate[1,1]
  severity <- array(c(
    cbind( matrix(c(1,0,0,0,0,0,0,0,0,1), ncol = 2), severity[,,1]),
    cbind(matrix(c(1,0,0,0,0,0,0,0,0,1), ncol = 2), severity[,,2])
  ),
  dim = c(5,6,2),
  )
  #---------------simulation loop starts--------------------
  #loop for subclinical atrial fibrillation
  #set seed for reproducibility
  cores <- detectCores()
  cl <- makeCluster(cores[1] -1)
  registerDoParallel(cl)
  set.seed(seed)
  output <- foreach (i = 1:size, .errorhandling = "remove") %dorng% {
  #output <- list()
  #for (i in 1:size) {
    observed_state <- initial_state
    observation <- data.frame(state = observed_state, morbidity = 5, qalm = base_qaly[1], cost = monthly.cost.of.treatment) #observed state, disability (5 is no disability), base qaly 0.794
    af_diagnosis <- FALSE
    lifeLine <- rep(1,months)
    probLine <- array(rep(rate,months), dim = c(dim(rate), months)) #time dependent probabilities
    if (policy[2,2] == 0) { #if no intention to medicate
      costLine <- rep(0, months) # base health care costs
    } else if (policy[2,2] > 0) { # if intention to medicate
      costLine <- rep(monthly.cost.of.treatment, months) # monthly medication cost if medicated (the multiplicator 12 is removed later in the code)
    }
    for (n in 1:months){

      #if (prog.bar) {
      # create progress bar on first iteration, update and close on last
      #  if (n == 1) pb <- txtProgressBar(min = 1, max = months, style = 3)
      #    setTxtProgressBar(pb, n)
      #  if (n == months) {
      #    close(pb)
      #    rm(pb)
      #  }
      #}

      action <- sample(actions, 1, prob = policy[,observed_state])
      observed_state <- sample(states, 1, prob = probLine[, action, n]) # the next observed state
      #Clinical atrial fibrillation
      if (observed_state == 7) {
        af_diagnosis <- TRUE
        lifeLine[n] <- 1*lifeLine[n]
        morbidity <- 5
      }
      else {
        morbidity <- sample(1:5, 1, prob = severity[,observed_state,action]) # random morbidity from observed health state
        costLine[n:min(n+11, months)] <- costLine[n:min(n+11, months)] + costs.first.year[morbidity, observed_state]/12
        costLine[min(n+12, months):months] <- costLine[min(n+12, months):months] + costs.subsequent.years[morbidity, observed_state]/12
        lifeLine[n:min(n+5, months)] <- qaly[morbidity, 1,n]*lifeLine[n:min(n+5, months)] #qaly until 6 months after event
        lifeLine[min(n+6, months):months] <- qaly[morbidity,2,n]*lifeLine[min(n+6, months):months] #qaly from 6 months after event
        #geometrically increase death rate for a year after the disability
        temprate <- 1-colSums(rbind( probLine[1,1,n]*death_rates_coefficients_after_disability[observed_state], rate[3:7,] ))
        probLine[c(1,2),,(n):min((n+12), months)] <- rbind(Death = probLine[1,1,n]*death_rates_coefficients_after_disability[observed_state], Susceptible = temprate) #increased base rate to die for a year after event
        if (end.NOAC.after.bleeding && observed_state %in% c(4,5)) {
          #end medication if hemorrhagic stroke or other intracranial bleeding
          policy <- array(c(1, 0,
                            1, 0,
                            1, 0,
                            1, 0,
                            1, 0,
                            1, 0,
                            0, 1),
                          dim = c(2,7),
                          dimnames = list(c("No NOAC", "NOAC"),
                                          c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding", "Clinical atrial fibrillation")
                          ))
        }
        if (end.NOAC.after.bleeding && observed_state == 3) {
          #start medication if stroke
          policy <- array(c(0, 1,
                            0, 1,
                            0, 1,
                            1, 0,
                            1, 0,
                            1, 0,
                            0, 1),
                          dim = c(2,7),
                          dimnames = list(c("No NOAC", "NOAC"),
                                          c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding", "Clinical atrial fibrillation")
                          ))
        }
      }
      #build the sample path
      observation <- rbind(observation, c(observed_state, morbidity, lifeLine[n]*base_qaly[n] * 0.97^power[n], costLine[n] * 0.97^power[n]))
      #death by stroke or bleed or accident breaks the loop
      if (lifeLine[n] == 0 || af_diagnosis) {break}
    }
    #loop for clinical atrial fibrillation if diagnosed
    if (af_diagnosis & n!=months){
      #af_states <- 1:length(af_rate)
      action <- 2 #choose to medicate always
      af_probLine <- array(rep(af_rate, months - n), dim = c(length(af_rate), months - n))
      for (t in (n+1):months){
        observed_state <- sample(1:6, 1, prob = af_probLine[, t-n]) # the next observed state
        morbidity <- sample(1:5, 1, prob = severity[,observed_state,action]) # random morbidity from observed health state
        costLine[t:min(t+11, months)] <- costLine[t:min(t+11, months)] + costs.first.year[morbidity, observed_state]/12
        costLine[min(t+12, months):months] <- costLine[min(t+12, months):months] + costs.subsequent.years[morbidity, observed_state]/12
        lifeLine[t:min(t+5, months)] <- qaly[morbidity, 1,t]*lifeLine[t:min(t+5, months)] #qaly until 6 months after event
        lifeLine[min(t+6, months):months] <- qaly[morbidity,2,t]*lifeLine[min(t+6, months):months] #qaly from 6 months after event
        #geometrically increase death rate for a year after the disability
        temprate <- 1-sum(c( af_probLine[1,t-n]*death_rates_coefficients_after_disability[observed_state], rate[3:7,] ))
        af_probLine[c(1,2),(t-n):min((t+12-n), months-n)] <- c(Death = af_probLine[1,t-n]*death_rates_coefficients_after_disability[observed_state], Susceptible = temprate) #increased base rate to die for a year after event
        #build the sample path
        observation <- rbind(observation, c(observed_state, morbidity, lifeLine[t]*base_qaly[t] * 0.97^power[t], costLine[t] * 0.97^power[t]))
        #death by stroke or bleed or accident breaks the loop
        if (lifeLine[t] == 0) {break}
      }
    }
    observation <- cbind(observation, cumsum(observation[,3]), cumsum(observation[,4]))
    try(colnames(observation) <- c("Observation", "Morbidity", "QALM", "Monthly costs", "Cumulative QALM", "Cumulative costs"))
    observation
  }
  #------------------------simulation loop ends---------------------------
  stopCluster(cl)
  return(output)
}
