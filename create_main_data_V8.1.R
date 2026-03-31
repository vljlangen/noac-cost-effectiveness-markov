# Simulation size
#sim <- 2000

effectCoefficients <- c(Stroke = 0.68, Bleed = 1.62) # Change stroke and bleed values to modify medication effects
if (noac.effect) {
  effectCoefficients <- oneway.effectCoefficients
}
total_bleed_rate <- bleed.rate
bleed_proportions <- c(0.0018, 0.0015, 0.0073)/total_bleed_rate
bleed_distribution <- c(0.1, 0.1, 0.8)
if (change.bleed.distribution) {
  bleed_distribution <- bleed.distribution
}
total_bleed_rate_difference <- effectCoefficients[2] * total_bleed_rate - total_bleed_rate
stroke_rate <- c(NoNOAC = 1, NOAC = effectCoefficients[1])*stroke.rate
bleed_rate <- matrix(c(bleed_proportions * total_bleed_rate,
                      bleed_distribution*total_bleed_rate_difference + bleed_proportions * total_bleed_rate),
                    ncol = 3,
                    byrow = TRUE,
                    dimnames = list(c("No NOAC", "NOAC"),
                                    c("ICH","Subdural","Other major bleed")))
death_rate <- death.rate
afRate <- 0.075

rates <- c(death_rate/12, stroke_rate[1]/12, bleed_rate[1,]/12, afRate/12)
rates_noac <- c(death_rate/12, stroke_rate[2]/12, bleed_rate[2,]/12, afRate/12)
healthStates_rates <- array(c(rates[1], 1- sum(rates), rates[2:6], # monthly rates without OAC
                                        rates_noac[1], 1- sum(rates_noac), rates_noac[2:6]), # monthly rates with OAC
                                      dim = c(7,2),
                                      dimnames = list(c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding", "Clinical atrial fibrillation"),
                                                      c("No NOAC", "NOAC")))

healthStates_rates_AF <- c(0.116533816/12, 0.985536, 0.025317808/12, 0.005012542/12, 0.005012542/12, 0.028399475/12)

source(severity_matrix_path)

# Probabilities for severity given the observed health state
event_severity <- array(c(data[1,1], data[2,1], data[3,1], data[4,1], 0, 
                          data[5,1], data[6,1], data[7,1], data[8,1], 0, 
                          data[9,1] , data[10,1], 0, data[11,1], data[12,1], 
                          data[13,1], data[14,1], 0, data[15,1], data[16,1],
                          
                          data[1,2], data[2,2], data[3,2], data[4,2], 0, 
                          data[5,2], data[6,2], data[7,2], data[8,2], 0, 
                          data[9,2] , data[10,2], 0, data[11,2], data[12,2], 
                          data[13,2], data[14,2], 0, data[15,2], data[16,2]),
                        dim = c(5,4,2),
                        dimnames = list(c("Death", "Severe disability", "Moderate disability", "Mild disability", "No disability"),
                                        c("Ischemic stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding"),
                                        c("No NOAC", "NOAC")))


severity_qaly <- array(c(0, 0.16, 0.60, 0.88, 1,
                         0, 0.45, 0.73, 0.89, 1),
                       dim = c(5,2),
                       dimnames = list(c("Death", "Severe disability", "Moderate disability", "Mild disability", "No disability"),
                                       c("until 6 months after event", "from 6 months after event")))

base_qaly <- c(rep(0.794,36), rep(0.733, 120-36))
qaly <- severity_qaly%o%rep(1,121) # Outer product to define lifelong QALYs

policy_No_NOAC <- array(c(1, 0,
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

policy_Yes_NOAC <- array(c(0, 1,
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

####################################
# First-year cost random variable #
####################################
# This creates the cost tables for each observed health event and disability level
costs <- list(data.frame(death = c(0, 0, 0, 0, 0), 
                    susceptible = c(0, 0, 0, 0, 0), 
                    Ischemic_Stroke = c(14192, 100516, 52721, 6259, 0), 
                    Hemorrhagic_Stroke = c(16508, 65906, 52721, 16880, 0), 
                    Other_intracranial_bleeding = c(22020, 22020, 0, 22020, 22020), 
                    extracranial_bleeding = c(17318, 17318, 0, 17318, 17318),
                    row.names = c("death",
                                  "severe",
                                  "moderate",
                                  "mild",
                                  "No_disability")),
              data.frame(death = c(0, 0, 0, 0, 0),
                         susceptible = c(0, 0, 0, 0, 0),
                         Ischemic_Stroke = c(0, 102506, 55425, 0, 0),
                         Hemorrhagic_Stroke = c(0, 75798, 79006, 0, 0),
                         Other_intracranial_bleeding = c(0, 75798, 0, 0, 0),
                         extracranial_bleeding = c(0, 75798, 0, 0, 0),
                         row.names = c("death",
                                       "severe",
                                       "moderate",
                                       "mild",
                                       "No_disability"))
)

#############################################################
# Simulate data for plotting (QALYs over time for both policies)
#############################################################
source(cost_sampler_path)

print("Creating main data for no NOAC medication policy...")
test.no.10k <- cost_sampler(
  rate = healthStates_rates, 
  af_rate = healthStates_rates_AF, 
  policy = policy_No_NOAC, 
  severity = event_severity, 
  qaly = qaly, 
  months = 120, 
  size = sim, 
  base_qaly = base_qaly, 
  initial_state = 2, # initial state is susceptible
  seed = seed,
  costs.df = costs,
  monthly.cost.of.treatment = monthly.noac.cost,
  end.NOAC.after.bleeding = FALSE,
  discount = discount.factor,
  d.value = d.value # discount value
  )

print("Creating main data for yes NOAC medication policy...")
test.yes.10k <- cost_sampler(
  rate = healthStates_rates, 
  af_rate = healthStates_rates_AF, 
  policy = policy_Yes_NOAC, 
  severity = event_severity, 
  qaly = qaly, 
  months = 120, 
  size = sim, 
  base_qaly = base_qaly, 
  initial_state = 2, # initial state is susceptible without any morbidities
  seed = seed,
  costs.df = costs,
  monthly.cost.of.treatment = monthly.noac.cost,
  discount = discount.factor,
  d.value = d.value #discount value
  )
