################## ICER calculations #######################
qaly.yes <- mean(sapply(1:sim, function(n) {
  rown.yes <- dim(test.yes.10k[[n]])[1]
  test.yes.10k[[n]][rown.yes, "Cumulative QALM"]
}))

qaly.no <- mean(sapply(1:sim, function(n) {
  rown.no <- dim(test.no.10k[[n]])[1]
  test.no.10k[[n]][rown.no, "Cumulative QALM"]
}))

costs.yes <- mean(sapply(1:sim, function(n) {
  rown.yes <- dim(test.yes.10k[[n]])[1]
  test.yes.10k[[n]][rown.yes, "Cumulative costs"]}))

costs.no <- mean(sapply(1:sim, function(n) {
  rown.no <- dim(test.no.10k[[n]])[1]
  test.no.10k[[n]][rown.no, "Cumulative costs"]}))

inc.qaly <- (qaly.yes - qaly.no)/12
inc.cost <- (costs.yes - costs.no)
icer <- inc.cost / inc.qaly

cat(sprintf("Incremental QALYs per patient (monthly): %.3f\n", inc.qaly))
cat(sprintf("Incremental cost per patient (EUR): %s\n", formatC(inc.cost, format = "f", digits = 2, big.mark = ",")))
cat(sprintf("Incremental cost-effectiveness ratio (ICER) (EUR / QALY): %s\n", formatC(icer, format = "f", digits = 2, big.mark = ",")))



### Analyze differences in health state occurrences between policies
observed.states.names <- c("Death", "Susceptible", "Ischemic Stroke", "Intracerebral bleeding", "Other intracranial bleeding", "Major bleeding other than intracranial bleeding", "Clinical atrial fibrillation")
state_counts_no <- sapply(1:7, function(state) {
  sum(sapply(1:sim, function(n) {
    rown.no <- dim(test.no.10k[[n]])[1]
    any(test.no.10k[[n]][1:rown.no, "Observation"] == state)
  }))
})
names(state_counts_no) <- observed.states.names

state_counts_yes <- sapply(1:7, function(state) {
  sum(sapply(1:sim, function(n) {
    rown.yes <- dim(test.yes.10k[[n]])[1]
    any(test.yes.10k[[n]][1:rown.yes, "Observation"] == state)
  }))
})
names(state_counts_yes) <- observed.states.names

stroke.diff <- state_counts_yes["Ischemic Stroke"] - state_counts_no["Ischemic Stroke"]
stroke.diff.percent <- (state_counts_yes["Ischemic Stroke"] - state_counts_no["Ischemic Stroke"]) / state_counts_no["Ischemic Stroke"] * 100
print(sprintf("Difference in number of ischemic strokes: %d", stroke.diff))
print(sprintf("Percentage difference in number of ischemic strokes: %.2f%%", stroke.diff.percent))

death.diff <- state_counts_yes["Death"] - state_counts_no["Death"]
death.diff.percent <- (state_counts_yes["Death"] - state_counts_no["Death"]) / state_counts_no["Death"] * 100
print(sprintf("Difference in number of deaths: %d", death.diff))
print(sprintf("Percentage difference in number of deaths: %.2f%%", death.diff.percent))

major.bleeding.diff <- state_counts_yes["Major bleeding other than intracranial bleeding"] - state_counts_no["Major bleeding other than intracranial bleeding"]
major.bleeding.diff.percent <- (state_counts_yes["Major bleeding other than intracranial bleeding"] - state_counts_no["Major bleeding other than intracranial bleeding"]) / state_counts_no["Major bleeding other than intracranial bleeding"] * 100
print(sprintf("Difference in number of major bleedings other than intracranial bleeding: %d", major.bleeding.diff))
print(sprintf("Percentage difference in number of major bleedings other than intracranial bleeding: %.2f%%", major.bleeding.diff.percent))
