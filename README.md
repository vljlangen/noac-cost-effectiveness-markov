# NOAC Cost-Effectiveness Markov Model

Markov model for cost-effectiveness analysis of NOAC treatment.

## Files

- `main_V7.1.R` - Main entry point
- `create_main_data_V7.1.R` - Main simulation
- `main_results_V7.1.R` - ICER calculations
- `probabilistic_sensitivity_analysis_create_data_V7.1.R` - PSA analysis
- `cost_sampler_V7.1.R` - Simulation function
- `severity_matrix.R` - Severity probabilities

## Usage

1. **Install dependencies**: The script uses `pacman` to load required packages. Install `pacman` if needed:
   ```r
   install.packages("pacman")
   ```

2. **Set parameters** in `main_V7.1.R` (lines ~101-114):
   - `sim`: Number of simulations (default: 10000)
   - `seed`: Random seed for reproducibility (default: 46692)
   - `monthly.noac.cost`: Monthly cost of NOAC treatment in € (default: 50)
   - `discount.factor`: Apply discounting (default: TRUE)
   - `bleed.rate`, `stroke.rate`, `death.rate`: Annual event rates

3. **Run the analysis**:
   ```r
   source("main_V7.1.R")
   ```

4. **Output**: 
   - Main simulation results stored in `test.no.10k` and `test.yes.10k`
   - Probabilistic sensitivity analysis results in `test` data frame
   - Cost-effectiveness scatter plot saved as PDF/PNG/TIFF/JPEG

