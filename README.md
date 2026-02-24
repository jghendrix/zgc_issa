Based on the iSSA guild targets workflow: https://issa-guild.github.io/book/index.html

Code and data for submission to Journal of Animal Ecology. The integrated step selection analyses are all contained within the targets() workflow, based on the _targets.R file in the main directory, and the functions contained within /R.

Code to determine seasonal thresholds based on spring greenup is found in seasonal_determination.R in main directory.

Code to calculate seasonal range size and compare to stoichiometric distributions models is found in kernel_analyses.R file.

Code to validate iSSA model fit, as per Fieberg et al 2024 https://doi.org/10.1111/2041-210X.14336, is available in lineup_protocol.R file.

Note that we are unable to publicly share the raw GPS locations for this caribou population, so we have omitted the code taking the GPS locations to create aKDE, and included only the final derived distributions.

Code used to create the underlying stoichiometric distribution models: https://doi.org/10.5281/zenodo.18760996
