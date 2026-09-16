# Update the R version and/or packages in the lockfile used by `renv` through this script
# Simply click "Source" to run it

# The `snapshot` function in the `renv` package is what can update the lockfile,
# but this script ensures that all recommended customizations in the function call
# are used


# Use 'renv' 
require(renv)


snapshot(exclude = "SDA", prompt = FALSE)
