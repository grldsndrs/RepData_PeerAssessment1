#' Analysis Script
#' Use the project template library
library('ProjectTemplate')
load.project()

# for (dataset in project.info$data)
# {
#   message(paste('Showing top 5 rows of', dataset))
#   print(head(get(dataset)))
# }

#' ## Create Intro report

# List scripts to include in reports
wd <- getwd();
mdList <- paste0(wd,c("/munge/01-A.R" ,"/src/generate_plots.R"))

# Create new analysis script
file.remove("README.md")
file.create("README.md")
sapply(mdList, function(x) file.append("README.md",x),simplify = FALSE)

# Use knitr to generate report
rmarkdown::knitr_options_html(fig_width=3, fig_height=2,keep_md=FALSE,fig_retina	=NULL)
rmarkdown::render(input="README.md",output_format="md_document")

rmarkdown::includes( after_body=TRUE)
rmarkdown::tufte_handout()


