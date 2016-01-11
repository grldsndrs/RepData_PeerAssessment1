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
mdList <- paste0(wd,c("/munge/01-A.R" ))

# Create new analysis script
file.remove("README.R")
file.create("README.R")
sapply(mdList, function(x) file.append("README.R",x),simplify = FALSE)

# Use knitr to generate report
rmarkdown::render(input="README.R",output_format=NULL)
rmarkdown::render(input="PA1_template.Rmd",output_format="md_document",output_file = "README.md")
rmarkdown::render(input="README.R",output_format=NULL)

rmarkdown::includes( after_body=TRUE)
rmarkdown::knitr_options_html()
rmarkdown::tufte_handout()


