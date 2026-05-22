## if nesscessary, install libglpk-dev, libgsl-dev, libfl-dev, libuv1-dev
## sudo apt install libcairo2-dev libfontconfig1-dev pandoc
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("tidyverse", "Biostrings", "dplyr"), update = FALSE)



# requirements.R
required_pkgs <- c(
  "fs",
  "googledrive",
  "googlesheets4",
  "reprex"
  # "Wrench",
  # "ggtext",
  # "ggraph",
  # "DT",
  # "readxl",
  # "corncob",
  # "VennDiagram",
  # "DirichletMultinomial",
  # "mia",
  # "ANCOMBC",
  # "emmeans",
  # "pbkrtest",
  # "glmnet",
  # "torch",
  # "devtools",
  # "skimr",
  # "rstatix",
  # "ggpubr",
  # "reshape2",
  # "ALDEx2",
  # "Maaslin2"
)

to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install)) BiocManager::install(to_install)

# if(!"microViz" %in% rownames(installed.packages())){
#   install.packages(
#     "microViz",
#     repos = c(davidbarnett = "https://david-barnett.r-universe.dev", getOption("repos"))
#   )
# }

if(!"testthat" %in% rownames(installed.packages())){
  install.packages("testthat")
}
# 
# if(!"patchwork" %in% rownames(installed.packages())){
#   # install.packages("devtools")
#   devtools::install_github("thomasp85/patchwork")
# }
# 
# if(!"pairwiseAdonis" %in% rownames(installed.packages())){
#   install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
# }
