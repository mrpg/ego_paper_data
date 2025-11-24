#!/usr/bin/env Rscript
#
# Installation script for R package dependencies
# Required for replication of Engel, Grossmann & Ockenfels (2025)
#
# This script installs all required R packages for the analysis code.
# Run this script once before running plots.R or tables.R
#
# Usage:
#   source("install_dependencies.R")
# or from command line:
#   Rscript install_dependencies.R

cat("Installing R package dependencies...\n\n")

# List of CRAN packages to install
cran_packages <- c(
  "dplyr",      # Data manipulation
  "tidyr",      # Data reshaping (pivot functions)
  "ggplot2",    # Plotting
  "Cairo",      # PDF output for plots
  "patchwork",  # Combining multiple plots
  "lmtest",     # Testing linear regression models
  "sandwich",   # Robust covariance matrix estimation
  "plm"         # Panel data models
)

cat("Step 1/2: Installing CRAN packages...\n")
cat("Packages to install:", paste(cran_packages, collapse=", "), "\n\n")

# Install CRAN packages
install.packages(cran_packages, repos = "https://cloud.r-project.org/")

cat("\nStep 2/2: Installing texreg from custom fork...\n")

# Install remotes package if not already installed
if (!requireNamespace("remotes", quietly = TRUE)) {
  cat("Installing 'remotes' package first...\n")
  install.packages("remotes", repos = "https://cloud.r-project.org/")
}

# Install texreg from custom fork (required for regression table formatting)
cat("Installing texreg from GitHub (mrpg/texreg_fork, ref: 6268c7f)...\n")
remotes::install_github("mrpg/texreg_fork", ref = "6268c7f")

cat("\n")
cat("========================================\n")
cat("Installation complete!\n")
cat("========================================\n")
cat("\nYou can now run the analysis scripts:\n")
cat("  source('plots.R')   # Generate figures\n")
cat("  source('tables.R')  # Generate tables\n")
cat("  source('tables2.R') # Generate Table A1 bounds\n")
