# Survey Weighting Suite - Usage Guide

## Table of Contents
1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [Data Import](#data-import)
4. [Weighting Methods](#weighting-methods)
5. [Diagnostics](#diagnostics)
6. [Export](#export)
7. [Troubleshooting](#troubleshooting)

## Introduction

The Survey Weighting Suite is a comprehensive tool for applying various weighting methods to survey data. This guide will walk you through the main features and functionality of the application.

## Getting Started

1. Launch the application by running the following in R:
   ```r
   shiny::runApp()
2. Install required dependencies first if needed:
   ```r
   source("install.R")
   install_dependencies()

## Data Import

The application supports multiple file formats:

* CSV files
* Excel files (.xlsx)
* SPSS files (.sav)
* R data files (.rds)

To import data:

1. Go to the "Data Management" tab
2. Select your file format
3. Choose the encoding (UTF-8, ISO-8859-1, or UTF-16)
4. Upload your data file
5. Preview the imported data in the "Data Preview" section

## Weighting Methods

The following weighting methods are available:

### Rake Weighting (Iterative Proportional Fitting)
* Best for matching marginal distributions
* Iteratively adjusts weights to match population totals

### Post-stratification
* Used when complete population cross-tabulations are available
* Creates weights based on full cross-classification

### Calibration
* Uses auxiliary variables linearly
* Adjusts weights to match known population totals

### Inverse Probability Weighting (IPW)
* Models selection/participation probability
* Useful when response mechanisms need to be accounted for

## Diagnostics

After computing weights, the following diagnostics are available:

* Weight distribution visualization
* Diagnostic plots
* Summary statistics including:
  * Convergence error
  * Influence scores
  * Leverage scores

## Export

Results can be exported in multiple formats:
* CSV
* Excel (.xlsx)
* SPSS (.sav)
* R data file (.rds)

To export:
1. Click the "Download Weights" button
2. Choose your desired export format
3. Select whether to include diagnostic information
4. Save the file to your computer

## Troubleshooting

Common issues and solutions:

### Data Import Issues
* Verify file format is supported
* Check file encoding
* Ensure file size is under maximum limit (30MB by default)

### Computation Errors
* Check input data for missing values
* Verify target proportions sum to 1
* Review convergence parameters if applicable

For additional help, click the "Help" button in the application header.

