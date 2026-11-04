# Code to accompany "How to attribute the burden of respiratory disease to respiratory viruses using time series data"

By Daniel Weinberger and Joshua Warren


This repository contains data and R code to replicate figures and simulations presented in the article. 

## Getting started
1. Clone the reposiotry to your computer from github
2. The project uses renv for package management. Run renv::restore()  to install relevant packages

## Organization
[0] Cleaning and organization of the data

[1] Model_fitting_and_data_simulation.R Code to fit a model to observed data and simulate from that model

[2A] Fit the corrct model to the data and eestimates attributable percent

[2B] Fits mis-specified models to the data and estimates attributable percent

[3] and [4] Scripts for generating figures from the paper