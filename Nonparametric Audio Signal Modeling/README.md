# Modeling the Periodic Variations in Music Signals
---

## Nonparametric Statistics

This project took a subsample of size 2205 from an audio signal that was sampled at 44.1 kHz and attempted modeling it with nonparametric regression. The subsample was taken at every fifth point, so the models were fit on 441 sample points and made predictions on the full 2205 subsample of the signal to see how well the periodic variations in the audio were captured. 

# Methodology

* Loess regression
* Thin-plate smoothing splines

# Software

## SAS

* proc import 
* proc contents 
* proc print 
* proc gplot 
* proc loess
* proc tpspline 

## R Packages

* tuneR
* tidyverse 
* broom 
* ggpubr 
* fANCOVA 
* fields  

# Results 

Loess regression tended to perform better in producing a curve that had the closest representation to the actual audio signal. This resulted from a quadratic loess fit using smoothing parameter 0.01. 
