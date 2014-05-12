Scripts for Insect Taphonomy
===========

Data files and R scripts for data processing and figures for Karr and Clapham (2014) insect taphonomy paper.

Run script data_acq.R first to acquire data and filter based on criteria described in paper.

1) fig1_plot.R: calculates proportion of articulated specimens per time interval; plots figure 1
 
2) glm_all_env.R: performs logistic regression models (articulation as a function of age and age+environment) for all insects; plots figure 3

3) glm_all_size.R: performs logistic regression model (articulation as a function of log10 wing element length) for all insects; plots figure 4

4) glm_orders_env.R: performs logistic regression models (articulation as a function of age and age+environment) for six suprafamilial groups; plots figure 2

5) glm_orders_env.R: performs logistic regression model (articulation as a function of log10 wing element length) for six suprafamilial groups; plots figure 5
