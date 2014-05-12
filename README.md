Scripts for Insect Taphonomy
===========

Data files and R scripts for data processing and figures for Karr and Clapham (2014) insect taphonomy paper.

Run script data_acq.R first to acquire data and filter based on criteria described in paper.

1) fig1_plot.R: calculates proportion of articulated specimens per time interval; plots figure 1
 
2) glm_all_env.R: performs logistic regression models (articulation as a function of age and age+environment) for all insects; plots figure 3

3) glm_all_param.R: performs logistic regression with all parameters (age, size, environment, is_beetle) for all insects and predicts articulation while holding each one constant (excluding age). Parameters converted to binomial using Cenozoic and Carboniferous averages as end points and simulating smooth variation between. Plots figure 6.

4) glm_all_size.R: performs logistic regression model (articulation as a function of log10 wing element length) for all insects; plots figure 4

5) glm_orders_env.R: performs logistic regression models (articulation as a function of age and age+environment) for six suprafamilil groups; plots figure 2

6) glm_orders_size.R: performs logistic regression model (articulation as a function of log10 wing element length) for six suprafamilial groups; plots figure 5
