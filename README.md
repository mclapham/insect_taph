Scripts for Insect Taphonomy
===========

Data files and R scripts for data processing and figures for Karr and Clapham (2014) insect taphonomy paper.

IMPORTANT: Run data_acq.R script first to acquire data and filter based on criteria described in paper.

1) act_prd_comparison.R: compares actual proportion articulated against predicted articulation based only on the environment, mean size, and proportion of beetles in each 10 Myr bin. Plots figure 7.

2) environ_count.R: calculates proportion of occurrences per paleoenvironment group for each suprafamilial clade; data for table 3

3) fig1_plot.R: calculates proportion of articulated specimens per time interval; plots figure 1
 
4) glm_all_env.R: performs logistic regression models (articulation as a function of age and age+environment) for all insects; plots figure 3

5) glm_all_param.R: performs logistic regression with all parameters (age, size, environment, is_beetle) for all insects and predicts articulation while holding each one constant (excluding age). Parameters converted to binomial using Cenozoic and Carboniferous averages as end points and simulating smooth variation between. Plots figure 6.

6) glm_all_pubyr.R: performs logistic regression relating articulation as a function of publication year, for all insects and only for Coleoptera; plots figure 9.

7) glm_all_size.R: performs logistic regression model (articulation as a function of log10 wing element length) for all insects; plots figure 4

8) glm_coleo_pubyr.R: performs logistic regression models (articulation as a function of age and age+environment) for all beetles and only for species named after 1950; plots figure 8

9) glm_orders_env.R: performs logistic regression models (articulation as a function of age and age+environment) for six suprafamilial groups; plots figure 2

10) glm_orders_pubyr.R: performs logistic regression models (articulation as a function of age and age+environment) for six suprafamilial groups, both for all taxa and only for species named post-1950; plots supplementary figure.

11) glm_orders_size.R: performs logistic regression model (articulation as a function of log10 wing element length) for six suprafamilial groups; plots figure 5

12) glm_results.R: performs and compares logistic regression models for all insects and for six suprafamilial groups; data for tables 1 and 2
