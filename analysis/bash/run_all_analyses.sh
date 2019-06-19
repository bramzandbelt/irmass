# !/bin/bash
#
# run_all_analyses.sh


# 1. Preprocessing & quality check #############################################

# Preprocess the performance log files =========================================
sh analysis/bash/01_preprocess_log_files.sh

# Assess task performance criteria =============================================
sh analysis/bash/02_assess_task_performance_criteria.sh

# 2. Participant-level race model analyses #####################################

# Effect of stop-signal delay on probability of responding bimanually
sh analysis/bash/03_individual_analysis_effect_ssd_on_prob_responding_given_stopsignal.sh

# Difference between stop-respond and no-signal response times
sh analysis/bash/04_individual_analysis_rt_difference_nosignal_stoprespond.sh

# Effect of stop-signal delay on probability on stop-respond response times
sh analysis/bash/05_individual_analysis_effect_ssd_on_stoprespond_rt.sh

# 3. Group-level race model analyses ###########################################

# Effect of stop-signal delay on probability of responding bimanually
#sh analysis/bash/06_group_analysis_effect_ssd_on_prob_responding_given_stopsignal.sh

# Difference between stop-respond and no-signal response times
sh analysis/bash/07_group_analysis_rt_difference_nosignal_stoprespond.sh

# Effect of stop-signal delay on probability on stop-respond response times
sh analysis/bash/08_group_analysis_effect_ssd_on_stoprespond_rt.sh

# 4. Exploratory analyses ######################################################

# Verify support for and against independent race model under different Bayes factor criteria
sh analysis/bash/09_exploration_support_for_hypotheses_under_different_bayes_factor_criteria.sh

# Verify support for and against independent race model under different priors
sh analysis/bash/10_exploration_support_for_hypotheses_under_different_priors.sh

