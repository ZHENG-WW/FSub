# FSub
FSub is a feature selection method that divides features into <br>
four categories (relevant features, interaction features, redundant features, and irrelevant feature), <br>
in order to accommodate both the requirement for <br>
minimal-optimal features and the requirement for all relevant features simultaneously.

#implementation <br>
source("fsub.R") <br>
mci.res = mci("target", train_set) <br>
#the output of FSub_s <br>
mci.res$summary$mean <br>

#the output of FSub_r <br>
mci.res$summary$rank <br>

#the output of FSub_e <br>
mci.res$summary$summary <br>
