# PrOType

Molecular Classification of Ovarian Cancer


Please find the necessary scripts to run the classification portion of the nanostring pipeline. The scripts should be run in a sequential order as follows:

1. `step0_internalValidation`: returns a series of plots pertaining to the internal validation of the classifier over bootstrap samples of the training set.
2. `step1_validation.R`: performs the validation stage of the classifier by fitting the top performing algorithms (from the previous step) on the training set, cut 1, and predicting the results of the leave-out set, cut 2.
3. `step2_crossPlatform`: predicts all four nanostring batches from the fits of the previous step and returns the predictions.
4. `step3_overlap`: predicts the results for the overlapping samples of array and nanostring, then returns plots comparing the results with those published.
