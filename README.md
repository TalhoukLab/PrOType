# PrOType

Molecular Classification of Ovarian Cancer


<<<<<<< HEAD
Please find the necessary scripts to run the classification portion of the nanostring pipeline. A directory named data should be created and should contain the needed data to run the scripts.

The scripts should be run in a sequential order as follows:

1. `step0_internalValidation`: returns a series of plots pertaining to the internal validation of the classifier over bootstrap samples of the training set. The internal validation indices are pre-computed.
2. `step1_validation.R`: performs the validation stage of the classifier by fitting the top performing algorithms (from the previous step) on the training set, cut 1, and predicting the results of the leave-out set, cut 2.
3. `step2_crossPlatform`: predicts all four nanostring batches from the fits of the previous step and returns the predictions.
4. `step3_overlap`: predicts the results for the overlapping samples of array and nanostring, then returns plots comparing the results with those published.
=======
Please find the necessary scripts to execute the full HGSC Classifier pipeline. Please follow the following two steps in sequential order:

1. **HGSC_Classifier**: Contains the unsupervised and supervised training of the HGSC molecular subtypes. Please refer the the [README](https://github.com/AlineTalhouk/PrOType/blob/master/HGSC_Classifier/final_pipeline/README.md) for detailed instructions on how to run.
2. **Nanostring_Classifier**: Contains the scripts required for post-cluster selection of the top performing algorithms and the final nanostring classifier. Please refer to the [README](https://github.com/AlineTalhouk/PrOType/tree/master/Nanostring_Classifier) for detailed instructions on how to run.
>>>>>>> origin/master
