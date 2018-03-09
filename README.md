# PrOType

Molecular Classification of Ovarian Cancer

This is a repository of all the necessary scripts to run the Vancouver analysis of the molecular subtypes of High Grade Serous Ovarian Carcinoma. 

![Caption for the picture.](StudyDesign2.png)

In order to successfully run the scripts on this repository, a directory named data should be created and should contain the needed data.

To reproduce the full analysis the scripts should be run in a sequential order as follows:

1. `array_classifier`: performs the unsupervised analysis on the array data and uses the discovery labels to train several models and perform the array validation above.
2. `nanostring_classifier`: performs the validation of the classifier on the NanoString data in the first validation and compares the labels. Subtype is assigned to the entire NanoString dataset.
