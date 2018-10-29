# File Structure

## Outputs

The outputs directory `outputDir` will have the following file structure:

```
|-- cross_platform
|   |-- analysis
|   `-- predictions
|-- gene_selection
|   |-- boot_freq
|   |-- final_model
|   |-- plots
|   |-- predict
|   |-- retrain
|   |-- sum_freq
|   `-- train
|-- nanostring
|   |-- evals
|   `-- predictions
|-- post_processing
|   |-- evals
|   |-- fits
|   |-- plots
|   `-- predictions
|-- supervised
|   |-- merge_eval
|   |-- summary
|   |-- top_ci
|   `-- train_eval
`-- unsupervised
    |-- clustering
    |-- consensus
    |-- consmat
    |-- data_pr
    |-- final
    |-- map_genes
    |-- merge_cm
    `-- prep_data
```

## Logs

The logs directory `baseLogDir` will have the following file structure:

```
|-- cross_platform
|   |-- analysis
|   `-- predictions
|-- gene_selection
|   |-- boot_freq
|   |-- final_model
|   |-- post_processing
|   |-- predict
|   |-- retrain
|   |-- sum_freq
|   `-- train
|-- post_processing
|-- supervised
|   |-- merge_eval
|   |-- summary
|   |-- top_ci
|   `-- train_eval
`-- unsupervised
    |-- clustering
    |-- consensus
    |-- consmat
    |-- data_pr
    |-- final
    |-- map_genes
    `-- merge_cm
```

## Scripts

The scripts directory `workDir` will have the following file structure:

```
|-- R_file
|   |-- cross_platform
|   |   |-- analysis
|   |   `-- predictions
|   |-- gene_selection
|   |   |-- boot_freq
|   |   |-- final_model
|   |   |-- post_processing
|   |   |-- predict
|   |   |-- retrain
|   |   |-- sum_freq
|   |   `-- train
|   |-- nanostring
|   |-- post_processing
|   |-- supervised
|   |   |-- merge_eval
|   |   |-- summary
|   |   |-- top_ci
|   |   `-- train_eval
|   `-- unsupervised
|       |-- clustering
|       |-- consensus
|       |-- consmat
|       |-- data_pr
|       |-- final
|       |-- map_genes
|       |-- merge_cm
|       `-- prep_data
`-- sh_file
    |-- cross_platform
    |   |-- analysis
    |   `-- predictions
    |-- gene_selection
    |   |-- boot_freq
    |   |-- final_model
    |   |-- post_processing
    |   |-- predict
    |   |-- retrain
    |   |-- sum_freq
    |   `-- train
    |-- post_processing
    |-- supervised
    |   |-- merge_eval
    |   |-- summary
    |   |-- top_ci
    |   `-- train_eval
    `-- unsupervised
        |-- clustering
        |-- consensus
        |-- consmat
        |-- data_pr
        |-- final
        |-- map_genes
        |-- merge_cm
        `-- prep_data
```
