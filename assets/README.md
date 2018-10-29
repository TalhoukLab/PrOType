# Assets

This directory contains assets/utilities used by the PrOType pipeline.

- [`check.sh`](https://github.com/AlineTalhouk/PrOType/blob/master/assets/check.sh): a number of simple checks to verify that parameters do not have invalid values.
- [`clean.sh`](https://github.com/AlineTalhouk/PrOType/blob/master/assets/clean.sh): the directories removed when `make clean` is called.
- [`dev_params.sh`](https://github.com/AlineTalhouk/PrOType/blob/master/assets/dev_params.sh): developer parameters that are not normally modified unless you know are very familiar with pipeline.
- [`study_design.png`](https://github.com/AlineTalhouk/PrOType/blob/master/assets/study_design.png): flow chart of the study design
- [`submit_local.py`](https://github.com/AlineTalhouk/PrOType/blob/master/assets/submit_local.py): a Python implementation of job submission to use when `qsub` doesn't exist on the server.
- [`submit_queue.sh`](https://github.com/AlineTalhouk/PrOType/blob/master/assets/submit_queue.sh): submit a job using `qsub`, with useful console printouts.
- [`utils.R`](https://github.com/AlineTalhouk/PrOType/blob/master/assets/utils.R): project-wide R utility functions.
