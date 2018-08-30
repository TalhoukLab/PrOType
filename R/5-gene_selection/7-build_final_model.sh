#!/bin/bash

. ./Parameters.sh

# Make directories for R script, shell script
R_dir=$workDir/R_file/gene_selection/build_final_model
sh_dir=$workDir/sh_file/gene_selection/build_final_model
mkdir -p $R_dir
mkdir -p $sh_dir

# Content of R file
R_file=$R_dir/build_final_model.R
echo 'outputDir <- "'$outputDir'"' > $R_file
echo 'source("R/5-gene_selection/7-build_final_model.R")' >> $R_file

# Content of sh file
sh_file=$sh_dir/build_final_model.sh
echo 'Rscript' $R_file > $sh_file
chmod +x $sh_file

# Add to queue if qsub exists, submit by python otherwise
file_to_submit=()
if command -v qsub &>/dev/null; then
  file_to_submit+=($sh_file)
  echo -e "$GREEN_TICK Added to queue: $sh_file"
fi

# Submit to queue if qsub exists, to python otherwise
logDir=$baseLogDir'/gene_selection/build_final_model'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh
else
    python assets/submit_local.py --num_parallel 4 --file_location $workDir --step build_final_model
fi
