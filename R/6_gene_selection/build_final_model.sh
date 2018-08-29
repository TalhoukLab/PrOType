#!/bin/bash

. ./Parameters.sh

# Make directories for R script, shell script
R_dir=$workDir$dataset/R_file/build_final_model
sh_dir=$workDir$dataset/sh_file/build_final_model
mkdir -p $R_dir
mkdir -p $sh_dir

file_to_submit=()

R_file=$R_dir/build_final_model.R
sh_file=$sh_dir/build_final_model.sh

# Content of R file
echo 'outputDir <- "'$outputDir'"' > $R_file
echo 'source("R/6_gene_selection/build_final_model.R")' >> $R_file

# Content of sh file
echo 'Rscript' $R_file > $sh_file
chmod +x $sh_file

# Add to queue
if command -v qsub &>/dev/null; then
  echo "Adding To Queue: $sh_file"
  file_to_submit+=($sh_file)
fi

# Submit by python if no qsub
if command -v qsub &>/dev/null; then
    :
else
    python assets/submit_local.py --num_parallel 4 --file_location $workDir --step training_part2
fi

# Submit to queue
logDir=$baseLogDir'/gene_selection/build_final_model'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
fi
