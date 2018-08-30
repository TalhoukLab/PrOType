#!/bin/bash

. ./Parameters.sh

# Make directories for R script, shell script
R_dir=$workDir/R_file/gene_selection/training_part2
sh_dir=$workDir/sh_file/gene_selection/training_part2
mkdir -p $R_dir
mkdir -p $sh_dir

# Content of R file
R_file=$R_dir/training_part2.R
echo 'outputDir <- "'$outputDir'"' > $R_file
echo 'source("R/5-gene_selection/6-training_part2.R")' >> $R_file

# Content of sh file
sh_file=$sh_dir/training_part2.sh
echo 'Rscript' $R_file > $sh_file
chmod +x $sh_file

# Add to queue if qsub exists
file_to_submit=()
if command -v qsub &>/dev/null; then
  echo "Adding To Queue: $sh_file"
  file_to_submit+=($sh_file)
fi

# Submit to queue if qsub exists, to python otherwise
logDir=$baseLogDir'/gene_selection/training_part2'
if command -v qsub &>/dev/null; then
    . ./assets/submit_queue.sh

    echo "Finished Submitting files.  Check progress with \"qstat -u ${user}\""
    echo "The logs can be found in \"${logDir}\""
else
    python assets/submit_local.py --num_parallel 4 --file_location $workDir --step training_part2
fi
