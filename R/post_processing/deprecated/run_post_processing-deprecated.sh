echo 'for (dataset in unlist(strsplit("'"${dataSets[*]}"'", " "))) {' > $Rname
    echo 'cli::cat_line("Starting Part 0\n")' >> $Rname
    echo 'cli::cat_line(dataset, "\n")' >> $Rname
    echo 'output_dir <- "'$outputDir'/iv_summary"' >> $Rname
    echo 'source("array_classifier/2_post_processing/step0_InternalValidation.R")' >> $Rname
echo '}' >> $Rname
echo 'datasets <- unlist(strsplit("'"${dataSets[*]}"'", " "))' >> $Rname
echo 'algs <- purrr::set_names(unlist(strsplit("'"${classificationAlgs[*]}"'", " ")))' >> $Rname
echo 'output_dir <- "'$outputDir'"' >> $Rname
echo 'cli::cat_line("Starting Part 1\n")' >> $Rname
echo 'source("array_classifier/2_post_processing/step1_BuildFinalModel.R")' >> $Rname
echo 'cli::cat_line("Starting Part 2\n")' >> $Rname
echo 'source("array_classifier/2_post_processing/step2_ArrayValidation.R")' >> $Rname
