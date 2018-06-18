#!/bin/bash

##################################################
############# Input Paremeters for Run ###########
##################################################

. ./Parameters.sh

## specify data set to use
#if [ "$dataset" = "" ]
#then echo "Data Set Cannot Be Empty"
#	exit 1
#fi
#
## specify number of reps
#if ! [[ $reps -gt 0 ]]
#then echo "Reps Needs To Be Positive Integer"
#	exit 1
#fi
#
## specify the number of clusters k
#if ! [[ $k -gt 0 ]]
#then echo "K (number of clusters) needs to be a positive integer"
#	exit 1
#fi
#
## specify the working directory
#if [ "$workDir" = "" ]
#then echo "Working directory must be specified"
#	exit 1
#fi
#
## specify the input directory
#if [ "$inputDir" = "" ]
#then echo "Input directory must be specified"
#	exit 1
#fi
#
## specify the output directory
#if [ "$outputDir" = "" ]
#then echo "Output directory must be specified"
#	exit 1
#fi
#
## specify log directory
#if [ "$logDir" = "" ]
#then echo "Log directory must be specified"
#		 exit 1
#fi
#
## specify R path to place in $PATH
#if [ "$RPath" = "" ];
#then echo "Path to R directory must be specified"
#fi

# algorithms, consensus and modulus parameters



##################################################
########## Create Directory Structure  ###########
##################################################

for dataset in "${dataSets[@]}"; do

    echo "Removing folder"
    rm -rf $workDir$dataset'/R_file/clust'
    rm -rf $workDir$dataset'/R_file/merge'
    rm -rf $workDir$dataset'/R_file/consensus'
    rm -rf $workDir$dataset'/sh_file/clust'
    rm -rf $workDir$dataset'/sh_file/merge'
    rm -rf $workDir$dataset'/sh_file/consensus'

    mkdir -p $outputDir$dataset'/rds_out_'$dataset
    mkdir -p $outputDir$dataset'/con_mat_'$dataset
    mkdir -p $outputDir$dataset'/imputed_clust_'$dataset

    echo "Creating folders"
    mkdir -p $workDir$dataset'/R_file/clust'
    mkdir -p $workDir$dataset'/R_file/merge'
    mkdir -p $workDir$dataset'/R_file/eval'
    mkdir -p $workDir$dataset'/R_file/consensus'
    mkdir -p $workDir$dataset'/sh_file/clust'
    mkdir -p $workDir$dataset'/sh_file/merge'
    mkdir -p $workDir$dataset'/sh_file/consensus'
    mkdir -p $logDir



    ##################################################
    ############ Create R & Shell Scripts  ###########
    ##################################################

    for i in ${algs[@]}; do

        r=0 # parameter required for partial merge

        for s in `seq 1 $reps`; do
            #************************************************
            #*********** create clust scripts ***************
            #************************************************

            # File names for R script, rds output file, shell job script
            R_clust=$workDir$dataset/R_file/clust/$i$s.R
            sh_clust=$workDir$dataset/sh_file/clust/$i$s.sh

            echo "Creating: {$R_clust}: {$sh_clust}"

            if [ ! -f "${workDir}1_Unsupervised/1_clust_data.R" ]; then
                echo "ERROR: File not found, check 'workDir': ${workDir}1_Unsupervised/1_clust_data.R"
                exit 1
            fi

            if [ ! -f "${workDir}1_Unsupervised/2_impute_missing.R" ]; then
                            echo "ERROR: File not found, check 'workDir': ${workDir}1_Unsupervised/2_impute_missing.R"
                            exit 1
                    fi

            if [ ! -f "${workDir}1_Unsupervised/3_con_mat.R" ]; then
                            echo "ERROR: File not found, check 'workDir' in Parameters.sh: ${workDir}1_Unsupervised/3_con_mat.R"
                            exit 1
                    fi

            if [ ! -d "${outputDir}${dataset}/data_pr_${dataset}" ]; then
                            echo "ERROR: Directory not found, check 'outputDir' and 'dataset' in Parameters.sh: ${outputDir}${dataset}/data_pr_${dataset}"
                            exit 1
                    fi

            # Content of R file
            touch $R_clust
            echo 'k<-'$k >> $R_clust
            echo 's<-'$s >> $R_clust
            echo 'algs<- "'$i'"' >> $R_clust
            echo 'pr<- "cs"' >> $R_clust
            echo 'sfdir<- "'$outputDir$dataset'"' >> $R_clust
            echo 'ndat<- "'$dataset'"' >> $R_clust
            echo 'datadir<- "'$outputDir$dataset'/data_pr_'$dataset'"' >> $R_clust
            echo 'cdat<- readRDS(paste0(datadir,"/cdat_","'$dataset'",".rds"))' >> $R_clust
            echo 'shouldCompute <- '$shouldCompute >> $R_clust
            echo 'source("'$workDir'1_Unsupervised/1_clust_data.R")' >> $R_clust
            echo 'source("'$workDir'1_Unsupervised/2_impute_missing.R")' >> $R_clust
            echo 'source("'$workDir'1_Unsupervised/3_con_mat.R")' >> $R_clust

            # Contents of sh file
            touch $sh_clust
            echo '#!/bin/sh' >> $sh_clust
            echo 'export PATH='$RPath':$PATH' >> $sh_clust
            echo 'Rscript' $R_clust >> $sh_clust

            chmod +x $sh_clust

            #************************************************
            #*********** create merge scripts ***************
            #************************************************

            # Create code to merge files
            # ONLY WORKS for reps > c ==> modulus=0
            if (($s % $c == 0 )); then

                r=$((r+1))

                # file names
                R_merge=$workDir$dataset/R_file/merge/Merge_$i$s.R
                sh_merge=$workDir$dataset/sh_file/merge/Merge_$i$s.sh

                if [ ! -f "${workDir}1_Unsupervised/4_merge_partial_consmat.R" ]; then
                                echo "ERROR: File not found, check 'workDir': ${workDir}1_Unsupervised/4_merge_partial_consmat.R"
                                exit 1
                        fi

                # Content of R file
                touch $R_merge
                echo 'ndat<- "'$dataset'"' >> $R_merge
                echo 'dir <- "'$outputDir$dataset'"' >> $R_merge
                echo 'algs<- "'$i'"' >> $R_merge
                echo 'c <- '$c >> $R_merge
                echo 'r <- '$r >> $R_merge
                echo 'reps <- '$reps >> $R_merge
                echo 'k <- '$k >> $R_merge
                echo 'shouldCompute <- '$shouldCompute >> $R_merge
                echo 'source("'$workDir'1_Unsupervised/4_merge_partial_consmat.R")' >> $R_merge

                # Content of sh file
                touch $sh_merge
                echo '#!/bin/sh' >> $sh_merge
                echo 'export PATH='$RPath':$PATH' >> $sh_merge
                echo 'Rscript' $R_merge >> $sh_merge

                chmod +x $sh_merge
            else
                echo "Skipping Merge Files"
            fi
        done

    done


    #************************************************
    #********* create final merge scripts ***********
    #************************************************

    # file names
    R_merge_final_clust=$workDir$dataset/R_file/merge/Merge_final_clust.R
    R_merge_final_consmat=$workDir$dataset/R_file/merge/Merge_final_consmat.R

    # Create R scripts
    touch $R_merge_final_clust
    echo 'ndat<- "'$dataset'"' >> $R_merge_final_clust
    echo 'dir <- "'$outputDir$dataset'"' >> $R_merge_final_clust
    echo 'algs<- strsplit("'${algs[@]}'", " ")[[1]]' >> $R_merge_final_clust
    echo 'reps<- '$reps >> $R_merge_final_clust
    echo 'k<-'$k >> $R_merge_final_clust
    echo 'shouldCompute <- '$shouldCompute >> $R_merge_final_clust
    echo 'source("'$workDir'1_Unsupervised/5_merge_clust.R")' >> $R_merge_final_clust

    # Create sh scirpts
    touch $R_merge_final_consmat
    echo 'ndat<- "'$dataset'"' >> $R_merge_final_consmat
    echo 'dir <- "'$outputDir$dataset'"' >> $R_merge_final_consmat
    echo 'algs<- strsplit("'${algs[@]}'", " ")[[1]]' >> $R_merge_final_consmat
    echo 'shouldCompute <- '$shouldCompute >> $R_merge_final_consmat
    echo 'source("'$workDir'1_Unsupervised/6_merge_complete_consmat.R")' >> $R_merge_final_consmat


    #************************************************
    #*********** create consensus scripts ***********
    #************************************************

    for l in "${cons[@]}"; do

        # file names
        R_cons=$workDir$dataset/R_file/consensus/Create_$l.R
        sh_cons=$workDir$dataset/sh_file/consensus/Create_$l.sh

        # create R scripts
        touch $R_cons
        echo 'ndat<- "'$dataset'"'>>$R_cons
        echo 'cons.funs<-"'$l'"'>>$R_cons
        echo 'k<- 3'>>$R_cons
        echo 'dir <-"'$outputDir$dataset'/data_pr_'$dataset'"'>>$R_cons
        echo 'shouldCompute <- '$shouldCompute >> $R_cons
        echo 'source("'$workDir'1_Unsupervised/7_con_fun.R")'>>$R_cons

        # create sh scripts
        touch $sh_cons
        echo '#!/bin/sh'>>$sh_cons
        echo 'export PATH='$RPath':$PATH'>>$sh_cons
        echo 'Rscript' $R_cons>>$sh_cons

        chmod +x $sh_cons
    done


    #************************************************
    #********* create evalutation scripts ***********
    #************************************************
    R_eval=$workDir$dataset/R_file/eval/eval_run.R

    # create R script
    touch $R_eval
    echo "fdir <- '"$outputDir"'" > $R_eval
    echo "dat <- '"$dataset"'" >> $R_eval
    echo "referenceClass <- '"$referenceClass"'" >> $R_eval
    echo 'shouldCompute <- '$shouldCompute >> $R_eval
    echo "source('"$workDir"1_Unsupervised/8_Evalution.R')" >> $R_eval

    # Complete
    echo 'File creation complete..'
done