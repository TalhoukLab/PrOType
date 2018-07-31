#!/bin/bash

. ./Parameters.sh

##################################################
########## Create Directory Structure  ###########
##################################################

for dataset in "${dataSets[@]}"; do

    echo "Removing folder"
    rm -r $workDir$dataset


    mkdir -p "$outputDir$dataset/rds_out_$dataset"
    mkdir -p "$outputDir$dataset/con_mat_$dataset"
    mkdir -p "$outputDir$dataset/con_mat_merged_$dataset"
    mkdir -p "$outputDir$dataset/imputed_clust_$dataset"

    echo "Creating folders"
    mkdir -p "$workDir$dataset/R_file/merge"
    mkdir -p "$workDir$dataset/R_file/eval"
    mkdir -p "$workDir$dataset/R_file/consensus"
    mkdir -p "$workDir$dataset/sh_file/merge"
    mkdir -p "$workDir$dataset/sh_file/consensus"
    mkdir -p "$logDir"



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
            mkdir -p "$workDir$dataset/R_file/clust"
            mkdir -p "$workDir$dataset/sh_file/clust"

            R_clust=$workDir$dataset/R_file/clust/$i$s.R
            sh_clust=$workDir$dataset/sh_file/clust/$i$s.sh

           #echo "Creating: {$R_clust}: {$sh_clust}"

            if [ ! -d "${outputDir}${dataset}/data_pr_${dataset}" ]; then
                            echo "ERROR: Directory not found, check 'outputDir' and 'dataset' in Parameters.sh: ${outputDir}${dataset}/data_pr_${dataset}"
                            exit 1
                    fi

            # Content of R file
            echo 'k<-'$k > $R_clust
            echo 's<-'$s >> $R_clust
            echo 'algs<- "'$i'"' >> $R_clust
            echo 'pr<- "cs"' >> $R_clust
            echo 'sfdir<- "'$outputDir$dataset'"' >> $R_clust
            echo 'ndat<- "'$dataset'"' >> $R_clust
            echo 'datadir<- "'$outputDir$dataset'/data_pr_'$dataset'"' >> $R_clust
            echo 'cdat<- readRDS(paste0(datadir,"/cdat_","'$dataset'",".rds"))' >> $R_clust
            echo 'shouldCompute <- '$shouldCompute >> $R_clust
            echo 'source("R/unsupervised/clust_data.R")' >> $R_clust

            # Contents of sh file
            echo 'Rscript' $R_clust > $sh_clust

            chmod +x $sh_clust

            #************************************************
            #*********** create merge scripts ***************
            #************************************************

            # Create code to merge files
            # ONLY WORKS for reps > c ==> modulus=0
            if (($s % $c == 0 )); then

                r=$((r+1))

                # file names
                mkdir -p $workDir$dataset/R_file/merge
                mkdir -p $workDir$dataset/sh_file/merge
                R_merge=$workDir$dataset/R_file/merge/Merge_$i$s.R
                sh_merge=$workDir$dataset/sh_file/merge/Merge_$i$s.sh

                # Content of R file
                echo 'ndat<- "'$dataset'"' > $R_merge
                echo 'dir <- "'$outputDir$dataset'"' >> $R_merge
                echo 'algs<- "'$i'"' >> $R_merge
                echo 'c <- '$c >> $R_merge
                echo 'r <- '$r >> $R_merge
                echo 'reps <- '$reps >> $R_merge
                echo 'k <- '$k >> $R_merge
                echo 'shouldCompute <- '$shouldCompute >> $R_merge
                echo 'source("R/unsupervised/merge_partial_consmat.R")' >> $R_merge

                # Content of sh file
                echo 'Rscript' $R_merge > $sh_merge

                chmod +x $sh_merge
            else
                : #echo "Skipping Merge Files"
            fi
        done

    done


    #************************************************
    #********* create final merge scripts ***********
    #************************************************

    # file names
    mkdir -p $workDir$dataset/R_file/merge
    mkdir -p $workDir$dataset/sh_file/merge

    R_merge_final_clust=$workDir$dataset/R_file/merge/Merge_final_clust.R
    R_merge_final_consmat=$workDir$dataset/R_file/merge/Merge_final_consmat.R

    # Create R scripts
    echo 'ndat<- "'$dataset'"' > $R_merge_final_clust
    echo 'dir <- "'$outputDir$dataset'"' >> $R_merge_final_clust
    echo 'algs<- strsplit("'${algs[@]}'", " ")[[1]]' >> $R_merge_final_clust
    echo 'reps<- '$reps >> $R_merge_final_clust
    echo 'k<-'$k >> $R_merge_final_clust
    echo 'shouldCompute <- '$shouldCompute >> $R_merge_final_clust
    echo 'source("R/unsupervised/merge_clust.R")' >> $R_merge_final_clust

    # Create sh scirpts
    echo 'ndat<- "'$dataset'"' > $R_merge_final_consmat
    echo 'dir <- "'$outputDir$dataset'"' >> $R_merge_final_consmat
    echo 'algs<- strsplit("'${algs[@]}'", " ")[[1]]' >> $R_merge_final_consmat
    echo 'shouldCompute <- '$shouldCompute >> $R_merge_final_consmat
    echo 'source("R/unsupervised/merge_complete_consmat.R")' >> $R_merge_final_consmat


    #************************************************
    #*********** create consensus scripts ***********
    #************************************************

    for l in "${cons[@]}"; do

        mkdir -p $workDir$dataset/R_file/consensus
        mkdir -p $workDir$dataset/sh_file/consensus

        # file names
        R_cons=$workDir$dataset/R_file/consensus/Create_$l.R
        sh_cons=$workDir$dataset/sh_file/consensus/Create_$l.sh

        # create R scripts
        echo 'ndat<- "'$dataset'"' > $R_cons
        echo 'cons.funs<-"'$l'"'>> $R_cons
        echo "k <- $k" >> $R_cons
        echo 'dir <-"'$outputDir$dataset'/data_pr_'$dataset'"' >> $R_cons
        echo 'shouldCompute <- '$shouldCompute >> $R_cons
        echo 'source("R/unsupervised/con_fun.R")' >> $R_cons

        # create sh scripts
        echo 'Rscript' $R_cons > $sh_cons

        chmod +x $sh_cons
    done


    #************************************************
    #********* create evalutation scripts ***********
    #************************************************
    mkdir -p $workDir$dataset/R_file/eval

    R_eval=$workDir$dataset/R_file/eval/eval_run.R

    # create R script
    echo "fdir <- '"$outputDir"'" > $R_eval
    echo "dat <- '"$dataset"'" >> $R_eval
    echo "referenceClass <- '"$referenceClass"'" >> $R_eval
    echo 'shouldCompute <- '$shouldCompute >> $R_eval
    echo "source('R/unsupervised/final_clust.R')" >> $R_eval

done
echo 'File creation complete..'
