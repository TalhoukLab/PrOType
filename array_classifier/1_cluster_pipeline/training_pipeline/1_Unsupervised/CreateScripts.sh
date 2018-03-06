#!/bin/sh

##################################################
############# Input Paremeters for Run ###########
##################################################

. 1_Unsupervised/Parameters.sh

## specify data set to use
#if [ "$dataSet" = "" ]
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
algs=(nmfbrunet nmflee distalgs rest)
cons=(majority kmodes CSPA LCEcts LCEsrs LCEasrs)
c=100 # use for determining splitting criterion (min 100 reps required)



##################################################
########## Create Directory Structure  ###########
##################################################

rm -rf $workDir$dataSet'/R_file/clust'
rm -rf $workDir$dataSet'/R_file/merge'
rm -rf $workDir$dataSet'/R_file/consensus'
rm -rf $workDir$dataSet'/sh_file/clust'
rm -rf $workDir$dataSet'/sh_file/merge'
rm -rf $workDir$dataSet'/sh_file/consensus'

mkdir -p $outputDir$dataSet'/rds_out_'$dataSet
mkdir -p $outputDir$dataSet'/con_mat_'$dataSet
mkdir -p $outputDir$dataSet'/imputed_clust_'$dataSet

mkdir -p $workDir$dataSet'/R_file/clust'
mkdir -p $workDir$dataSet'/R_file/merge'
mkdir -p $workDir$dataSet'/R_file/eval'
mkdir -p $workDir$dataSet'/R_file/consensus'
mkdir -p $workDir$dataSet'/sh_file/clust'
mkdir -p $workDir$dataSet'/sh_file/merge'
mkdir -p $workDir$dataSet'/sh_file/consensus'
mkdir -p $logDir



##################################################
############ Create R & Shell Scripts  ###########
##################################################

for i in "${algs[@]}"; do
	
	r=0 # parameter required for partial merge

	for s in `seq 1 $reps`; do
		#************************************************
		#*********** create clust scripts ***************
		#************************************************

		# File names for R script, rds output file, shell job script
		R_clust=$workDir$dataSet/R_file/clust/$i$s.R
		sh_clust=$workDir$dataSet/sh_file/clust/$i$s.sh
	
		# Content of R file
		touch $R_clust
		echo 's<-'$s >> $R_clust
		echo 'algs<- "'$i'"' >> $R_clust	
		echo 'pr<- "cs"' >> $R_clust	
		echo 'sfdir<- "'$outputDir$dataSet'"' >> $R_clust	
		echo 'ndat<- "'$dataSet'"' >> $R_clust
		echo 'datadir<- "'$outputDir$dataSet'/data_pr_'$dataSet'"' >> $R_clust
		echo 'cdat<- readRDS(paste0(datadir,"/cdat_","'$dataSet'",".rds"))' >> $R_clust
		echo 'source("'$workDir'1_Unsupervised/1_clust_data.R")' >> $R_clust	
		echo 'source("'$workDir'1_Unsupervised/2_impute_missing.R")' >> $R_clust	
		echo 'source("'$workDir'1_Unsupervised/3_con_mat.R")' >> $R_clust	
		
		# Contents of sh file
		touch $sh_clust
		echo '#!/bin/sh' >> $sh_clust
		echo 'export PATH='$RPath':$PATH' >> $sh_clust
		echo 'Rscript' $R_clust >> $sh_clust

		#************************************************
		#*********** create merge scripts ***************
		#************************************************

		# Create code to merge files
		# ONLY WORKS for reps > c ==> modulus=0
		if (($s%$c == 0 )); then

			r=$((r+1))

			# file names
			R_merge=$workDir$dataSet/R_file/merge/Merge_$i$s.R
			sh_merge=$workDir$dataSet/sh_file/merge/Merge_$i$s.sh

			# Content of R file
			touch $R_merge
			echo 'ndat<- "'$dataSet'"' >> $R_merge
			echo 'dir <- "'$outputDir$dataSet'"' >> $R_merge
			echo 'algs<- "'$i'"' >> $R_merge	
			echo 'c <- '$c >> $R_merge
			echo 'r <- '$r >> $R_merge
			echo 'merge <- "partial"' >> $R_merge
			echo 'reps <- '$reps >> $R_merge
			echo 'k <- '$k >> $R_merge
			echo 'source("'$workDir'1_Unsupervised/5_merge_consmat.R")' >> $R_merge	

			# Content of sh file
			touch $sh_merge
			echo '#!/bin/sh' >> $sh_merge
			echo 'export PATH='$RPath':$PATH' >> $sh_merge
			echo 'Rscript' $R_merge >> $sh_merge
		fi		
	done
	
done


#************************************************
#********* create final merge scripts ***********
#************************************************

# file names
R_merge_final_clust=$workDir$dataSet/R_file/merge/Merge_final_clust.R
R_merge_final_consmat=$workDir$dataSet/R_file/merge/Merge_final_consmat.R

# Create R scripts
touch $R_merge_final_clust
echo 'ndat<- "'$dataSet'"' >> $R_merge_final_clust
echo 'dir <- "'$outputDir$dataSet'"' >> $R_merge_final_clust
echo 'algs<- strsplit("'${algs[@]}'", " ")[[1]]' >> $R_merge_final_clust	
echo 'reps<- '$reps >> $R_merge_final_clust
echo 'source("'$workDir'1_Unsupervised/4_merge_clust.R")' >> $R_merge_final_clust	

# Create sh scirpts
touch $R_merge_final_consmat
echo 'ndat<- "'$dataSet'"' >> $R_merge_final_consmat
echo 'dir <- "'$outputDir$dataSet'"' >> $R_merge_final_consmat
echo 'algs<- strsplit("'${algs[@]}'", " ")[[1]]' >> $R_merge_final_consmat	
echo 'merge <- "complete"' >> $R_merge_final_consmat
echo 'source("'$workDir'1_Unsupervised/5_merge_consmat.R")' >> $R_merge_final_consmat


#************************************************
#*********** create consensus scripts ***********
#************************************************

for l in "${cons[@]}"; do
	
	# file names
	R_cons=$workDir$dataSet/R_file/consensus/Create_$l.R
	sh_cons=$workDir$dataSet/sh_file/consensus/Create_$l.sh
	
	# create R scripts
	touch $R_cons
	echo 'ndat<- "'$dataSet'"'>>$R_cons
	echo 'cons.funs<-"'$l'"'>>$R_cons
	echo 'k<- 3'>>$R_cons	
	echo 'dir <-"'$outputDir$dataSet'/data_pr_'$dataSet'"'>>$R_cons	
	echo 'source("'$workDir'1_Unsupervised/6_con_fun.R")'>>$R_cons	

	# create sh scripts
	touch $sh_cons
	echo '#!/bin/sh'>>$sh_cons
	echo 'export PATH='$RPath':$PATH'>>$sh_cons
	echo 'Rscript' $R_cons>>$sh_cons
done


#************************************************
#********* create evalutation scripts ***********
#************************************************
R_eval=$workDir$dataSet/R_file/eval/eval_run.R

# create R script
touch $R_eval
echo "fdir <- '"$outputDir"'" > $R_eval
echo "dat <- '"$dataSet"'" >> $R_eval
echo "source('"$workDir"1_Unsupervised/7_Evalution.R')" >> $R_eval


# Complete
echo 'File creation complete..'
