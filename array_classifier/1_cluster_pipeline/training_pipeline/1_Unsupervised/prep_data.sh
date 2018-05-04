#!/bin/sh

. 1_Unsupervised/Parameters.sh

##Get the data for this run
#if [ "$dataSet" = "" ]
#then echo "Data Set Cannot Be Empty"
#	exit 1
#fi
#
#if [ "$workDir" = "" ]
#then echo "Working directory must be specified"
#	exit 1
#fi
#
#if [ "$inputDir" = "" ]
#then echo "Input directory must be specified"
#	exit 1
#fi
#
#if [ "$outputDir" = "" ]
#then echo "Output directory must be specified"
#	exit 1
#fi

echo "Removing Folders"				 
rm -rf $workDir$dataSet'/R_file/prep'
rm -rf $workDir$dataSet'/sh_file/prep'

echo "Creating folders"
mkdir -p $workDir$dataSet'/R_file/prep'
mkdir -p $workDir$dataSet'/R_file/clust'
mkdir -p $workDir$dataSet'/sh_file/prep'
mkdir -p $workDir$dataSet'/sh_file/clust'
mkdir -p $outputDir$dataSet
mkdir -p $outputDir$dataSet'/data_pr_'$dataSet

#File names for R script, rds output file, shell job script
Rname=$workDir$dataSet/R_file/prep/prep.R
shname=$workDir$dataSet/sh_file/prep/prep.sh

echo "Working with folders: $Rname: $shname"

#Content of R file
touch $Rname
echo 'pr<- "cs"'>>$Rname	
echo 'sfdir<- "'$outputDir$dataSet'"'>>$Rname	
echo 'ndat<- "'$dataSet'"'>>$Rname
echo 'datadir<- "'$outputDir$dataSet'/data_pr_'$dataSet'"'>>$Rname
echo 'dpath<- "'$inputDir'"'>>$Rname
echo 'source("'$workDir'1_Unsupervised/0_read_data.R")'>>$Rname

touch $shname
echo '#!/bin/sh'>>$shname
echo 'echo "Running shell Script"'>>$shname
echo 'export PATH=/share/data/apps/R/R-3.2.5/bin:$PATH'>>$shname
echo 'Rscript' $Rname>>$shname

echo 'Preparation Script Completed!'

chmod 755 $shname
$shname

echo 'Preparation of Data Completed!'
