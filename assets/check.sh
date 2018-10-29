#!/bin/bash

# Check whether an input is a positive integer. If not, logout.
function is_pos_int() {
  var=$1
  val=$(eval echo "$"$var)
  if ! [[ $val =~ ^[1-9][0-9]*$ ]]; then
    echo $var=$val is not a positive integer.
    exit 1
  fi
}

# Specify the working directory. If empty, logout.
if [ "$workDir" = "" ]; then
  echo "Working directory must be specified"
	exit 1
fi

# Specify the output directory. If empty, logout.
if [ "$outputDir" = "" ]; then
  echo "Output directory must be specified"
	exit 1
fi

# Specify the log directory. If empty, logout.
if [ "$baseLogDir" = "" ]; then
  echo "Log directory must be specified"
	exit 1
fi

# Specify the normalization method. If empty, logout.
if [ "$normalizeBy" = "" ]; then
  echo "Normalization method must be specified"
  exit 1
fi

# Specify the minimum variance. If negative, logout.
if ! [[ $minVar -ge 0 ]]; then
  echo "Minimum variance must be greater than or equal to 0"
	exit 1
fi

# Specify the normalization type. If empty, logout.
if [ "$normType" = "" ]; then
  echo "Normalization type must be specified"
	exit 1
fi

# Specify R path to place in $PATH. If empty, logout.
if [ "$RPath" = "" ]; then
  echo "Path to R directory must be specified"
  exit 1
fi

# Check whether reps, top, k are positive integers
for param in reps top k
  do is_pos_int $param
done

# Specify number of items to merge per script. If not divisible, logout.
if ! [[ $c =~ ^[1-9][0-9]* && $(echo "$reps % $c" | bc) == 0 ]]; then
  echo "c needs to be a positive integer and reps needs to be divisible by c"
	exit 1
fi
