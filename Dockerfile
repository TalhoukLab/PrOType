FROM rocker/tidyverse:3.3.1
RUN apt-get update -y && apt-get install -y git
ADD install_packages.R /tmp/install_packages.R
RUN Rscript /tmp/install_packages.R
# docker run --rm -it -h docker.example.com -v ~/Documents/Projects/PrOType/:/PrOType/ --privileged protype:latest bash