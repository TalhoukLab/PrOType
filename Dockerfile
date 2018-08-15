FROM mattpaletta/r-base:3.5.1
#RUN mkdir -p /PrOType/packrat
RUN R -e 'install.packages("packrat", repos="http://cran.rstudio.com", dependencies=TRUE);'
COPY packrat /PrOType/packrat
RUN R -e 'packrat::restore(project="/PrOType");'
ADD . /PrOType
# docker run --rm -it -h docker.example.com -v ~/Documents/Projects/PrOType/:/PrOType/ --privileged protype:latest bash
