#!/bin/bash

# Setup pyspark to use jupyter lab
export PYSPARK_DRIVER_PYTHON="jupyter"
export PYSPARK_DRIVER_PYTHON_OPTS="notebook --no-browser --port=8888"

# Start PySpark local mode
pyspark

# Start PySpark YARN mode
# pyspark --master yarn --deploy-mode cluster \ # Error
#pyspark --master yarn --deploy-mode client \
#--driver-memory 1G \
#--num-executors 1 \
#--executor-cores 1 \
#--executor-memory 2G

# Start PySpark Standalone mode
#pyspark --master "spark://bdse125.example.org:7077" \
#--driver-memory 1G \
#--num-executors  1\
#--executor-cores 1 \
#--executor-memory 2G
