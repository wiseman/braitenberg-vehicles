#!/bin/ksh
# Run Skij. You must already have Java set up to run.
# You must also be connected to the Skij directory
#   (or modify the classpath below)

java -classpath skij.jar:$CLASSPATH com.ibm.jikes.skij.Scheme $*


