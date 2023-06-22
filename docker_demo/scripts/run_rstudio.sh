#!/bin/bash

# run container from image
docker run --rm -ti -e DISABLE_AUTH=true -p 8787:8787 rocker/rstudio