#!/bin/bash

# Install renv
R -e 'install.packages("renv")'

# Update package list and install git
apt-get update && apt-get install -y git
apt-get install -y libgsl-dev

# Clone the repository
cd scripts
git clone https://github.com/boyiechen/version_control_examples.git

# Modify .env file to change path to mapped Dropbox path
sed -i 's#^DROPBOX_PATH=.*#DROPBOX_PATH=/dropbox#g' version_control_examples/svar_model/.env

# Run master_script.R
cd version_control_examples/svar_model
Rscript code/master_script.R

