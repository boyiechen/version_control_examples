
## Replication Instruction

This R project is created with RStudio, so it works the best to run all the script under RStudio. All path is written in relative path under the project root folder.

You may also run all the scripts by the following command:
```shell
cd PROJECT_FOLDER
Rscript code/master_script.R
```

Please refer to `code/mater_script.R` for step-by-step details. 


## Prepare the environment

1. clone the repository to your project folder by
```shell
git clone https://github.com/a0981906660/SVAR.git
```

2. Redirect to the project root folder, open up the Rproject file `SVAR.Rproj` with RStudio

3. RStudio will automatically load the dependency information for you. You may need to run the following R commands to perfectly replicate the environment and install the packages.

```R
install.packages("renv")
renv::status()
renv::install()
```

4. Finally, run `master_script.R`

