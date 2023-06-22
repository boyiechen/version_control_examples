# Version Control for Researchers in Social Science

This repo displays some example and common use cases that a research may encounter in her daily life, and how version control tools such as git can help. 
Furthermore, although containerization is not always considered a version control tool, it can be used to be a good dependency manager, as you can always dispose a container and create another one.
The concept of containerization matches the spirit of reproducibility, which is one of the most important research essence.


### Topics
The following topic will be covered:
- version control with git
- dependency control with [`renv`](https://rstudio.github.io/renv/articles/renv.html) or [`pyenv`](https://github.com/pyenv/pyenv)
- dependency control with Docker


# Preparation

Here are some tools/softwares you need to pre-install before you proceed with the examples in this repo.
1. [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
2. [Docker](https://docs.docker.com/engine/install/)

Please visit the websites accordingly for detailed information about the installation.


### Remark
- If you're using Windows, you may want to use [WSL](https://learn.microsoft.com/en-us/windows/wsl/install) for a smoother experience with Docker

- If you're using MacOS, make sure you've installed [Xcode](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwjev7L7_tX_AhVejYkEHfhKCqkQFnoECCgQAQ&url=https%3A%2F%2Fapps.apple.com%2Fus%2Fapp%2Fxcode%2Fid497799835%3Fmt%3D12&usg=AOvVaw2fEvMbfRtGhB4SPHYB54NX&opi=89978449) and the corresponding command line tool.

- For people who are afraid of CLI (command line interface), GitHub also offers a GUI application, [GitHub Desktop](https://desktop.github.com), which is worth trying.

