# Containerize your coding environment

With containerization tools such as Docker/Kubernetes, it is easy to ship your coding environment as a whole with a single image. However, sometimes it is worthy to do the other way -- always start from a purely clean environment and have all your dependencies set up.

In this practice, we reuse our previous R project example, but this time with the power of containerization.

## Preparation
- Docker installation
    - Linux: 
    - MacOS: Docker Desktop
    - Windows: Docker Desktop / WSL

## FAQ

#### file not found error even when running with WSL

If you're using Windows, even if you specify all the paths correctly, it is likely to see the following error:
```
root@BOYIE-LT:/mnt/c/Users/boyie/repo/version_control_examples/docker_demo# docker compose up rservice
[+] Running 1/0
 ⠿ Container docker_demo-rservice-1  Created                                                                                           0.0s
Attaching to docker_demo-rservice-1
docker_demo-rservice-1  | /bin/bash: line 1: /scripts/setup.sh: cannot execute: required file not found
docker_demo-rservice-1 exited with code 127
```

It can be caused by different line endings among OS. So try `dos2unix`. To be more specific:
```shell
dos2unix scripts/setup.sh
```


