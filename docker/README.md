# Docker compilation for arrowtooth

Open a command line and navigate to the docker directories where the `Dockerfile` resides.

1. Compile a `cgrandin/admb` image and push to `cgrandin/admb`. The necessary files are located at [https://github.com/pbs-assess/gfiscam/tree/wsl2/docker]

1. Compile a `cgrandin/csasdown` image and push to `cgrandin/csasdown`. The necessary files are located at [https://github.com/pbs-assess/csasdown/tree/master/docker]

1. Compile a `cgrandin/csasdown-admb` image and push to `cgrandin/csasdown-admb`. The necessary files are located at [https://github.com/cgrandin/docker-projects/tree/master/csasdown-admb]. The commands to build and push are:
 - `docker build . --no-cache -t cgrandin/csasdown-admb`
 - `docker push cgrandin/csasdown-admb`
 
1. Compile a `cgrandin/arrowtooth` image and push to `cgrandin/csasdown`.  The commands to build and push are:
 - `docker build . --no-cache -t cgrandin/arrowtooth`
 - `docker push cgrandin/arrowtooth`

# Files in this directory

1. `.Renviron` - Contains GitHub PAT token and is copied into the Docker image
1. `docker-compose.yaml` - Not currently used. For testing utility of Docker compose features
1. `Dockerfile` - Main file used to build the Docker image
1. `install_packages.R` - An R script called from the Dockerfile used to install R packages

# To run the container

1. Open command line, and navigate to the directory where the Arrowtooth project has been cloned
1. Run the following:
 - `docker run -d -p 8787:8787 --name=arrowtooth --mount type=bind,source="$(pwd)",target=/home/rstudio cgrandin/arrowtooth`
1. Open a web browwser and type in the URL bar:
 - `localhost:8787`
 - Enter Rstudio name and password which are `rstudio` and `qwerty`
