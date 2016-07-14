# BacArena

BacArena is an agent based modeling framework for cellular communities.

Existing genome-scale metabolic models can be used to simulate growth and interactions in time and space.
In BacArena two well established methods are merged: a) Flux balance analysis to predict the activity of
metabolic reactions and b) Agent based modelling in order to provide an environment.

It has been  efficiently implemented in R language (with some C++ routines) and is freely available [CRAN](https://cran.r-project.org/package=BacArena).

Features:
- Each organism is represented individually
- Simulation of >10 different species and thousands of organisms on your desktop computer
- Diffusion of substances
- Screening of phenotypes
- Detection of crossfeeding
- Chemotaxis
- Kinetics of reactions
- Separation of simulation and evaluation
- Rich evaluation methods (data mining)
- Reproducible simulations
- Object oriented implementation
- Easily expandable due to rule based approach


## Installation

- Install the latest release: 
  ```r
install.packages("BacArena")
```

- Install the development version:
  ```r
library(devtools)
install_github("euba/bacarena")
```

- Special hints for linux user:
  - glpk header files needed, e.g. for debian install package: libglpk-dev

- Special hints for windows user:
  - Besides [R](https://cran.r-project.org/bin/windows/base/) you need to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)


## Quick start
```r
library("BacArena")
openArena()
```


## Documentation

A tutorial is available: [Introduction](https://github.com/euba/BacArena/raw/master/vignettes/BacArena-Introduction.pdf) 


## Issues

Please report bugs, disorders or features you would like to see: [Issues](https://github.com/euba/BacArena/issues)
