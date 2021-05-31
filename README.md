# arrowtooth

[![Travis build status](https://travis-ci.org/pbs-assess/arrowtooth.svg?branch=master)](https://travis-ci.org/pbs-assess/arrowtooth)
[![Coverage status](https://codecov.io/gh/pbs-assess/arrowtooth/branch/master/graph/badge.svg)](https://codecov.io/github/pbs-assess/arrowtooth?branch=master)

There is a Docker image for this package! See https://github.com/pbs-assess/arrowtooth/wiki/Running-the-docker-image-with-this-package

An R package which contains functions for exploration of models and for generating a CSAS stock assessment Research document using [csasdown](https://github.com/pbs-assess/csasdown).

Created on March 30, 2020 using the https://github.com/pbs-assess/csasdown package.

Some content moved from the latex method used in the 2014 assessment:

https://github.com/cgrandin/csas-latex

Much of the integration will be based on the herring Science Response document:

https://github.com/pbs-assess/herringsr

Packages integral to the model running and loading include:

https://github.com/pbs-assess/gfutilities

https://github.com/pbs-assess/gfiscam

https://github.com/pbs-assess/gfiscamutils

For french integration:

https://github.com/pbs-assess/rosettafish

## To run the Arrowtooth Statistical Catch-at-age model (SCA)

```r
devtools::load_all(".")
run_af_sca()
```

This will open an HTML page in your browser which shows input data and output from an SCA
model implemented in the [MSEtool](https://github.com/tcarruth/MSEtool) package. Internally,
the model is implemented using [TMB](https://github.com/kaskr/adcomp).

The data come from two files generated using the [gfdata](https://github.com/pbs-assess/gfdata) package. This project contains functions which convert these data into the required formats
for use in `MSEtool`.

## CSAS Research document status
In March of 2020 is was thought that a rather large age request for Arrowtooth may be completed and that we would be able to use the iSCAM age-structured model as was used in the previous assessment. Because of this, the structure of the CSAS document was set up to read in iSCAM model output and make figures in the correct way so that we could create the document side-by-side with the model explorations. To make this work, you need those model outputs. Contact the authors of this repository for this.
