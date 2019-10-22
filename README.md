# Supplemental code for: "Effect of reductions in observed disease and improved antibiotic stewardship on antibiotic prescribing in Massachusetts between 2011 and 2015"
## Stephen M. Kissler, R. Monina Klevens, Michael L. Barnett, Yonatan H. Grad

The primary script in this repository is 'disease-stewardship.R'. That script calls 'icd.R' and pulls in the namcs**.csv files. It generates the figures and table in the main text. 

The file 'sinusoidfit.nb' is a _Mathematica_ script that generates a maximum likelihood sinusoidal regression with linear envelope boundaries, as described in the main text (Methods) and Supplemental Information. The file 'sinusoidfit.pdf' is a .pdf copy of the _Mathematica_ script.
