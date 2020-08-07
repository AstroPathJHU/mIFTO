# Multiplex Immunoflouresence Titration Optimization (mIFTO) R Package 
#### v. 2.00.0021
###### Created by: Benjamin Green & Charles Roberts

###### Tumor Microenvironment Technology Development Center
###### The Johns Hopkins University Bloomberg~Kimmel Institute for Cancer Immunotherapy
###### Correspondence to: bgreen42@jhu.edu

## ***Section 1: Summary***
This R package was developed to help organize and quantify the inForm Cell Analysis output for pixel-by-pixel, cell-by-bell, and tissue segmentation data. The primary goal of this package is to aid in the assessment and determination of optimum staining conditions for multiplex immunofluorescence titrations on Akoya’s scanning and staining platform. There are two primary functions of mIFTO; FOP() and mIFTOapp(). FOP was designed to measure the ‘fractions of positivity’ for individual conditions. Then, with a unique identifier, the group conditions together into a single csv output file. The function uses the inForm output tables (IF or IHC) for cell segmented, colocalization data, or tissue segmented data as input. The other primary function, mIFTOapp(), was developed to aid in determining an optimum condition for a series of reagent titrations. The output includes pixel histograms of intensity, t-statistics, signal-to-noise ratios, fractions of positivity, and boxplots of expression profiles. 


