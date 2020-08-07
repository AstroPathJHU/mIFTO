# Multiplex Immunoflouresence Titration Optimization (mIFTO) R Package 
#### <div align="center">***v.2.00.0021***</div>
#### <div align="center">Created by: Benjamin Green & Charles Roberts</div>
#### <div align="center">Tumor Microenvironment Technology Development Center</div>
#### <div align="center">The Johns Hopkins University Bloomberg~Kimmel Institute for Cancer Immunotherapy</div>
#### <div align="center">Correspondence to: bgreen42@jhu.edu</div>

## ***Section 1: Summary***
This R package was developed to help organize and quantify the inForm Cell Analysis output for pixel-by-pixel, cell-by-bell, and tissue segmentation data. The primary goal of this package is to aid in the assessment and determination of optimum staining conditions for multiplex immunofluorescence titrations on Akoya’s scanning and staining platform. There are two primary functions of mIFTO; ```FOP()``` and ```mIFTOapp()```. FOP was designed to measure the ‘fractions of positivity’ for individual conditions. Then, with a unique identifier, the group conditions together into a single csv output file. The function uses the inForm output tables (IF or IHC) for cell segmented, colocalization data, or tissue segmented data as input. The other primary function, ```mIFTOapp()```, was developed to aid in determining an optimum condition for a series of reagent titrations. The output includes pixel histograms of intensity, t-statistics, signal-to-noise ratios, fractions of positivity, and boxplots of expression profiles. 
## ***Section 2: Getting Started***
#### ***Section 2.1: Initial Package Install***
Open an Rstudio session to get started. Next, install the package from github using the following commands:
```
install.packages(‘devtools’)
library(devtools)
install_github('beng1290/mIFTO')
```
When the package begins to install a number of messages will appear in the console indicating the status of different events, especially during the initial install or just after R has been updated, this is normal. Some of the messages may be in red, this does not indicate an error. If there is an error during installation, usually the output message to the console will start with ```“Error:”``` or ```“Warning:”```, then describe the corresponding error. 
The installation of ‘devtools’ is usually the most interactive and takes the longest. This is a provided R package which is used for external programmers to develop R packages. During the ```devtools``` installation command (command 1 above), R may require some user input, examples of this include: 
1.	R may indicate that some packages should be updated. The user can either indicate to update all, some or none of the packages. It is usually best to select the update all option. 
2.	A dialog box may also open asking the user if the package should ‘be compiled from source’, it is usually best to answer ‘yes’ to this 
3.	A dialog box may open telling the user that a package is in use and that the R session must be restarted to continue installation of a package. It is usually best to allow R to do so. Note that is may take a few moments for R to reinitialize after it restarts, this is normal. 

Errors are common in this step, usually when updating packages. If a package fails to update properly and crashes the installation process an error message is usually output beginning with: ```“Error: package ‘failed_package_name’``` failed to install’. Installing the package separately may be necessary. Enter ```install.packages(‘failed_package_name’)```; replacing ‘failed_package_name’ with the name of the failed package.

For more advanced users of R, it may be easier to specific additional options during a package installation. For example, when installing ‘devtools’ the following options can be specified:

```install.packages(‘devtools’, ask = FALSE, quiet = TRUE, verbose = FALSE)```

This command will install ```‘devtools’``` without asking the user if it is okay to updated or compile a package and will carry out the procedures automatically. The command will also significantly reduce the output to the console. 

When the ```install_github('beng1290/mIFTO')``` command is used, R will output the following messages, followed by a number of other messages. 

![Figure 1 Image](R/www/Fig1.PNG)
