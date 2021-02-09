# Multiplex Immunoflouresence Titration Optimization (mIFTO) R Package - workflow

## check.vars
  - [check.gen.input](#checkgeninput "Title")
  - [check.chkbx.input.vars](#checkchkbxinputvars "Title")
  - [check.define.input.paths](#checkdefineinputpaths "Title")
### check.gen.input
  - check values in the general input table for errors or mistakes
  - ```Slide_ID```: check slide descriptors
    - ERROR: check for empty
    - WARNING: check for illegal characters '-|+|&'
    - WARNING: check for and remove whitespace
    - split at commas for different slide descriptors
  - ```Antibody```: check antibody variable
    - ERROR: check for empty
  - ```Concentration```: check concentration variable
    - ERROR: check for empty
    - WARNING: check for and remove whitespace
    - split at commas and convert to numeric values
      - ERROR: on error, this indicates non-numeric values so exit
      - ERROR: check if the list is not sorted or that any of the values are less than zero
  - ```Opal1```: Fluorophore TSA valye
    - ERROR: check for empty
  - ```Antibody_Opal```: combine the Antibody and Opal1 variables with a space, put Opal1 in paraen.
    - ```paste0(Antibody, '_(', Opal1, ')')```
  - Evalutate the image\ folder names
    - if naming convention is set to true
      - ```titration.type.name``` variable is set to either ```Antibody```, 'Primary Antibody' input, or ```Opal````, 'Fluorophore (TSA)' input, based on the ```titration.type``` specified in the drop down menu 'What is being titrated?'
      - The code will only search for concentrations following the ```titration.type.name```
    - if naming convention is set to false
      - set the ```titration.type.name``` to blank
      - this means it will search for the concentration in every part of the name
### check.chkbx.input.vars
  - for the specified run type ('pixels','cells','tissue') extract the ```Vars``` variable (```Vars_pixels```, ```Vars_cells```, ```Vars_tissue```)
  - collapse the ```vars``` on commas and check the string for specifiers
  - ```flowout```: output the flow-like csv results
  - ```decile.logical```: output decile results
  - ```ihc.logical```: an IHC was included in the analysis and should have results exported
  - ```m.folders```: whether or not the data is in one folder or folders separated by dilution
  - ```threshold.logical```: whether or not the data has defined thresholds
### check.define.input.paths
  - ```wd```: choose the working directory
    - ERROR: on selecting cancel
  - append the ```titration.type.name``` to the concentration as the search paths for data
    - the search is set so that the slide name must either end with the concentration or have no additional 0s attached to the concentration
    - ERROR: check that there is one and only one path for each concentration
  - check if the IHC value was indicated
    - if the IHC is indicated check that at least one file of the specified data type is included for each slide
