This is a lab notebook following the the format suggested by Noble 2009 in "A quick guide to organizing computational biology projects".
It incorperates the functionality of RStudio and the R packages rmarkdown, knitr, packrat, and labtools to integrate analysis, note taking, and results into a sharable and reproducable format.
Below are descriptions of what the various files and folders in this folder do.


* `.gitignore`: Lists what files should be ignored by git version control. 
* `.Rprofile`: Code ran by R each time an R session is started.
* `yyyy_mm_dd-notebook.Rproj`: The RStudio project settings file.
* `bin`: Contains and binary programs used for analysis.
* `data`: Contains raw/static data.
* `reports`: Contains reports composed of one or more notes from the `content` folder.
* `references`: Contains a .bibtex file for markdown citations and possibly the pdfs of cited papers. 
* `content`: Contains notes and analyses automatically ran when building.
* `src`: Contains custom scripts used for analyses.
* `info.yaml`: Information on attributes of the notebook.
