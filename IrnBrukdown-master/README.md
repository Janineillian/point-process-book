# IrnBrukdown
Bookdown template with Springer latex class

## Compiling the book 

This is easiest using Rstudio. 

1. Load Rstudio and open the "irnbrukdown.Rpoj" --- it is necessary that you have an Rproj in the same directory as 
all the other files for the book in order for the building of the book to work. 

2. With the R project open, ther should be a tab called "Build" in the Environment page (usually top-right pane). Click
   on this tab and then click "Build Book". You can use the drop down menu to select which formats are built. By default, it should
   build "gitbook" and "pdf". 
   
3. The book should open automatically once built. 


## Key Input Files 

<code> index.Rmd </code>: There must be a file by this name containing the Markdown header "---". Document class is set to "svmono"
the Springer monograph Latex template. Below the header "---" is a code chunk called "postlatex", this chunk is required in order 
for the svmono class to work (it is an ugly programming hack). 

<code> _bookdown.yml </code>: the final line of this file starting "rmd_files" is where you specify the Rmd files for each 
chapter in the order you want them to appear in the book. The book's file name is set here too. 

<code> _output.yml </code>: specify parameters for each output type. In particular for "gitbook", you can set what is printed 
before and after the table of contents. You can also specify your CSS style file. 

<code> style.css </code>: This is the style file used by gitbook, if you are familiar with CSS.

<code> toc.css </code>: The CSS style file for the table of contents in gitbook

<code> preamble.tex </code>: Include here any latex code (e.g. including packages, defining new commands) and it will be 
added to the beginning the Latex file created using bookdown, before the PDF is made. Please keep the "\usepackage{}" command
as this includes all the packages required by the Springer template. 

<code> book.bib </code> and <code> packages.bib </code>: include here your references for the book (to papers etc...) and a 
separate file is used to R packages (not sure why). 

## Key Output Files 

Once the book is built, the output is saved in the directory <code> _book </code>. Generated figures are saved in <code> _bookdown_files </code>. 


