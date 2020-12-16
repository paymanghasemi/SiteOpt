# SiteOpt: an Open-source R-package for Site Selection and Portfolio Optimization
**License**:  SiteOpt is an open source package and it is free for noncommercial use/research 

               https://github.com/paymanghasemi/SiteOpt/blob/master/SiteOpt%20Terms%20and%20Conditions.pdf

 
# Installation Guidelines (3 steps)
After opening each link, a download button will appear at the top right corner of the page.

1.  (**Mandatory**) Please watch the 8-min installation video (to install SiteOpt through RStudio): 

               https://usf.box.com/s/5b167q17sxe0m9hmcozzf3es9wdpcmri 

  

2.  (**Mandatory**) Please download the suitable executable files, which contain SCIP and Julia, based on your computer characteristics through one of the following links: 

          Windows users: 
                https://usf.box.com/s/hkj2t1be3is0zcm8je1manze9k9gy6ob 
          Mac users:
                https://usf.box.com/s/llxl7jhbzpg7rtnr4tt9ray03p5i0hal 
          Linux users:
                https://usf.box.com/s/1iopiu6ssx3bkv34u6cd5cfs9ufqyjct  
3.  (Optional) If you prefer to replace SCIP with a powerful commercial solver such as CPLEX then watch the following 3-min video:  

                https://usf.box.com/s/8y4wodzpihuec0mihfr1zhswahonmd58 

# Usage Guidelines (2 steps)

1.  (**Mandatory**) Please download a simple example and watch its 3-min video through the following link in order to test SiteOpt: 

                https://usf.box.com/s/ldkslkm7mwlf3v19sq016pnkfz6mtxf4 

 

2.  (Optional) Please download and read the user manual for further information: 

                https://usf.box.com/s/125flpzjwexfub8lqfsyuyxcf1ez4k0y 
                
                
# Example
We  demonstrate  application  of  SiteOpt in different scenarios using  a  representative  parcel  selection (reserve  design) problem  with  50  parcels. The following figure represents the current status of the parcels in our example.

![Images](images/Initial.jpg)

You can download the excel file of this example from the following link:
                
                https://usf.box.com/s/ohp590atcs0hq127tx9m85tg4z7cb89n
                
To load the SiteOpt in Rstudio as well as to import the example into Rstudio environment, use the following code in Rstudio (assuming the example's excel file is in "E:\Folder\Example.xlsx") :

              # Loading the SiteOpt library in R:
              library(SiteOpt)

              # Importing the information of Parcels, first objective function, and risk objective function:
              SiteOpt::Import_data(Address = "E:\\Folder\\Example.xlsx",First_Objective_Sense = "Max")



# Supporting and Citing

