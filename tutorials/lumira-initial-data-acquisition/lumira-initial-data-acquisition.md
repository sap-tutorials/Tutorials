---
title: Initial data acquisition in SAP BusinessObjects Lumira
description: Data acquisition from local sample MS Excel file
primary_tag: products>sap-businessobjects-lumira
tags: [  tutorial>beginner, products>sap-businessobjects-lumira ]
---
## Prerequisites  
- **Proficiency:** Beginner
- **Tutorials:** [Download and install SAP BusinessObjects Lumira](http://www.sap.com/developer/tutorials/lumira-install.html)

## Next Steps
- [Initial data exploration in SAP BusinessObjects Lumira](http://www.sap.com/developer/tutorials/lumira-initial-data-exploration.html)

## Details
### You will learn  
How to acquire data into SAP BusinessObjects Lumira using sample MS Excel file.

### Time to Complete
**5 Min**.

---


[ACCORDION-BEGIN [Step 1: ](Create new document)] ￼

Open SAP BusinessObjects Lumira. Create new document by selecting **File** -> **New** from the menu.

You can also press `Ctrl+N`.

![Creating a new document in SAP BusinessObjects Lumira](Lum01-01.png)

> In this example screenshots from SAP BusinessObjects Lumira version 1.30 in English are being used. They may be slightly different between versions and application languages.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create new dataset)] ￼

Create a new Dataset by selecting **Microsoft Excel** as a source, pressing **Next**, and then selecting `BestRunCorp_Retail.xlsx` file from the your user's directory `…\Documents\SAP Lumira Documents\Samples\`. SAP BusinessObjects Lumira installation comes with a few sample files, and this MS Excel file with data is just one of them.

> If for any reason you do not have that file or that directory in your local installation, then download [`BestRunCorp_Retail.xlsx`](./BestRunCorp_Retail.xlsx) file.

![Adding the first dataset from MS Excel to the new document in SAP BusinessObjects Lumira](Lum01-02.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Choose columns in dataset preview)] ￼

Dataset preview is displayed. At this stage you can decide what columns from the dataset are needed for further analysis. You will not need **Year-Quarter** and **GMR** (Gross Margin) columns, so uncheck them.

![Columns "GMR" and "Year-Quarter" have been deselected](Lum01-04a.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Check the record count)] ￼

Click on **Show records count** to check numbers of rows and columns to be acquired from this dataset. You are about to create a document with a dataset that has 16317 rows with 13 columns.

![Checking the size of a dataset before importing it into SAP BusinessObjects Lumira](Lum01-05a.png)

Click **Create**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](New document available)] ￼

SAP BusinessObjects Lumira opens the dataset in the new document ready for data discovery and visualizations. One of so called 'rooms' is open for your further activities: **Prepare**, **Visualize**, or **Compose**.

> It depends on the preferences settings accessible from **File** -> **Preferences** -> **General** -> **Default Room**

![The new document is created in SAP BusinessObjects Lumira and Prepare room is selected for further work with a dataset](Lum01-06.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Save current document)] ￼

Save the current document locally at your computer as `Tutorial1`.

![Saving your first document in SAP BusinessObjects Lumira](Lum01-07.png)

> Please note that all SAP BusinessObjects Lumira documents are saved in the same default directory in user's `…\Documents\SAP Lumira Documents\` folder.

> Lumira documents have extension `.lums`. These files are zipped archives containing multiple files, like configuration and data files, visualization thumbnails etc.

[DONE]
[ACCORDION-END]


## Next Steps
- [Initial data exploration in SAP BusinessObjects Lumira](http://www.sap.com/developer/tutorials/lumira-initial-data-exploration.html)
