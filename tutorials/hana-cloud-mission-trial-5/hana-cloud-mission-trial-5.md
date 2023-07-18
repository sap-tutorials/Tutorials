---
parser: v2
author_name: Christopher Kollhed
author_profile: https://github.com/chriskollhed
auto_validation: true
time: 5
tags: [ tutorial>beginner, software-product>sap-hana-cloud, software-product-function>sap-hana-cloud\,-sap-hana-database]
primary_tag: software-product>sap-hana-cloud
---

# Import Data into SAP HANA Cloud, SAP HANA Database
<!-- description --> Learn in this tutorial how to use the SAP HANA database explorer to import the sample data needed for this mission from a tar.gz file.

## Prerequisites
- You have access to [SAP HANA Cloud trial](hana-cloud-mission-trial-2) or [SAP HANA Cloud free tier](hana-cloud-mission-trial-2-ft), or a production environment of SAP HANA Cloud, SAP HANA database
- You have completed the tutorial to [provision an instance of SAP HANA Cloud, SAP HANA database](hana-cloud-mission-trial-3)


## You will learn
- How to import catalog objects from your local machine to your database using the SAP HANA database explorer

## Intro
>
> ![Alex Banner](banner-alex.png)
>
> Reminder: This tutorial is part of a mission, in which you will help Alex, the CEO of Best Run Travel, to answer a concrete business question with SAP HANA Cloud, SAP HANA database.
>
> *Alex needs to know the top 5 partners of their agency and wants to find out the days with maximum booking of each partner.*

---

### Download the sample data set


SAP provides a free data model focused on flight data for anyone to use. We're going to import this sample data and use it to help you complete the mission for Best Run Travel.

Download the [SFLIGHT sample data](https://github.com/SAP/hana-xsa-opensap-hana7/raw/snippets_2.3.2/ex2/sflight_hana.tar.gz) from the public SAP GitHub repository and save it on your local machine.

Note the location of the file.




### Open the SAP HANA database explorer


1.	Under Instances and Subscriptions, open SAP HANA Cloud Central.
    ![HCC application](hcc-app.png)

2.	In the **Actions** column, click on the **three dots** and select the option to **Open in SAP HANA Database Explorer**.

    ![Open the SAP HANA database explorer](open-dbx.png)

3.	SAP HANA database explorer will open in a new tab.





### Import the data to your catalog


1.	In the pane on the left, expand your database and right-click on **Catalog**.

2.	Click on **Import Catalog Objects**.

    ![DBX - import catalog objects](ss-02-dbx-import-catalog-objects.png)

3.	Where it says **Local archive**, click on **Browse** and select the `SFLIGHT` file you previously downloaded to your local machine.

    ![Browse](ss-03-browse.png)

4.	Wait until the archive is uploaded completely. You can see the status of the upload next to the **Browse** button.

    ![DBX uploading archive](ss-04-dbx-uploading-archive.png)

5.	Once the upload is completed, you will see a list of **Catalog Objects**. All of the objects will be automatically selected for import.

    ![DBX catalog objects](ss-05-dbx-catalog-objects.png)

6.	Keep all options as they are and then click on **Import**.

7.	Once the import is completed, you will see a confirmation notification on the top right-hand side of the screen.

    ![DBX import completed successfully](ss-06-dbx-import-completed-successfully.png)

> - Note that you can also import individual `.csv` files in a similar manner. Simply right click on Tables in your catalog and select **Import Data**.
>
>     ![Import CSV1](ss-07-import-CSV1.png)

> - A wizard will guide you through the steps. You can select to add the data to an existing or new table and which schema the import should happen in.
>
>     ![Import CSV2.png](ss-08-import-CSV2.png)

> - You will then be asked to specify the column mappings, as well as how errors should be handled.
>
>     ![Import CSV3.png](ss-09-import-CSV3.png)




### Preview the data


Once the data is imported, you can take a look at it.

Click on **Tables** to view all your newly imported tables. Please note that your data will be automatically stored in a new schema named `SFLIGHT`.

To ensure that you can see your new tables, click on the **Choose Schema** button next to the schema name on the bottom left-hand side panel.

![view tables - change schema](ss-10-view-tables-change-schema.png)

Then select the schema `SFLIGHT`. You can uncheck the default `DBADMIN` schema to see only the `SFLIGHT` tables you just imported.

![Select Schema](ss-11-select-schema.png)

Click **OK** and the list of available tables will be displayed on the same side panel.

![View Tables](ss-12-view-tables.png)

If you want to see the data inside any of your tables, right click on any of them and then click on **Open Data**.

![Open Data](ss-13-open-data.png)

This will open the table on the main panel.

![Data Preview resized](ss-14-data-preview.png)

You can browse the dataset to get a better overview of the data available.

*Well done!*

You have completed the fifth tutorial of this mission! Now you know how to import tar.gz files to your database using the SAP HANA database explorer.

Learn in the next tutorial how to create and manage users and privileges. 


### Knowledge Check






---
