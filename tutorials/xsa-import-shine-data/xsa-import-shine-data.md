---
title: Import Tables and Large Datasets (XS Advanced)
description: Import SHINE sample tables and data using SAP Web IDE for SAP HANA.
primary_tag: products>sap-hana
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
auto_validation: true
tags: [  tutorial>beginner, topic>big-data, products>sap-hana, products>sap-hana\,-express-edition ]
time: 5
---

## Prerequisites  
 - This tutorial is designed for SAP HANA on premise and SAP HANA, express edition. It is not designed for SAP HANA Cloud.
 - **Tutorials:** [Create an HDI Module](https://developers.sap.com/tutorials/xsa-hdi-module.html)

## Details
### You will learn  
This tutorial will guide you through the process of downloading sample data and database artifacts and importing it into SAP HANA using SAP Web IDE for SAP HANA.

If you are looking to import the entire SAP HANA `INteractive` Education model for XS Advanced, follow this tutorial instead: [https://developers.sap.com/tutorials/xsa-ml-e2e-access-shine.html](xsa-ml-e2e-access-shine)

---

[ACCORDION-BEGIN [Step 1: ](Download the archive)]

Download the file `data.zip` from our public [GitHub repository](https://github.com/SAP-samples/hana-xsa-opensap-hana7/raw/snippets_2.4.0/ex2/core-db/data.zip).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Import Archive)]

Import the archive into the data folder. This will probably be `src\data`. Right-click on the folder and choose `Import->From File System`:

![Import from file system](1.png)

**Browse** for the downloaded `data.zip` file, keep the **Extract archive** flag and click on **OK**.

![Import from file system](2.png)

 **Confirm** that files with the same name will be overwritten

![Confirm](3.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Purge duplicates and Build)]

Delete the files  `header.csv`,  `item.csv` and `load.hdbtabledata` (if available) to avoid conflicts during build:

![Delete](4.png)

Also delete `PurchaseOrder.hdbtable` and `PurchaseOrderItem.hdbtable` (if available).
![Delete](4-1.png)

Also delete the `SFlight` folder (if available).
![Delete](4-2.png)

**Build** the db module:

![Build](5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Check the imported entities in the Database Explorer)]

The Database Explorer allows you to see the database artifacts you create in the different HDI containers.

Right-click on the database module and choose **Open HDI Container**

![Check tables](10.png)

> Note: If you do not find this option, use the middle icon on the left sidebar or from the menu, `Tools-Database Explorer`:
> Use the `+` icon to add a database container into the Explorer.
> ![HRTT](7.png)
> Locate your HDI container and add it. You will find it as a concatenation of your user ID and the name of your project.
> ![HRTT](8.png)

You can now navigate the tables and check their content. Click on **Open Data** for table `TCURC`

![HRTT](12.png)

[VALIDATE_1]
[ACCORDION-END]


---
