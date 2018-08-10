---
title: SAP HANA XS Advanced, import tables and large datasets
description: Import SHINE sample tables and data using SAP WebIDE for SAP HANA
primary_tag: products>sap-hana
tags: [  tutorial>beginner, topic>big-data, products>sap-hana, products>sap-hana\,-express-edition ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Create an HDI Module](https://www.sap.com/developer/tutorials/xsa-hdi-module.html)


## Next Steps
  - [Create a Dimension Graphical View](https://www.sap.com/developer/tutorials/xsa-graphical-view.html)

## Details
### You will learn  
This tutorial will guide you through the process of downloading sample data and database artifacts and importing it into SAP HANA using SAP Web IDE for SAP HANA.

If you are looking to import the entire SAP HANA `INteractive` Education model for XS Advanced, follow this tutorial instead: [https://www.sap.com/canada/developer/tutorials/xsa-ml-e2e-access-shine.html](https://www.sap.com/canada/developer/tutorials/xsa-ml-e2e-access-shine.html)

### Time to Complete
**5 Min**

---

[ACCORDION-BEGIN [Step 1: ](Download the archive)]

Download the file `data.zip` from our public [GitHub repository](https://github.com/SAP/com.sap.openSAP.hana5.templates/raw/hana2_sps01/ex2/core-db/data.zip).


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Import Archive)]

Import the archive into the data folder. This will probably be `src\data`. Right-click on the folder and choose `Import->From File System`:

![Import from file system](1.png)

**Browse** for the downloaded `data.zip` file, keep the **Extract archive** flag and click on **OK**.

![Import from file system](2.png)

 **Confirm** that files with the same name will be overwritten

![Confirm](3.png)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Purge duplicates and Build)]

Delete the files `FLIGHT.hdbcds`, `header.csv`,  `item.csv` and `load.hdbtabledata` (if available) to avoid conflicts during build:

![Delete](4.png)

**Build** the db module:

![Build](5.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Check the imported entities in the Database Explorer)]

Formerly known as HANA Runtime Tools or `HRTT` in previous versions, the Database Explorer allows you to see the database artifacts you create in the different HDI containers. Use the middle icon on the left sidebar or from the menu, `Tools->Database Explorer`:

![HRTT](6.png)

>In SPS12, the HANA Runtime Tool was accessed from `https://<hostname>:51006` or, for `SAP HANA, express edition`. from `http://<hostname>:51018`

Use the `+` icon to add a database container into the Explorer.

![HRTT](7.png)

Locate your HDI container and add it. You will find it as a concatenation of your user ID and the name of your project.

![HRTT](8.png)

Once added you can see the tables and their content.

![HRTT](9.png)

[ACCORDION-END]


---

## Next Steps
- [Create a Dimension type Graphical View](https://www.sap.com/developer/tutorials/xsa-graphical-view.html)
