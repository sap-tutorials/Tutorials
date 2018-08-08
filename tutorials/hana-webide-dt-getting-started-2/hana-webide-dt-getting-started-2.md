---
title: Create Extended Storage
description: Create extended storage for dynamic tiering.
auto_validation: true
primary_tag: products>sap-hana-dynamic-tiering
tags: [  tutorial>beginner, products>sap-hana, products>sap-hana-dynamic-tiering, products>sap-web-ide ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **System** Access to SAP Web IDE for the HANA System you are working with.
 - **Tutorials:** [Create a New User and Assign Permissions](https://www.sap.com/developer/tutorials/hana-webide-dt-getting-started-1.html)

## Next Steps
 - **Tutorials:** [Create Tables and Import Data](https://www.sap.com/developer/tutorials/hana-webide-dt-getting-started-3.html)

## Details
### You will learn  
* How to connect to a SAP HANA database.
* How to verify the status of SAP HANA dynamic tiering.
* How to create extended storage for the dynamic tiering host.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Connect to a SAP HANA database)]
Open and login to Web IDE. You should be greeted to a blank-looking screen. Click on the Database Explorer tab on the left, and click the "+" button in the new panel.

![2.1](assets/hana-webide-dt-getting-started-cfd2bfa5.png)

In the new window, select **SAP HANA Database (Multitenant)** as the database type, enter the **host name** and **instance number** and login as the **TPCH** user we created. Check the **save user and password** so you won't have to re-enter your credentials when re-connecting to the system. Click OK to add the database.

![2.2](assets/hana-webide-dt-getting-started-2d4e714e.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Verify extended storage status)]
To verify extended storage status, we will be using SAP HANA cockpit. Login to SAP HANA cockpit, click the *Resources* link at the top, and select the appropriate resource, which should be the tenant database in the HANA System you are working with.

![2.3](assets/hana-webide-dt-getting-started-df948257.png)

In the *Overall Database Status* tile (at the top), click on the area indicating the running services.

![2.4](assets/hana-webide-dt-getting-started-1da7c1f3.png)

A *Manage Services* window should pop up. Scroll down to the `esserver` service to see its status. If what you see is:

* Empty and/or the "SAP HANA dynamic tiering" line does not exist, it probably means that dynamic tiering is not installed. Please install and register dynamic tiering before proceeding.
* Status of "Errors" with a red circle. It might mean that dynamic tiering is not running. Please restart the dynamic tiering process before proceeding.
* Status of "Installed but not running yet" with a yellow triangle, then proceed to Create Extended Storage section below.
* Status of "Running" with a green box, then you are all set and can skip the rest of this tutorial section by scrolling to the bottom of this lesson and clicking the link to part 3.

![2.5](assets/hana-webide-dt-getting-started-1a10eab9.png)

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create extended storage)]
In Web IDE, right click on the database you are working with, and select *Open SQL Console* or use the shortcut *Ctrl + Alt + C*.

![2.6](assets/hana-webide-dt-getting-started-3d2ecb5c.png)

In the SQL console, paste the following script:
```SQL
CREATE EXTENDED STORAGE AT '<your.machine.name>' size 1000 MB;
```
Replace "<your.machine.name>" with the host name of the machine where the dynamic tiering component is installed.

> Note: This script will create 1 GB of `dbspace`. This is small for DT (warm store) which can scale up to handle 100 TB of data, but 1 GB is sufficient for tutorial purposes.

![2.7](assets/hana-webide-dt-getting-started-ed433cc3.png)

Execute the script using the green play button. It will create the extended storage `dbspace` for SAP HANA dynamic tiering. Repeat step 2 to verify that extended storage is setup successfully.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Additional information)]
The "CREATE EXTENDED STORAGE" statement creates the warm store and the required `dbspaces`. A `dbspace` is a logical name for a container of files used to store the dynamic tiering table data and related objects. `Dbspaces` are specialized to manage specific types of data. Types of `dbspaces` used by dynamic tiering include `ES_SYSTEM`, `ES_USER`, `ES_DELTA` etc. Creating the extended storage `dbspace` is a prerequisite to creating extended tables.

[DONE]

[ACCORDION-END]

## Next Steps
 - **Tutorials:** [Create Tables and Import Data](https://www.sap.com/developer/tutorials/hana-webide-dt-getting-started-3.html)

---
