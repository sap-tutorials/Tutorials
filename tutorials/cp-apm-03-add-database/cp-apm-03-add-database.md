---
title: Add a Database to Your Business Application
description: Deploy the data model to the SAP HANA database that is associated with your enterprise or trial account.
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [  tutorial>intermediate, topic>java, products>sap-cloud-platform, products>sap-web-ide ]
time: 10
---

## Prerequisites  
 - **Tutorials:** [Add a UI to Your Business Application](https://www.sap.com/developer/tutorials/cp-apm-02-add-ui.html)

## Details
### You will learn  
  - How to deploy the data model you created for your business application to the SAP HANA database using SAP Web IDE Full-Stack

---

[ACCORDION-BEGIN [Step 1: ](Add a database)]

Right-click the **`db`** module and choose **Build**.

![Build the db module](build-db.png)

Wait for the notification that says the build was successful.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Enable SAP HANA Database Explorer)]

To view the generated deployment artifacts, SAP HANA Database Explorer must be enabled in SAP Web IDE. If you have already enabled the SAP HANA Database Explorer, go to step 3.

1. Go to **Tools | Preferences | Features**.
2. Search for the database explorer and enable it.

    ![Enable the database explorer](enable-database-explorer.png)

3. Choose **Save**.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Open the SAP HANA Database Explorer)]

1. Go to **Tools | Database Explorer**.
2. Choose **Connect**.

    ![Connect to the database](connect-database.png)

3. Select the bookshop database.

    ![Add the database](add-database.png)

Once connected you can view the different database artifacts. For example, the books table that you have created.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Fill in initial data)]

1. Download the following files:

    [`Authors.csv`] (https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cp-apm-03-add-database/csv/Authors.csv)

    [`Books.csv`] (https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cp-apm-03-add-database/csv/Books.csv)

    [`Orders.csv`] (https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cp-apm-03-add-database/csv/Orders.csv)

    [`Data.hdbtabledata`] (https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/cp-apm-03-add-database/csv/Data.hdbtabledata)
    
2. In your workspace, go to `bookshop/db/src/`, right-click on the **`src`** folder and choose **Import | File or Project**.
3. Browse to the location where you have saved the `Authors.csv` file.
4. In the **Import to** field, add **`/csv`** to the default location and choose **OK**.

    >This creates a **`/csv`** folder under `/bookshop/db/src` and adds the `Authors.csv` file to that location.

5. Repeat steps 2 and 3 to add the remaining 3 files to **`/csv`** folder.  

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Re-build the db module and run the app)]

1. Right-click the **`db`** module and choose **build**.
2. Right-click the **app** module and choose **Run | Run Configurations**.
3. Choose **`Run flpSandbox.html`** and deselect **Run with mock data**.
4. Choose **Save and Run**.
5. Open your app from the SAP Fiori Launchpad and choose **Go**.

You now have actual data served automatically.

[DONE]

[ACCORDION-END]

---
