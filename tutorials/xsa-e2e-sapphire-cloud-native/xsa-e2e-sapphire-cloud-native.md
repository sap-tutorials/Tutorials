---
title: SAPPHIRE - Create a cloud-native application in SAP HANA
description: Combine SAP HANA and Fiori to create a cloud-native application using a micro-services approach.
primary_tag: topic>cloud
tags: [  tutorial>beginner, topic>big-data, topic>cloud, topic>html5, topic>odata, topic>sapui5, products>sap-hana ]
---

## Prerequisites  
 - **Proficiency:** Beginner

## Details
### You will learn  
You will learn how to create a cloud-native applications involving SAP HANA, Node.js and the Fiori master-detail template using SAP Web IDE.

**This tutorial can only be completed at SAPPHIRE**

### Time to Complete
**20 Min**

---

[ACCORDION-BEGIN [Step 1: ](Connect to SAP Web IDE for SAP HANA)]

Have you just created your own Virtual Machine on Google Cloud Platform?

If **yes**, go to this URL `https://hxehost:53075`

If **not**, click on the **SAP HANA** button in the favorite bar in Google Chrome. ![Button](button.png)

If a warning about the certificate appears, choose **Advanced > Continue to `hxehost`** to continue to the site.

![Incognito Chrome](chrome2.png)

Log in with user `XSA_DEV` and `HXEHana2`

![Incognito Chrome](password.png)

> You are now logged in to SAP Web IDE for SAP HANA, a tool to develop applications. You will be building a multi-target application. This is a collection of micro-services combined in the same development lifecycle to create a business application.

[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Create a project)]

This is the SAP Web IDE for SAP HANA. It is the development tool for native SAP HANA development.
If you see any existing folders, use the right-click menu to delete them.

![Delete any existing projects](3.png)

**Right-click** on Workspace and choose **New > Project from template**.

![Create a new project](4.png)

Click **Next**.

![Create a new project](5.png)

Call your project `APP` and click **Next**.

![Create a new project](6.png)

Click **Finish** to create your project.

![Create a new project](7.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a database module)]

**Right-click** `APP` and choose **New > SAP HANA Database Module**.

![Create a new DB module](8.png)

Call the module `db` and click **Next**.

![Create a new DB module](9.png)

Click the checkbox to **Build module after creation**

![Create a new DB module](db.png)

Right-click `src` and click **Import > File or Project**.

![Import data](import.png)

User **browse** to navigate to the folder `Desktop/1- HANA Data Import` and choose the file `src (native).zip`.

![Import data](11.png)

Delete the name of the file and click **OK**. When prompted, **confirm** the import.

![Import data](12.png)

**Build** the database module.

![Import data](13.png)

Congratulations! You have just created two tables and loaded data into them.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Expose data for consumption)]

You will now create a Node.js module to expose the data in the tables using an OData service.
**Right-click** `APP` and select **New > Node.js Module**.

![Create Node.js module](14.png)

Call it `js ` and click **Next**.

![Create Node.js module](15.png)

Tick the box next to `Enable XSJS support`, and click **Finish**

![Create Node.js module](16.png)

Expand the `js` folder and **right-click** the `lib` folder. Create a new file.

![Create Node.js module](17.png)

Call it `service.xsodata`

![Create Node.js module](18.png)

**Paste** the following contents into the file

```javascript

service {
	"APP.db::SO.Header" as "Header"
	navigates ("Items" as "Item");

	"APP.db::SO.Item" as "Item";
	association "Items" principal "Header"("SALESORDERID")
	multiplicity "1" dependent  "Item"("SALESORDERID") multiplicity "*";

}
```
**Save**  the file.

Open the `mta.yaml` file, click on the `js` module and use the **`+`** sign to add the `hdi_db` and `db` dependencies under **Requires**.

![Create Node.js module](yaml.png)

**Save** the `mta.yaml` file.

**Save** and **Run** the `js` module.

![Create Node.js module](21.png)

This will build and redeploy the entire application because the `mta.yaml` file has been modified. This file describes the dependencies between the different modules (micro-services) and the order in which they are deployed.

Once the application is **Running**, you can proceed to the last step.

![Create Node.js module](running.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Use the Fiori master-detail wizard to visualize your app)]

You will now create the web module using the SAP Fiori master-detail wizard.

Right-click `APP` and choose **New > SAP Fiori Master-Detail Module**.

![Create UI5 module](23.png)

Call it `web`, and click **Next**.

![Create UI5 module](24.png)

Choose the available service and click **Next**.

![Create UI5 module](25.png)

Map the fields for the header of the sales order in the wizard.

![Create UI5 module](26.png)

**Scroll down** to map the fields for the items in the sales orders.

![Create UI5 module](27.png)

**Right-click** on the web module, and select **Run as > Web Application**.

![Create UI5 module](run.png)

**Scroll down to select** the file `index.html`.

![Create UI5 module](28.png)


> ### **Congratulations!**
>You have created an instance of an SAP HANA service, a Node.js and a web micro-services and glued them together to produce a multi-target application. This can run both on premise or in the cloud.
>&nbsp;


[ACCORDION-END]

---
