---
title: Create a Node.js module to expose OData services
description: Create a Node.js module to implement backend logics and expose OData services
auto_validation: true
primary_tag: products>sap-hana
tags: [  tutorial>beginner, topic>odata, products>sap-hana ]
---

## Prerequisites
 - This tutorial is designed for SAP HANA on premise and SAP HANA, express edition. It is not designed for SAP HANA Cloud.
 - **Proficiency:** Beginner

## Details
### You will learn  
Create a Node.js module to expose data in an OData service.

### Time to Complete
**10 Min**

---

[ACCORDION-BEGIN [Step 1: ](Create a Node.js module)]

Create a Node.js module. Right-click on your project and select `New->Node.js Module`:

![Create a Node.js module](1.png)

Call it `js`:

![Create a Node.js module](2.png)

Add a description, check the `XSJS support` box and click on **Finish**

![Create a Node.js module](3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create an OData file)]

You will now create an OData file to expose the contents of the artifacts you defined in your CDS module. This is not really Node.js development but the utilization of the runtime environment to expose OData. You can see real examples of Node.js development in the SHINE model later.

Create a file called `xsodata/PO.xsodata` under `js->lib`

![Create a Node.js module](odata.png)

Open the file called `PO.xsodata` and add the following content:

```sql
service {
	"PO.PO_VIEW" as "POHeader"
	keys ("PURCHASEORDERID")
	navigates ("Items" as "POItem");

	"PO.ITEM" as "POItem";
	association "Items" principal "POHeader"("PURCHASEORDERID")
	multiplicity "1" dependent  "POItem"("PURCHASEORDERID") multiplicity "*";

}
```

>Note: You can disregard the warning in the OData definition

![Build js module](error.png)

**Save and Build** the `js` module.

![Build js module](10.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add the dependency between the modules)]

You have a database module and now you also have a Node.js module. These modules could technically be executed separately, even deployed separately. However, the Node.js module needs data from the database module. You need to add this dependency in the file that keeps them all together, the `mta.yaml`.

Double-click on the file to open it:

![edit yaml file](11.png)

Make sure you are on the **MTA Editor** and on the `js` module:

![edit yaml file](12.png)

Add both the HDI container and database modules to the `Requires` section of the `js` modules.

![Edit yaml](7.png)

**Save** the MTA file.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Test the service)]

**Run** the `js` module by right-clicking on the folder and selecting **Run as Node.js application**

![Run the js module](13.png)

>Note: This may take a couple of minutes as the module will be built first.

Click on the URL to open a new tab:

![Run the js module](8.png)

Edit the URL to access the OData service, replace `index.xsjs` with `/xsodata/PO.xsodata`:

![Run the js module](9.png)

Add `?$format=json` to the end of the URL. Use the results to answer the question below.

[VALIDATE_1]
[ACCORDION-END]
