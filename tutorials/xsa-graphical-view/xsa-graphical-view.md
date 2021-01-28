---
title: Create a Dimension Graphical Calculation View (XS Advanced)
description: Create a graphical calculation view with a dimension data type.
primary_tag: products>sap-hana
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
auto_validation: true
tags: [  tutorial>intermediate, topic>sql, products>sap-hana, products>sap-hana\,-express-edition   ]
time: 15
---
## Prerequisites  
- This tutorial is designed for SAP HANA on premise and SAP HANA, express edition. It is not designed for SAP HANA Cloud.
- **Tutorials:** [Import SHINE tables and data](https://developers.sap.com/tutorials/xsa-import-shine-data.html)

## Details
### You will learn  
- How to create a calculation view with a Dimension data type
- How to perform the basic modelling operations, such as projections and joins
- This tutorial is also available [as a video](https://www.youtube.com/watch?v=HEiyR7clkrQ)

---

[ACCORDION-BEGIN [Step 1: ](Create a new folder with a namespace)]

Create a new file called `models/.hdinamespace` under `/db/src`.

![Create namespace](hdi.png)

> Although there is no need to use namespaces in general, calculation views are consumed from external reporting tools best with namespaces

Use the following code in the file

```JSON
{
    "name":    "db.models",
    "subfolder": "ignore"
}

```

**Save** and close the file.

If you do not see the file, go to `View-> Show hidden files`.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a calculation view)]

Right-click on the models folder and create a new calculation view

![New CV](1.png)

Call it `PRODUCTS` and choose type `DIMENSION` and click **Create**

![Call it Products](2.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Join Products and Business Partners)]

Click the `Create join` button and drop the node at the bottom of the design window

![New Join node](join1.gif)

Right-click on the node and call it `Product_BP`

![New Join node](rename.png)

Press **enter** and select the node again, choose the **+** sign to add data sources

![Call it Products](4.png)

Add `MD.Products` and `MD.BusinessPartners` to the node. Use the **+** sign to search for the entities.

![Add data sources](a1.gif)

Double-click on the name of the node to open the join definition. Drag and drop `SUPPLIER.PARTNERID` to `PARTNERID` to connect the two tables.

![Add data sources](a2.gif)

Click on the mapping tab and select `PRODUCTID`, `TYPECODE`, `CATEGORY`, `NAMEID`, `CURRENCY`, `PRICE`,  `DESCID`, `PARTNERID`, `COMPANYNAME` and `ADDRESES.ADDRESSID` and then choose Add To Output.

![Call it Products](6.png)

Change the name of the column `Category` to `ProductCategory`

![Call it Products](7.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Join products, business partners and addresses)]

Create a new join node, and drag the existing node as input

![Add data sources](a3.gif)

Use the **+** sign to add `MD.Addresses` to this join node.

![Add addresses](8.png)

Join the two tables by `ADDRESSES_ADDRESSID` and `ADDRESSID`.

![Add addresses](9.png)

Go to the mapping tab to add columns to the output. Select all columns from the `Product_BP` node except `ADDRESSES_ADDRESSID`. From `MD.Addresses` table select  `CITY`, `POSTALCODE`, `STREET`, `BUILDING`, `COUNTRY` and `REGION`.

> You can double-click on `Product_BP` to add all of the columns to the output and then right-click on `ADDRESSES_ADDRESSID` to remove that output column

![Add addresses](10.png)

Rename the current join node to `Address`

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Join products with texts)]

Add a new join node and use the `Address` node as input. Add `Util.Texts` as a data source.

![Add texts](11.png)

Join `NAMEID` to `TEXTID`. Set the join to `Text join`  and the language column to `LANGUAGE`.

![Add texts](12.png)

In the mapping tab, select all columns from the Address node except `NAMEID` and add them to the output. From the `Util.Texts` table select `TEXT`.
Change the name of the `TEXT` column in the output to `ProductName`.

![Add texts](13.png)

Rename the join node to `ProductName`.

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Join products, business partners and addresses)]

Add a new join node and use `ProductName` as an input. Add `Util.Texts` again as a source.

![Add texts](11.png)

Use a text join between `DESCID` and `TEXTID`.

![Add texts](14.png)

Repeat the process of adding columns to the output via the mapping tab. Select all columns from the `ProductName` node except `DESCID`. From the `Util.Texts` table select `TEXT` but change the name of  to `ProductDesc`.

![Add texts](15.png)

Rename the join node to `ProductDesc`.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Finalize the projection)]

Connect the node `ProductDesc` to the projection node. Use the `AutoLayout` feature to align the nodes and the `Auto Map by name` button to create the output.  Save the Calculation View.

![Final view](final.png)

**Build** the view then right-click on it and choose `Data Preview`.

![Final view](17.png)

The output should be similar to the following. Copy the SQL statement produced by the `SQL` button into the validation below to complete it

![Final view](sql.png)

[VALIDATE_1]

[ACCORDION-END]


---
