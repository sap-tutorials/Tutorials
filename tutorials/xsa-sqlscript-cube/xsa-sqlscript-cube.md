---
title: Creating a Calculation View with a Cube data type and Star Join
description: Leveraging SQLScript in Stored Procedures & User Defined Functions
primary_tag: products>sap-hana
tags: [  tutorial>intermediate, topic>sql, products>sap-hana, products>sap-hana\,-express-edition   ]
---
## Prerequisites  
- **Proficiency:** Intermediate
- **Tutorials:** [Creating a Graphical Calculation View](http://www.sap.com/developer/tutorials/xsa-graphical-view.html)

## Next Steps
- [Creating Stored Procedures](http://www.sap.com/developer/tutorials/xsa-sqlscript-stored-proc.html)

## Details
### You will learn  
Now to create a calculation view using cube data and a star join
**Please note - This tutorial is based on SPS11**

### Time to Complete
**15 Min**.

---


[ACCORDION-BEGIN [Step 1: ](Create a new Calculation View)]

Now you will create another Calculation view &#151; but one which uses the Cube data type and therefore aggregation of the results. It will combine purchase order table data with the product view we created in the previous step.  Because we have various currency based amounts in our source data, we will also create a calculated column which contains a currency conversion.Using the same steps as the previous part of the exercise; in your models folder, create a new Calculation View
![New Calculation View](1.png)
Name your new view `PURCHASE_ORDERS`. Set the Data Category to CUBE. This will create a view very similar in capabilities to the older Analytical View.

![New Calculation View](2.png)
[DONE][ACCORDION-END][ACCORDION-BEGIN [Step 2: ](Insert join)]Insert a Join Node into the Scenario. Add the `PO.Header` and `PO.Item` tables to this Join Node.
![New Calculation View](3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create inner join)]

Create an inner join between `PURCHASEORDERID` from the header table to the `PURCHASEORDERID` column of the item table

![New Calculation View](4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add columns)]

Using the Mapping tab, add the columns `HISTORY.CREATEDAT` from the Header table and the `PURCHASEORDERID`, `PURCHASEORDERITEM`, `PRODUCT.PRODUCTID`, `CURRENCY`, and `GROSSAMOUNT` columns from the Item table

![New Calculation View](5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Connect join nodes)]

Create another Join node and connect your first node to this new one. Press the plus button next to this node. Search for and select the PRODUCTS view from the previous part of the exercise to insert it into the Join of your new view.  

![New Calculation View](6.png)

Create a join on the `PRODUCT_PRODUCTID` to the `PRODUCTID` column.

![New Calculation View](7.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create input parameter)]

From the Parameters tab, create an input parameter named `IP_O_TARGET_CURRENCY`.

![New Calculation View](8.png)

Configure as shown with type `NVARCHAR` length 3 with a Semantic type of Currency. It should be mandatory and have a default value of USD.

![New Calculation View](9.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create new calculation view)]

From the Mapping tab, select all the columns of the `Join_1` node and all the columns from the PRODUCTS node and add them to the output

![New Calculation View](10.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Create a new Calculated Column )]

In the Calculated `Columns/Counters` tab, create a new Calculated Column for the common currency gross amount.

![New Calculation View](11.png)

Create a Calculated Column named `ConvGrossAmount` which is a summarized measure for the base column Gross Amount. This involves writing an Expression that sets the base value of `GROSSAMOUNT`. Configure as shown here.

![New Calculation View](12.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Connect the `Join_2` node to the Aggregation node)]

Connect the `Join_2` node to the Aggregation node and use the Auto Map by Name button

![New Calculation View](13.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Set measures and attributes)]

Return to the Semantics Scenario and then the Columns tab. Set the `GrossAmount` as measures and all of others as attributes. (They may already be defaulting into the correct values, but please double check).

![New Calculation View](14.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Edit privileges)]

In the View Properties tab, change the Apply Privileges to the blank value.

![New Calculation View](15.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Save and build)]

Save your View.

![New Calculation View](16.png)

Build the `hdb` module and then return to the HRTT tool. Your container will now have an entry in the Column Views folder for this new Calculation View.

![New Calculation View](17.png)

[DONE]
[ACCORDION-END]



## Next Steps
-  - [Creating Stored Procedures](http://www.sap.com/developer/tutorials/xsa-sqlscript-stored-proc.html)
