---
title: SAP HANA XS Advanced, Create a graphical calculation view with a Star join
description: Creating a Calculation View with a Cube data type, a Star Join and a currency conversion
primary_tag: products>sap-hana
tags: [  tutorial>intermediate, products>sap-hana, products>sap-hana\,-express-edition   ]
---
## Prerequisites  
- **Proficiency:** Intermediate
- **Tutorials:** [Creating a Graphical Calculation View](http://www.sap.com/developer/tutorials/xsa-graphical-view.html)

## Next Steps
- [Creating Stored Procedures](http://www.sap.com/developer/tutorials/xsa-sqlscript-stored-proc.html)

## Details
### You will learn  
Now to create a calculation view using cube data and a star join.

### Time to Complete
**15 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Create a new Calculation View)]

You will now create a calculation view of type cube. This type of view allows for the aggregation of data and will use the dimension calculation view you created in the previous step. Create a new calculation view in your `models` folder.![new calc view](1.png)Add a name and set the `Star join` flag to true.![Calculation view star join](2.png)This calculation view would have been an Analytical view in previous versions of SAP HANA.[DONE][ACCORDION-END][ACCORDION-BEGIN [Step 2: ](Insert join for Purchase Order items and header)]Insert a Join Node into the Scenario. Add the `PO.Header` and `PO.Item` tables to this Join Node.
![Add PO header and items](3.png)


Create an inner join between `PURCHASEORDERID` from the header table to the `PURCHASEORDERID` column of the item table

![Create Join](4.png)


Using the Mapping tab, add the columns `HISTORY.CHANGEDAT` from the Header table and the `PURCHASEORDERID`, `PURCHASEORDERITEM`, `PRODUCT.PRODUCTID`, `CURRENCY`, and `GROSSAMOUNT` columns from the Item table. You can select all the columns first and then press the **Add to Output** button instead of dragging individual fields.

![New Calculation View](5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a new join node to integrate the Dimension Calculation view )]

Before you can insert the dimension view, you need to add the `PRODUCTID` field to it. You will also remove the `CURRENCY` filter so you can create a conversion later. Open the Dimension calculation view and add the field `PRODUCTID` from the base node into the list of output columns in the mapping tab. Right-click on the new field and select **Propagate to Semantics**

![Create join node](6_1.png)

Go into the `Filter expression` tab and erase the SQL filter condition. **Build** the view by right-clicking on it and return to the cube view.

Create another Join node and connect your first node to this new one. Press the **+** button next to this node. Search for and select the `CD_PRODUCT` view from the previous video tutorial.

![Create join node](6.png)

Create a join on the `PRODUCT_PRODUCTID` to the `PRODUCTID` column.

![Join Product ID fields](7.png)

Add all the columns to the output.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create an input parameter)]

From the Parameters tab, create an input parameter named `IP_O_TARGET_CURRENCY`.

![Create Inpur Parameter](8.png)

Configure as shown with type `NVARCHAR` length 3 with a Semantic type of Currency. It should be mandatory and have a default value of USD.

![Config Input parameters](9.png)

>Note: You can click on the names of the join nodes to change them to something more meaningful.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Map the Star Join node)]

**Drag** the latest join with the Dimension Calculation view and PO data into the `star join`.

From the Mapping tab, select all the columns of the star join and add them to the output.

![New Calculation View](10.png)

**Add** the `GROSSAMOUNT` column again and rename it to `OriginalGrossAmount`

![Add grossamount field twice](10_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Configure currency conversion)]

In the Semantics node, in the **Columns** tab, select the `GROSSAMOUNT` measure and click on `Assign Semantics`.

![Assign semantics](11.png)

Choose `Amount with currency code` and click **OK**.

![Assign semantics](12.png)

Perform the following configurations as in the screenshot below:

- Choose `Column` as the display currency and assign the proper column.
- Mark the **Conversion** flag and the configuration tables will fill themselves with the standard configuration tables.
- Fix the client to `001`. You can check the values of the SHINE data model in the `Database explorer`
- Set the source currency to the currency from the products
- Set the target currency to the input parameter
- Set the exchange type to the desired exchange type (1001 in this example)
- Set the conversion date to `HISTORY_CHANGEDAT`
- Set the behavior upon failure

![Configure conversion](13.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Build and run)]

Check the **Semantics** tab and leave the field `Apply privileges` blank.

![APPLY PRIVILEGES BLANK](16.png)

**Build** the view. Right-click on the view on the left-side menu and click on **Data preview**.

![Data preview](14.png)

Fill in the **From** parameter for conversion with `USD` and click on **Open Content** (the green `play` button).

See how the conversion has been performed for those currencies in table TCURR, where the conversion type is the one you selected (1001 in this example)

![Data preview raw](15.png)

[DONE]
[ACCORDION-END]




## Next Steps
-  - [Creating Stored Procedures](http://www.sap.com/developer/tutorials/xsa-sqlscript-stored-proc.html)
