---
title: Working with the SAP Vora Tools
description: SAP Vora 1.4: Vora Tools provide a data browser for viewing and exporting data in tables and views, an SQL editor for creating and running SQL scripts, and a modeler for creating data models
primary_tag: products>sap-vora
tags: [  tutorial>beginner, topic>big-data, topic>sql, products>sap-vora ]
---

## Prerequisites  
 - [Working with Document Store Engine in SAP Vora](http://www.sap.com/developer/tutorials/vora-ova-zeppelin7.html)


## Next Steps
 - [Working with SAP HANA data source](https://www.sap.com/developer/tutorials/vora-ova-hana-datasource.html)

## Details
### You will learn  
You will learn how to use Data Browser and Modeler from Vora Tools.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](SAP Vora Tools)]
The SAP Vora 1.4 Tools provide a data browser for viewing and exporting data in tables and views, an SQL editor for creating and running SQL scripts, and a modeler for creating data models.

SAP Vora makes available OLAP-style capabilities for data on Hadoop, in particular, a hierarchy implementation that allows you to define hierarchical data structures and perform complex computations on different levels of data.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Accessing the Tools)]
You can access the SAP Vora tools via web browser on the port 9225 of the Vora host: `http://<IP_ADDRESS>:9225`. In your SAP Vora, developer edition, you can use user `vora`.

The initial screen appears as follows.
![Logon to Tools](vtools_01.jpg)

You will be presented with the initial landing page once logged in successfully.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Data Browser)]
Continue by selecting the **Data Browser**. This node will give a list of all object created in the platform (from previous Zeppelin tutorials) you can also preview data.
![Data Browser](vtools_02.jpg)

Expand the tree views of Vora In-Memory Engine and Vora Disk Engine.
![Expand](vtools_03.jpg)

Continue by registering the tables: `PRODUCTS` in the In-Memory Engine and `COMPLAINTS_DISK` in the Disk Engine. You will be using the Vora Modeling tool to create a model, querying data across two engines.
![Registration](vtools_04.jpg)

Table registration will take a few moments.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Modeler)]
Select the **Modeler** button once both tables are registered. This is where you will be doing the bulk of the work now.
![Modeler](vtools_05.jpg)

Expand the `Vora In-Memory Engine` and `Vora Disk Engine` tree view once the Modeler tool is loaded.
![Trees](vtools_06.jpg)

Create a new SQL View called `V_CUSTOMER_ISSUES` by clicking on the **`Add`** button.
![Add](vtools_07.jpg)

Select View Type `SQL` and name it `V_CUSTOMER_ISSUES`.
![View create](vtools_08.jpg)

Once the view is created, the next step is to add Data Sources (two tables from previous steps). Click on the **Add Data Source** button. You can start typing the table name or supply a `*` to list all known Data Sources as shown below.
![Add first data source](vtools_09.jpg)

Repeat to add second table.
![Add first data source](vtools_10.jpg)

Continue by selecting desired columns as output by clicking on the highlighted radio buttons next to each required column. Note that the selected output columns will appear in the right hand pane **Columns**.
![Select Columns](vtools_11.jpg)

Join the two tables on the common column `COMPLAINT_ID`. First use your mouse pointer to select the `COMPLAINT_ID` column in the `PRODUCTS` table. Hold the mouse button down while dragging it to the `COMPLAINTS_DISK` table and drop it on the `COMPLAINT_ID` column.
![Join tables](vtools_12.jpg)

The **Join Definition** window appears. Change the type to `Left Outer` and select **OK** when done.
![Left Outer](vtools_13.jpg)

Save the view. Now preview what you have done so far by clicking on the **Preview** button as demonstrated below. Note the join linking the two tables.
![Join preview](vtools_14.jpg)

Successful Data Preview.
![Data preview](vtools_15.jpg)

Add a 'where' condition to filter for issues logged via the `Web` by using the right hand pane. Look for **Where** and click the **+** button. The **Where Condition** window appears. Enter condition by selecting options or by typing the condition in the **Expression** box.
```sql
COMPLAINTS_DISK.SUBMITTED_VIA = 'Web'
```
![Condition](vtools_16.jpg)

Save your changes and do a **Data Preview**.
![Data Preview with condition](vtools_17.jpg)

Modify the existing view to count issues logged by `PRODUCT`. Start by selecting only `PRODUCT` as the output column. You can deselect unwanted columns by clicking on them.
![Deselecting columns](vtools_18.jpg)

Add a calculated column `P_COUNT` to count `COMPLAINT_ID`.
```sql
count( PRODUCTS.COMPLAINT_ID )
```
![Calculated column](vtools_19a.jpg)

Add `PRODUCT` to the **Group By** clause.
![Group by column](vtools_19.jpg)

Save changes and do a **Data Preview** again.
![Data Preview with calculated column](vtools_20.jpg)

The modelled view is now ready for reporting. Click on the **Data Browser** in Vora Tools. You will notice the newly created object under **Vora In-Memory Engine**.
![View in Data Browser](vtools_21.jpg)

[DONE]
[ACCORDION-END]


## Next Steps
- [Working with SAP HANA data source](https://www.sap.com/developer/tutorials/vora-ova-hana-datasource.html)
