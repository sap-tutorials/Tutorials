---
title: Generating Alerts Using a Derived Window in SAP HANA Smart Data Streaming
description: Part 6 of 9. Create and use Derived Window to analyze events, filter data, and generate alerts.
auto_validation: true
primary_tag: products>sap-hana-streaming-analytics
tags: [ tutorial>beginner, products>sap-hana-streaming-analytics, products>sap-hana-studio ]
author_name: Bill Jiang
author_profile: https://github.com/BillJiangSAP
time: 15
---
## Details
### You will learn  
 - Using a Derived Window to analyze events and filter data.
 - Using reference data from a HANA table for detecting alert conditions.

---

[ACCORDION-BEGIN [Step 1: ](Add a Derived Window)]    

1. Click **Derived Window** in the **Palette** and drop it onto the canvas.

    ![1-dropderivedwindow](1-dropderivedwindow.png)

2. Add a connector from **`AVG_TEMP`** to the new window.

    ![add connector](2-addconnector.png)

3. Change the new window name to `ALARM_TEMP` and press **Enter** when done.

    ![rename to alarm temp](3-renametoalarmtemp.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Edit Column Expressions)]  

4. Click Add Column Expression **f(x)** icon shown below.

    ![add column expression](4-addcolumnexp.png)

5. Click the **Copy Columns from Input** menu item to execute it. You can also press **c**.

    ![copy columns from input](5-copycolumnsfrominput.png)

6. Select all except for **`MIN_TEMP`** and **`TEMP_UNIT`**, then click **OK**.

    ![select columns](6-selectcolumns.png)

7. Click Add Column Expression **f(x)** icon then **Column Expression** menu item to add a column to this window. Then repeat (i.e. add 2 new columns total). You can also press **c**.

    ![column expression](7-columnexpression.png)

8. Rename the first new column to `ALARM_TYPE` and press **Enter** key when done.

    ![rename expression](8-renameexpression.png)

9. Rename the 2nd new column to `ALARM_DESC`.

    ![name expression](9-nameexpression.png)

10. Double-click on **`simpleResultExpression6`** to edit the expression.

    ![edit expression](10-editexpression.png)

11. Enter `'TEMPERATURE'` in the expression edit box for the **`ALARM_TYPE`** column. This will set the "type" of all alarms emitted by this window to the string "TEMPERATURE". Press **Enter** key to confirm entry.

    ![rename expression](11-renameexpression.png)

12. Enter `'Machine not maintaining temperature'` in the expression box for the **`ALARM_DESC`** column. Press **Enter** key to confirm entry.

    ![rename second expression](12-renamesecondexp.png)

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add a Filter)]  

13. Now click **Add Query Clause** to add a query clause. We want to add a filter such that this window only contains rows for machines that have a current average temperature above the max specified for the machine.

    ![add query](13-addquery.png)

14. Click the **Filter** menu item to execute it. You can also press **f**.

    ![add filter](14-addfilter.png)

15. Double-click on **1** under the **Other** tab to edit the filter expression.

    ![change filter expression](15-changefilterexp.png)

16. Change the filter expression to: `AVG_TEMP.AVG_TEMP > AVG_TEMP.MAX_TEMP`. Use **Ctrl+Space** for completion assist.

    ![filter expression](16-filterexp.png)

17. Click Compile Project icon to check for errors.

    ![compile for error](17-compileforerror.png)

[DONE]

[ACCORDION-END]
