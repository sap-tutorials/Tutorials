---
title: Filter and Output to SAP HANA Table in SAP HANA Smart Data Streaming
description: Part 3 of 9. Add a filter and output the desired events to a SAP HANA Table through SAP HANA Output adapter.
auto_validation: true
primary_tag: products>sap-hana-streaming-analytics
tags: [ tutorial>beginner, products>sap-hana-streaming-analytics, products>sap-hana-studio ]
author_name: Bill Jiang
author_profile: https://github.com/BillJiangSAP
time: 15
---

## Details
### You will learn  
 - Applying a filter to only capture events of interest. Filters match each event against the defined criteria, only passing those that match.
 - Adding a HANA Output adapter to connect the filtered stream to a HANA table, capturing the events in the HANA database.
 - Compiling the streaming project and check for errors.



---

[ACCORDION-BEGIN [Step 1: ](Add a Filter)]    

1. Drag and drop the **Filter** item in the **Streams and Windows** drawer of the **Palette** into the canvas.

    ![add filter](filter/1-add-filter.png)

2. Rename the stream, **`Filter1`**, to `ACTIVITY_HIST` by clicking on the filter icon.

    ![rename](filter/2-rename-filter.png)

3. Select **Connector** from the **Palette**.

    ![connector](filter/3-add-connector.png)

4. Drag the Connector from **`MACHINEDATA`** to **`ACTIVITY_HIST`** in order to connect them.

    ![connect](filter/4-connect-parts.png)

5. Double-click on the **1** under **`ACTIVITY_HIST`** > **Filter** to edit the filter expression.

    ![filter expression](filter/5-filter-expression.png)

6. Enter `MACHINEDATA.EVENT_NAME='DOOR'` in the text box to define the filter expression. User **Ctrl+Space** for content assist. Confirm your entry by pressing the **Enter** key.

    ![expression](filter/6-expression.png)

7. Click on the All Iconic button (icon shown below) to collapse all the items.

    ![collapse all](filter/7-collapse.png)

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Capture Events in HANA Table)]    

1. Select **HANA Output** from the **Palette** and drag it to the canvas.

    ![add HANA table](hana-table/1-add-hana-table.png)

2. Click on the Edit icon to edit the Adapter Properties.

    ![properties](hana-table/2-edit-properties.png)

3. Select the **`freezermon_service`** data service for **Database Service Name**. Select the entry **Target Database Schema Name** by clicking it and click on **...**.

    ![choose service and database](hana-table/3-choose-service-and-database.png)

4. Enter the name of the HANA database schema you will be using in the **Value** box. We have created our tables in `SYSTEM`, so use that. Then click **OK**.

    ![select databse](hana-table/4-select-databse.png)

5. Enter `ACTIVITY_HIST` in the **Target Database Table Name** text box and press **Enter** key. Then click **OK**.

    ![choose table](hana-table/5-choose-table.png)

6. Now use the **Connector** tool to connect the adapter to the **`ACTIVITY_HIST`** stream. Select the connector tool, click on the **`ACTIIVTY_HIST`** stream, then click on the **`HANA_Output1`** adapter.

    ![connect](hana-table/6-connector.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Compile and Check for Errors)]    

1. Click the Compile Project icon shown below.

    ![compile](compile/1-compile.png)

2. Check the **Problems** view to see if the project compiled without errors.

    ![check errors](compile/2-check-errors.png)

[DONE]

[ACCORDION-END]
