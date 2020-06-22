---
title: Using a HANA Reference Element in a Smart Data Streaming project
description: Part 4 of 9. Add a HANA Reference element and using a Join operator to combine reference and streaming data.
auto_validation: true
primary_tag: products>sap-hana-streaming-analytics
tags: [ tutorial>beginner, products>sap-hana-streaming-analytics, products>sap-hana-studio ]
author_name: Bill Jiang
author_profile: https://github.com/BillJiangSAP
time: 15
---

## Details
### You will learn  
 - How to add a HANA Reference element to the project
 - How to join event streams to SAP HANA tables in order to enrich raw event data
 - How to filter and analyzing data based on context or historical information from HANA




---

[ACCORDION-BEGIN [Step 1: ](Add HANA Reference Element)]    

1. This is a continuation from part 3 of this tutorial group. Before proceeding, please make sure that `"freezer_monitoring"` project is opened and the project diagram is the active tab in HANA Studio. If not, go to **File** > **Open File...** to open the project. Navigate to the **SAP HANA Administration Console** perspective.

    ![go to sap admin console](add-hana-table/1-goto-sap-adm.png)

2. Open the **Catalog** for the HANA system in the HANA **Systems** view.

    ![open catalog](add-hana-table/2-open-catalog.png)

3. Open the "SYSTEM" schema and drag then drop the table **`MACHINE_REF`** onto the canvas.

    ![add machineref](add-hana-table/3-add-machineref.png)

    Note: if you don't see the tables you have created, try right clicking the schema and selecting **Refresh**. Make sure to use the same schema you create the table with before. In this case, you are using SYSTEM.

4. Select **Reference**, **Inline** and then click **OK**.

    ![create table ccl](add-hana-table/4-create-table-ccl.png)

5. Change the name of the reference element to `MACHINE_REF` by clicking on the icon shown below.

    ![change name to machineref](add-hana-table/5-change-name-to-machineref.png)

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Join Event Stream To The HANA Table)]    

1. Click **Join** in the **Palette** and drop it onto the canvas.

    ![join-element](event-streaming/1-join-element.png)

2. Select the **Connector** tool. (Tip: press **Shift** key while performing the action. This will keep it selected for adding multiple connections)

    ![connect stream to join](event-streaming/2-connect-stream-to-join.png)

3. First click on **`MACHINEDATA`** and then **`Join1`** to connect them.

    ![connect machinedata to join](event-streaming/3-connect-machinedata-to-join.png)

4. Now add a connection from **`MACHINE_REF`** to **`Join1`**.

    ![connect machineref to join](event-streaming/4-connect-machineref-to-join.png)

5. Click **Select** in the **Palette** to release the Connector tool (or press **ESC**).

    ![deselect connector](event-streaming/5-diselect-connector.png)

6. Rename **`Join1`** to `DEVICE_EVENTS` and press **Enter** key.

    ![rename join](event-streaming/6-rename-join.png)

7. Hover over the **`DEVICE_EVENTS`** shape so that the toolbar appears and then click the **f(x)** (Add Column Expression) icon shown below.

    ![add column expression](event-streaming/7-add-column-expression.png)

8. Click the **Copy Columns from Input** menu item to execute it. You can also press **c**.

    ![copy columns from input](event-streaming/8-copy-columns-from-input.png)

9. Click **Select All** or you can also press **Alt+s**. Uncheck the 2nd MACHINEID field that is named **`MACHINE_REF.MACHINEID`** (we don't want it twice) and then click **OK**.

    ![choose columns to copy](event-streaming/9-choose-columns-to-copy.png)

10. Now set the join condition. Double-click on **`, MACHINE_REF`**.

    ![set join condition](event-streaming/10-set-join-condition.png)

11. When it prompts you to save the project, click **Yes**.

    ![Save](event-streaming/11-save-project.png)

12. We want to join on MACHINEID. Select **MACHINEID : string** in each source column and then click **Add**. Once its been added click **OK**.

    ![join clause](event-streaming/12-join-clause.png)

[DONE]

[ACCORDION-END]
