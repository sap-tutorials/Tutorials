---
title: Create a Streaming Project with SAP HANA Streaming Analytics
description: Create a SAP HANA Streaming Analytics project using the streaming plug-in for Eclipse.
primary_tag: products>sap-hana-streaming-analytics
tags: [  tutorial>beginner, topic>internet-of-things, products>sap-hana-streaming-analytics, products>sap-hana\,-express-edition   ]
time: 20
---

## Prerequisites  
 - **Tutorials:** [Installing and Configuring the Streaming Studio Plugin](https://www.sap.com/developer/tutorials/hxe-ua-streaming-plugin.html)

## Details
### You will learn  
 - How to create a new streaming project
 - Define an input stream to receive incoming events, including the field structure (schema) for the incoming events
 - Add a filter to apply to the event stream

 In this tutorial series we will be building a project using a simple Internet of Things (IoT) scenario.  Our project will be designed to monitor a set of freezer units that are equipped with sensors to report temperature, power on/off events, and door open/close events. We will apply various operators to analyze and transform this data, generate alerts, and capture information in the HANA database.

---

[ACCORDION-BEGIN [Step 1: ](Create a new streaming project)]

Click the **File** menu. Then click the **New** > **Project...**.

![Create a new Project](1-create-a-new-project.png)

Expand **SAP HANA streaming analytics** and select **New Streaming Project**, then click **Next**.

![Select Streaming Project](2-select-streaming-project-02.png)

Enter `freezer_monitor` in the **Name** box (be sure to only use lower case) and accept the default **Directory** location.

![Provide name and location](3-project-name.png)

Click **Finish**.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create an Input Stream)]

All events enter a streaming project via an input stream or input window. When you created the project, it created an input stream called "NEWSTREAM" by default. We will use that stream and just rename it.  Alternatively you could add another input stream (or window) by dragging it from the palette and dropping it into the project diagram.

![add a stream](1-add-a-stream.png)

Click the icon to the left of the name **NEWSTREAM** and change the name of this stream to **MACHINEDATA**, then press **Enter** key.

![rename a stream](2-rename-a-stream.png)

> Note that stream names are case sensitive


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Define columns for each field in the incoming events)]

Streams and windows all have a fixed set of typed columns (aka schema). Events arriving on a particular input stream must contain a set of fields that is compatible (number and type of fields) with the input stream.  Events with different field sets will typically be processed by different input streams.

Click on the **+** to the left of Schema to expand it.

![show schema](3-show-schema.png)

Select the **MACHINEDATA** shape so that the toolbar appears. Click on the **Add Column** tool.

![add column](4-add-column.png)

Click on **Add Column** three more times, so there are a total of 5 columns.

![add 5 columns](5-add-5-columns.png)

Double click on the name **`Column1`** and change the name of this column to **MACHINEID**.

![rename column](6-rename-column.png)

Double click on **(INTEGER)** to the right of **MACHINEID** to change the datatype for this column to string. (after double clicking, click the drop down arrow, and select **string**)

Change the rest of the column names and data types as follows:

> Note that column names are case sensitive

Column Name         | Type
:----------------   | :----------------
`MACHINEID`         | `string`
`EVENT_TIME`        | `msdate`
`EVENT_NAME`        | `string`
`EVENT_DESCRIPTION` | `string`
`EVENT_VALUE`       | `string`

Your stream should look like this:

![all columns](8-all-columns.png)

Click here **-**  to collapse the shape.

![minimize](9-minimize.png)

Note: You can just drag the shape to a new position to organize our diagram.

![drag stream](10-drag-stream.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add a filter)]

Drag and drop the **Filter** item in the **Streams and Windows** drawer of the **Palette** into the canvas.

![add filter](1-add-filter-02.png)

Rename the stream, `Filter1`, to **`ACTIVITY_HIST`** by clicking on the filter icon.

![rename](2-rename-filter.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Connect it to an event stream)]

Select **Connector** from the **Palette**.

![connector](3-add-connector.png)

Click on the **`MACHINEDATA`** shape and then on the **`ACTIVITY_HIST`** shape in the visual editor to connect them, directing the data flow from the input stream to the filter.

![connect](4-connect-parts.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Define the filter criteria)]

Double-click on the **1** under **`ACTIVITY_HIST`** > **Filter** to edit the filter expression.

![filter expression](5-filter-expression.png)

Enter `MACHINEDATA.EVENT_NAME='DOOR'` in the text box to define the filter expression.

> Note that you can use **Ctrl+Space** for content assist. Confirm your entry by pressing the **Enter** key.

![expression](6-expression.png)

Click on the All Iconic button (icon shown below) to collapse all the items.

![collapse all](7-collapse.png)


[ACCORDION-END]
