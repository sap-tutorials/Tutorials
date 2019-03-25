---
title: Create a Streaming Project with SAP HANA Streaming Analytics
description: Create an SAP HANA streaming analytics project using the streaming plug-in for SAP HANA studio.
primary_tag: products>sap-hana-streaming-analytics
tags: [  tutorial>beginner, topic>internet-of-things, products>sap-hana-streaming-analytics, products>sap-hana\,-express-edition   ]
author_name: Aaron Patkau
author_profile: https://github.com/aptk001
auto_validation: true
time: 20
---

## Prerequisites  
 - **Tutorials:** [Installing and Configuring the Streaming Studio Plugin](https://developers.sap.com/tutorials/hxe-ua-streaming-plugin.html)

## Details
### You will learn
 - How to create a new streaming project
 - How to define an input stream to receive incoming events, including the field structure (schema) for the incoming events
 - How to add a filter to apply to the event stream

 In this tutorial series you will build a project using a simple Internet of Things (IoT) scenario.  The project will be designed to monitor a set of freezer units that are equipped with sensors to report temperature, power on/off events, and door open/close events. You will apply various operators to analyze and transform this data, generate alerts, and capture information in the HANA database.

---

[ACCORDION-BEGIN [Step 1: ](Create a new streaming project)]

Go to **File** > **New** > **Project...**.

![Create a new Project](1-create-a-new-project.png)

Expand **SAP HANA streaming analytics** and select **New Streaming Project**, then **Next**.

![Select Streaming Project](2-select-streaming-project-02.png)

Enter `freezer_monitor` in the **Name** field (be sure to only use lowercase) and accept the default **Directory** location.

![Provide name and location](3-project-name.png)

Select **Finish**.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create an Input Stream)]

All events enter a streaming project via an input stream or input window. When you create the project, it creates a default input stream called "NEWSTREAM".

![add a stream](1-add-a-stream.png)

Use this stream and just rename it.  Alternatively, add another input stream (or window) by dragging it from the **Palette** and dropping it onto the project diagram.
Click the icon to the left of the stream's name and change it to **MACHINEDATA**, then press **Enter**.

![rename a stream](2-rename-a-stream.png)

> Note that stream names are case-sensitive

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Define columns for each field in the incoming events)]

Streams and windows all have a fixed set of typed columns (schema). Events arriving on a particular input stream must contain a set of fields that is compatible (number and type of fields) with the input stream.  Events with different field sets are typically processed by different input streams.

Click on the **+** to the left of **Schema** to expand it.

![show schema](3-show-schema.png)

Select the **MACHINEDATA** element so that the toolbar appears. Choose the **Add Column** option.

![add column](4-add-column.png)

Click **Add Column** three more times, so there are a total of 5 columns.

![add 5 columns](5-add-5-columns.png)

Double-click on the column name **`Column1`** and change it to **MACHINEID**.

![rename column](6-rename-column.png)

Double-click on **(INTEGER)** to the right of **MACHINEID** to change the data type for this column to string (after double-clicking, select the dropdown arrow, and choose **string**).

Change the rest of the column names and data types as follows:

> Note that column names are case-sensitive

Column Name         | Type
:----------------   | :----------------
`MACHINEID`         | `string`
`EVENT_TIME`        | `msdate`
`EVENT_NAME`        | `string`
`EVENT_DESCRIPTION` | `string`
`EVENT_VALUE`       | `string`

Your stream should look like this:

![all columns](8-all-columns.png)

Click the **-** button to collapse the element.

![minimize](9-minimize.png)

> Note that you can just drag the element to a new position to organize your diagram.

![drag stream](10-drag-stream.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add a filter)]

Drag and drop the **Filter** element from the **Streams and Windows** drawer of the **Palette** onto the canvas.

![add filter](1-add-filter-02.png)

Rename the stream, `Filter1`, to **`ACTIVITY_HIST`** by clicking on the filter icon.

![rename](2-rename-filter.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Connect it to an event stream)]

Select **Connector** from the **Palette**.

![connector](3-add-connector.png)

Click on the **`MACHINEDATA`** element and then on the **`ACTIVITY_HIST`** element in the visual editor to connect them. This directs the data flow from the input stream to the filter.

![connect](4-connect-parts.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Define the filter criteria)]

Double-click on the **1** under **`ACTIVITY_HIST`** > **Filter** to edit the filter expression.

![filter expression](5-filter-expression.png)

Enter `MACHINEDATA.EVENT_NAME='DOOR'` in the text field to define the filter expression.

> Note that you can use **Ctrl+Space** for content assist. Confirm your entry by pressing **Enter**.

![expression](6-expression.png)

Select the **All Iconic** option (icon shown below) to collapse all the items.

![collapse all](7-collapse.png)

[DONE]

[ACCORDION-END]
