---
title: Create a Moving Average on an Event Stream Using an Aggregation Window
description: Add an aggregate window to create a moving average on the event data, smoothing out the raw data.
author_name: Aaron Patkau
author_profile: https://github.com/aptk001
primary_tag: products>sap-hana-streaming-analytics
tags: [  tutorial>beginner, topic>internet-of-things, products>sap-hana-streaming-analytics, products>sap-hana\,-express-edition   ]
auto_validation: true
time: 20
---
## Prerequisites
 - **Tutorials:** [Join an Event Stream to a HANA Table](https://developers.sap.com/tutorials/sds-stream-table-join.html)

## Next Steps
- **Tutorials:** [Generating Alerts Using a Derived Window in SAP HANA Streaming Analytics](https://developers.sap.com/tutorials/sds-event-stream-alerts.html)


## Details
### You will learn  
 - How to add an aggregate window to hold multiple data entry and observe patterns
 - How to use a time-based sliding event window to compute metrics and monitor trends

---
Unlike streams, which are stateless, windows are like tables that hold information from one event to the next. Here you will add an aggregate window that is keyed by **`MACHINEID`**, which will hold a set of aggregate values for each machine.

If you don't want to use the visual editor, then you can skip to step 8 to view the CCL and copy it into your project.


[ACCORDION-BEGIN [Step 1: ](Add an Aggregate Window to the Project)]


Click on **Aggregate** in the **Streams and Windows** drawer of the **Palette** and then click on the canvas to add it to your project.

![drop aggregate window](1-dropaggregatewindow.png)

Rename the stream to `AVG_TEMP` by clicking on the aggregate symbol, then press **Enter**.

![rename](2-rename.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Connect the Aggregate Window to the Event Stream)]

Select the **Connector** tool in the **Palette** and click on **`DEVICE_EVENTS`** first, then on **`AVG_TEMP`** to connect them.

![connector](3-connector.png)

[VALIDATE_2]


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add columns to the window)]

Click the **Add Column Expression** ( **f(x)** ) button shown below.

![add column](4-addcolumn.png)

Choose **Copy Columns from Input** from the menu to open the column selection dialog.

![copy columns](5-copycolumns.png)

Select every option by clicking **Select All** or pressing **Alt+s**. Uncheck **`DEVICE_EVENTS.EVENT_NAME`**, **`DEVICE_EVENTS.EVENT_DESCRIPTION`**, and **`DEVICE_EVENTS.MACHINETYPE`**. Click **OK**.

![select columns to copy](6-selectcolumnstocopy.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Set a retention policy on the input)]

Now you will set a retention policy on the input to this aggregation. This determines the set of events you are aggregating over. In this case, you'll define it by time.

Expand the **Inputs** tab and right-click on **`DEVICE_EVENTS`**. Select **Keep Policy** to open a dialog box to configure the policy.

![keep policy](8-keeppolicy.png)

Click **Time** and enter `30 seconds` in the entry box. Click **OK**.

![policy edit](9-policyedit.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Edit the GROUP BY clause)]

To define the **GROUP BY** clause, expand the **Group By** compartment in the **`AVG_TEMP`** element by clicking on **+**.

Double-click on **`GROUP BY unassigned_group_by`**.

![group info](11-groupinfo.png)

Select the entry **`DEVICE_EVENTS.MACHINEID`**, then click **Add >>** and **OK**.

![group criteria](12-groupcriteria.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add a GROUP FILTER)]

Now you need to add a GROUP filter, since we only want to aggregate temperature readings. This will filter out the Door open/close events and the Power on/off events before you aggregate.

Click the **Add Group Clause** ( **{ }** ) button shown below.

![group filter clause](13-groupfilterclause.png)

Choose **Group Filter Clause** from the menu to add the group filter expression.

![click group filter clause](14-clickgroupfilterclause.png)

Double-click on **Group Filter 1**.

![rename group filter](15-renamegroupfilter.png)

Enter `DEVICE_EVENTS.EVENT_NAME='TEMP'` as the filter expression in the text field. You can use **Ctrl+Space** for content assist. Confirm your entry by pressing **Enter**.

![name group filter](16-namegroupfilter.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Edit the Column Expressions)]

Expand the **Column Expressions** compartment to edit expressions.

![go to column expression](17-gotocolumnexpression.png)

Double-click on **`DEVICE_EVENTS.EVENT_TIME`**.

![change event time](18-changeeventtime.png)

Edit the expression for **`EVENT_TIME`**. Change it to: `last(DEVICE_EVENTS.EVENT_TIME)`. This will cause the aggregate values for the group to show the event time of the last event received in the group. Confirm your entry by pressing **Enter**.

![last event time](19-lasteventtime.png)

Double-click on the name **`EVENT_VALUE`** and rename this column to **`AVG_TEMP`**. Confirm your entry by pressing **Enter**.

![20-change event value name](20-changeeventvaluename.png)

Double-click on the expression for **`AVG_TEMP`**, which is currently set to **`DEVICE_EVENTS.EVENT_VALUE`**.

You are now going to edit this expression to compute an average. Also, since the value field is a string, before you can compute an average, you need to convert it to a number. Change the expression to: `avg(to_decimal(DEVICE_EVENTS.EVENT_VALUE, 4, 2))`. Confirm your entry by pressing **Enter**.

![average event value](22-avgeventvalue.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Check your work)]

At this point you can compile the project to check for errors. If you switch to the CCL editor ( **F6** or click the **Switch to Text** button in the Eclipse toolbar), your code should look like this:

```SQL

/**@SIMPLEQUERY=AGGREGATE*/
CREATE OUTPUT WINDOW AVG_TEMP
PRIMARY KEY DEDUCED
KEEP ALL
AS SELECT
    DEVICE_EVENTS.MACHINEID MACHINEID ,
	LAST ( DEVICE_EVENTS.EVENT_TIME ) EVENT_TIME ,
	avg ( to_decimal(DEVICE_EVENTS.EVENT_VALUE, 4, 2) ) AVG_TEMP ,
	DEVICE_EVENTS.MAX_TEMP MAX_TEMP ,
	DEVICE_EVENTS.MIN_TEMP MIN_TEMP ,
	DEVICE_EVENTS.LOCATION LOCATION ,
	DEVICE_EVENTS.TEMP_UNIT TEMP_UNIT
FROM DEVICE_EVENTS KEEP 30 SEC
GROUP FILTER DEVICE_EVENTS.EVENT_NAME = 'TEMP'
GROUP BY DEVICE_EVENTS.MACHINEID ;

```

[DONE]

[ACCORDION-END]
