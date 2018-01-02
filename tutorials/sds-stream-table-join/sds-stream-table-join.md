---
title: Join an event stream to a HANA table
description: Add a HANA Reference element to a streaming project. The Reference element is a proxy for a HANA table, allowing the HANA table to be accessed within the streaming project. Then, using a Join operator, join the input stream to the HANA table to enrich the incoming events with reference data from the HANA database.
primary_tag: products>sap-hana-streaming-analytics
tags: [  tutorial>beginner, topic>internet-of-things, products>sap-hana-streaming-analytics, products>sap-hana\,-express-edition   ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Run and Test a Streaming Project](https://www.sap.com/developer/tutorials/sds-run-test.html)

## Next Steps
   **Tutorials:** [Create a Moving Average on an Event Stream using an Aggregation Window](https://www.sap.com/developer/tutorials/sds-event-stream-moving-average.html)

## Details
### You will learn  
 - How to reference a HANA database table in your streaming project using a HANA Reference element.  In streaming projects, a Reference element points at a HANA database table and pulls data in from the table as needed
 - How to join a live event stream to a HANA table to enrich raw event data with reference data from the HANA database

### Time to Complete
**20 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Add a HANA database table to your project)]

After completing the previous tutorial in this series, you should still have your project open, but you are likely in the Streaming Run-Test perspective.  Go back to the Streaming Development perspective.  If the project isn't still open in the visual editor, open it now.

- if the project is open in the CCL editor, use the **Switch to Visual** tool in the toolbar, or F6, to switch to the Visual editor
- if the project was closed, open it in the visual editor by finding it in the Project Explorer view and then double click on the `freezer_monitor.cclnotation` file in the project folder

If you would prefer to use the CCL editor at this point, rather than the visual editor, you can skip the remainder of this step along with steps 2 and 3 below and go to the final, optional, step where you can copy the CCL and paste it into your project.  

Go to the **Data Services** tab in the SAP HANA Streaming Development perspective. Double click on your HXE host and then double click on the **Server-wide** folder to load the defined data services.

Expand the `hanadb` service and the **STREAMING** schema.

![open catalog](2-open-catalog.png)

In the **STREAMING** schema, grab the table **`MACHINE_REF`** and drag it onto the canvas. When you drop it onto the diagram it will open a dialog box that let's you choose what to create

![add machineref](3-add-machineref.png)

> Note that you can also create a REFERENCE element from the palette, but then you will have to manually configure it, including the schema (column names and types).  This is a much easier way. Alternatively, you can add it in the CCL Editor as described in (optional) step 4 of this tutorial.

In the dialog box:

- set the Service to **`hanadb`**
- select **Reference**, **Inline**
- then click **OK**

![create table ccl](4-create-table-ccl.png)

Change the name of the reference element to `MACHINE_REF` by clicking on the icon shown below and editing the name.

![change name to machineref](5-change-name-to-machineref.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Join the Event Stream to the Reference table)]

Click **Join** in the **Stream and Windows** drawer of the **Palette** and drop it onto the canvas.

> Tip: click the small arrow bar at the bottom of the palette drawer to scroll down

![join-element](1-join-element.png)

Select the **Connector** tool.

![connect stream to join](2-connect-stream-to-join.png)

First click on **`MACHINEDATA`** and then **`Join1`** to connect them.

![connect machinedata to join](3-connect-machinedata-to-join.png)

Select the **Connector** tool again and now add a connection from `MACHINE_REF` to `Join1`.

![connect machineref to join](4-connect-machineref-to-join.png)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Configure the Join operator)]

Rename **`Join1`** to `DEVICE_EVENTS` and press **Enter** key.

![rename join](6-rename-join.png)

Click on **`DEVICE_EVENTS`** to select it.  The shape toolbar will appear

Click the **f(x)** (Add Column Expression) icon shown below.

![add column expression](7-add-column-expression.png)

Click the **Copy Columns from Input** menu item to execute it.

![copy columns from input](8-copy-columns-from-input.png)

Click **Select All** or you can also press **Alt+s**. Uncheck the 2nd MACHINEID field that is named **`MACHINE_REF.MACHINEID`** (we don't want it twice) and then click **OK**.

![choose columns to copy](9-choose-columns-to-copy.png)

Now set the join condition. Double-click on **`, MACHINE_REF`**.

![set join condition](10-set-join-condition.png)

When it prompts you to save the project, click **Yes**.

![Save](11-save-project.png)

We want to join on MACHINEID. Select **MACHINEID : string** in each source column and then click **Add**. Once its been added click **OK**.

![join clause](12-join-clause.png)


[ACCORDION-END]

## Optional

[ACCORDION-BEGIN [Option:](Edit project in the CCL editor)]

If you want to view the CCL for your project, or if you want to add the `REFERENCE` and `JOIN` elements using the CCL editor rather than the visual editor, you can copy the code below.

To open the project in the CCL editor, in the **SAP HANA Streaming Development** perspective do one of the following:

- if the project is still open in the visual editor, use the **Switch to Text** tool in the the Eclipse toolbar
- if the project is not open in any editor, double click on `freezer_monitor.ccl` in the project folder

Here's the CCL for these elements:

```SQL

CREATE REFERENCE MACHINE_REF
    SCHEMA (
	MACHINEID string ,
	MACHINETYPE string ,
	MAX_TEMP decimal(4,2) ,
	MIN_TEMP decimal(4,2) ,
	LOCATION string ,
	TEMP_UNIT string )
	PRIMARY KEY ( MACHINEID )
	PROPERTIES service = 'hanadb' ,
	source = 'MACHINE_REF' ,
	sourceSchema = 'STREAMING' ;

  /**@SIMPLEQUERY=JOIN*/
  CREATE OUTPUT STREAM DEVICE_EVENTS
  AS SELECT
      MACHINEDATA.MACHINEID MACHINEID ,
  	MACHINEDATA.EVENT_TIME EVENT_TIME ,
  	MACHINEDATA.EVENT_NAME EVENT_NAME ,
  	MACHINEDATA.EVENT_DESCRIPTION EVENT_DESCRIPTION ,
  	MACHINEDATA.EVENT_VALUE EVENT_VALUE ,
  	MACHINE_REF.MACHINETYPE MACHINETYPE ,
  	MACHINE_REF.MAX_TEMP MAX_TEMP ,
  	MACHINE_REF.MIN_TEMP MIN_TEMP ,
  	MACHINE_REF.LOCATION LOCATION ,
  	MACHINE_REF.TEMP_UNIT TEMP_UNIT
  FROM MACHINEDATA INNER JOIN MACHINE_REF
  ON MACHINEDATA.MACHINEID = MACHINE_REF.MACHINEID ;


```


[ACCORDION-END]
