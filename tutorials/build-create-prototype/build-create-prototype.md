---
title: Create a BUILD Prototype
description: Create a high fidelity prototype with BUILD
primary_tag: products>build>build
tags: [  tutorial>beginner, products>build>build ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Getting Started with BUILD](http://www.sap.com/developer/tutorials/build-getting-started.html)


## Next Steps
 - [Collect user feedback in BUILD](http://www.sap.com/developer/tutorials/build-collect-feedback.html)

## Details
### You will learn  
Create a prototype application with BUILD's gallery and the easy to use drag and drop interface.


### Time to Complete
**25 Min**

---

[ACCORDION-BEGIN [Step 1: ](View Gallery applications)]
Log in to your [BUILD account](https://www.build.me/splashapp/).

![gallery location in menu bar](1.png)

From the top menu bar, select **Gallery**. This will take you to the listing of all available template application prototypes you can use with BUILD. The Gallery allows you to quick start an application prototype without having to build it from scratch.

![gallery overview page](2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Select a Gallery app)]
In the **Gallery**, find the **search bar** and type in _inventory_.

![gallery search bar for inventory](3.png)

This will filter down the list of applications. Select the **Inventory Dashboard** application from the filtered list.

![inventory dashboard gallery app](4.png)

The prototype overview will provide you details about what screens are included and their navigation and what files are included.

![inventory dashboard app overview page](5.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Clone a prototype)]
Cloning an prototype will add the prototype to your project collection. Once in your collection, you can make edits and modifications to your local version.

On the prototype overview, click the **Clone** button under the basic details.

![clone button on inventory dashboard](6.png)

Verify that you want clone the application prototype when prompted by click **Clone**.

![clone project prompt](7.png)

When the clone is complete, BUILD will let you know. When prompted, click **OK** to view the cloned prototype in your workspace.

![cloning complete](8.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Modify the prototype)]
To open the UI editor, click on one of the screens under the **Prototype** section.

![select prototype to open ui editor](9.png)

Here you can search for controls, drag and drop them onto the screen, and modify properties of the controls. You can also re-order data and remove controls that you don't need.

![UI editor screen](10.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add a table)]
To add a table to the main chart area, start by clicking the grayed-out chart area. This will select this view.

![select grayed-out chart area](11.png)

On the left hand side, you will find a _controls_ list. Search for **Table** in this list.

![controls search in left side menu bar](12.png)

Find the *Table* control under the **List** control types.

![table details](13.png)

**Drag and drop** the control to underneath the chart.

![drag table control to chart area](14.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add columns to the table)]
You can customize the control from the GUI interface. To add columns, **right click** on a column in the table.

This will bring up a local menu bar. Select **Add Column Left** from the options to create a new column.

![right click menu with add column highlighted](15.png)

**Repeat this action.** You should have 2 new columns.

![table control with 2 new columns](16.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Rename the Column Headers)]
The names of the columns can also be modified in the GUI. To do this, **double click on the header name**.

![column header highlighted for renaming](17.png)

This will highlight the header name and make it editable. Rename the headers to match the names below.

**Part ID** | **Image** | **Part's Name** | **Availability** | **Demand**
--- | --- | --- | --- | ---

![renamed column headers final](18.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Add controls to the table cells)]
In the controls list on the left hand side, search for **Label**.

![label search in control menu](19.png)

**Drag and drop** the label control into the _first column first cell_.

In the control list, search for **Image**.

![image search in control menu](20.png)

**Drag and drop** the image control into the _second column first cell_.

![table with added controls](21.png)

You can modify control properties in the right hand side bar. In the _properties_ pane for the image control, find the **Size** property. Change the **Height (h)** from _Auto_ to `50px`

![properties pane, size highlighted](22.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Populate the data)]
To populate data to the table, find the **Data** tab on the _left hand menu bar_. This will show the pre-populated data sources for this prototype.

![data selected in lef side pane](23.png)

Select the **Parts** entity and **drag and drop it on the table**.

![parts entity mapped to table](24.png)

Column names that match properties of the entity will automatically map. You can also manually bind a specific field to a control. To bind a column to a specific field, **expand the entity** on the data pane to see the available fields.

![parts entity expanded to show fields](24a.png)

Drag and drop the **Image** field to the **Image Control**.

![images field mapped to image column](24b.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Preview the prototype)]
To preview the final view, click on the **Eye** icon in the top tool bar.

![preview icon highlighted](25.png)

This will show you a "live" version of your prototype. You can preview how you application prototype will look on desktop, tablet, and mobile devices.

![preview of app prototype](26.png)

To get back to the UI Editor, click the **UI Editor button**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Change the theme)]
You can modify the overall color scheme, known as a "Theme" in SAP Fiori. **Go back to the UI Editor**.

![theme icon highlighted](27.png)

In the UI Editor, locate the **icon with a paintbrush** on the top tool bar. **Click it**.

![theme options with Belize highlighted](28.png)

Select **SAP Belize** from the menu items. This will update the overall theme for the application. No need to update each individual screen or control!

![prototype with Belize theming](29.png)

[DONE]
[ACCORDION-END]




## Next Steps
- [Collect user feedback in BUILD](http://www.sap.com/developer/tutorials/build-collect-feedback.html)
