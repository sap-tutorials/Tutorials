---
title: Change from mock server to live OData service
description: Connect to a live backend system for your application.
auto_validation: true
primary_tag: topic>sapui5
tags: [  tutorial>beginner, topic>sapui5, products>sap-cloud-platform, products>sap-web-ide ]
---

## Prerequisites  
 - **Proficiency:** Beginner


## Details
### You will learn  
Change your data source from a mock server created by BUILD to a live OData service. It is important to know how to configure the connection to the backend of an application, which you will learn here.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Reopen SAP Web IDE)]

When you create a new destination in SAP Cloud Platform, SAP Web IDE must be updated to get the latest information. **Refresh or close and re-open** your SAP Web IDE.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add the OData service)]

You need to add the OData service to your application. **Right click** on the project name. Select **New > OData Service**.

![new OData service path](1.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Select the service)]

The OData service wizard will pop up. On the **Data Connection** page, select **Service URL** from the _Sources_ options.

![data connection source selection](2.png)

In the drop down, select **`Toys OData service`**.

![data connection service selection](3.png)

Enter **/** for the relative URL and click **Test**. If you configured your destination and OData connection properly, you will see a **list of entities in the OData service**, which include Sets and Parts.

![testing the service](4.png)

Click **Next** if your data is correct.

![next highlighted](5.png)

Leave Overwrite existing OData service connections unchecked. Click **Finish**.

![finish highlighted on confirmation screen](6.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Validate the data source)]

Open the **metadata** file for the new OData service by opening the `webapp`, then the `localService`, and finally the `toysData` folders.

![folder structure to find data folder](validate-4-a.png)

**Double click** on the `metadata.xml` file to open it. **Copy the `NavigationProperty`** element in the XML under the entity `SetsType`.

![element to copy from metadata file](validate-4-b.png)


[VALIDATE_4]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Change the Component file)]

Locate the **`Component.js`** file under the *`webapp`* folder. **Double click** to open the file.

![location of Component file](7.png)

One update need to be made to this file to switch from the Mock Data to the Live Data service. You will need to update the navigation properties to match the new entity name of **Sets** instead of `ToysSet`.

![places to update in Component file](8.png)

Change the word `ToysSet` to **`Sets`**.

![changes in Component file](9.png)

**SAVE** your changes.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Change the manifest file)]

Open the **`manifest.json`** file also located in the `webapp` folder.

SAP Web IDE defaults to the _Description Editor_. You will need to be in the **Code Editor**. To change to the Code Editor, locate the toolbar on the bottom of the screen. **Click Code Editor** in the toolbar.

![change from description editor](10a.png)

You will know you are in the Code Editor because the file is all code. If you see tabs and input fields, you are still in the Description Editor. Make sure your screen matches the screenshot below.

![what the code editor looks like](10b.png)

Locate the `dataSources` array in the manifest file. You have added a new data source called `toysData` when you added the OData service in the earlier steps. You want to make this your **local** data service.

![data sources highlighted in manifest file](10c.png)

**Rename** *`local`* to `mockservice`.

**Rename** *`toysData`* to `local`.

![new data source names in manifest file](11.png)

**SAVE** your changes.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Update the Master view)]

Open the **Master view**, or the view ending with `_S0`.

![master view location in files](12.png)

The **items binding** in the List control needs to be updated to match the new entity name.

![area to change highlighted in master view](13.png)

**Change** *`/ToysSet`* to `/Sets`.

![updated master view](14.png)

**SAVE** your changes.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Update the Detail controller)]

Open the **Detail controller**, or the controller ending in `_S1`.

![detail controller location in file](15.png)

The **expand parameter** needs to be updated to match the new entity description.

Towards the bottom of the `onInit` function, right before the `onAfterRendering` function, there is a reference to the _expanded entity name_ that needs to be updated.

![binding parameter for the bar chart and table](17.png)

In the `oBindingParameters` variable, **Change the value** of `path` from *`PartOf`* to **`Parts`** and `entitySet` from *`ToysSet`* to **`Sets`**.

![binding parameter change to Parts and Sets](18.png)

**SAVE** your changes.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Validate your initialization function)]
**Copy the code** from your `onInit` function in the Detail (`S1`) controller. Paste it in the box below.


[VALIDATE_9]
[ACCORDION-END]
