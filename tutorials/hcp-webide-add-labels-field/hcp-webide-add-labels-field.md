---
title: Add labels and a new field to your app
description: Learn how to insert labels into the detail view of your app and add additional fields.
tags: [tutorial:product/hcp, tutorial:product/sapui5_web_ide, tutorial:product/mobile, tutorial:interest/gettingstarted, tutorial:product/sap_ui5]
---

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** [Deploy your mobile web app to SAP HANA Cloud Platform](http://go.sap.com/developer/tutorials/hcp-deploy-mobile-web-app.html)

## Details

### You will learn
When you built the app in previous tutorial series, the template added two fields in the detail view header (UnitsInStock and UnitsOnOrder). Those fields appear in the detail view without labels, so you will add some for them. 

 ![Application fields requiring labels](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-labels-field/mob2-1_0.png)

To explore a bit more of SAP Web IDE, you will use the Web IDE search feature to identify which file to edit. 

You will also learn how to add a field to your app. This is useful since the template can quickly generate a working app for you, but you are not restricted to what it generates.

**Time to complete: <5 min**

### Adding labels to the Detail View

1. In SAP Web IDE, click on the **Search icon** (the magnifying lens icon at the top of the right-hand toolbar), enter `UnitsInStock` in the search field and click **Search**.

    ![Activating the SAP Web IDE search pane](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-labels-field/mob2-1_label_1.png)

2. In the search results, and you will see two files were found. One is in the **Detail.view.xml** file which is in the view folder. Since you want to change the detail view part of the app – that is the file to edit.

    >Tip: If you hover your cursor over an item in the search result list, a window opens to show some context around that item.
 
    ![SAP Web IDE search pane results](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-labels-field/mob2-1_label_2.png)


3. Double-click the UnitsInStock line in **Detail.view.xml** to open that file in the edit pane. 

    >Notice that the search term is highlighted in the source file. You can click on the **Search icon** again to close the search pane. 

    ![Clicking on SAP Web IDE search pane results](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-labels-field/mob2-1_label_3.png)

4. You will need to modify two **ObjectAttribute** lines in the header: **UnitsInStock** and **UnitsOnOrder**.

    ![XML view elements to modify](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-labels-field/mob2-1_label_4.png)

5. Insert the titles as attributes into the ObjectAttribute areas highlighted on the right below. You can type in the `title="xyz"` strings, or replace the lines entirely with the text below. The edited lines should look like the screenshot below.

    ```xml
    <ObjectAttribute title="Units In Stock" text="{UnitsInStock}"/>
    <ObjectAttribute title="Units On Order" text="{UnitsOnOrder}"/>
    ```
    ![Modified XML view elements](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-labels-field/mob2-1_label_5.png)

6. Save your edts, select **index.html** and click the **Run** button to preview the Northwind application with the added labels. If you do not see your labels it is probably because the browser has cached the page. To fix this, see the browser cache section below.

    ![Labels added to fields](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-labels-field/mob2-1_label_6.png)


####<a name="browser_cache"></a>Browser cache:
The browser cache will frequently prevent changes from showing up. There are a few options to handle this (all assume you have the Web IDE preview window displayed):

1. Hold down the shift key while clicking the refresh icon in the browser.
2. Turn on Developer Tools (Google Chrome: F12 on a PC, or Option-Command-I on a Mac), then right-click on the refresh icon and select **Hard Reload** or **Empty Cache and Hard Reload**
3. If you are doing a lot of web development, when you open Developer Tools, click on the **Network** tab and then select **Disable Cache**. 
4. If you have Developer Tools enabled, and you are contunuing to work through tutorials, it is convenient to leave the preview tab open. After you save a change in Web IDE, instead of selecting **index.html** and clicking **Run**, simply switch back to the preview tab and select **Hard Reload** or **Empty Cache and Hard Reload** to reload the app. 

 
### Adding a data field in the header section
In this section, you will add the Product ID field from the OData source to the header section of the detail page just above the Units In Stock and Units On Order. To do this you will add an XML element just above the `ObjectAttribute` elements that were edited in the previous section.

1. Go back to the **northwind > view> Detail.view.xml** in the edit pane (or reopen if you closed it). Add the following XML snippet inside the section as shown below.

    ```xml
    <ObjectAttribute title="Product ID" text="{ProductID}"/>
    ```

    >Note: in this XML element, both the label (title field) and value (text field) are included.
 
    ![Inserted XML view element](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-labels-field/mob2-1_field_1.png)

2. Save your edits, and either:

    - Switch to your preview tab with Developer Tools open and select **Hard Reload** or **Empty Cache and Hard Reload**  
    - or select the **index.html** and click the **Run** button to preview the Northwind application with the added ProductID field. If you do this, you will probably have to follow the procedure above on [Browser cache](#browser_cache) to see your change.

    ![New XML view element displayed in the app](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-labels-field/mob2-1_field_2.png)
 
## Next Steps
 - [Round the currency field](http://go.sap.com/developer/tutorials/hcp-webide-round-currency.html)