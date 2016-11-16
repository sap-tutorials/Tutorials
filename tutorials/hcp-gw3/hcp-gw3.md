---
title: Add labels and a new field to your app
description: Learn how to insert labels into the detail view of your app and add additional fields.
tags: [ products>sap-hana-cloud-platform, products>sap-web-ide, topic>cloud, topic>html5, topic>mobile, topic>odata, tutorial>beginner ]
---

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** [Deploy your mobile web app to SAP HANA Cloud Platform](http://go.sap.com/developer/tutorials/hcp-gw2.html)

## Next Steps
 - [Implementing labels using the i18n (internationalization) features of SAPUI5](http://go.sap.com/developer/tutorials/hcp-gw4.html)

## Details

### You will learn
When you built the app in previous tutorial series, the template added two fields in the detail view header (`Details` and `DeliveryStatusDescription`). Those fields appear in the detail view without labels, so you will add some for them.

To explore a bit more of SAP Web IDE, you will use the Web IDE search feature to identify which file to edit.

You will also learn how to add a field to your app. This is useful since the template can quickly generate a working app for you, but you are not restricted to what it generates.

### Time to Complete
**<5 min**

### Adding labels to the Detail View

---

1. In SAP Web IDE, click on the **Search icon** (the magnifying lens icon at the top of the right-hand toolbar), enter `DeliveryStatusDescription` in the search field and click **Search**.



2. In the search results, and you will see two files were found. One is in the **`Detail.view.xml`** file which is in the view folder. Since you want to change the detail view part of the app – that is the file to edit.

    >Tip: If you hover your cursor over an item in the search result list, a window opens to show some context around that item.




3. Double-click the `DeliveryStatusDescription` line in **`Detail.view.xml`** to open that file in the edit pane.

    >Notice that the search term is highlighted in the source file. You can click on the **Search icon** again to close the search pane.



4. You will need to modify two **`ObjectAttribute`** lines in the header: **`Details`** and **`DeliveryStatusDescription`**.



5. Insert the titles as attributes into the ObjectAttribute areas highlighted on the right below. You can type in the `title="xyz"` strings, or replace the lines entirely with the text below. The edited lines should look like the screenshot below.

    ```xml
    <ObjectAttribute title="Details" text="{Details}"/>
    <ObjectAttribute title="Delivery Status" text="{DeliveryStatusDescription}"/>
    ```


6. Save your edts, select `index.html` and click the **Run** button to preview the Northwind application with the added labels. If you do not see your labels it is probably because the browser has cached the page. To fix this, clear the browser cache.




## Next Steps
 - [Implementing labels using the i18n (internationalization) features of SAPUI5](http://go.sap.com/developer/tutorials/hcp-gw4.html)
