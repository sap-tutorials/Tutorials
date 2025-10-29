---
parser: v2
author_name: Fiona Murphy
author_profile: https://github.com/MCMANUSF
primary_tag: products>sap-translation-hub
tags: [  tutorial>beginner, products>sap-translation-hub, products>sap-cloud-platform, programming-tool>sapui5 ]
---

# Replace hardcoded text in a sample Fiori app (Neo environment)
<!-- description --> Replace some texts in the Fiori app using the Suggestion Service from SAP Translation Hub

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Prepare a sample Fiori app for translation](https://developers.sap.com/tutorials/sth-prepare-fiori-app-translation.html)

## You will learn  
You will learn how to change hard coded text in a HTML5 application with the suggestion service from SAP Translation Hub.
## Time to Complete
**10 Min**.


### Locate SAP Web IDE in the cockpit


In the service catalog, locate the *SAP Web IDE* tile by searching for *Web*, and then choose the tile.

![Locate SAP Web IDE in the cockpit](sth-prep-locate-IDE.png)

### Open SAP Web IDE


Choose *Open SAP Web IDE*.

![Open SAP Web IDE]

### Open the existing project


Open the `webapp` folder structure for the already imported Fiori reference app  `sample.ApprovePurchaseOrders`.

![Open the folder structure](sth-open-folder-structure.png)

### Find code for column that requires header text


Now we can use the suggestion service to add the missing text for the column header. To do this, open the `S3_ProductDetails.view.xml` file in the `View` folder and use **`Ctrl+F`** to search for `*<Column id="linksColumn" width="12rem"`.


![Search for text]

### Add code for column header text


Replace the final closing bracket (/>) after `vAlign="Top"`* with the following code:

```
<header>
<Label id="reviewratingtitleLbl" text="" />
</header>
</Column>
```

The result should look like this:

![Replace text]

### Activate suggestion service


To use the suggestion service, you activate it by placing the cursor between the quotation marks and pressing **`Ctrl+Space`**. A small box appears, in which you start typing *Review Rating*. The suggestion service proposes some texts that you can use.

![Activate suggestion service]

Once you choose a suggested text, *Review Rating Title* in this example, the text is added with an ID and reference to the `i18n.properties` file.

![i18n reference added]

### Check the i8n.properties file


To confirm that the text and ID have been added to the `i18n.properties` file, expand the `i18n` folder and open the `i18n.properties` file.
You should see `Review Rating Title` at the end of the file.

![i18n properties file changed]

Now you know how to create a new app based on a sample Fiori app and add a text to the app using the suggestion service from SAP Translation Hub.

