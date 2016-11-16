---
title: Implementing labels using the i18n (internationalization) features of SAPUI5
description: Utilize the i18n framework in SAPUi5 to extract "hard-coded" strings in your app and maintain them in a single file.
tags: [ products>sap-hana-cloud-platform, products>sap-web-ide, topic>cloud, topic>html5, topic>mobile, topic>odata, topic>sapui5, tutorial>beginner ]
---

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** [Add labels and a new field to your app](http://go.sap.com/developer/tutorials/hcp-gw3.html)

## Next Steps
 - 

## Details

### You will learn
In the previous tutorial you added some labels to the details view by inserting a `title=”xxx”` XML snippet into an `ObjectAttribute` element. In this tutorial, you will extract those strings to the **`messageBundle.properties`** file (which you just used to insert the currency symbol) and insert a reference to them in place of the string literals in your app. The appearance of the app will not change, but having the strings in one place will make the app easier to maintain, and will enable the support of different languages and locales (which you will do as part of this series). The three labels you will work with in this tutorial are:

 * Product ID
 * Units In Stock
 * Units On Order

### Time to Complete
**< 5 min**

---

1. To begin, open the **`northwind > i18n > messageBundle.properties`** file and insert the two lines below.

    ```xml
    label_Details=Details
    label_DeliveryStatusDescription=Delivery Status
    ```

2. Following the same pattern used for the currency example: `{i18n>key}`, the three references you will use are in the **`Detail.view.xml`** file are:

    * `{i18n>label_Details}`
    * `{i18n>label_DeliveryStatusDescription}`

    Edit the `Detail.view.xml` file to replace the hard-coded title strings with the references above. The `ObjectAttribute` lines should look like this:

    ```xml
    <ObjectAttribute text="{i18n>label_Details}{DeliveryStatusDescription}"/>
    <ObjectAttribute text="{i18n>label_DeliveryStatusDescription}{Note}"/>
    ```


3. Save your changes and reload the preview tab or run the app. The app should look the same, but the implementation is will simplify the support of other languages.



## Next Step:

