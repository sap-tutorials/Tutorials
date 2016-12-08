---
title: Implementing labels using the i18n (internationalization) features of SAPUI5
description: Utilize the i18n framework in SAPUi5 to extract "hard-coded" strings in your app and maintain them in a single file.
tags: [ products>sap-hana-cloud-platform, products>sap-web-ide, topic>cloud, topic>html5, topic>mobile, topic>odata, topic>sapui5, tutorial>beginner ]
---

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** [Insert a currency symbol for display](http://www.sap.com/developer/tutorials/hcp-webide-insert-currency-symbol.html)

## Next Steps
 - [Add A Tab and additional fields in an SAPUI5 app](http://www.sap.com/developer/tutorials/hcp-webide-add-tab.html)

## Details

### You will learn
In the previous tutorials you added some labels to the details view by inserting a `title=”xxx”` XML snippet into an `ObjectAttribute` element. In this tutorial, you will extract those strings to the **`messageBundle.properties`** file (which you just used to insert the currency symbol) and insert a reference to them in place of the string literals in your app. The appearance of the app will not change, but having the strings in one place will make the app easier to maintain, and will enable the support of different languages and locales (which you will do as part of this series). The three labels you will work with in this tutorial are:

 * Product ID
 * Units In Stock
 * Units On Order

### Time to Complete
**< 5 min**

---

1. To begin, open the **`northwind > i18n > messageBundle.properties`** file and insert the three lines below.

    ```xml
    label_ProductID=Product ID
    label_UnitsInStock=Units In Stock
    label_UnitsOnOrder=Units On Order
    ```

    ![messageBundle.properties](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-labels-i18n/mob2-4_1.png)

2. Following the same pattern used for the currency example: `{i18n>key}`, the three references you will use are in the **`Detail.view.xml`** file are:

    * `{i18n>label_ProductID}`
    * `{i18n>label_UnitsInStock}`
    * `{i18n>label_UnitsOnOrder}`

    Edit the `Detail.view.xml` file to replace the hard-coded title strings with the references above. Your file should look like this:

     ![Modified Detail.view.xml file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-labels-i18n/mob2-4_2.png)


3. Save your changes and reload the preview tab or run the app. The app should look the same, but the implementation is will simplify the support of other languages.

    ![Modified Detail.view.xml file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-labels-i18n/mob2-4_3.png)

## Next Step:
 - [Add A Tab and additional fields in an SAPUI5 app](http://www.sap.com/developer/tutorials/hcp-webide-add-tab.html)
