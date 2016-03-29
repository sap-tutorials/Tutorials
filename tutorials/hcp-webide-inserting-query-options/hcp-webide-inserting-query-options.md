---
title: Inserting OData query options into your SAPUI5 app
description: Learn how to insert OData query options to your SAPUI5 app
tags: [ tutorial:product/hcp,tutorial:product/mobile,tutorial:product/sap_ui5,tutorial:product/sapui5_web_ide,tutorial:technology/odata ]
---

## Prerequisites  
 - **Proficiency:** Beginner | Intermediate | Expert
 - **Tutorials:** [An Open Data Protocol (OData) primer for developers](http://go.sap.com/developer/tutorials/hcp-webide-odata-primer.html)

## Next Steps
 - Manually creating a data model to use in SAP Web IDE's Mock Data server (coming soon)

## Details
### You will learn  
The previous tutorial introduced a number of OData query options that developers can use to offload logic from the client to the OData service. In this tutorial, you will learn how to incorporate one or more query options into your SAPUI5 application.

### Time to Complete
**10 Min**.

---

1. Log into your [HCP account](https://account.hanatrial.ondemand.com) and open SAP Web IDE tab in a Google Chrome browser. Ensure that your latest code has been committed to your git repository. If so, you should have green dots next to the folders and files in your project (as shown below). If not, [commit your files to Git now](http://go.sap.com/developer/tutorials/hcp-webide-commit-git.html).

     ![Project folder](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-inserting-query-options/mob3-5_1.png)
          
2. Open the **northwind** project folder and then the **view** folder. Double-click on `Master.view.xml`. Locate the `items` attribute in the `List` element.

     The `items` attribute points to the primary collection (Products) of the Northwind OData service which is displayed in the “master” list of the app. Since this is the last part of the OData query, you will insert any query options in this attribute.
 
     You may want to add some line breaks in your code to match the image below. It will make it simpler to change the `items` attribute in the steps below.
     
     ![Items attribute](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-inserting-query-options/mob3-5_2.png)
     
3. If you run your app, you will notice that the OData results are returned sorted by the `ProductID` field. This default behaviour is driven by the `PropertyRef` in the `Key` element of the data model (`ProductID` in this case). You can see the key in the Northwind [metadata document](http://services.odata.org/V2/Northwind/Northwind.svc/$metadata) or your `northwind/model/metadata.xml` file.

    ![Key element](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-inserting-query-options/mob3-5_3.png)

4. It is easier for a user to find a given product by name if the list is in alphabetical order. To implement this through a query option, append the `items` attribute in `Main.view.xml` with `?$orderby=ProductName`. The `?` is used as the first separator in an HTTP URL. The rest of the query option should be familiar to you after going through the previous tutorial.

    The items attribute should be:

    ```xml
    items="{/Products?$orderby=ProductName}"
    ```
    
    ![modified items attribute](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-inserting-query-options/mob3-5_4.png)
    
5. Save your change and run the app ( remember to empty the cache and reload if necessary). You will see that the master list is now sorted by product name.

    ![alphanumberically ordered](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-inserting-query-options/mob3-5_5.png)
 
6. Adding additional query options is almost as easy as copying in the characters from a URL (if you are trying out the query in a browser window first). There are two items to be aware of when going beyond one query option in your app:
     - The query option strings you are adding are being inserted into an XML document
     - While the ampersand character `&` is is used to separate options in an HTTP URL, it is not allowed in XML as a stand-alone character

     The good news is that there are five predefined entities in XML, one of which is for the ampersand. The predefined entities follow the pattern: `&name;`. The five predefined entities in XML are:
     
     
     Name    |  Character  | XML Format | Description
     :-------| :-----------| :--------- | :-----------
     `quot`  | `"`         | `&quot;`   | Double quotation mark
     `amp`   | `&`         | `&amp;`    | Ampersand
     `apos`  | `'`         | `&apos;`   | Apostrophe
     `lt`    | `<`         | `&lt;`     | Less-than sign
     `gt`    | `>`         | `&gt;`     | Greater-than sign
     
     
7. To sort by the product name *and* exclude products that are out of stock, the query option would need to be formatting like the string below (the `&` in the URL is replaced with `&amp;`). Modify the `items` attribute to 

    ```xml
    items="{/Products?$orderby=ProductName&amp;$filter=UnitsInStock gt 0}"
    ```
    
    Save your change, switch to the app preview tab and select Hard Reload or Empty Cache and Hard Reload. Your app should now look like this:
     
    ![items update](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-inserting-query-options/mob3-5_7a.png)
 
    ![ordered and filtered](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-inserting-query-options/mob3-5_7b.png)
    
8. Suppose there is a need to support a particular sales team in operating unit that only sells products from specific suppliers, you can add in a second logical test. Modify the items attribute as shown below to exclude products from Supplier IDs 1 and 2:

    ```xml
    items="{/Products?$orderby=ProductName&amp;$filter=UnitsInStock gt 0 and SupplierID gt 2}"
    ```
    
    Save your change, switch to the app preview tab and select Hard Reload or Empty Cache and Hard Reload. Your view file and app should now look like this:
     
    ![items update](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-inserting-query-options/mob3-5_8a.png)
     
    ![ordered and filtered, exluding two suppliers](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-inserting-query-options/mob3-5_8b.png)
    
9. To rollback your edits, you can return the items attribute by either editing the line as shown below, or by using Git. To use Git, click on the **Git** icon in Web IDE, click the arrow under **Discard** in the **Git pane** and click **OK** on the Git confirmation dialog box. After clicking the discard arrow, the green dot (indicating the version in your project folder is the same as that in Git) will reappear next to `Master.view.xml` (as well as the **view** and **northwind** folders).

    ```xml
    items="{/Products}"
    ```
    
    ![Discard changes using git](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-inserting-query-options/mob3-5_9.png)



## Next Steps
 - Manually creating a data model to use in SAP Web IDE's Mock Data server (coming soon)
