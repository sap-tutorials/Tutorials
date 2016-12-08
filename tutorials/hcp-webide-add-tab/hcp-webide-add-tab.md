---
title: Add a tab and additional fields to an SAPUI5 app
description: Learn how to add an additional tab, and more data fields to an SAPUI5 app.
tags: [ products>sap-hana-cloud-platform, products>sap-web-ide, topic>cloud, topic>html5, topic>mobile, topic>odata, tutorial>intermediate ]
---

## Prerequisites
 - **Proficiency:** Intermediate
 - **Tutorials:** [Insert a currency symbol for display](http://www.sap.com/developer/tutorials/hcp-webide-insert-currency-symbol.html)

## Next Steps
 - [Calculate and display a new field in an SAPUI5 app](http://www.sap.com/developer/tutorials/hcp-webide-calculate-new-field.html)

## Details

### You will learn
When you created your initial app, SAP Web IDE template included one tab containing a few fields from the Supplier collection. In this tutorial, you will add another tab using the Web IDE Layout Editor and make a few other changes:
 * Change the icon for the Supplier tab
 * Add a new Details tab to hold other Product data
 * Create a view fragment file for the new tab
 * Add in other Supplier related data fields to the Supplier tab
 * Extract the Supplier fragment title field to the `messageBundle.properties` file

### Time to Complete
**< 5 min**

---

### Change the icon for the Supplier tab

1. Log into your HCP account and open SAP Web IDE in a Google Chrome browser.

    Open the `northwind` project folder and then the `view` folder. Right-click on `Detail.view.xml` and select **Open With > Layout Editor** (you must use Google Chrome to open the Layout Editor).

    ![Layout Editor](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_icon_1.png)

2. When the Layout Editor opens, click on the `Supplier` icon. The **Properties and Data pane** will open.

    ![Properties and Data pane](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_icon_2.png)

3. In another browser tab, open the [SAPUI5 Icon Explorer](https://openui5.hana.ondemand.com/iconExplorer.html). The Icon Explorer shows all of the icons built into the SAPUI5 library.

    ![UI5 icon explorer](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_icon_3.png)

4. Enter `supplier` into the search field, and click on the **supplier** item in the list. The supplier icon is shown in the various UI5 controls.

    ![Supplier icon in the SAPUI5 Icon Explorer](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_icon_4.png)

5. To change the current icon in your app, switch back to the Web IDE tab, and enter `sap-icon://supplier` in the **Icon** field. If you tab out of the **Icon** field, the icon will update in the **Layout Editor** view.

    ![changing the icon](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_icon_5.png)



### Add a new Details tab to hold other Product data
1. In SAPUI5, a tab is called an `IconTabFilter`. To add one to your app, click on **Container** in the palette pane. Click on the **Icon Tab Filter**, then drag and drop it next to the **Supplier** tab.

    Depending on the path you drag, the new tab may snap to the first (left) spot. You can drag it left and right to place it on the right of the **Supplier `IconTabFilter`** before letting the mouse button up. You can also release the mouse button to place it where it snaps to first, then click and drag it to the position you want.

    ![adding a new Icon Tab Filter](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_tab_1.png)

2. With the new Icon Tab Filter selected, make the following changes in the **Properties and Data pane** and save your changes.

    - **Icon:** `sap-icon://product`
    - **Text:** `Details`
    - **Count:** \< delete the 10 \>
    - **Icon Color:** `Default`

    ![Editing a Icon Tab Filter](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_tab_2.png)

### Create a view fragment file for the new tab
Content for a new tab is displayed in a view fragment file.

1. To create a new fragment file, right-click on the **view** folder, and select **New > File**.

    ![create a new file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_fragment_1.png)

2. Enter `ProductDetailInfoForm.fragment.xml` for the new file name and click **OK**.

    ![new file dialog](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_fragment_2.png)

3. The new file will open in the editor. Copy the text below, paste it into the new file, right-click in the editor and select **Beautify** (which will re-indent the XML file), then save your changes.

    ```
    <core:FragmentDefinition xmlns:core="sap.ui.core" xmlns:f="sap.ui.layout.form" xmlns:l="sap.ui.layout" xmlns="sap.m">
    <l:Grid defaultSpan="L12 M12 S12" id="productFragment" width="auto">
    <l:content>
    <f:SimpleForm columnsL="1" columnsM="1" editable="false" emptySpanL="4" emptySpanM="4" labelSpanL="3" labelSpanM="3"
layout="ResponsiveGridLayout" maxContainerCols="2" minWidth="1024" title="{i18n>tab_product_sub_title}">
    <f:content>
    <Label text="{i18n>label_CategoryID}"/>
    <Text text="{CategoryID}"/>
    <Label text="{i18n>label_ReorderLevel}"/>
    <Text text="{ReorderLevel}"/>
    <Label text="{i18n>label_Discontinued}"/>
    <Text text="{Discontinued}"/>
    </f:content>
    </f:SimpleForm>
    </l:content>
    </l:Grid>
    </core:FragmentDefinition>
    ```

    In addition to the standard XML wrapper, there are a few key items to call out in the source above:

    - In the `Grid` element, the id is set to `productFragment`. This allows you to programmatically refer to the grid in the future if needed
    - In the `SimpleForm` element, the title is set to a string in `messageBundle.properties`. You will add that entry in the next step
    - In the `f:content` element, three additional fields from the Products collection are added (Label and the string) The label strings will be added next.

    ![ProductDetailInfoForm.fragment.xml](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_fragment_3.png)

4. Open `messageBundle.properties` and replace all of the content of the file with the lines below. These are the strings for the new Product tab, as well as the Supplier tab (which you will edit next).

    ```xml
    masterTitle=Products
    detailTitle=Product Inventory Details
    notFoundTitle=Not Found
    notFoundText=The requested resource was not found
    masterListNoDataText=No entities
    masterSearchPlaceholder=Search
    masterSearchTooltip=Search for product name
    currencySymbol=€
    label_ProductID=Product ID
    label_UnitsInStock=Units In Stock
    label_UnitsOnOrder=Units On Order
    label_CategoryID=Category ID
    label_ReorderLevel=Product Reorder Level
    label_Discontinued=Discontinued status
    label_SupplierID=Supplier ID
    label_CompanyName=Company Name
    label_ContactName=Contact Name
    label_ContactTitle=Contact Title
    label_Address=Address
    label_City=City
    label_PostalCode=Postal Code
    label_Country=Country
    label_Region=Region
    label_Phone=Phone
    label_Fax=Fax
    label_HomePage=Home page
    tab_supplier_short_title=Supplier
    tab_supplier_sub_title=Supplier Details
    tab_product_short_title=Product
    tab_product_sub_title=Product Details
    ```

    Your `messageBundle.properties` file should look like this now.:

    ![messageBundle.properties file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_fragment_4.png)


### Add in other Supplier related data fields to the Supplier tab

1. Open `DetailInfoForm.fragment.xml` (which is the fragment for the **Supplier** tab). In the `f:SimpleForm` element, replace the `title="Supplier"` attribute with `title="{i18n>tab_supplier_sub_title}"` and save your change. Be sure to keep the double quotes. This replaces the string inserted by the template with a reference to one in the `messageBundle.properties` file.

    ![DetailInfoForm title](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_supplier_fields_1.png)

2. The Web IDE template inserted three of the fields in the Suppliers Collection (which is linked to the Product Collection). To add in all of the fields available, open `DetailInfoForm.fragment.xml`, and replace all of the content within the `<f:content>` element with the source below, **Beautify** and save your edits.

    ```xml
    <Label text="{i18n>label_SupplierID}"/>
    <Text text="{SupplierID}"/>
    <Label text="{i18n>label_CompanyName}"/>
    <Text text="{CompanyName}"/>
    <Label text="{i18n>label_ContactName}"/>
    <Text text="{ContactName}"/>
    <Label text="{i18n>label_ContactTitle}"/>
    <Text text="{ContactTitle}"/>
    <Label text="{i18n>label_Address}"/>
    <Text text="{Address}"/>
    <Label text="{i18n>label_City}"/>
    <Text text="{City}"/>
    <Label text="{i18n>label_PostalCode}"/>
    <Text text="{PostalCode}"/>
    <Label text="{i18n>label_Country}"/>
    <Text text="{Country}"/>
    <Label text="{i18n>label_Region}"/>
    <Text text="{Region}"/>
    <Label text="{i18n>label_Phone}"/>
    <Text text="{Phone}"/>
    <Label text="{i18n>label_Fax}"/>
    <Text text="{Fax}"/>
    <Label text="{i18n>label_HomePage}"/>
    <Text text="{HomePage}"/>
    ```

    Your `DetailInfoForm.fragment.xml` file should look like this now:

    ![DetailInfoForm fields](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_supplier_fields_2.png)

3. The last step is to add a `content` element to link the new fragment file to the Details Icon Tab Filter. Open `Detail.view.xml`, replace the text within the `items` element with the text below, **Beautify** and save your changes.

    ```xml
    <IconTabFilter icon="sap-icon://supplier" key="Supplier" text="{i18n>tab_supplier_short_title}">
    <content>
    <core:Fragment fragmentName="com.test.northwind.view.DetailInfoForm" type="XML"/>
    </content>
    </IconTabFilter>
    <IconTabFilter count="" icon="sap-icon://product" iconColor="Default" id="__filter3"  text="{i18n>tab_product_short_title}">
    <content>
    <core:Fragment fragmentName="com.test.northwind.view.ProductDetailInfoForm" type="XML"/>
    </content>
    </IconTabFilter>
   ```

    The XML you pasted in did a few things:
    - Changed the label on the **Supplier** tab to a string in the `messageBundle.properties` file (but otherwise that `IconTabFilter` tag was unchanged)
    - Changed the Product `IconTabFilter` from a self-closing element to a multi-line element (this is necessary to add in the `content` element) and set the label string
    - Added a `content` element for the Product tab with a reference to the new fragment you created

    ![content element](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_supplier_fields_3.png)

4. Save your changes and reload the preview tab or run the app. Remember, you may need to do a hard reload or clear the cache to see your changes. When the app loads, you will see the additional fields on the **Supplier** tab. Click on the **Product** tab to see the fields there.

    ![Supplier tab](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_supplier_fields_4a.png)

    ![products tab](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-add-tab/mob3-1_supplier_fields_4b.png)

5. You can now re-deploy the app to HCP so you will be able to see your changes on your mobile device.

## Next Steps
 - [Calculate and display a new field in an SAPUI5 app](http://www.sap.com/developer/tutorials/hcp-webide-calculate-new-field.html)
