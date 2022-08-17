---
title: Extend an MDK App with Customer Orders
description: Display a customer order list and its details.
auto_validation: true
primary_tag: software-product>mobile-development-kit-client
tags: [ tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, software-product>sap-business-technology-platform, software-product>mobile-development-kit-client, software-product>sap-mobile-services, software-product>sap-business-application-studio ]
time: 30
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Details
### You will learn
  - How to enhance customer details with its order information
  - How to create a new page for displaying the order details

You may clone an existing project from [GitHub repository](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/tree/master/3-Enhance-Your-First-MDK-App-with-Additional-Functionalities/3-cp-mobile-dev-kit-upload-logs) to start with this tutorial.

---

To enhance your MDK app with customer order information, you need to carry out the following tasks:

*  Create a new order list page
*  Create a new order details page
*  Create a new navigation action to the new order details page
*  Write a JavaScript logic to calculate total number of orders for a customer
*  Display top 5 orders in customer detail page
*  Add a header control to the orders grid of the customer detail page to display a text label
*  Add a footer control to the orders grid of customer detail page to show total order count

![MDK](img_1.gif)

[ACCORDION-BEGIN [Step 1: ](Create a new order list page)]

This page will display customer orders list, you will add an **Object Table** control that is used to display information (like Sales order ID, order creation date, gross amount and life cycle status name) about an object.

>You can find more details about [available controls in section page](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Page/SectionedTable/SectionedTable.schema.html).

1. In SAP Business Application Studio project, right-click the **Pages** | **MDK: New Page** | select **Section Page** | **Next**.

    !![MDK](img_1.1.png)

    >You can find more details about [section pages](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/fiori-ui/mdk/section-page.html).

2. Enter the **Page Name** `Customers_Orders` and click **Next** and then **Finish** on the confirmation step.

    !![MDK](img_1.2.png)

3. In the **Properties** pane, set the caption to **Customer Orders**.

    !![MDK](img-1.3.png)

4. Next, add an **Object Table** control to display information like sales order ID, order creation date, gross amount and life cycle status name.

    In the Layout Editor, expand the **Controls** | **Data Bound Container** group, drag and drop the **Object Table** control onto the page area.

    !![MDK](img_1.4.gif)

5. In the **Properties** pane, provide the below information:

    | Property | Value |
    |----|----|
    | `Service`| Select `SampleServiceV2.service` from the dropdown |
    | `Entity` | Select `SalesOrderHeaders` from the dropdown |
    | `QueryOptions`| `$filter=CustomerId eq '{CustomerId}'&$orderby=CreatedAt desc` |

    !![MDK](img-1.5.png)

    >For a given customer id, query expression will filter top 5 order entries returned in descending when sorted by the order creation date property.

    >!![MDK](img-1.5.gif)

6. Now, start binding Object Table properties with `SalesOrderHeaders` entity set properties.

    Provide the below information:

    | Property | Value |
    |----|----|
    | `Description`| `$(D,{CreatedAt},'','',{format:'medium'})` |
    | `DetailImage`| Remove the default value and leave it blank |
    | `Footnote` | Remove the default value and leave it blank |
    | `PreserveIconStackSpacing`| `false` |
    | `ProgressIndicator` | Remove the default value and leave it blank |
    | `Status`| `$(C,{GrossAmount},{CurrencyCode},'',{maximumFractionDigits:2,useGrouping:true})` |
    | `Subhead` | bind to `{CustomerId}` |
    | `Substatus`| bind to `{LifeCycleStatusName}` |
    | `Title`| bind to `{SalesOrderId}` |

    !![MDK](img-1.6.png)

    >`$(D,{CreatedAt},'','',{format:'medium'})` is an expression of how to format a date, end result would be like June 20, 2020. By default it will be formatted to the device's locale setting. More details on Date Formatter is available in [documentation](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/development/property-binding/i18n-formatter.html#date-formatter).

    >!![MDK](img-1.6.gif)

    >`$(C,{GrossAmount},{CurrencyCode},'',{maximumFractionDigits:2,useGrouping:true})` is an expression of how to format currency value, end result would be like €200.44. By default it will be formatted to the device's locale setting.  More details on Currency Formatter is available in [documentation](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/development/property-binding/i18n-formatter.html#currency-formatter).

    >!![MDK](img-1.7.gif)

7. In the **Search** section of the **Properties** pane, change both the **Search Enabled** property and **Barcode Scanner** property to **`true`**.

    !![MDK](img-1.7.png)

8. In the **Behavior** section of the **Properties** pane, select `DisclosureIndicator` to **`AccessoryType`** property.

    !![MDK](img-1.8.png)

9. In the `EmptySection` of the **Properties** pane, provide **`No Orders Found`** for the **Caption** property.

    !![MDK](img-1.9.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new order details page)]

This page will show related details for an order. In this page, you will add an **Object Header** control that is used to display information (like first name, last name, date of birth, email address & phone number) about the header of an object and **Static Key Value** control to display key value pair items like address, city, postal code & country.

1. In SAP Business Application Studio project, right-click the **Pages** | **MDK: New Page** | select **Section Page** | **Next**.

    !![MDK](img_1.1.png)

2. Enter the Page Name `SalesOrders_Details` and click **Next** and the **Finish** on the confirmation step.

    !![MDK](img_2.2.png)

3. In the **Properties** pane set the Caption to **Order Details**.

    !![MDK](img-2.3.png)

4. Next, you will add a **Static Key Value** container and its item **Key Value Item** to display information like sales order id, life cycle status & date of order creation name.

    >**Static Key Value** is a container that can display one or more key value pair items on a section page. In this container, you can include a Key Value Item, a simple key value cell that displays a label and a text pair. You can find more details [here](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Page/SectionedTable/Container/KeyValue.schema.html) about this container.

    In the Layout Editor, expand the **Controls** | **Static Container** section, drag and drop the **Static Key Value** control onto the page area.

    !![MDK](img-2.4.png)

5. Now, add key value item to this container. In the Layout Editor, expand the **Controls** | **Static Items** section, drag and drop the **Key Value Item** control onto the page area.

    !![MDK](img_2.5.png)

    Provide the below information:

    | Property | Value |
    |----|----|
    | `KeyName`| `Order Number` |
    | `Value` | bind to `{SalesOrderId}` |

    !![MDK](img-2.5.1.gif)

    >Make sure to select values for the mentioned properties only from `SalesOrderHeader` entity. You may find similar values from other entities.

6. Repeat the above step by adding 5 more **Key Value Item** on the page.

    !![MDK](img_2.6.png)

    Provide the below information:

    | Property | Value |
    |----|----|
    | `KeyName`| `Status` |
    | `Value` | bind to `{LifeCycleStatusName}` |

    | Property | Value |
    |----|----|
    | `KeyName`| `Created At` |
    | `Value` | `$(D,{CreatedAt},'','',{format:'medium'})` |

    | Property | Value |
    |----|----|
    | `KeyName`| `Net Amount` |
    | `Value` | `$(C,{NetAmount},{CurrencyCode},'',{maximumFractionDigits:2,useGrouping:true})` |

    | Property | Value |
    |----|----|
    | `KeyName`| `Tax Amount` |
    | `Value` | `$(C,{TaxAmount},{CurrencyCode},'',{maximumFractionDigits:2,useGrouping:true})` |

    | Property | Value |
    |----|----|
    | `KeyName`| `Total Amount` |
    | `Value` | `$(C,{GrossAmount},{CurrencyCode},'',{maximumFractionDigits:2,useGrouping:true})` |

    You should have final binding for all key value items as below:

    !![MDK](img-2.6.1.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create navigation actions)]

1. Now, you will create two **Navigation actions** that opens the **Order List** and **Order Details** page when called.

    Right-click the **Actions** folder | **MDK: New Action** | choose **MDK UI Actions** in **Category** | click **Navigation Action** | **Next**.

    Provide the below information:

    | Field | Value |
    |----|----|
    | `Action Name`| `NavToCustomers_Orders` |
    | `PageToOpen` | Select `Customers_Orders.page` from the dropdown |

    !![MDK](img-3.1.png)

    Click **Next** and then **Finish** on the confirmation step.

2. Repeat the above step and create a new navigation action.

    Provide the below information:

    | Field | Value |
    |----|----|
    | `Action Name`| `NavToSalesOrders_Details` |
    | `PageToOpen` | Select `SalesOrders_Details.page` from the dropdown |

    !![MDK](img-3.2.png)

    Click **Next** and then **Finish** on the confirmation step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Set the OnPress of the customer Orders)]

Go back to the `Customers_Orders.page` and set the `OnPress` event of the Object Table. You will link the Object Table to the `NavToSalesOrders_Details.action` so that when an end-user selects a order, the `SalesOrders_Detail.page` will open. MDK automatically passes the selected order to the details page.

In `Customers_Orders.page`, select the Object Table, **click** the 3 dots icon under the **Events** tab for the `OnPress` event to open the **Object Browser**.

Double-click the `NavToSalesOrders_Details.action` and click **OK** to set it as the `OnPress` Action.

!![MDK](img-4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Write JavaScript logic to calculate total number of orders)]

You will show a total count of orders for a customer in `Customers_Detail.page`. You will write a JavaScript logic for this calculation.

1. Right-click the **Rules** folder | **MDK: New Rule File** | select **Empty JS Rule**.

2. Enter the Rule name `Customers_OrderCount`, press `Enter`.

    !![MDK](img_5.2.png)

3. Copy and paste the following code.

    ```JavaScript
    export default function CustomerOrderCount(context) {
        //The following currentCustomer will retrieve the current customer record
        const currentCustomer = context.getPageProxy().binding.CustomerId;
        //The following expression will retrieve the total count of the orders for a given customer
        return context.count('/DemoSampleApp/Services/SampleServiceV2.service', 'SalesOrderHeaders', `$filter=CustomerId eq '${currentCustomer}'`).then((count) => {
            return count;
        });
    }    
    ```

4. Save the changes to the `Customers_OrderCount.js` file.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Display top 5 orders in customer detail page)]

1. You will add an **Object Table** compound to display top 5 orders information in the `Customers_Detail.page`.

    In the Layout Editor, expand the **Controls** | **Data Bound Container** group, drag and drop the **Object Table** control onto the page area.

    !![MDK](img-6.1.gif)

2. In the **Properties** pane, provide below information:

    |----|----|
    | `Service`| Select `SampleServiceV2.service` from the dropdown |
    | `Entity` | Select `{@odata.readLink}/SalesOrders` from the dropdown |
    | `QueryOptions`| `$top=5&$orderby=CreatedAt desc` |

    !![MDK](img-6.2.png)

    >The **`odata.readLink`** annotation contains the read URL of the entity or collection.

    > **`SalesOrders`** is a navigation property in Customer entity to `SalesOrderHeader` entity. You can find this information in OData service metadata document.

    >!![MDK](img_6.2.1.png)

    >**`QueryOptions`** expression will filter order entries returned in descending when sorted by the order creation date property.

3. Now, start binding Object Table properties with `SalesOrderHeaders` entity set properties.

    In the **Appearance** section of the **Properties** pane, provide the below information:

    |----|----|
    | `Description`| Remove the default value and leave it blank  |
    | `DetailImage`| Remove the default value and leave it blank  |
    | `Footnote` | Remove the default value and leave it blank  |
    | `PreserveIconStackSpacing`| select `false` from the dropdown |
    | `ProgressIndicator` | Remove the default value and leave it blank  |
    | `Status`| `$(C,{GrossAmount},{CurrencyCode},'',{maximumFractionDigits:2,useGrouping:true})` |
    | `Subhead` | `$(D,{CreatedAt},'','',{format:'medium'})` |
    | `Substatus`| bind to `{CurrencyCode}` |
    | `Title`| bind to `{SalesOrderId}` |

    !![MDK](img-6.3.png)

4. In the **Behavior** section of the **Properties** pane, select `DisclosureIndicator` to **`AccessoryType`** property.

    !![MDK](img-6.4.png)

5. In the `EmptySection` of the **Properties** pane, provide  **`No Customer Orders Found`** to **Caption** property.

    !![MDK](img-6.5.png)

6. You may also want to open `SalesOrders_Detail.page` when clicking on any order in `Customers_Detail.page`. For this, you will set the  `OnPress` event of the **Object Table** and link it to `NavToSalesOrders_Details.action` so that when an end-user selects a order, the Order Details page will open. MDK automatically passes the selected order to the details page.

    In `Customers_Detail.page`, select the Object Collection, click the 3 dots icon under the **Events** tab for the `OnPress` event to open the **Object Browser**.

    Double-click the `NavToSalesOrders_Details.action` and click **OK** to set it as the `OnPress` action.

    !![MDK](img-6.6.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Add Header control to  orders grid)]

1. For orders grid area, you will add a header to display some text label in `Customers_Detail.page`.

    In the Layout Editor, expand the **Controls** | **Section Bar** section, drag and drop the **Header** control above the order grid area.

    !![MDK](img-7.1.gif)

2. Provide the below information:

    | Property | Value |
    |----|----|
    | `Caption`| `Customer Orders` |

    !![MDK](img-7.2.png)

    >You can find more details about [Header control](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Page/SectionedTable/Common/Header.schema.html).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Add Footer control to show total order count)]

1. For orders grid area, you will also add a footer to display total count of orders for a customer.

    In the Layout Editor, expand the **Controls** | **Section Bar** section, drag and drop the **Footer** control above the order grid area.

    !![MDK](img-8.1.gif)

2. Provide the below information:

    | Property | Value |
    |----|----|
    | `Attribute Label`| click link icon, select **Rules** from dropdown and bind it to `Customers_OrderCount.js` |
    | `Caption`| `See All` |
    | `FooterStyle`| Select `attribute` from the dropdown |
    | `AccessoryType`| Select `DisclosureIndicator` from the dropdown |

    !![MDK](img-8.2.png)

    >You can find more details about [Footer control](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Page/SectionedTable/Common/Footer.schema.html).

3. You may also want to open **Customer Orders** page when clicking **See All**. For this, you will set the `OnPress` event of the **Footer** control and link it to `NavToCustomers_Orders.action` so that when an end-user clicks on **See All**, the **Customer Orders** page will open.

    In `Customers_Detail.page`, select the Footer control **See All**, click the 3 dots icon under the **Events** tab for the `OnPress` property to open the **Object Browser**.

    Double-click the `NavToCustomers_Orders.action` and click **OK** to set it as the `OnPress` Action.

    !![MDK](img-8.3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Deploy the application)]

Deploy the updated application to your MDK client.

1. Right-click `Application.app` and select **MDK: Deploy**.

    !![MDK](img-9.1.png)

2. Select deploy target as **Mobile & Cloud**.

    !![MDK](img-9.2.png)

    You should see success message for both deployments.

    !![MDK](img-9.3.png)

    >Alternatively, you can select *MDK: Redeploy* in the command palette (View menu>Find Command OR press Command+Shift+p on Mac OR press Ctrl+Shift+P on Windows machine), it will perform the last deployment.

    >!![MDK](img-4.3.1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Run the app)]

[OPTION BEGIN [Android]]

1. Re-launch the app on your device, you may asked to authenticate with passcode or Biometric authentication. When you see a confirmation pop-up, tap **OK**. Tap **Customer List**.

    ![MDK](img_10.1.png)

    Click a customer record, you will see the **Customer Orders** area in customer detail page and also total count of orders.

    ![MDK](img_10.2.png)

    >If you see _No Customer Orders Found_ message, try with other customer record.

2. Tapping on any order navigates to its details page.

    ![MDK](img_10.3.png)

3. Navigate back to **Customer Detail** page, tap **See All**, which navigates to the **Customer Orders** page.  

    ![MDK](img_10.4.png)

    ![MDK](img_10.5.png)

[OPTION END]

[OPTION BEGIN [iOS]]

1. Re-launch the app on your device, you may asked to authenticate with passcode or Biometric authentication. When you see a confirmation pop-up, tap **OK**.

    ![MDK](img_10.6.png)

2. Tap **Customer List** | click a customer record. You will see the **Customer Orders** area in customer detail page and also total count of orders.

    ![MDK](img_10.7.png)

    >If you see _No Customer Orders Found_ message, try with other customer record.

3. Tapping on any order navigates to its details page.

    ![MDK](img_10.8.png)

4. Navigate back to **Customer Detail** page, tap **See All**, which navigates to the **Customer Orders** page.  

    ![MDK](img_10.9.png)

    ![MDK](img_10.10.png)

[OPTION END]

[OPTION BEGIN [Web]]

1. Either click the highlighted button or refresh the web page to load the changes.

    !![MDK](img-10.11.png)

    >If you see the error `404 Not Found: Requested route ('xxxxx-dev-nsdemosampleapp-approuter.cfapps.xxxx.hana.ondemand.com') does not exist.` while accessing the web application, make sure that in your space cockpit, highlight applications are in started state.

    >!![MDK](img-10.12.png)

2. Click **Customer List** | click a customer record. You will see the **Customer Orders** area in customer detail page and also total count of orders.

    !![MDK](img_10.13.png)

    >If you see _No Customer Orders Found_ message, try with other customer record.

3. Clicking on any order navigates to its details page.

    !![MDK](img_10.14.png)

4. Navigate back to **Customer Detail** page, tap **See All**, which navigates to the **Customer Orders** page.

    !![MDK](img_10.15.png)

    !![MDK](img_10.16.png)

[OPTION END]

[VALIDATE_4]
[ACCORDION-END]

---
