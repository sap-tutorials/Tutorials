---
title: Implement Create Entity and Linking Entities in an MDK App
description: Create relationship between two OData entities.
auto_validation: true
primary_tag: products>mobile-development-kit-client
tags: [ tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>mobile-development-kit-client, software-product-function>sap-cloud-platform-mobile-services ]
time: 30
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Details
### You will learn
  - How to create relationship between parent and child entities
  - How to create a local child entity to an existing parent entity
  - How to create a local parent entity first and then a local child  entity
  - How to implement dynamic data subscription

You may clone an existing project from [GitHub repository](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/tree/master/3-Enhance-Your-First-MDK-App-with-Additional-Functionalities/4-cp-mobile-dev-kit-customer-order) to start with this tutorial.

---

For this tutorial, you will use [Mobile Services sample backend](https://developers.sap.com/tutorials/cp-mobile-dev-kit-ms-setup.html#c65de16e-1ad8-4427-a7ce-98e3dec2bf67) which has parent-child relationship setup among entities. For example, A customer can have `n` (>=0) number of sales orders.

To implement deep insert between parent and child OData entities, you need to carry out the following tasks:

*  Create a new page for creating an order
*  Add an action bar item to the new page for cancelling the current activity
*  Create a new `CreateEntity` OData action to  create a new sales order  
*  Create a new message action for displaying failure message if order creation fails
*  Create a navigation action to show order creation page from Customer detail page
*  Implement data subscription to update count value when a new sales order is created

![MDK](img_1.gif)

[ACCORDION-BEGIN [Step 1: ](Create new page for new Sales order record)]

In this step, you will create the _Create Order_ page as a **Form Cell Page**. This type of page allows for form input style changes. The page will provide only a subset of items available on the Customer Detail page. You will add the fields that will be editable by the end-user.

1. Right click the **Pages** folder | **New MDK Page** | **Form Cell Page** | **Next**.

    ![MDK](img_001.1.png)

    >A Form Cell Page is suitable for pages that generate new objects or modify existing objects. It includes a form cell container by default. You can add form sections, multiple containers or action controls to this page. Under each container section, you can add various container items.

    >You can find more details about [Form Cell page](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/65c0ed1f448346cb89fa84992dc5df9c.html).

2. Enter the Page Name `SalesOrderHeaders_Create` and click **Next** and the **Finish** on the Confirmation step.

    ![MDK](img_002.png)

3. In the Properties pane, set the **Caption** to **Create Order**.

    ![MDK](img_003.png)

4. Now, you will add the fields (like Currency Code, Net Amount, Tax Amount, Gross Amount, Life cycle status, Life cycle status name and order creation date) for creating a new sales order record by the end-user.

    In the Layout Editor, expand the **Control** | **Container Item** section.

    >You can find more details about [available controls in Form Cell page](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/a0358d7a9c3b46e0819f28ae779def24.html).

5. Drag and drop a **Simple Property** onto the Page area.

    ![MDK](img_004.gif)

6. Drag and drop five additional **Simple Property** controls and one **Date Picker** control onto the page so you have seven total controls.

    ![MDK](img_005.png)

7. Select the first **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreateCurrencyCode` |
    | `Caption` | `CurrencyCode` |
    | `isEditable`| `true` |
    | `Value`| `EUR` |

    ![MDK](img_006.gif)

    >Under **Value** property, you can set some default values.

8. Select the second **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreateNetAmount` |
    | `Caption` | `NetAmount` |
    | `isEditable`| `true` |
    | `Value`| `18.010` |

    ![MDK](img_007.png)

9. Select the third **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreateTaxAmount` |
    | `Caption` | `TaxAmount` |
    | `isEditable`| `true` |
    | `Value`| `108.010` |

    ![MDK](img_008.png)

10. Select the forth **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreateGrossAmount` |
    | `Caption` | `GrossAmount` |
    | `isEditable`| `true` |
    | `Value`| `126.02` |

    ![MDK](img_009.png)

11. Select the fifth **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreateLifeCycleStatus` |
    | `Caption` | `LifeCycleStatus` |
    | `isEditable`| `true` |
    | `Value`| `N` |

    ![MDK](img_010.png)

12. Select the sixth **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreateLifeCycleStatusName` |
    | `Caption` | `LifeCycleStatusName` |
    | `isEditable`| `true` |
    | `Value`| `New` |

    ![MDK](img_011.png)

13. Select the last control **Date Picker** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreatedate` |
    | `Caption` | `Creation Date` |
    | `isEditable`| `true` |
    | `Mode`| `datetime` |
    | `Value`| `Enter Date` |

    ![MDK](img_012.png)

    At the end, page should look like below:

    ![MDK](img_013.png)

14. Save the changes to the `SalesOrderHeaders_Create.page`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add cancel button on create customer page)]

Now, you will add a button on the Create Order page and set its `onPress` to `CloseModalPage_Cancel.action`.

1. Drag and drop an **Action Bar Item** to the upper left corner of the action bar.

    >Action Bar Item is a button that users can use to fire actions when pressed. You can add an Action Bar Item only to the Action Bar (at the top of the page).

    ![MDK](img_014.gif)

2. In the **Properties** pane, click the **link icon** to open the object browser for the **System Item** property.

    Double click the **Cancel** type and click **OK**.

    ![MDK](img_015.png)

    >System Item are predefined system-supplied icon or text. Overwrites _Text_ and _Icon_ if specified.

3. Now, you will set the `onPress` event to `CloseModalPage_Cancel.action`.

    In **Events** tab, click the **link icon** for the `OnPress` property to open the object browser.

    Double click the `CloseModalPage_Cancel.action` and click **OK** to set it as the `OnPress` Action.

    ![MDK](img_016.png)

4. Save the changes to the `SalesOrderHeaders_Create.page`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create navigation action)]

1. Now, create a navigation action that will open the `SalesOrderHeaders_Create.page` when executed.

    Right-click the **Actions** folder | **New MDK Action** | choose **MDK UI Actions** in **Category** | click **Navigation Action** | **Next**.

    ![MDK](img_016.1.png)

2. Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `NavToSalesOrderHeaders_Create` |
    | `Page To Open` | select `SalesOrderHeaders_Create.page` |
    | `ModalPage`| check this option |

    ![MDK](img_017.png)

3. Click **Next** and then **Finish** on the confirmation step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add Create Order toolbar item to customer detail page)]

You will add a toolbar item to the `Customers_Detail.page` called **Create Order**. You will link this button to the navigation action you just created. This event will open the `SalesOrderHeaders_Create.page` when the Add button is pressed by the end-user.

1. In `Customers_Detail.page`, drag and drop a **Toolbar Item** to the lower left of the page.

    ![MDK](img_018.gif)

2. In the Properties pane, set **Caption** to **Create Order**.

    ![MDK](img_019.png)

3. In the Properties pane, click the **Events** tab, click the **link icon** for the `OnPress` property to open the object browser.

    Double click the `NavToSalesOrderHeaders_Create.action` and click **OK** to set it as the `OnPress` Action.

    ![MDK](img_020.png)

4. Save the changes to the `Customers_Detail.page`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Store the created data locally)]

The next step is to store newly created record locally for an offline application or send the new record directly back to the backed for online applications. You will now create an action to map the changes received from the Create Order page to the corresponding field in the OData service. You will also show a failure message if the create action fails to save the changes.

1. Define a failure message.

    Right-click the **Actions** folder | **New MDK Action** | choose **MDK Message Actions** in **Category** | click **Message Action** | **Next**.

    ![MDK](img_020.1.png)

    Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `CreateSalesOrderHeaderEntityFailureMessage` |
    | `Type` | select `Message` |
    | `Message` | `Failed to Create Sales Order record` |
    | `Title` | `Create Sales Order` |
    | `OKCaption` | `OK` |
    | `OnOK` | `--None--` |
    | `CancelCaption` | leave it blank |
    | `OnCancel` | `--None--`|

    ![MDK](img_022.png)

    Click **Next** and then **Finish** on the Confirmation step.

2. You will create the **OData action** to create a sales order entity.

    >You can find more details about [Create Entity Action](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/9cafb37ca8ad49e6930dba857352a3e6.html).

    Right-click the **Actions** folder | **New MDK Action** | choose **MDK Data Actions** in **Category** | click **OData Action** | **Next**.

    ![MDK](img_023.png)

    Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `SalesOrderHeaders_CreateEntity` |
    | `Type` | `CreateEntity` |
    | `Service`| `SampleServiceV2` |
    | `EntitySet` | `SalesOrderHeaders` |

    ![MDK](img_024.png)

3. Click **Next**.

4. Since in `SalesOrderHeaders_Create.page`, we have defined seven properties (Currency Code, Net Amount, Tax Amount, Gross Amount, Life Cycle Status, Life Cycle Status Name and Creation Date) to be added, now in this step, we will bind them to respective UI Controls.

    Check the `CreatedAt` property (if not already) and click the **link icon** to open the object browser.

    Change the drop down in the object browser to `Controls & ClientData`, click the **Current Page** radio button.

    In the search box start typing the control name `FCCreatedate`. The list will filter down to show the matching values. Double click the **Value (Value)** entry under the `FCCreatedate` field and click **OK** to set binding.

    ![MDK](img_025.gif)

5. Repeat the above step for remaining properties: `CurrencyCode`, `GrossAmount`, `LifeCycleStatus`, `LifeCycleStatusName`, `NetAmount` and `TaxAmount`.

    ![MDK](img_026.png)

6. Under `CreateLinks`, select the checkbox for `CustomerDetails` and provide `{{#Page:-Previous/#Property:@odata.readLink}}` value under **Options** column.

    ![MDK](img_027.png)

    >`CreateLinks` allows you to link one or more entity objects to one of the target Entity's navigation property.

    >`Page:-Previous`: A target path that when resolved will return the `PageProxy` object of the page before the currently displayed page.

    >In [Mobile Services sample backend](https://developers.sap.com/tutorials/cp-mobile-dev-kit-ms-setup.html#c65de16e-1ad8-4427-a7ce-98e3dec2bf67), click **Metadata URL** and you will find `CustomerDetails` navigation property for `SalesOrderHeader` entity.

    >![MDK](img_028.png)

      Click **Next** and **Finish** on the confirmation screen. The action editor will open with the `SalesOrderHeaders_CreateEntity.action` loaded.

7. Define _Success_ and _Failure_ actions for `SalesOrderHeaders_CreateEntity.action`.

    In the action editor for the new action, expand the **Common Action Properties** and provide the below information:

    | Property | Value |
    |----|----|
    | `Success Action` | `CloseModalPage_Complete.action` |
    | `Failure Action` | `CreateSalesOrderHeaderEntityFailureMessage.action` |

    >When `SalesOrderHeaders_CreateEntity.action` gets executed successfully then `CloseModalPage_Complete.action` will be triggered or if `SalesOrderHeaders_CreateEntity.action` fails then `CreateSalesOrderHeaderEntityFailureMessage.action` will be triggered.

    ![MDK](img_029.png)

8. Save the changes to the `SalesOrderHeaders_CreateEntity.action`.

9. Now, that the `SalesOrderHeaders_CreateEntity.action` has been created, you will need to call this action when the end-user presses the **Save** button. You will add a **Save** button on the `SalesOrderHeaders_Create.page` and link it to the `SalesOrderHeaders_CreateEntity.action`.

    In `SalesOrderHeaders_Create.page`, **drag and drop** an **Action Bar Item** to the upper right corner of the action bar.

    ![MDK](img_030.gif)

    Click the **link** icon to open the object browser for the **System Item** property.

    Double click the **Save** type and click **OK**.

    ![MDK](img_032.png)

10. In the Properties pane | **Events** tab, click the **link** icon for the `OnPress` property to open the object browser, bind it to `SalesOrderHeaders_CreateEntity.action`.

    ![MDK](img_033.png)

11. Save the changes to the `SalesOrderHeaders_Create.page`.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add data subscription to Customer detail page)]

In `Customers_Detail.page` you added total number of order counts for a given customer. When a new `SalesOrder` is created, this count doesn't get updated automatically unless you navigate back and forth to this page.

> `DataSubscriptions` : it is a way to listen to data changes that when triggers should cause a UI element to redraw. If your control or section has a target, that target is automatically subscribed for data change events. Otherwise you can also explicitly subscribe to `DataSubscriptions` by specifying an entity set name or `readLink` in an array. You can find more details [here](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/6e173df205af4d0eb5c0592e94fc5cdb.html).

1. In `Customers_Detail.page`, select **Customer Orders** Object Table control. In **Properties** section, click **+** icon under **Misc** and double click `SalesOrderHeaders` and click **OK**.

    ![MDK](img_034.gif)

2. Save the changes to the `Customers_Detail.page`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Deploy and activate the application)]

1. Deploy the updated application to your MDK client.

    Right-click the `DemoSampleApp` MDK Application in the project explorer pane and select **MDK Deploy and Activate**.

    ![MDK](img_026.1.png)

2. Confirm the destination name and application id match where you want to deploy and click **Next**.

    ![MDK](img_014.1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Test the application)]

>Make sure you are choosing the right device platform tab above.

[OPTION BEGIN [Android]]

1. Re-launch the app on your device, you may asked to authenticate with passcode or Fingerprint. You will see a confirmation pop-up, tap **OK**.

2. Tap **CUSTOMER LIST**, tap one of the available customer record,  you will then navigate to Customer detail page.

3. You will see the **CREATE ORDER** option in customer detail page. Tap on it to create a new sales order.

    ![MDK](img_037.1.png)

4. As we provided default values to the properties, you may change it if required. Tap on save icon.

    ![MDK](img_038.1.png)

    Now, you will notice that count value for **SEE ALL** is increased by one as you implemented in step 6.

    ![MDK](img_039.1.png)

5. On Main page, tap **SYNC** to send local changes to the backend, a successful message will be shown.

    ![MDK](img_040.1.png)

[OPTION END]

[OPTION BEGIN [iOS]]

1. Re-launch the app on your device, you may asked to authenticate with passcode or Touch ID. When you see a confirmation pop-up, tap **OK**.

2. Tap **Customer List**, tap one of the available customer record,  you will then navigate to Customer detail page.

3. You will see the **Create Order** option in customer detail page. Tap on it to create a new sales order.

    ![MDK](img_037.png)

4. As we provided default values to the properties, you may change it if required. Tap **Save**.

    ![MDK](img_038.png)

    Now, you will notice that count value for **See All** is increased by one as you implemented in step 6.

    ![MDK](img_039.png)

5. On Main page, tap **Sync** to send local changes to the backend, a successful message will be shown.

    ![MDK](img_040.png)

[OPTION END]

Congratulations, you have successfully completed **Enhance Your First MDK App with Additional Functionalities** mission and you are now all set to [Level Up with the Mobile Development Kit](mission.mobile-dev-kit-level-up)mission.


[DONE]
[ACCORDION-END]


---
