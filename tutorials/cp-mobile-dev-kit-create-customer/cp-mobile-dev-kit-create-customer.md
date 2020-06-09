---
title: Create a Customer Record in an MDK App
description: Allow the user to create a customer record in an MDK app.
auto_validation: true
primary_tag: products>mobile-development-kit-client
tags: [ tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>mobile-development-kit-client, software-product-function>sap-cloud-platform-mobile-services ]
time: 25
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- You have created an MDK app as described in [Create Your First Mobile App with the Mobile Development Kit (MDK)](mission.mobile-dev-kit-get-started).

## Details
### You will learn
  - How to create a customer record with information such as name, email, phone number & date of birth
  - How to store this record locally
  - How to sync local changes with backend


You may clone an existing project from [GitHub repository](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/tree/master/2-Create-Your-First-Mobile-App-with-the-Mobile-Development-Kit) to start with this tutorial.

---

[ACCORDION-BEGIN [Step 1: ](Create new page for new customer record)]

In this step, you will create the `Customers_Create.page` as a **Form Cell Page**. This type of page allows for form input style changes. The page will provide only a subset of items available on the Customer Detail page. You will add the fields that will be editable by the end-user.

1. Right-click the **Pages** folder | **New MDK Page** | **Form Cell Page** | **Next**.

    ![MDK](img_001.1.png)

    >A Form Cell Page is suitable for pages that generate new objects or modify existing objects. It includes a form cell container by default. You can add form sections, multiple containers or action controls to this page. Under each container section, you can add various container items.

    >You can find more details about [Form Cell page](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/fiori-ui/mdk/formcell-page.html).

2. Enter the Page Name `Customers_Create` and click **Next** and the **Finish** on the Confirmation step.

    ![MDK](img_002.png)

3. In the **Properties** pane, set the **Caption** to **Create Customer**.

    ![MDK](img_003.png)

      Now, you will add the fields (like first name, last name, phone, email address & date of birth) for creating a new customer record by the end-user.

4. In the Layout Editor, expand the **Control** | **Container Item** section.

    >You can find more details about [available controls in Form Cell page](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Page/FormCell/Container.schema.html).

5. Drag and drop a **Simple Property** onto the Page area.

    ![MDK](img_005.gif)

6. Drag and drop three additional Simple Property controls onto the page so you have four total controls.

    ![MDK](img_005.png)

7. Select the first **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreateFirstName` |
    | `Caption` | `First Name` |
    | `isEditable`| `true` |
    | `PlaceHolder`| `Enter Value` |

    ![MDK](img_006.1.png)

8. Select the second **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreateLastName` |
    | `Caption` | `Last Name` |
    | `isEditable`| `true` |
    | `PlaceHolder`| `Enter Value` |

    ![MDK](img_008.png)

9. Select the third **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreatePhone` |
    | `Caption` | `Phone` |
    | `isEditable`| `true` |
    | `PlaceHolder`| `Enter Value` |

    ![MDK](img_009.png)

10. Select the last **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreateEmail` |
    | `Caption` | `Email` |
    | `isEditable`| `true` |
    | `PlaceHolder`| `Enter Value` |

    ![MDK](img_010.png)

11. Drag and drop a **Date Picker** control onto the page area for date of birth parameter.

    ![MDK](img_010.1.gif)

    Provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreateDOB` |
    | `Caption` | `DOB` |

12. Save the changes to the `Customers_Create.page`.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add cancel button on create customer page)]

Now, you will add a button on the Create Customer page and set it's `onPress` to `CloseModalPage_Cancel.action`.

1. Drag and drop an **Action Bar Item** to the upper left corner of the action bar.

    >Action Bar Item is a button that users can use to fire actions when pressed. You can add an Action Bar Item only to the Action Bar (at the top of the page).

    ![MDK](img_011.gif)

2. In the **Properties** pane, click the **link icon** to open the object browser for the **System Item** property.

3. Double click the **Cancel** type and click **OK**.

    ![MDK](img_013.1.png)

    >System Item are predefined system-supplied icon or text. Overwrites _Text_ and _Icon_ if specified.

4. Now, you will set the `onPress` event to `CloseModalPage_Cancel.action`.

    In **Events** tab, click the **link icon** for the `OnPress` property to open the object browser.

    Double click the `CloseModalPage_Cancel.action` and click **OK** to set it as the `OnPress` Action.

    ![MDK](img_015.gif)

5. Save the changes to the `Customers_Create.page`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create navigation action)]

Now, create a navigation action that will open the `Customers_Create.page` when executed.

1. Right-click the **Actions** folder | **New MDK Action** | choose **MDK UI Actions** in **Category** | click **Navigation Action** | **Next**.

2. Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `NavToCustomers_Create` |
    | `Page To Open` | select `Customers_Create.page` |
    | `ModalPage`| check this option |

    ![MDK](img_016.png)

3. Click **Next** and then **Finish** on the confirmation step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add create button to customer list page)]

You will add a button to the `Customers_List.page` called **Add**. You will link this button to the navigation action you just created. This event will open the `NavToCustomers_Create.action` when the Add button is pressed by the end-user.

1. In `Customers_List.page`, drag and drop an **Action Bar Item** to the upper right of the action bar.

    ![MDK](img_016_1.gif)

2. Click the **link icon** to open the object browser for the `SystemItem` property.

3. Double click the **Add** type and click **OK**.

4. ![MDK](img_017.png)

    In the Properties pane, click the **Events** tab, click the **link icon** for the `OnPress` property to open the object browser.

5. Double click the `NavToCustomers_Create.action` action and click **OK** to set it as the `OnPress` Action.

    ![MDK](img_019.gif)

6. Save the changes to the `Customers_List.page`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Store the created data locally)]

The next step is to store newly created record locally for an offline application or send the new record directly back to the backed for online applications. You will now create an action to map the changes received from the `Customers_Create.page` to the corresponding field in the OData service. You will also show a failure message if the create action fails to save the changes.

1. First, define a failure message.

    Right-click the **Actions** folder | **New MDK Action** | choose **MDK Message Actions** in **Category** | click **Message Action** | **Next**.

    ![MDK](img_020.png)

2. Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `CreateCustomerEntityFailureMessage` |
    | `Type` | select `Message` |
    | `Message` | `Failed to Create Customer record` |
    | `Title` | `Create Customer` |
    | `OKCaption` | `OK` |
    | `OnOK` | `--None--` |
    | `CancelCaption` | leave it blank |
    | `OnCancel` | `--None--`|

    ![MDK](img_021.png)

3. Click **Next** and then **Finish** on the Confirmation step.

    Next, you will create the **OData Create action** to update entity action that will map the changes to the correct entities in the OData service and save the changes.

    >You can find more details about [Create Entity Action](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Action/ODataService/CreateEntity.schema.html).

4. Right-click the **Actions** folder | **New MDK Action** | choose **MDK Data Actions** in **Category** | click **OData Action** | **Next**.

    ![MDK](img_023.png)

5. Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `Customers_CreateEntity` |
    | `Type` | `CreateEntity` |
    | `Service`| `SampleServiceV2` |
    | `EntitySet` | `Customers` |

    ![MDK](img_024.png)

6. Click **Next**.

7. In **Property and Create Links** step, uncheck **City**.

    Since in `Customer_Create.page`, we defined five properties (First Name, Last Name, Phone, Email & date of birth) to be added, now in this step, we will bind them to respective UI Controls.

    Check the `DateOfBirth` property and click the **link icon** to open the object browser.

    Change the drop down in the object browser to `Controls & ClientData`, click the **Current Page** radio button.

    In the search box start typing the control name `FCCreateDOB`. The list will filter down to show the matching values. Double click the **Value (Value)** entry under the `FCCreateDOB` field and click **OK** to set binding.

    ![MDK](img_026.gif)

8. Repeat the above step for remaining properties: `EmailAddress`, `FirstName`, `LastName` and `PhoneNumber`.

    ![MDK](img_027.png)

    ![MDK](img_027.1.png)

    Click **Next** and **Finish** on the confirmation screen. The action editor will open with the `Customers_CreateEntity.action` loaded.

      Next, define _Success_ and _Failure_ actions for `Customers_CreateEntity.action`.

9. In the action editor for the new action, expand the **Common Action Properties** and provide the below information:

    | Property | Value |
    |----|----|
    | `Success Action` | `CloseModalPage_Complete.action` |
    | `Failure Action` | `CreateCustomerEntityFailureMessage.action` |

    >When `Customers_CreateEntity.action` gets executed successfully then `CloseModalPage_Complete.action` will be triggered or if `Customers_CreateEntity.action` fails then `CreateCustomerEntityFailureMessage.action` will be triggered.

    ![MDK](img_028.png)

10. Save the changes to the `Customers_CreateEntity.action`.

11. Next, you will set the `OnPress` event of the _Save_ button.

    Now, that the Create action is created, you will need to call the Create action when the end-user presses the **Save** button. You will add a **Save** button on the `Customers_Create.page` and link it to the `Customers_CreateEntity.action`.

    In `Customers_Create.page`, **drag and drop** an **Action Bar Item** to the upper right corner of the action bar.

    ![MDK](img_029.png)

12. Click the **link** icon to open the object browser for the **System Item** property.

    Double click the **Save** type and click **OK**.

    ![MDK](img_030.png)

    In the Properties pane | **Events** tab, click the **link** icon for the `OnPress` property to open the object browser, bind it to `Customers_CreateEntity.action`.

    ![MDK](img_031.png)

13. Save the changes to the `Customers_Create.page`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Deploy and activate the application)]

Deploy the updated application to your MDK client.

1. Right-click the `DemoSampleApp` MDK Application in the project explorer pane and select **MDK Deploy and Activate**.

    ![MDK](img_026.1.png)

2. Confirm the destination name and application id match where you want to deploy and click **Next**.

    ![MDK](img_014.1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Test the application)]

>Make sure you are choosing the right device platform tab above.

[OPTION BEGIN [Android]]

1. Re-launch the app on your device, you may asked to authenticate with passcode or Fingerprint. You will see a _Confirmation_ pop-up, tap **OK**.

2. Tap **CUSTOMER LIST**, click **+** icon to create a new record.

    ![MDK](img_032.1.jpg)

3. Enter the values, and tap save icon.

    ![MDK](img_033.1.jpg)

    Local record gets created accordingly.

    ![MDK](img_034.1.jpg)

    Since this is an Offline application, new entry is added to the request queue of the local store which needs to be sent or uploaded to the backend explicitly.  

    >MDK base template has added a **Sync** button on main page of the app to upload local changes from device to the backend and to download the latest changes from backend to the device. Actions | Service | `UploadOffline.action` & `DownloadOffline.action`.

4. On Main page, tap **SYNC**, a successful message will be shown.

    ![MDK](img_036.1.png)

[OPTION END]

[OPTION BEGIN [iOS]]

1. Re-launch the app on your device, you may asked to authenticate with passcode or Touch ID. You will see a _Confirmation_ pop-up, tap **OK**.

2. Tap **Customer List**, click **+** icon to create a new record.

    ![MDK](img_032.png)

3. Enter the values, and tap **Save**.

    ![MDK](img_033.png)

    Local record gets created accordingly.

    ![MDK](img_034.png)

    Since this is an Offline application, new entry is added to the request queue of the local store which needs to be sent or uploaded to the backend explicitly.  

    >MDK base template has added a **Sync** button on main page of the app to upload local changes from device to the backend and to download the latest changes from backend to the device. Actions | Service | `UploadOffline.action` & `DownloadOffline.action`.

4. On `Main` page, tap **Sync**, a successful message will be shown. As Sync is pressed, `UploadOffline.action` gets triggered to upload local changes from device to the backend and on success of this call, `DownloadOffline.action` is being called.

    ![MDK](img_036.png)

[OPTION END]

You can cross verify if a record has been updated in the backend.

>Backend URL can be found in [Mobile Services Cockpit](cp-mobile-dev-kit-ms-setup).

>Mobile Applications | Native/Hybrid | click the MDK App com.sap.mdk.demo | Mobile Sample OData ESPM | click Root URL `v2` | add `/Customers`/ at the end of the URL

![MDK](img_037.png)

You have successfully created a Customer Record and you are now all set to [Delete a Customer Record in an MDK App](cp-mobile-dev-kit-delete-customer).

[VALIDATE_1]
[ACCORDION-END]

---
