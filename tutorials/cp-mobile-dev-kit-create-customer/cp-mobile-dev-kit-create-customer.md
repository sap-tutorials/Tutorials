---
title: Create a Customer Record in an MDK App
description: Allow the user to create a customer record in an MDK app.
auto_validation: true
primary_tag: software-product>mobile-development-kit-client
tags: [ tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, software-product>sap-business-technology-platform, software-product>mobile-development-kit-client, software-product>sap-mobile-services, software-product>sap-business-application-studio ]
time: 25
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- You have created an MDK app as described in [Create Your First Mobile App with the Mobile Development Kit (MDK)](mission.mobile-dev-kit-get-started).

## Details
### You will learn
  - How to create a customer record
  - How to store changes locally on Mobile app and sync these changes with backend
  - How to update a record in web application


If you didn't follow the prerequisite then you may clone an existing project from [GitHub repository](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/tree/master/2-Create-Your-First-Mobile-App-with-the-Mobile-Development-Kit) to start with this tutorial.

---

![MDK](img_1.gif)


[ACCORDION-BEGIN [Step 1: ](Create new page for new customer record)]

In this step, you will create the `Customers_Create.page` as a **Form Cell Page**. This type of page allows for form input style changes. The page will provide only a subset of items available on the Customer Detail page. You will add the fields that will be editable by the end-user.

1. Right-click the **Pages** folder | **MDK: New Page** | **Form Cell Page** | **Next**.

    !![MDK](img_1.1.png)

    >A Form Cell Page is suitable for pages that generate new objects or modify existing objects. It includes a form cell container by default. You can add form sections, multiple containers or action controls to this page. Under each container section, you can add various container items.

    >You can find more details about [Form Cell page](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/fiori-ui/mdk/formcell-page.html).

2. Enter the Page Name `Customers_Create` and click **Next** and the **Finish** on the Confirmation step.

    !![MDK](img_1.2.png)

3. In the **Properties** pane, set the **Caption** to **Create Customer**.

    !![MDK](img-1.3.png)

    Now, you will add the fields (like first name, last name, phone, email address & date of birth) for creating a new customer record by the end-user.

4. In the Layout Editor, expand the **Controls** section, drag and drop a **Simple Property** onto the Page area.

    !![MDK](img_1.4.gif)

    >You can find more details about [available controls in Form Cell page](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Page/FormCell/Container.schema.html).

5. Drag and drop three additional Simple Property controls onto the page so you have four total controls.

    !![MDK](img_1.5.png)

6. Select the first **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreateFirstName` |
    | `Caption` | `First Name` |
    | `PlaceHolder`| `Enter Value` |

    !![MDK](img-1.6.png)

7. Select the second **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreateLastName` |
    | `Caption` | `Last Name` |
    | `PlaceHolder`| `Enter Value` |

    !![MDK](img-1.7.png)

8. Select the third **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreatePhone` |
    | `Caption` | `Phone` |
    | `PlaceHolder`| `Enter Value` |

    !![MDK](img-1.8.png)

9. Select the last **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreateEmail` |
    | `Caption` | `Email` |
    | `KeyboardType` | `Email` |
    | `PlaceHolder`| `Enter Value` |

    !![MDK](img-1.9.png)

    >[`KeyboardType`](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Page/FormCell/SimpleProperty.schema.html#keyboardtype) streamlines the data entry. When entering an email address, it would display the email address keyboard in mobile clients.

10. Drag and drop a **Date Picker** control onto the page area for date of birth parameter.

    !![MDK](img_1.10.gif)

    Provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCCreateDOB` |
    | `Caption` | `DOB` |

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add cancel button on create customer page)]

Now, you will add a button on the Create Customer page and set it's `onPress` to `CloseModalPage_Cancel.action`.

1. Drag and drop an **Action Bar Item** to the upper left corner of the action bar.

    >Action Bar Item is a button that users can use to fire actions when pressed. You can add an Action Bar Item only to the Action Bar (at the top of the page).

    !![MDK](img_2.1.png)

2. In the **Properties** pane, click the **link icon** to open the **Object Browser** for the **System Item** property. Double click the **Cancel** type and click **OK**.

    !![MDK](img-2.2.png)

    >System Item are predefined system-supplied icon or text. Overwrites _Text_ and _Icon_ if specified.

3. Now, you will set the `onPress` event to `CloseModalPage_Cancel.action`.

    In **Events** tab, click the 3 dots icon for the `OnPress` property to open the object browser. Double click the `CloseModalPage_Cancel.action` and click **OK** to set it as the `OnPress` Action.

    !![MDK](img-2.3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create navigation action)]

Now, create a navigation action that will open the `Customers_Create.page` when executed.

1. Right-click the **Actions** folder | **MDK: New Action** | choose **MDK UI Actions** in **Category** | click **Navigation Action** | **Next**.

    !![MDK](img_3.1.png)

2. Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `NavToCustomers_Create` |
    | `PageToOpen` | Select `Customers_Create.page` from the dropdown |
    | `ModalPage`| Select `true` from the dropdown |

    !![MDK](img-3.2.png)

3. Click **Next** and then **Finish** on the confirmation step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add create button to customer list page)]

You will add a button to the `Customers_List.page` called **Add**. You will link this button to the navigation action you just created. This event will open the `NavToCustomers_Create.action` when the Add button is pressed by the end-user.

1. In `Customers_List.page`, drag and drop an **Action Bar Item** to the upper right of the action bar.

    !![MDK](img_4.1.png)

2. Click the **link icon** to open the object browser for the `SystemItem` property. Double click the **Add** type and click **OK**.

    !![MDK](img-4.2.png)

3. In the Properties pane, click the **Events** tab, click the 3 dots icon for the `OnPress` property to open the **Object Browser**. Double click the `NavToCustomers_Create.action` action and click **OK** to set it as the `OnPress` Action.

    !![MDK](img-4.3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Store the created data locally)]

The next step is to store newly created record locally for an offline application or send the new record directly back to the backed for online applications. You will now create an action to map the changes received from the `Customers_Create.page` to the corresponding field in the OData service. You will also show a failure message if the create action fails to save the changes.

1. Right-click the **Actions** folder | **MDK: New Action** | choose **MDK Message Actions** in **Category** | click **Message Action** | **Next**.

    !![MDK](img_5.1.png)

    Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `CreateCustomerEntityFailureMessage` |
    | `Type` | Select `Message` from the dropdown |
    | `Message` | `Failed to Create Customer record` |
    | `Title` | `Create Customer` |
    | `OKCaption` | `OK` |
    | `OnOK` | `--None--` |
    | `CancelCaption` | leave it blank |
    | `OnCancel` | `--None--`|

    !![MDK](img_5.2.png)

    Click **Next** and then **Finish** on the Confirmation step.

2. Next, you will create the **OData Create action** to update entity action that will map the changes to the correct entities in the OData service and save the changes.

    >You can find more details about [Create Entity Action](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Action/ODataService/CreateEntity.schema.html).

    Right-click the **Actions** folder | **MDK: New Action** | choose **MDK Data Actions** in **Category** | click **OData Action** | **Next**.

    !![MDK](img_5.4.png)

    Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `Customers_CreateEntity` |
    | `Type` | Select `CreateEntity` from the dropdown |
    | `Service`| Select `SampleServiceV2.service` from the dropdown |
    | `EntitySet` | Select `Customers` from the dropdown |

    !![MDK](img-5.5.png)

    Click **Next**.

    In **Property and Create Links** step, uncheck **City**.

    Since in `Customer_Create.page`, you defined five properties (First Name, Last Name, Phone, Email & date of birth) to be added, now in this step, you will bind them to respective UI Controls.

    Check the `DateOfBirth` property and click the **link icon** to open the object browser.

    Change the drop down in the object browser to `Controls & ClientData`, click the **Current Page** radio button.

    In the search box start typing the control name `FCCreateDOB`. The list will filter down to show the matching values. Double click the **Value (Value)** entry under the `FCCreateDOB` field and click **OK** to set binding.

    !![MDK](img-5.7.gif)

    Repeat the above step for remaining properties: `EmailAddress`, `FirstName`, `LastName` and `PhoneNumber`.

    !![MDK](img-5.8.png)

    !![MDK](img-5.8.1.png)

    Click **Next** and **Finish** on the confirmation screen. The action editor will open with the `Customers_CreateEntity.action` loaded.

3. Next, define _Success_ and _Failure_ actions for `Customers_CreateEntity.action`.

    In the action editor for the new action, expand the **Common Action Properties** and provide the below information:

    | Property | Value |
    |----|----|
    | `Success Action` | Click the link icon and bind it to `CloseModalPage_Complete.action` |
    | `Failure Action` | Click the link icon and bind it to `CreateCustomerEntityFailureMessage.action` |

    !![MDK](img-5.9.png)

    >When `Customers_CreateEntity.action` gets executed successfully then `CloseModalPage_Complete.action` will be triggered or if `Customers_CreateEntity.action` fails then `CreateCustomerEntityFailureMessage.action` will be triggered.

4. Next, you will set the `OnPress` event of the _Save_ button.

    Now, that the Create action is created, you will need to call the Create action when the end-user presses the **Save** button. You will add a **Save** button on the `Customers_Create.page` and link it to the `Customers_CreateEntity.action`.

    In `Customers_Create.page`, **drag and drop** an **Action Bar Item** to the upper right corner of the action bar.

    !![MDK](img_5.10.png)

    Click the **link** icon to open the object browser for the **System Item** property.

    Double click the **Save** type and click **OK**.

    !![MDK](img-5.11.png)

    In the **Events** tab, click the 3 dots icon for the `OnPress` property to open the **Object Browser**, bind it to `Customers_CreateEntity.action`.

    !![MDK](img-5.12.png)

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Deploy the application)]

Deploy the updated application to your MDK client.

1. Right-click `Application.app` and select **MDK: Deploy**.

    !![MDK](img-6.1.png)

2. Select deploy target as **Mobile & Cloud**.

    !![MDK](img-6.2.png)

    You should see success message for both deployments.

    !![MDK](img-6.3.png)

    >Alternatively, you can select *MDK: Redeploy* in the command palette (View menu>Find Command OR press Command+Shift+p on Mac OR press Ctrl+Shift+P on Windows machine), it will perform the last deployment.

    >!![MDK](img-4.3.1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Run the app)]

>Make sure you are choosing the right device platform tab above.

[OPTION BEGIN [Android]]

1. Re-launch the app on your device, you may asked to authenticate with passcode or Biometric authentication. You will see a _Confirmation_ pop-up, tap **OK**.

2. Tap **Customer List**, click **+** icon to create a new record.

    ![MDK](img_7.1.png)

3. Enter the values, and tap save icon.

    ![MDK](img_7.2.png)

    Local record gets created accordingly.

    ![MDK](img_7.3.png)

    Since this is an Offline application, new entry is added to the request queue of the local store which needs to be sent or uploaded to the backend explicitly.  

    >MDK base template has added a **Sync** button on main page of the app to upload local changes from device to the backend and to download the latest changes from backend to the device. Actions | Service | `UploadOffline.action` & `DownloadOffline.action`.

4. On Main page, tap **Sync**, a successful message will be shown.

    ![MDK](img_7.4.png)

[OPTION END]

[OPTION BEGIN [iOS]]

1. Re-launch the app on your device, you may asked to authenticate with passcode or Biometric authentication. You will see a _Confirmation_ pop-up, tap **OK**.

2. Tap **Customer List**, click **+** icon to create a new record.

    ![MDK](img_7.5.png)

3. Enter the values, and tap **Save**.

    ![MDK](img_7.6.png)

    Local record gets created accordingly.

    ![MDK](img_7.7.png)

    Since this is an Offline application, new entry is added to the request queue of the local store which needs to be sent or uploaded to the backend explicitly.  

    >MDK base template has added a **Sync** button on main page of the app to upload local changes from device to the backend and to download the latest changes from backend to the device. Actions | Service | `UploadOffline.action` & `DownloadOffline.action`.

4. On `Main` page, tap **Sync**, a successful message will be shown. As Sync is pressed, `UploadOffline.action` gets triggered to upload local changes from device to the backend and on success of this call, `DownloadOffline.action` is being called.

    ![MDK](img_7.8.png)

[OPTION END]

[OPTION BEGIN [Web]]

1. Either click the highlighted button or refresh the web page to load the changes.

    !![MDK](img-7.10.png)

    >If you see the error `404 Not Found: Requested route ('xxxxx-dev-nsdemosampleapp-approuter.cfapps.xxxx.hana.ondemand.com') does not exist.` while accessing the web application, make sure that in your space cockpit, highlight applications are in started state.

    >!![MDK](img-7.11.png)   

2. Click **Customer List**, click **+ Add** icon to create a new record.

    !![MDK](img_7.12.png)

3. Enter the values, and click **Save**.

    !![MDK](img_7.13.png)

    Local record gets created accordingly.

    !![MDK](img_7.14.png)

[OPTION END]

You can cross verify if a record has been updated in the backend.

>Backend URL can be found in [Mobile Services Cockpit](cp-mobile-dev-kit-ms-setup).

>**Mobile Applications** | **Native/Hybrid** | click the MDK App **com.sap.mdk.demo** | **Mobile Connectivity** | click **Launch in Browser** icon

>!![MDK](img-7.8.1.png)

>It will open the URL in a new tab, remove `?auth=uaa` and add `/Customers` at the end of the URL.

![MDK](img_7.9.png)

[VALIDATE_1]
[ACCORDION-END]

---
