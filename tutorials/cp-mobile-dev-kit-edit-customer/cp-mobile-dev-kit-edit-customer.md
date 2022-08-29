---
title: Modify a Customer Record in an MDK App
description: Allow editing of customer details in an MDK app.
auto_validation: true
primary_tag: software-product>mobile-development-kit-client
tags: [ tutorial>beginner, operating-system>ios, operating-system>android, topic>mobile, software-product>sap-business-technology-platform, software-product>mobile-development-kit-client, software-product>sap-mobile-services, software-product>sap-business-application-studio ]
time: 25
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Details
### You will learn
  - How to create a new page for modifying customer details such as name, email and phone number
  - How to store changes locally on Mobile app and sync these changes with backend
  - How to update a record in web application

---


![MDK](img_1.gif)

[ACCORDION-BEGIN [Step 1: ](Create a new page for modifying customer data)]

Regardless of whether your application is online or offline, you can allow users to modify data in the application.

For online applications, the changes are saved to the backend immediately.

For offline applications, the changes are stored locally until they are synced using an Upload action.

In this step, you will create the _Edit Customer Detail_ page as a **Form Cell Page**. This type of page allows for form input style changes. The page will provide only a subset of items available on the Customer Detail page. You will add the fields that will be editable by the end-user.

1. Right-click the **Pages** folder | **MDK: New Page** | **Form Cell Page** | **Next**.

    !![MDK](img_1.1.png)

    >A Form Cell Page is suitable for pages that generate new objects or modify existing objects. It includes a form cell container by default. You can add multiple containers or action controls to this page. Under each container section, you can add various controls.

    >You can find more details about [Form Cell page](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/fiori-ui/mdk/formcell-page.html).

2. Enter the Page Name `Customers_Edit` and click **Next** and the **Finish** on the Confirmation step.

    !![MDK](img_1.2.png)

3. In the **Properties** pane set the Caption to **Update Customer**.

    !![MDK](img-1.3.png)

4. Now, you will add the fields (like first name, last name, phone & email address) that will be editable by the end-user.

    In the Layout Editor, expand the **Controls** group.

    Drag and drop a **Simple Property** onto the Page area.

    !![MDK](img_1.4.gif)

5. Drag and drop three additional Simple Property controls onto the page so you have four total controls.

    !![MDK](img_1.5.png)

6. Select the first **Simple Property control** and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCFirstName` |
    | `Caption` | `First Name` |
    | `Value`| click the link icon and bind it to `FirstName` property of the Customer entity |

    !![MDK](img-1.6.png)

7. Select the second Simple Property control and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCLastName` |
    | `Caption` | `Last Name` |
    | `Value`| click the link icon and bind it to `LastName` property of the Customer entity |

    !![MDK](img-1.7.png)

8. Select the third Simple Property control and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCPhone` |
    | `Caption` | `Phone` |
    | `Value`| click the link icon and bind it to `PhoneNumber` property of the Customer entity |

    !![MDK](img-1.8.png)

9. Select the last Simple Property control and provide the below information:

    | Property | Value |
    |----|----|
    | `Name`| `FCEmail` |
    | `Caption` | `Email` |
    | `Value`| click the link icon and bind it to `EmailAddress` property of the Customer entity |

    !![MDK](img-1.9.png)

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Define a close page-cancel action)]

You will create a `CloseModalPage_Cancel.action` that closes the current page and cancels or interrupts any execution in process. This will be used with the cancel button on the **Edit Customer page**.

>You can close pages with the option to terminate ongoing events or wait until they are complete. Visit [documentation](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Action/ClosePage.schema.html)for more details about Close Page Action.

1. Right-click the **Actions** folder | **MDK: New Action** | choose **MDK UI Actions** in **Category** | click **Close Page Action** | **Next**.

    !![MDK](img_2.1.png)

2. Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `CloseModalPage_Cancel` |
    | `DismissModal` | Select `Canceled` from the dropdown |
    | `CancelPendingActions`| Select `true` from the dropdown |

    !![MDK](img_2.2.png)

3. Click **Next** and then **Finish** on the confirmation step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add cancel button on the Edit Customer page)]

Now, you will add a button on the Edit Customer page and set it's `onPress` to `CloseModalPage_Cancel.action` created in step 2.

1. In `Customers_Edit.page`, drag and drop an **Action Bar Item** control to the upper left corner of the action bar.

    >Action Bar Item is a button that users can use to fire actions when pressed. You can add an Action Bar Item only to the Action Bar (at the top of the page).

    !![MDK](img-3.1.gif)

2. In the Properties pane, click the **link icon** to open the object browser for the **System Item** property.

    Double-click the **Cancel** type and click **OK**.

    !![MDK](img-3.2.gif)

    >System Item are predefined system-supplied icon or text. Overwrites _Text_ and _Icon_ if specified.

3. Now, you will set the `onPress` event to `CloseModalPage_Cancel.action`.

    In **Events** tab, click the 3 dots icon for the `OnPress` property to click the **Object Browser**.

    Double-click the `CloseModalPage_Cancel.action` and click **OK** to set it as the `OnPress` Action.

    !![MDK](img-3.3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create navigation action)]

Now, create a navigation action that will open the `Customers_Edit.page` when executed.

1. Right-click the **Actions** folder | **MDK: New Action** | choose **MDK UI Actions** in **Category** | click **Navigation Action** | **Next**.

    !![MDK](img_4.1.png)

2. Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `NavToCustomers_Edit` |
    | `PageToOpen` | Select `Customers_Edit.page` from the dropdown |
    | `ModalPage`| Select `true` from the dropdown |

    !![MDK](img_4.2.png)

3. Click **Next** and then **Finish** on the confirmation step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add edit button to customer details page)]

You will add a button to the `Customers_Detail.page` called **Edit**. You will link this button to the navigation action you just created. This event will open the `Customers_Edit.page` when the Edit button is pressed by the end-user.

1. In `Customers_Detail.page`, drag and drop an **Action Bar Item** to the upper right of the action bar.

    !![MDK](img_5.1.1.png)

2. Click the **link icon** to open the object browser for the **System Item** property.

    Double-click the **Edit** type and click **OK**.

    !![MDK](img-5_2.png)

3. In the Properties pane, click the **Events** tab, click the 3 dots icon for the `OnPress` property to open the **Object Browser**.

    Double-click the `NavToCustomers_Edit.action` and click **OK** to set it as the `OnPress` Action.

    !![MDK](img-5_3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Store the updated data locally)]

The next step is to store newly updated record locally for an offline application or send the updated record directly back to the backed for online applications. You will now create an action to map the changes received from the `Customers_Edit.page` to the corresponding field in the OData service. You will also show a failure message if the update action fails to save the changes.

First, define a failure message.

1. Right-click the **Actions** folder | **MDK: New Action** | choose **MDK Message Actions** in **Category** | click **Message Action** | **Next**.

    !![MDK](img_6.1.png)

2. Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `UpdateCustomerEntityFailureMessage` |
    | `Type` | Select `Message` from the dropdown |
    | `Message` | `Failed to Save Customer Updates` |
    | `Title` | `Update Customer` |
    | `OKCaption` | `OK` |
    | `OnOK` | `--None--` |
    | `CancelCaption` | leave it blank |
    | `OnCancel` | `--None--` |

    !![MDK](img_6.2.png)

3. Click **Next** and then **Finish** on the confirmation step.

4. Next, you will define **Close Page-Complete Action** which allows the end-user to close the page and allow any execution to continue.

    >You can close pages with the option to terminate ongoing events or wait until they are complete.

    Right-click the **Actions** folder | **MDK: New Action** | choose **MDK UI Actions** in **Category** | click **Close Page Action** | **Next**.

    !![MDK](img_6.4.png)

5. Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name` | `CloseModalPage_Complete` |
    | `DismissModal` | Select `Completed` from the dropdown |
    | `CancelPendingActions` | Select `false` from the dropdown |

    !![MDK](img_6.5.png)

6. Click **Next** and then **Finish** on the confirmation step.

7. Next, you will create the **OData Update action** to update entity action that will map the changes to the correct entities in the OData service and save the changes.

    >You can find more details about [Update Entity Action](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Action/ODataService/UpdateEntity.schema.html).

    Right-click the **Actions** folder | **MDK: New Action** | choose **MDK Data Actions** in **Category** | click **OData Action** | **Next**.

    !![MDK](img_6.7.png)

8. In the **Operation and Service Selection** step, provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `Customers_UpdateEntity` |
    | `Type` | Select `UpdateEntity` from the dropdown |
    | `Service`| Select `SampleServiceV2.service` from the dropdown |
    | `EntitySet`| Select `Customers` from the dropdown |
    | `ReadLink`| click link icon and Double-click `readLink` |

    !![MDK](img-6.8.png)

    >The `readLink` is a direct reference to an individual entity set entry.

9. Click **Next**.

10. In **Property and Update Links** step, uncheck **City**.

11. Since in `Customers_Detail.page`, you defined four properties (First Name, Last Name, Phone & Email) to be edited, now, in this step, you will bind them to respective UI Controls.

    Check the `EmailAddress` property and click the **link icon** to open the object browser.

    Change the drop down in the object browser to `Controls & ClientData`, click the **Current Page** radio button.

    In the search box start typing the control name `FCEmail`. The list will filter down to show the matching values. Double-click the **Value (Value)** entry under the `FCEmail` field and click **OK** to set binding.

    !![MDK](img-6.11.gif)

12. Repeat the above step for remaining properties: `FirstName`, `LastName` and `PhoneNumber`.

    !![MDK](img-6.12.png)

    Click **Next** and **Finish** on the confirmation screen. The action editor will open with the `Customers_UpdateEntity.action` loaded.

13. Next, define _Success_ and _Failure_ actions for `Customers_UpdateEntity.action`.

    In the action editor for the new action, expand the **Common Action Properties** and provide the below information:

    | Property | Value |
    |----|----|
    | `Success Action` | Click the link icon and bind it to `CloseModalPage_Complete.action` |
    | `Failure Action` | Click the link icon and bind it to `UpdateCustomerEntityFailureMessage.action` |

    >When `Customers_UpdateEntity.action` gets executed successfully then `CloseModalPage_Complete.action` will be triggered or if `Customers_UpdateEntity.action` fails then `UpdateCustomerEntityFailureMessage.action` will be triggered.

    !![MDK](img-6.13.png)

14. Next, you will set the `OnPress` event of the _Save_ button.

    Now, that the Update action is created, you will need to call the Update action when the end-user presses the **Save** button. You will add a **Save** button on the `Customers_Edit.page` and link it to the `Customers_UpdateEntity.action`.

    In `Customers_Edit.page`, **drag and drop** an **Action Bar Item** to the upper right corner of the action bar.

    !![MDK](img_5.1.png)

    Click the **link** icon to open the object browser for the **System Item** property.

    Double-click the **Save** type and click **OK**.

    !![MDK](img-6.15.png)

15. In the Properties pane | **Events** tab, click the 3 dots icon for the `OnPress` property to open the **Object Browser**, bind it to `Customers_UpdateEntity.action`.

    !![MDK](img-6.16.png)

[VALIDATE_4]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Deploy the application)]

Deploy the updated application to your MDK client.

1. Right-click `Application.app` and select **MDK: Deploy**.

    !![MDK](img-7.1.png)

2. Select deploy target as **Mobile & Cloud**.

    !![MDK](img-7.2.png)

    You should see success message for both deployments.

    !![MDK](img-7.3.png)

    >Alternatively, you can select *MDK: Redeploy* in the command palette (View menu>Find Command OR press Command+Shift+p on Mac OR press Ctrl+Shift+P on Windows machine), it will perform the last deployment.

    >!![MDK](img-4.3.1.png)
    
[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Run the app)]

>Make sure you are choosing the right device platform tab above.

[OPTION BEGIN [Android]]

1. Re-launch the app on your device, you may asked to authenticate with passcode or Biometric authentication. You will see a _Confirmation_ pop-up, tap **OK**.

2. Tap **Customer List**, tap one of the available customer record,  you will then navigate to Customer detail page. Tap `edit` icon.

    ![MDK](img_8.2.1.png)
    ![MDK](img_8.2.2.png)

3. For example, update First Name from `Isabelle` to `Carolina`. Tap save icon.

    ![MDK](img_8.3.1.png)

    Local record gets updated accordingly.

    ![MDK](img_8.3.2.png)

4. You can cross verify if the record has been updated in the backend.

    >Backend endpoint can be found in [Mobile Services Cockpit](cp-mobile-dev-kit-ms-setup).

    >**Mobile Applications** | **Native/Hybrid** | click the MDK App **com.sap.mdk.demo** | **Mobile Connectivity** | click **Launch in Browser** icon

    >!![MDK](img-8.9.1.png)

    >It will open the URL in a new tab, remove `?auth=uaa` and add `/Customers` at the end of the URL.

    But here result is pointing to old First Name (`Isabelle`).

    ![MDK](img_8.9.png)

    Since this is an Offline application, new entry is added to the request queue of the local store which needs to be sent or uploaded to the backend explicitly.  

    >MDK base template has added a **Sync** button on main page of the app to upload local changes from device to the backend and to download the latest changes from backend to the device. Actions | Service | `UploadOffline.action` & `DownloadOffline.action`.

5. On Main page, tap **Sync**, a successful message will be shown.

    ![MDK](img_8.5.png)

Now, refresh the URL to check if record has been updated in the backend. As Sync is pressed, `UploadOffline.action` gets triggered to upload local changes from device to the backend and on success of this call, `DownloadOffline.action` is being called.

![MDK](img_8.11.png)

[OPTION END]

[OPTION BEGIN [iOS]]

1. Re-launch the app on your device, you may asked to authenticate with passcode or Biometric authentication. You will see a _Confirmation_ pop-up, tap **OK**.

2. Tap **Customer List**, tap one of the available customer record,  you will then navigate to Customer detail page. Tap **Edit**.

    ![MDK](img_8.6.1.png)
    ![MDK](img_8.6.2.png)

3. For example, updating First Name from `Isabelle` to `Carolina`. Tap **Save**.

    ![MDK](img_8.7.png)

    Local record gets updated accordingly.

    ![MDK](img_8.8.png)

4. You can cross verify if the record has been updated in the backend.

    >Backend endpoint can be found in [Mobile Services Cockpit](cp-mobile-dev-kit-ms-setup).

    >**Mobile Applications** | **Native/Hybrid** | click the MDK App **com.sap.mdk.demo** | **Mobile Connectivity** | click **Launch in Browser** icon

    >!![MDK](img-8.9.1.png)

    >It will open the URL in a new tab, remove `?auth=uaa` and add `/Customers` at the end of the URL.

    But here result is pointing to old First Name (`Isabelle`).

    ![MDK](img_8.9.png)

    Since this is an Offline application, new entry is added to the request queue of the local store which needs to be sent or uploaded to the backend explicitly.  

    >MDK base template has added a **Sync** button on main page of the app to upload local changes from device to the backend and to download the latest changes from backend to the device. Actions | Service | `UploadOffline.action` & `DownloadOffline.action`.

5. On Main page, tap **Sync**, a successful message will be shown.

    ![MDK](img_8.10.png)

Now, refresh the URL to check if record has been updated in the backend. As Sync is pressed, `UploadOffline.action` gets triggered to upload local changes from device to the backend and on success of this call, `DownloadOffline.action` is being called.

![MDK](img_8.11.png)

[OPTION END]

[OPTION BEGIN [Web]]

1. Either click the highlighted button or refresh the web page to load the changes.

    !![MDK](img-8.12.png)

    >If you see the error `404 Not Found: Requested route ('xxxxx-dev-nsdemosampleapp-approuter.cfapps.xxxx.hana.ondemand.com') does not exist.` while accessing the web application, make sure that in your space cockpit, highlight applications are in started state.

    >!![MDK](img-8.12.2.png)

2. Click **Customer List**, click one of the available customer record,  you will then navigate to Customer detail page.

    !![MDK](img_8.12.1.png)

3. Click **Edit**.

    !![MDK](img_8.13.png)

4. For example, updating First Name from `Isabelle` to `Carolina`. Click **Save**.

    !![MDK](img_8.14.png)

    Record gets updated accordingly.

    !![MDK](img_8.15.png)

4. You can cross verify if the record has been updated in the backend.

    >Backend endpoint can be found in [Mobile Services Cockpit](cp-mobile-dev-kit-ms-setup).

    >**Mobile Applications** | **Native/Hybrid** | click the MDK App **com.sap.mdk.demo** | **Mobile Connectivity** | click **Launch in Browser** icon

    >!![MDK](img-8.9.1.png)

    >It will open the URL in a new tab, remove `?auth=uaa` and add `/Customers` at the end of the URL.

[OPTION END]

[DONE]
[ACCORDION-END]

---
