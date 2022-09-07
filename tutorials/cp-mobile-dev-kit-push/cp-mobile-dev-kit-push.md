---
title: Enable Push Notifications in the MDK Public Store Client
description: Use the SAP Mobile Services to enable push notifications in the MDK public store client.
auto_validation: true
primary_tag: software-product>mobile-development-kit-client
tags: [ tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, software-product>sap-business-technology-platform, software-product>mobile-development-kit-client, software-product>sap-mobile-services, software-product>sap-business-application-studio]
time: 20
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- **Tutorial group:** [Set Up for the Mobile Development Kit (MDK)](group.mobile-dev-kit-setup)
- **Download and install:** **SAP Mobile Services Client** on your [iOS](https://apps.apple.com/us/app/sap-mobile-services-client/id1413653544) or [Android](https://play.google.com/store/apps/details?id=com.sap.mobileservices.client) device (If you are connecting to `AliCloud` accounts then you will need to brand your [custom MDK client](cp-mobile-dev-kit-build-client) by allowing custom domains.)

## Details
### You will learn
  - How to use MDK push register action
  - How to use predefined push configuration for the public store version of SAP Mobile Services client (MDK client)
  - How to send push notification to an MDK app

You may clone an existing metadata project from [GitHub repository](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/tree/master/4-Level-Up-with-the-Mobile-Development-Kit/6-Enable-Push-Notifications-in-the-MDK-Public-Store-Client) and start directly with step 4 in this tutorial.

---


![MDK](img_7.7.png)

[ACCORDION-BEGIN [Step 1: ](Define push notification settings in app configuration)]

1. Open the [SAP Mobile Services cockpit](cp-mobile-dev-kit-ms-setup), click **Mobile Push Notification** feature.

    !![MDK](img-1.1.png)

    >You can add **Mobile Push Notification** feature by clicking on + icon in case feature is not already assigned to the application.

2. Select **SAP Mobile Services Client** under **Predefined for** option, click **Save**.

    !![MDK](img_1.2.png)

    >Predefined push is supported only for the MDK (SAP Mobile Services client) public store client.

    >If you want to enable push notification in your custom MDK client then follow [this](cp-mobile-dev-kit-push-customclient) tutorial.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new MDK project in SAP Business Application Studio)]

1. Launch the [Dev space](cp-mobile-bas-setup) in SAP Business Application Studio.

2. Click **Start from template** on Welcome page.

    !![MDK](img-2.2.png)

    >If you do not see the Welcome page, you can access it via **Help** menu or via **View** menu > Find Command > Welcome.

3. Select **MDK Project** and click **Start**.

    !![MDK](img-2.3.png)  

    >If you do not see the **MDK Project** option check if your Dev Space has finished loading or reload the page in your browser and try again.

4. In *Basic Information* step, provide the below information and click **Finish**:

    | Field | Value |
    |----|----|
    | `MDK Template Type`| Select `Empty` from the dropdown |
    | `Your Project Name` | Provide a name of your choice. `MDK_Push` is used for this tutorial |
    | `Your Application Name` | <default name is same as project name, you can provide any name of your choice> |
    | `Target MDK Client Version` | Leave the default selection as `MDK 6.0+ (For use with MDK 6.0 or later clients)` |
    | `Choose a target folder` | By default, the target folder uses project root path. However, you can choose a different folder path |

    !![MDK](img-2.4.png)

    >More details on _MDK template_ is available in [help documentation](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/bas.html#creating-a-new-project-cloud-foundry).  

5. After clicking **Finish**, the wizard will generate your MDK Application based on your selections. You should now see the `MDK_Push` project in the project explorer.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create MDK actions to register for push notifications)]

In this step, you will create the following actions:

* **Push Notification Register action**: this will register the device with SAP Mobile Services for push notification.

* **Message actions**: these will display a message if Push Notification Register action has succeeded or failed.


1. Create a Push Notification Register action.

    Right-click the **Actions** folder | **MDK: New Action** | choose **MDK Other Actions** in **Category** | click **Push Notification Register Action** | **Next**.

    !![MDK](img_3.1.1.png)

    Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `PushRegister` |

    !![MDK](img_3.1.2.png)

    >More details on _Push Notification Action_ is available in [help documentation](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Action/PushNotificationRegister.schema.html).

    Click **Next** and then **Finish** on the confirmation step.

2. Define a success message if the Push Register Notification action is succeeded.

    Right-click the **Actions** folder | **MDK: New Action** | choose **MDK Message Actions** in **Category** | click **Message Action** | **Next**.

    !![MDK](img_3.2.1.png)

    Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `PushRegisterSuccessMessage` |
    | `Type` | Select `Message` from the dropdown |
    | `Message` | `Push Notification registered` |
    | `Title` | `Success` |
    | `OKCaption` | `OK` |
    | `OnOK` | `--None--` |
    | `CancelCaption` | leave it blank |
    | `OnCancel` | `--None--` |

    !![MDK](img_3.2.2.png)

    Click **Next** and then **Finish** on the confirmation step.


3. Define a failure message if the Push Register Notification action is failed.

    Right-click the **Actions** folder | **MDK: New Action** | choose **MDK Message Actions** in **Category** | click **Message Action** | **Next**.

    !![MDK](img_3.2.1.png)

    Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `PushRegisterFailureMessage` |
    | `Type` | Select `Message` from the dropdown |
    | `Message` | `Push Notification didn't register` |
    | `Title` | `Failure` |
    | `OKCaption` | `OK` |
    | `OnOK` | `--None--` |
    | `CancelCaption` | leave it blank |
    | `OnCancel` | `--None--` |

    !![MDK](img_3.3.2.png)

    Click **Next** and then **Finish** on the confirmation step.

4. Define _Success_ and _Failure_ actions for `PushRegister.action`.

    In the action editor for the new action, expand the **Common Action Properties** and provide the below information:

    | Property | Value |
    |----|----|
    | `Success Action` | Click the link icon to bind it to `PushRegisterSuccessMessage.action` |
    | `Failure Action` | Click the link icon to bind it to `PushRegisterFailureMessage.action`  |

    >When `PushRegister.action` gets executed successfully then `PushRegisterSuccessMessage.action` will be triggered or if `PushRegister.action` fails then `PushRegisterFailureMessage.action` will be triggered.

    !![MDK](img-3.4.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Call the push register action)]

In the step, you will set and call the Push Register Notification action when app is updated with the new metadata.

>It is up to developers how they want to call a Push Register Notification action.

In `Application.app` file, bind the `OnDidUpdate` event to `PushRegister.action`.

!![MDK](img-4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Deploy the application)]

So far, you have learned how to build an MDK application in the SAP Business Application Studio editor. Now, you will deploy the application definitions to Mobile Services to use in the Mobile client.

1. Right-click `Application.app` and select **MDK: Deploy**.

    !![MDK](img-5.1.png)

2. Select deploy target as **Mobile Services**.

    !![MDK](img-5.2.png)

3. Select **Mobile Services Landscape**.

    !![MDK](img-5.3.1.png)  


4.  Select the application from Mobile Services.

    !![MDK](img-5.3.png)

5. If you want to enable source for debugging the deployed bundle, then choose **Yes**.

    !![MDK](img-4.4.png)

    You should see **Deploy to Mobile Services successfully!** message.

    !![MDK](img-5.4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Display the QR code for app onboarding the Mobile app)]

SAP Business Application Studio has a feature to display the QR code for onboarding in the Mobile client.

Click the **Application.app** to open it in MDK Application Editor and then click the **Application QR Code** icon.

!![MDK](img-6.1.png)

The On-boarding QR code is now displayed.

!![MDK](img-6.2.png)

>Leave the Onboarding dialog box open for the next step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Run the app)]

>Make sure you are choosing the right device platform tab above. Once you have scanned and on-boarded using the onboarding URL, it will be remembered. When you Log out and on-board again, you will be asked either to continue to use current application or to scan new QR code.

[OPTION BEGIN [Android]]

1. Follow [these steps](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/blob/master/Onboarding-Android-client/Onboarding-Android-client.md) to on-board the MDK client.

2. After you accept the app update, a message should show **Push Notification registered**. Click **OK**.

    ![MDK](img_7.1.png)

3. It is time now to send the first push notification from the **SAP Mobile Services push notification feature**.

    Navigate to Mobile Services cockpit. In **Mobile Push Notification** feature, switch to **Push Registrations** tab.

    There you will find information about user registered for push notification and also details about Push providers. Identify your Device ID and click **Send Notification**.

    !![MDK](img_7.4.png)

12. In notification dialog, type a notification message and click **Send**.

    !![MDK](img-7.5.png)

    You will see a success toast message.

    !![MDK](img_7.6.png)

    After sending notification, mobile device should receive the message.

    ![MDK](img_7.7.png)

[OPTION END]

[OPTION BEGIN [iOS]]

1. Follow [these steps](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/blob/master/Onboarding-iOS-client/Onboarding-iOS-client.md) to on-board the MDK client.

2. After you accept the app update, you will notice that it will first ask permission to display notifications.

    !![MDK](img_7.8.png)

3. If push registration is successful, a message should show **Push Notification registered**. Click **OK**.

    !![MDK](img_7.9.png)

4. It is time now to send the first push notification from the **SAP Mobile Services push notification feature**.

    Navigate to Mobile Services cockpit. In **Mobile Push Notification** feature, switch to **Push Registrations** tab.

    There you will find information about user registered for push notification and also details about Push providers. Identify your Device ID and click **Send Notification**.

    !![MDK](img_7.10.png)

11. In notification dialog, type a notification message and click **Send**.

    !![MDK](img-7.5.png)

    You will see a success toast message.

    !![MDK](img_7.6.png)

    After sending notification, mobile device should receive the message.

    !![MDK](img_7.11.png)

    If you have Apple watch connected to the iPhone device, you can also see same push notification on the Apple Watch.

    !![MDK](img_7.12.png)

    >MDK supports rich push notification. MDK does not run on smart watches or as an Apple watch application.

[OPTION END]

>Once you have scanned and on-boarded using the onboarding URL, it will be remembered. When you Log out and on-board again, you will be asked either to continue to use current application or to scan new QR code.

[VALIDATE_2]
[ACCORDION-END]

---
