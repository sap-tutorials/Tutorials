---
title: Implement Deep Linking to Another App from an MDK App
description: Open a web page or navigate to an installed app from an MDK app.
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
- **Download and install** **SAP Mobile Cards** on your [iOS](https://apps.apple.com/us/app/sap-mobile-cards/id1168110623) or [Android](https://play.google.com/store/apps/details?id=com.sap.content2go&hl=en) device

## Details
### You will learn
  - How to open SAP standard app like Mobile Cards from MDK public store client
  - How to open a web page

You may clone an existing project from [GitHub repository](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/tree/master/4-Level-Up-with-the-Mobile-Development-Kit/4-Implement-Deep-Linking-to-Another-App-from-an-MDK-App) and start directly with step 5 in this tutorial.

---

Deep links are used to send users directly to an app instead of a website or a store saving users the time and energy locating a particular page themselves – significantly improving the user experience.

If an app is already installed, you can specify a custom URL scheme (iOS Universal Links) or an intent URL (on Android devices) that opens that app. Using deep link, you can also navigate to specific events or pages, which could tie into campaigns that you may want to run.

![MDK](img_7.2.png)

>**This tutorial has been executed using public store MDK client which has out of the box functionality to open the SAP standard app like SAP Mobile Cards.
If you are building a custom version of Mobile development kit client, there you can implement deep links by specifying related custom URL scheme.**

[ACCORDION-BEGIN [Step 1: ](Create a new MDK project in SAP Business Application Studio)]

1. Launch the [Dev space](cp-mobile-bas-setup) in SAP Business Application Studio.

2. Click **Start from template** on Welcome page.

    !![MDK](img-1.2.png)

    >If you do not see the Welcome page, you can access it via **Help** menu or via **View** menu > Find Command > Welcome.

3. Select **MDK Project** and click **Start**.

    !![MDK](img-1.3.png)  

    >If you do not see the **MDK Project** option check if your Dev Space has finished loading or reload the page in your browser and try again.

4. In *Basic Information* step, select or provide the below information and click **Finish**:

    | Field | Value |
    |----|----|
    | `MDK Template Type`| Select `Empty` from the dropdown |
    | `Your Project Name` | Provide a name of your choice. `MDKDeepLink` is used for this tutorial |
    | `Your Application Name` | <default name is same as project name, you can provide any name of your choice> |
    | `Target MDK Client Version` | Leave the default selection as `MDK 6.0+ (For use with MDK 6.0 or later clients)` |    
    | `Choose a target folder` | By default, the target folder uses project root path. However, you can choose a different folder path |

    !![MDK](img-1.4.png)

    >The _MDK Empty Project_ template creates a Logout action, Close page action, rule and an empty page (`Main.page`). After using this template, you can focus on creating your pages, other actions, and rules needed for your application. More details on _MDK template_ is available in [help documentation](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/webide.html#creating-a-new-project).

5. After clicking **Finish**, the wizard will generate your MDK Application based on your selections. You should now see the `MDKDeepLink` project in the project explorer.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new rule)]

In the MDK editor, you will first create a message to display a confirmation dialog if user wants to leave the current app and then will create 2 new Rule files:

  * `OpenSAPMobileCards.js` to open SAP Mobile Cards app
  * `OpenSAPcom.js` to open `SAP.com` web page

  >You can find more details about [writing a Rule](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/development/rules.html).

1. Right-click the **Actions** folder | **MDK: New Action** | choose **MDK Message Actions** in **Category** | click **Message Action** | **Next**.

    !![MDK](img_1.1.png)

    Provide the below information:

    | Property | Value |
    |----|----|
    | `Action Name`| `Confirmation` |
    | `Type` | Select `Message` from the dropdown |
    | `Message` | `Do you want to leave the current app?` |
    | `Title` | `Confirm` |
    | `OKCaption` | `OK` |
    | `OnOK` | `--None--` |
    | `CancelCaption` | `CANCEL` |
    | `OnCancel` | `--None--` |

1. Right-click the **Rules** folder | **MDK: New Rule File** | select **Empty JS Rule**.

    !![MDK](img_2.0.png)

2. Enter the Rule name `OpenSAPMobileCards`, press `Enter`.

    !![MDK](img_2.1.png)

    Replace the generated snippet with below code.

    ```JavaScript
    export default function OpenSAPMobileCards(context) {
        // Get the Nativescript Utils Module
        const utilsModule = context.nativescript.utilsModule;
        return context.executeAction('/MDKDeepLink/Actions/Confirmation.action').then((result) => {
            if (result.data) {
                //This will open SAP Mobile Cards app
                return utilsModule.openUrl("com.sap.content2go://");
            } else {
                return Promise.reject('User Deferred');
            }
        });
    }
    ```
    !![MDK](img_2.2.png)

    >`openUrl` is a `NativeScript` API to open an URL on device. You can find more details about [this API](https://v6.docs.nativescript.org/core-concepts/utils#openurl-function).

3. Save your changes to the `OpenSAPMobileCards.js` file.

4. Create one more Rule file and name it to `OpenSAPcom`.

    Replace the generated snippet with below code.

    ```JavaScript
    export default function OpenSAPcom(context) {
        // Get the Nativescript Utils Module
        const utilsModule = context.nativescript.utilsModule;
        return context.executeAction('/MDKDeepLink/Actions/Confirmation.action').then((result) => {
            if (result.data) {
                //This will open SAP.com website
                return utilsModule.openUrl("https://www.sap.com");
            } else {
                return Promise.reject('User Deferred');
            }
        });
    }
    ```

    !![MDK](img_2.8.png)

5. Save your changes to the `OpenSAPcom.js` file.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add buttons on main page to open other apps or web pages)]

1. Next, on `Main.page`, drag and drop the **Button Table** Static Container control onto the Page.

    !![MDK](img_3.1.gif)

    >The controls available in Container section includes controls that act as containers for other controls, such as container items. A container is constant for all pages. The size of a container depends on the controls and contents included inside.  
    You can find more details about [Containers](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Page/SectionedTable/Container/ButtonTable.schema.html).

2. Now, you will add items to this Container control.

    Drag and drop the **Button** Static Item control onto the page.

    !![MDK](img_3.2.gif)

3. Repeat the above step, and drag and drop one more such **Button** Static Item control.

    !![MDK](img_3.3.png)

4. Select the first control and change its title to **Open SAP Mobile Cards**.

    !![MDK](img-3.4.png)

5. Change the title for the second button as below:

    !![MDK](img_3.5.png)

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Set onPress handler to the buttons)]

1. In this step, you will bind the JavaScript files to the `OnPress` of each button.

    In `Main.page`, select **Open SAP Mobile Cards** button. In the Properties pane, click the **Events** tab, click the **link icon** for the `Handler` property to open the object browser.

    Double-click the `OpenSAPMobileCards.js` and click **OK** to set it as the `OnPress` action.

    !![MDK](img-4.1.png)

2. Repeat the same and set the handler for **Open sap.com page** button to `OpenSAPcom.js`.

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

4. Select application from **Mobile Services**.

    !![MDK](img-5.3.png)   

5. If you want to enable source for debugging the deployed bundle, then choose **Yes**.

    !![MDK](img-4.4.png)    

    You should see **Deploy to Mobile Services successfully!** message.

    !![MDK](img-5.4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Display the QR code for onboarding the Mobile app)]

SAP Business Application Studio has a feature to display the QR code for onboarding in the Mobile client.

Click the **Application.app** to open it in MDK Application Editor and then click the **Application QR Code** icon.

!![MDK](img-6.1.png)

The On-boarding QR code is now displayed.

!![MDK](img-6.2.png)


>Leave the Onboarding dialog box open for the next step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Run the app)]

>Make sure you are choosing the right device platform tab above. Once you have scanned and on-boarded using the onboarding URL, it will be remembered. When you Log out and onboard again, you will be asked either to continue to use current application or to scan new QR code.

[OPTION BEGIN [Android]]

1. Follow [these steps](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/blob/master/Onboarding-Android-client/Onboarding-Android-client.md) to on-board the MDK client.

    After you accept app update, you will see the **Main** page with the buttons you added in previous step 3.

    ![MDK](img_7.1.png)

2. Tap **Open SAP Mobile Cards** and then tap **OK**.

    ![MDK](img_7.2.png)

    If you have already installed SAP Mobile Cards app, then MDK app will open it.

    ![MDK](img_7.3.png)

3. Tapping on **Open SAP.com page** will open SAP website.

    ![MDK](img_7.4.png)

[OPTION END]

[OPTION BEGIN [iOS]]

1. Follow [these steps](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/blob/master/Onboarding-iOS-client/Onboarding-iOS-client.md) to on-board the MDK client.

    After you accept app update, you will see the **Main** page with the buttons you added in previous step 3.

    !![MDK](img_7.7.png)

2. Tap **Open SAP Mobile Cards** and then tap **OK**.

    !![MDK](img_7.8.png)

    !![MDK](img_7.8.1.png)

    If you already installed SAP Mobile Cards app, then MDK app will open it.

    !![MDK](img_7.9.png)

3. Tapping on **Open sap.com page** will open SAP website.

    !![MDK](img_7.12.png)

    >To run this app in your branded client, you need to add Mobile Cards app URL schemes (`com.sap.content2go`)  in the info.plist.   

[OPTION END]

[VALIDATE_3]
[ACCORDION-END]

---
