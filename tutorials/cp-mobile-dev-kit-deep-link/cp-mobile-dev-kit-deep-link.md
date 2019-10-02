---
title: Implement Deep Link in an MDK App
description: Open a web page or navigate to an installed app from an MDK app.
auto_validation: true
primary_tag: products>mobile-development-kit-client
tags: [ tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>mobile-development-kit-client, software-product-function>sap-cloud-platform-mobile-services ]
time: 25
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- - **Tutorial:** [Set Up Initial Configuration for an MDK App](cp-mobile-dev-kit-ms-setup)
- - **Tutorial:** [Enable SAP Web IDE Full-Stack](webide-multi-cloud)
- - **Tutorial:** [Enable Mobile Services App Development Tools](cp-mobile-dev-tools-setup)
- **Download and install** **SAP Mobile Services Client** on your [iOS](https://itunes.apple.com/us/app/sap-mobile-services-client/id1413653544?mt=8) or [Android](https://play.google.com/store/apps/details?id=com.sap.mobileservices.client) device
- **Download and install** [Barcode Scanner](https://play.google.com/store/apps/details?id=com.google.zxing.client.android&hl=en) (required only for Android device)
- **Download and install** **SAP Mobile Cards** on your [iOS](https://apps.apple.com/us/app/sap-mobile-cards/id1168110623) or [Android](https://play.google.com/store/apps/details?id=com.sap.content2go&hl=en) device
- **Download and install** **SAP Fiori Client** on your [iOS](https://apps.apple.com/us/app/sap-fiori-client/id824997258) or [Android](https://play.google.com/store/apps/details?id=com.sap.fiori.client&hl=en) device

## Details
### You will learn
  - How to open SAP standard apps like Mobile Cards, Fiori Client from MDK generic client
  - How to open an UI5 app running in a  Fiori Client from an MDK generic client
  - How to open a web page

---

Deep links are used to send users directly to an app instead of a website or a store saving users the time and energy locating a particular page themselves – significantly improving the user experience.

If an app is already installed, you can specify a custom URL scheme (iOS Universal Links) or an intent URL (on Android devices) that opens that app. Using deep link, you can also navigate to specific events or pages, which could tie into campaigns that you may want to run.

![MDK](img_1.gif)

>**This tutorial has been executed using public store MDK client which has out of the box functionality to open SAP standard apps like SAP Mobile Cards and SAP Fiori Client.
If you are building a custom version of Mobile development kit client, there you can implement deep links by specifying related custom URL scheme.**

[ACCORDION-BEGIN [Step 1: ](Set up the application foundation)]

Make sure that you have completed required pre-Prerequisites mentioned in this tutorial.

This step includes creating the Mobile Development Kit project in the Editor.

Launch the SAP Web IDE and select the **MDK perspective** by clicking on the icon in the left panel.

Right click on Workspace folder and select **New** | **MDK Empty Project**.

![MDK](img_001.png)

>The _MDK Empty Project_ template creates a Logout action, Close page action, rule and an empty page (`Main.page`). After using this template, you can focus on creating your pages, other actions, and rules needed for your application.

>More details on _MDK template_ is available in
[help documentation](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/cfd84e66bde44d8da09f250f1b8ecee6.html).

Enter the Project Name as `MDKDeepLink` and click **Next**.

![MDK](img_002.png)

Leave the default values in _Application Creation_ step as it is, click **Finish**.

After clicking Finish, the wizard will generate your MDK Application based on your selections. You should now see the `MDKDeepLink` project in the project explorer.

![MDK](img_003.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new rule)]

In the MDK editor, you will create 4 new JavaScript files:

* `OpenSAPMobileCards.js` to open SAP Mobile Cards app
* `OpenSAPFioriClient.js` to open SAP Fiori Client app
* `OpenUI5.js` to open an UI5 app running in SAP Fiori Client app
* `OpenSAPcom.js` to open `SAP.com` web page

>You can find more details about [writing a Rule](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/ef1e3404ff5f4ca68676acbda10e4bd0.html).

Right click on the **Rules** folder | **New** | **File**.

![MDK](img_004.png)

Enter the file name `OpenSAPMobileCards.js`, click **OK**.

Copy and paste the following code.

```JavaScript
export default function openurl(clientAPI) {
// Get the Nativescript UI Dialogs Module
const dialogs = clientAPI.nativescript.uiDialogsModule;
// Get the Nativescript Utils Module
const utilsModule = clientAPI.nativescript.utilsModule;
return dialogs.confirm("Do you want to leave the current app?").then((result) => {
    if (result === true) {
        //This will open SAP Mobile Cards app
   return utilsModule.openUrl("com.sap.content2go://").then(	                
            (success) => Promise.resolve(success),
            (failure) => Promise.reject('The requested app or page is not available ' + failure));
    } else {
        return Promise.reject('User Deferred');
    }
});
}
```
![MDK](img_005.png)

>`openUrl` is a `NativeScript` API to open an URL on device. You can find more details about [this API](https://docs.nativescript.org/core-concepts/utils#openurl-function).

Repeat the above step and create another new file:

Enter the file name `OpenSAPFioriClient.js`, click **OK**.

Copy and paste the following code.

```JavaScript
export default function openurl(clientAPI) {
// Get the Nativescript UI Dialogs Module
const dialogs = clientAPI.nativescript.uiDialogsModule;
// Get the Nativescript Utils Module
const utilsModule = clientAPI.nativescript.utilsModule;
return dialogs.confirm("Do you want to leave the current app?").then((result) => {
    if (result === true) {
        //This will open SAP Fiori Client App
    return utilsModule.openUrl("com.sap.fiori.client.xcallbackurl://x-callback-url").then(               
            (success) => Promise.resolve(success),
            (failure) => Promise.reject('The requested app or page is not available ' + failure));
    } else {
        return Promise.reject('User Deferred');
    }
});
}
```

![MDK](img_006.png)

Repeat the above step and create another new file:

Enter the file name `OpenUI5.js`, click **OK**.

Copy and paste the following code.

```JavaScript
export default function openurl(clientAPI) {
// Get the Nativescript UI Dialogs Module
const dialogs = clientAPI.nativescript.uiDialogsModule;
// Get the Nativescript Utils Module
const utilsModule = clientAPI.nativescript.utilsModule;
return dialogs.confirm("Do you want to leave the current app?").then((result) => {
    if (result === true) {
        //This will open Software Downloads app running in SAP Fiori Client
return utilsModule.openUrl("com.sap.fiori.client.xcallbackurl://x-callback-url/openFioriUrl?url=https://launchpad.support.sap.com/#/softwarecenter").then(                  
            (success) => Promise.resolve(success),
            (failure) => Promise.reject('The requested app or page is not available ' + failure));
    } else {
        return Promise.reject('User Deferred');
    }
});
}
```

![MDK](img_007.png)

Create one more file and name it to `OpenSAPcom.js`.

Copy and paste the following code.

```JavaScript
export default function openurl(clientAPI) {
// Get the Nativescript UI Dialogs Module
const dialogs = clientAPI.nativescript.uiDialogsModule;
// Get the Nativescript Utils Module
const utilsModule = clientAPI.nativescript.utilsModule;
return dialogs.confirm("Do you want to leave the current app?").then((result) => {
    if (result === true) {
        //This will open SAP.com website
return utilsModule.openUrl("https://www.sap.com").then( 	                
            (success) => Promise.resolve(success),
            (failure) => Promise.reject('The requested app or page is not available ' + failure));
    } else {
        return Promise.reject('User Deferred');
    }
});
}
```

Save the changes.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add buttons on main page to open other apps or web pages)]

Next, on **Main page**, drag and drop the **Section Button Table** Container control onto the Page.

![MDK](img_009.gif)

>The controls available in Container section includes controls that act as containers for other controls, such as container items. A container is constant for all pages. The size of a container depends on the controls and contents included inside.  
You can find more details about [Containers](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/65c0ed1f448346cb89fa84992dc5df9c.html).

Now, you will add items to this Container control.

Drag and drop the **Section Button** Container Item control onto the page.

![MDK](img_010.gif)

Repeat the above step, and drag and drop 3 more such **Section Button** Container Item controls.

![MDK](img_011.png)

Select the first control and change its title to **Open SAP Mobile Cards**.

![MDK](img_012.png)

Repeat the above step and change the title for other controls as below:

![MDK](img_013.png)

Save the changes to `Main.page`.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Set onPress handler to the buttons)]

In this step, you will bind the JavaScript files to the `OnPress` of each button.

In `Main.page`, select **Open SAP Mobile Cards** button. In the Properties pane, click the **Events** tab, click the **link icon** for the `Handler` property to open the object browser.

Double Click on the `OpenSAPMobileCards.js` and click **OK** to set it as the `OnPress` action.

![MDK](img_014.gif)

Repeat the same and do the following:

Set the handler for **Open SAP Fiori Client** button to `OpenSAPFioriClient.js`.

Set the handler for **Open UI5 App** button to `OpenUI5.js`.

Set the handler for **Open SAP.com page** button to `OpenSAPcom.js`.

Save the changes to `Main.page`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Deploy and activate the application)]

So far, you have learned how to build an MDK application in the SAP Web IDE editor. Now, we deploy this application definition to Mobile Services.

Right click on the `MDKDeepLink` MDK Application in the project explorer pane and select **MDK Deploy and Activate**.

![MDK](img_015.png)

Let the default configuration as it is and click **Next**.

![MDK](img_016.png)

>_Filter Files_ will be filtered and ignored in web packing process.

>_Externals_ are the list of NPM modules that are part of the MDK Client application and should not be validated in the bundle.

Click the drop down for Destination Name and select the `mobileservices_cf` destination, you will find list of existing application IDs, select the one you have chosen while creating the project.

Click **Next** to finish the deployment from SAP Web IDE.

You should see **Application deployed successfully** message in console log.

![MDK](img_018.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Populate the QR code for app on-boarding)]

SAP Web IDE has a feature to generate QR code for app on-boarding.

Right click on the `MDKDeepLink` MDK Application in the project explorer pane and select **MDK Deploy and Activate**.

![MDK](img_015.png)

Let the default configuration as it is and click **Next**.

![MDK](img_016.png)

Click on the **QR-code icon** to populate the QR-code for app on-boarding.

![MDK](img_020.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Run the app in MDK client)]

>Make sure you are choosing the right device platform tab above.

The MDK client receives deployed metadata definitions as a bundle.

[OPTION BEGIN [Android]]

On Android, the camera app does not support scanning the QR-code. As alternative you can use the [Barcode scanner app](https://play.google.com/store/apps/details?id=com.application_4u.qrcode.barcode.scanner.reader.flashlight&hl=en_IN) to scan it.

Open the Barcode scanner app and start scanning the QR code showing in SAP Web IDE.

Tap **Open browser**.  It will open SAP Mobile Services Client app.

![MDK](img_013.1.jpg)

Tap **GET STARTED** to connect MDK client to SAP Cloud Platform.

![MDK](img_031.1.jpg)

Enter Email address and password to login to SAP Cloud Platform and tap **Log On** to authenticate.

![MDK](img_017.1.png)

Tap **AGREE** on `End User License Agreement`.

![MDK](img_033.jpg)

Choose a passcode with at least 8 characters for unlocking the app and click **NEXT**.

![MDK](img_034.png)

Confirm the passcode and tap **DONE**.

![MDK](img_035.jpg)

Optionally, you can enable fingerprint to get faster access to the app data.

![MDK](img_036.jpg)

Tap **OK**.

![MDK](img_037.jpg)

The MDK client receives deployed metadata definitions as a bundle.

Now, you will see **Main** page with the buttons you added in previous step 3.

![MDK](img_038.1.jpg)

Tap **OPEN SAP MOBILE CARDS** and then tap **OK**.

![MDK](img_039.jpg)

If you have already installed SAP Mobile Cards app, then MDK app will open it.

![MDK](img_040.jpg)

Tapping on **OPEN SAP FIORI CLIENT** will open **SAP Fiori Client** app.

>If you have access to any Fiori app or Fiori Launchpad page for example [SAP Support Launchpad](https://launchpad.support.sap.com), then enter that URL in your Fiori Client app.

![MDK](img_041.jpg)

Tapping on **OPEN UI5 APP** will open a specific app running in SAP Fiori Client as per `OpenUI5.js` file.

In below screenshot, there is one Software Downloads UI5 app part of SAP Support Launchpad.

![MDK](img_042.jpg)

Tapping on **OPEN SAP.COM PAGE** will open SAP website.

![MDK](img_043.jpg)

[OPTION END]

[OPTION BEGIN [iOS]]

On iPhone, open your camera app and start scanning the QR code, as shown below.

![MDK](img_013_1.png)

Tap the toast message to launch **SAP Mobile Services Client**. It will open SAP Mobile Services Client app.

Tap **Start** to connect MDK client to SAP Cloud Platform.

![MDK](img_021.png)

Enter Email address and password to login to SAP Cloud Platform and tap **Log On** to authenticate.

![MDK](img_022.png)

Tap **Agree** on `End User License Agreement`.

![MDK](img_023.png)

Choose a passcode with at least 8 characters for unlocking the app and tap **Next**.

![MDK](img_024.png)

Confirm the passcode and tap **Done**.

![MDK](img_025.png)

Optionally, you can enable Touch ID to get faster access to the app data.

![MDK](img_026.png)

Tap **OK**.

![MDK](img_027.png)

Now, you will see **Main** page with the buttons you added in previous step 3.

![MDK](img_028.png)

Tap **Open SAP Mobile Cards** and then tap **OK**.

![MDK](img_028.1.png)

If you already installed SAP Mobile Cards app, then MDK app will open it.

![MDK](img_028.2.png)

Tapping on **Open SAP Fiori Client** will open **SAP Fiori Client** app.

>If you have access to any Fiori app or Fiori Launchpad page for example [SAP Support Launchpad](https://launchpad.support.sap.com), then enter that URL in your Fiori Client app.

![MDK](img_028.3.png)

Tapping on **Open UI5 App** will open a specific app running in SAP Fiori Client as per `OpenUI5.js` file.

In below screenshot, there is one Software Downloads UI5 app part of SAP Support Launchpad.

![MDK](img_028.4.png)

Tapping on **Open SAP.com page** will open SAP website.

![MDK](img_028.5.png)

[OPTION END]

[DONE]
[ACCORDION-END]

---
