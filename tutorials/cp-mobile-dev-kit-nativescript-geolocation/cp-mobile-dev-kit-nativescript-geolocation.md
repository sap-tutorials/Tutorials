---
title: Add NativeScript Plugins in an MDK App
description: Build and run the Mobile Development Kit client with a non-visual extension functionality for Android and iOS platforms.
auto_validation: true
primary_tag: software-product>mobile-development-kit-client
tags: [ tutorial>advanced, operating-system>ios, operating-system>android, topic>mobile, software-product>sap-business-technology-platform, software-product>mobile-development-kit-client, software-product>sap-mobile-services, software-product>sap-business-application-studio ]
time: 35
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- **Tutorial**: [Set Up for the Mobile Development Kit (MDK)](group.mobile-dev-kit-setup)
- **Tutorial**: [Build Your Mobile Development Kit Client Using MDK SDK](cp-mobile-dev-kit-build-client) (Steps 1 to 3)

## Details
### You will learn
  - How to reference the `NativeScript` Geolocation plugin from a rule
  - How to add a `NativeScript` plugin to your branded MDK client
  - How to build a Mobile development kit client for iOS & Android and connect to SAP Mobile application
  - How to capture the device's current location

You may clone an existing metadata project from [GitHub repository](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/tree/main/6-Create-Extension-Controls-in-Mobile-Development-Kit-Apps/2-Add-NativeScript-Plugin-in-an-MDK-App) and start directly with step 4 in this tutorial.

---

To extend the functionality, or customize the look and feel, and behavior of your client app, you can use the existing `NativeScript` plugins like nativescript-geolocation, nativescript-nfc etc. , add this to the client and reference it from a rule.

In this tutorial, you will use the existing `NativeScript` plugin nativescript-geolocation to capture the device location: latitude & longitude.

![MDK](img_8.2.png)

[ACCORDION-BEGIN [Step 1: ](Create a new MDK project in SAP Business Application Studio)]

1. Launch the [Dev space](cp-mobile-bas-setup) in SAP Business Application Studio.

2. Click **Start from template** on Welcome page.

    !![MDK](img-1.2.png)

    >If you do not see the Welcome page, you can access it via **Help** menu or via **View** menu > Find Command > Welcome.

3. Select **MDK Project** and click **Next**.

    !![MDK](img-1.3.png)

    >If you do not see the **MDK Project** option check if your Dev Space has finished loading or reload the page in your browser and try again.

4. In *Basic Information* step, select or provide the below information and click **Finish**:

    | Field | Value |
    |----|----|
    | `MDK Template Type`| Select `Empty` from the dropdown |
    | `Your Project Name` | Provide a name of your choice. `MDK_Geolocation` is used for this tutorial |
    | `Your Application Name` | <default name is same as project name, you can provide any name of your choice> |
    | `Target MDK Client Version` | Leave the default selection as `MDK 6.0+ (For use with MDK 6.0 or later clients)` |  
    | `Choose a target folder` | By default, the target folder uses project root path. However, you can choose a different folder path |      

    !![MDK](img-1.4.png)

    >The _MDK Empty Project_ template creates a Logout action, Close page action, rule and an empty page (`Main.page`). After using this template, you can focus on creating your pages, other actions, and rules needed for your application. More details on _MDK template_ is available in [help documentation](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/webide.html#creating-a-new-project).

5. After clicking **Finish**, the wizard will generate your MDK Application based on your selections. You should now see the `MDK_Geolocation` project in the project explorer.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Create a new rule to capture the device location)]

In the MDK editor, you will create a new JavaScript file called `GetCoordinates.js` to capture the device location: latitude & longitude.

>You can find more details about [writing a Rule](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/development/rules.html).

1. Right-click the **Rules** folder | **MDK: New Rule File** | select **Empty JS Rule**.

    !![MDK](img_2.1.png)

2. Enter the Rule name `GetCoordinates`, click **Next** and then **Finish** on the confirmation step.

    !![MDK](img_2.2.png)

3. Replace the generated snippet with below code.

    ```JavaScript
    import * as geolocation from "@nativescript/geolocation";
    import { CoreTypes } from "@nativescript/core";
    export default async function GetCoordinates(context) {
        var logger = context.getLogger();
        console.log("Current Log Level: " + logger.getLevel());
        // check if geolocation is not enabled
            var locationIsEnabled = await geolocation.isEnabled();
        if (!locationIsEnabled) {
            // request for the user to enable it
            await geolocation.enableLocationRequest();
        }
        // Get current location with high accuracy
        return geolocation.getCurrentLocation({
            desiredAccuracy: CoreTypes.Accuracy.high, //This will return the finest location available
            updateDistance: 5, //Update distance filter in meters.
            timeout: 11000 //How long to wait for a location in ms.
        }).then(function (loc) {
            if (loc) {
                console.log(loc);
                console.log('\nCurrent Location: (' + loc.latitude + ',' + loc.longitude + ')');
                logger.log(loc.toString());
                var locMessage = '(' + "Latitude:" + loc.latitude + ',' + "Longitude:" + loc.longitude + ')';
                logger.log('Current Location: ' + locMessage, 'INFO');
                return locMessage;
            }
        }, function (e) {
            logger.log(e.message, 'ERROR');
        });
    }
    ```

  4. Save your changes to the `GetCoordinates.js` file.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Display the coordinates on a page)]

You will add this registered control in the `Main.page`.

  1. Click the `Main.page`, drag & drop **Static Key Value** container to the page area.

    !![MDK](img-3.1.gif)

  2. In **Properties** | **Layout**, change `NumberOfColumns` to 1.

    !![MDK](img_3.2.png)

  3. Drag & drop **Key Value Item** to the container.

    !![MDK](img_3.3.gif)

  4. Provide the following information:

    | Property | Value |
    |----|----|
    | `KeyName`| `Coordinates` |
    | `Value`| Bind it to rule `GetCoordinates.js` |

    !![MDK](img-3.4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Enable the Loading Indicator on Sectioned Table in the Main page)]

When the Main page loads, you can display a loading indicator informing about something is being processed before the data appears.  

Enable the loading indicator at the Sectioned Table level by providing the following information:

| Property | Value |
|----|----|
| `Enabled`| `true` |
| `Text`| Bind it to rule `Loading, please wait...` |

!![MDK](img-3.4.1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](List the NPM modules as external reference)]

In `GetCoordinates.js` file, you referred `@nativescript/geolocation` plugin. You now need to list this module as external references in BAS configuration so when bundling, MDK editor knows not to worry about these references.

1. Navigate **File** menu | **Settings** | **Open Preferences**.

    !![MDK](img_3.5.png)

2. Search with `mdk`, click **Edit in settings.json**.

    !![MDK](img-3.6.png)

3. Include below references in `mdk.bundlerExternals` and save the changes.

    >You can beautify the file by right clicking on the file ->select `Format Document` if required.

    ```JSON
    "@nativescript/geolocation"
    ```

     !![MDK](img-3.7.png)        

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

    If you want to enable source for debugging the deployed bundle, then choose **Yes**.

    !![MDK](img-4.4.png)    

    You should see **Deploy to Mobile Services successfully!** message.

    !![MDK](img-5.4.png)

    >When deploying from VS Code to App Update and using an MDK 6.0+ client, you need to set the TS Target to use es6 instead of the default es5 version. See below for the setting in VS Code where you change it for es6.
    !![MDK](img-4.3.1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add NativeScript plugin and External dependencies in your local .mdkproject)]

In order to use the existing `NativeScript` plugin in MDK client, you will need to first add it in `.mdkproject` and then create your branded MDK client.

1. Make sure that you have already completed steps 1 to 3 from [this](cp-mobile-dev-kit-build-client) tutorial.

2. Open `MDKProject.json` file and replace existing content with below:

    ```JSON
    {
      "App Display Name": "Demo Sample App",
      "AppName": "DemoSampleApp",
      "AppVersion": "1.0.0",
      "BundleID": "Enter your Bundle ID",
      "Externals": ["@nativescript/geolocation"],
      "NSPlugins": ["@nativescript/geolocation@8.0.0"],
      "UrlScheme": "mdkclient"
    }
    ```

!![MDK](img-5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Add googlePlayServicesVersion and Permission in App Resources Merge folder(Required only for Android client))]

With [Google Play services](https://developers.google.com/android/guides/overview), your app can take advantage of the latest, Google-powered features such as Maps, Google+, and more.

1. Create below file structure under `DemoSampleApp.mdkproject`.

            DemoSampleApp.mdkproject
              ├── App_Resources_Merge
                  └── Android
                      ├── app.gradle
                      └── src
                          └── main
                              └── AndroidManifest.xml


      !![MDK](img-6.png)

    >Files specified in the `.mdkproject/App_Resources_Merge` folder override a part of the files in `<generated-project>/app/App_Resources`. You can find more details about it in [help documentation](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/custom-client/app-resources-merge.html).


2. Provide below information in the `app.gradle` file. Save the changes.

    ```Java
    // add gradle dependencies here
    project.ext {
    	googlePlayServicesVersion = "16.+"
    }
    dependencies {
    	def googlePlayServicesVersion = project.googlePlayServicesVersion
    	implementation "com.google.android.gms:play-services-location:$googlePlayServicesVersion"
    }
    ```

3. Provide below information in the `AndroidManifest.xml` file. Save the changes.

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <manifest xmlns:android="http://schemas.android.com/apk/res/android">

    	<!-- Always include this permission -->
      <!-- This permission is for "approximate" location data -->
      <uses-permission android:name="android.permission.ACCESS_COARSE_LOCATION" />

      <!-- Include only if your app benefits from precise location access. -->
      <!-- This permission is for "precise" location data -->
      <uses-permission android:name="android.permission.ACCESS_FINE_LOCATION" />

      <!-- Required only when requesting background location access on
           Android 10 (API level 29) and higher. -->
      <uses-permission android:name="android.permission.ACCESS_BACKGROUND_LOCATION" />
    </manifest>
    ```


[VALIDATE_1]
[ACCORDION-END]



[ACCORDION-BEGIN [Step 8: ](Create & Run the MDK client)]

[OPTION BEGIN [Android]]

1. Create your MDK client either using MDK SDK by following the steps 4 & 5 from [this](cp-mobile-dev-kit-build-client) tutorial OR using SAP Cloud Build Service by following [this](cp-mobile-dev-kit-cbs-client) tutorial and run it in your device.

2. After you have accepted the app update, allow your app to access your location.

    ![MDK](img_8.1.png)

    In Main page, you will see device's current location.

    ![MDK](img_8.2.png)

[OPTION END]

[OPTION BEGIN [iOS]]

1. Create your MDK client either using MDK SDK by following the steps 4 & 5 from [this](cp-mobile-dev-kit-build-client) tutorial OR using SAP Cloud Build Service by following [this](cp-mobile-dev-kit-cbs-client) tutorial.

2. After you have accepted the app update, allow your app to access your location.

    ![MDK](img_8.3.png)

    In Main page, you will see device's current location.

    ![MDK](img_8.4.png)

[OPTION END]

[VALIDATE_4]
[ACCORDION-END]

---
