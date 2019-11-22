---
title: Build your MDK Client
description: Build and run the MDK client to connect to your SAP Cloud Platform mobile application.
auto_validation: true
primary_tag: products>mobile-development-kit-client
tags: [ tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>mobile-development-kit-client, software-product-function>sap-cloud-platform-mobile-services ]
time: 25
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- **Tutorial**: [Set Up Initial Configuration for an MDK App](cp-mobile-dev-kit-ms-setup)
- **Download the latest version of Mobile Development Kit SDK** either from [SAP Software Content Downloads](https://developers.sap.com/trials-downloads.html) or [SAP Marketplace](https://launchpad.support.sap.com/#/softwarecenter/template/products/%20_APP=00200682500000001943&_EVENT=DISPHIER&HEADER=Y&FUNCTIONBAR=N&EVENT=TREE&NE=NAVIGATE&ENR=73555000100900002601&V=MAINT&TA=ACTUAL&PAGE=SEARCH/MDK%20CLIENT%203.0) if you are a SAP Cloud Platform Mobile Services customer

## Details
### You will learn
  - How to install a Mobile development kit client SDK on Mac and Windows OS
  - How to build a Mobile development kit client for iOS and Android
  - How to connect to SAP Cloud Platform Mobile application

---

[ACCORDION-BEGIN [Step 1: ](Run MDK Dependencies Installer)]

>Make sure you are choosing the right development platform tab above.

Make sure that you have download latest version of MDK SDK as described in Prerequisites.

[OPTION BEGIN [Mac]]

Extract the downloaded zip file on your Mac OS.

![MDK](img_001.png)

>You will also find other files in the extracted folder. The README file contains information about the version requirements, dependencies and some getting started links.

MDK Dependencies Installer checks the status of the MDK dependencies and will install or upgrade the dependencies for you. Double click `MDK Dependencies Installer.app` file, click **Open**.

![MDK](img_002.png)

>If you find some issues (for example: app cant be opened because the identity of the developer cannot be confirmed) while opening this file, go to System Preferences > Security & Privacy and click **Open Anyway**.

Enter Admin user password and click **OK**.

![MDK](img_003.png)

>You may need to grant admin access via Privileges app. 

The installer will list all required components for iOS and Android platform and automatically check if they are already installed in the machine. Follow the installer UI to install the components you selected.

![MDK](img_004.gif)

[OPTION END]

[OPTION BEGIN [Windows]]

Extract the downloaded zip file on your Windows OS.

![MDK](img_001.1.PNG)

>You will also find other files in the extracted folder. The README file contains information about the version requirements, dependencies and some getting started links.

Extract `MDKDependenciesInstallerWindows.zip` file.

MDK Dependencies Installer checks the status of the MDK dependencies and will install or upgrade the dependencies for you. Double click `MDK_Dependencies_Installer.exe` file to open it.

![MDK](img_002.2.PNG)

The installer will list all required components for Android platform and automatically check if they are already installed in the machine. Follow the installer UI to install the components you selected.

![MDK](img_003.3.PNG)

>If MDK Dependencies Installer keeps showing `Loading...` then use command `MDK_Dependencies_Installer.exe –debug –debug` to see more information.
There might be some issue due to Security Policy. It can be fixed by executing this in `PowerShell`:
>`Set-ExecutionPolicy -ExecutionPolicy Bypass -Scope CurrentUser`

[OPTION END]

>You can look into console by clicking **Show Log** for execution of each dependencies.

Once you've installed these prerequisites, your machine is ready to generate and build an MDK project.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Installing the SDK dependencies)]

>Make sure you are choosing the right development platform tab above.

[OPTION BEGIN [Mac]]

To use the SDK to generate a mobile development kit client, the first step is to setup the SDK to create a client. Unzip `MDKClient_SDK.zip` if it is not already extracted.

![MDK](img_005.png)

From a terminal window, navigate to the `[path] -> MDKClient_SDK` folder and execute `./install.command`. If everything is fine, you will a success message in the console followed by next steps.

![MDK](img_006.png)

Once this completes, close the window. Notice that the `create-client.command` file has appeared in the SDK directory.

![MDK](img_007.png)

[OPTION END]

[OPTION BEGIN [Windows]]

To use the SDK to generate a mobile development kit client, the first step is to setup the SDK to create a client. Unzip `MDKClient_SDK.zip` if it is not already extracted.

![MDK](img_004.PNG)

From a command line window, navigate to the `[path] -> MDKClient_SDK` folder and execute `install.cmd`.

![MDK](img_005.5.PNG)

If everything is fine, you will a success message in the console followed by next steps.

![MDK](img_006.6.PNG)

Once this completes, close the window. Notice that the `create-client.cmd` file has appeared in the SDK directory.

![MDK](img_007.7.PNG)

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create your .mdkproject folder)]

>Make sure you are choosing the right development platform tab above.

[OPTION BEGIN [Mac]]

In the `MDKClient_SDK` folder, you will find the `template.mdkproject` folder. It is recommended that you copy this folder to another location to use it for future builds and paste it anywhere want and you can rename template to whatever you want but the directory name needs to end in `.mdkproject`.

![MDK](img_007.1.png)

The idea of retaining the `.mdkproject` folder is so you can use it again in the future to build additional MDK clients using the same settings using a different version of the MDK SDK.

>For this tutorial, I have named my `.mdkproject` folder `demosampleapp.mdkproject` and put it outside of the MDK Client folder structure. I will refer this name for the rest of this tutorial.

>![MDK](img_009.png)

Next, you will need to update the `BrandedSettings.json` and `MDKProject.json` files as needed for your client. Go into the `demosampleapp.mdkproject` folder.

![MDK](img_010.png)

[OPTION END]

[OPTION BEGIN [Windows]]

In the `MDKClient_SDK` folder, you will find the `template.mdkproject` folder. It is recommended that you copy this folder to another location to use it for future builds and paste it anywhere want and you can rename template to whatever you want but the directory name needs to end in `.mdkproject`.

![MDK](img_007.1.1.PNG)

The idea of retaining the `.mdkproject` folder is so you can use it again in the future to build additional MDK clients using the same settings using a different version of the MDK SDK.

>For this tutorial, I have named my `.mdkproject` folder `demosampleapp.mdkproject` and put it outside of the MDK Client folder structure. I will refer this name for the rest of this tutorial.

>![MDK](img_009.1.PNG)

Next, you will need to update the `BrandedSettings.json` and `MDKProject.json` files as needed for your client. Go into the `demosampleapp.mdkproject` folder.

![MDK](img_010.1.PNG)

[OPTION END]

Open the `MDKProject.json` file and update it as needed. This file has some build-time configurations such as the application name, version and bundle ID.

![MDK](img_011.png)

>Provide a unique `AppName`, for example: "Demo Sample App". This is the name of the application on the home screen of the device.

>For iOS `MDKProject.json's BundleID` should be the same **Identifier** `(AppID)` that is registered in Apple Developer account.

>Without matching them, trying to run the custom client in iOS device will result in failure.

>You can find more details about configuration of `MDKProject.json` file in [this](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/01e70c3bd0914761bb37f800029c0e24.html) help documentation.

Now open the `BrandedSettings.json` file and update it with information from your MDK Mobile Services application.

![MDK](img_012.png)

To find the correct URLs for your client, you should go to [Mobile Services cockpit](https://developers.sap.com/tutorials/cp-mobile-dev-kit-ms-setup.html#08a4320f-424c-4f94-8de0-d9a7be8378d5) and find your MDK application that you want to link to this client.

Click on `com.sap.mdk.demo` > **Security** tab.

Copy the Client ID, Redirect URL, OAuth Authorization & OAuth Token and paste to `ClientId`, `RedirectUrl`, `AuthorizationEndPointUrl` and `TokenUrl` parameters respectively.

![MDK](img_013.png)

`AppId`: App ID from `Info` tab.

![MDK](img_014.png)

`SapCloudPlatformEndpoint`: Server URL from `APIs` tab.

![MDK](img_015.png)

>For android client, screen sharing or taking screen shots are disabled by default. To enable it, `add "EnableScreenSharing": true` in `ConnectionSettings` section.

Regarding other properties:
**Debug settings**: The settings in the `DebugSettings` property are for development use and should not be enabled in a production setting.

**Log Settings**: Set this to the log level to be used when the client is launched.

**Demo**: If you want to access the app in the demo mode, you can configure required settings.

In the last section of `BrandedSettings.json` file, make these changes:

| Field | Value |
|----|----|
| `DetailLabelViewText` | `My sample custom client` |
| `EncryptDatabase` | `false` |
| `SigninButtonText` | `Start` |

![MDK](img_016.png)

>It is recommended to encrypt the database in production scenarios. You might want to set this property to false to extract the database for debugging purposes.

>You can find more details about branding in [this](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/01e70c3bd0914761bb37f800029c0e24.html) help documentation.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create the MDK Client)]

>Make sure you are choosing the right development platform tab above.

[OPTION BEGIN [Mac]]

You can create a client by running `./create-client.command` and providing the path to a valid `.mdkproject` directory.

![MDK](img_017.png)

You will be asked whether you would like to build for iOS or Android or All?

![MDK](img_018.png)

>**All** option was chosen in this tutorial as you will learn how to create the MDK client for iOS and Android.

Then, you will be asked whether you would like to build for device or simulator?

![MDK](img_018.1.png)

>**device** option was chosen for this tutorial.

Once the `create-client.command` script executed successfully, you will see **Application ready** message in terminal console.

![MDK](img_019.png)

You will also find your app created under the `MDKClient_SDK` folder.

![MDK](img_020.png)

[OPTION END]

[OPTION BEGIN [Windows]]

You can create a client by running `create-client.cmd` and providing the path to a valid `.mdkproject` directory.

![MDK](img_017.1.PNG)

Once the `create-client.cmd` script executed successfully, you will see **Application ready** message in terminal console.

![MDK](img_019.1.PNG)

 You will also find your app created under the `MDKClient_SDK` folder.

![MDK](img_020.1.1.PNG)

[OPTION END]

>This name of this folder is based on the `<App Name>` provided in the `MDKProject.json file` and this is the `NativeScript` project.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run the MDK Client)]

>Make sure you are choosing the right device platform tab above.

[OPTION BEGIN [Android]]

In this step, you will run the app on an android device. Attach the device to your Mac or Windows machine and run `tns device android` command to print a list of attached devices.

![MDK](img_020.3.png)

>Make sure **Developer option** and **USB debugging** option is enabled in android device.

Copy the **Device Identifier** value for your device.

In terminal or command line window, navigate to the app name folder **Demo Sample App** (in `MDClient_SDK` path) and use `tns run android --device <device identifier>` command to run the MDK client on android device.

![MDK](img_020.4.png)

>To run the MDK client on Android simulator, use `tns run android --emulator` command. Make sure that you have created a virtual device in Android Studio prior to running this command.

>Note: Before trying to launch the client on Android emulator, make sure that you have already configured a virtual device (Android Studio>AVD Manager). Otherwise, you may get error like No emulator image available for device identifier.

Once, above command gets successfully executed, you will see new MDK client up and running in Android device.

![MDK](img_029.jpg)

Here, you will notice that **app name**, **detailed label text** and **signing button text** have been updated as per changes done in step 3.

Tap **START** to connect MDK client to SAP Cloud Platform.

Enter Email address and password to login to SAP Cloud Platform and tap **Log On** to authenticate.

![MDK](img_030.1.1.png)

Tap **AGREE** on `End User License Agreement`.

![MDK](img_031.jpg)

Choose a passcode with at least 8 characters for unlocking the app and tap **NEXT**.

![MDK](img_032.jpg)

Confirm the passcode and tap **DONE**.

![MDK](img_033.jpg)

Optionally, you can enable fingerprint to get faster access to the app data.

![MDK](img_034.jpg)

Since there is no app metadata deployed yet to Mobile Services, hence you will see an empty screen.

![MDK](img_035.jpg)

>You can always interrupt running process in terminal window by pressing `control + C`.

>To build an **`APK` for an Android device**, use `tns build android --release`. More information about archiving can be found in `NativeScript` documentation [here](https://docs.nativescript.org/tooling/docs-cli/project/testing/build-android).

[OPTION END]

[OPTION BEGIN [iOS]]

In this step, you will run the app on an iOS device. Attach the device to your Mac and run `tns device ios` command to print a list of attached devices.

![MDK](img_020.1.png)

Copy the **Device Identifier** value for your device.

In terminal window, navigate to the app name folder **Demo Sample App** (in `MDClient_SDK` path) and use `tns run ios --device <device identifier>` command to run the MDK client on iOS device.

![MDK](img_020.2.png)

You can also run the app in Xcode. Open the project in Xcode with the command `open platforms/ios/<app name>.xcworkspace`, or open the workspace using the `File -> Open...` dialog in Xcode. Configure the application's code signing settings, then run the application for the target device.

>To run the MDK client on iOS simulator, use `tns run ios --emulator` command.

Once, above command gets successfully executed, you will see new MDK client up and running in your device.

![MDK](img_022.png)

Here, you will notice that **app name**, **detailed label text** and **signing button text** have been updated as per changes done in step 3.

Tap **Start** to connect MDK client to SAP Cloud Platform.

Enter Email address and password to login to SAP Cloud Platform and tap **Log On** to authenticate.

![MDK](img_023.1.1.png)

Tap **Agree** on `End User License Agreement`.

![MDK](img_024.png)

Choose a passcode with at least 8 characters for unlocking the app and tap **Next**.

![MDK](img_025.png)

Confirm the passcode and tap **Done**.

![MDK](img_026.png)

Optionally, you can enable Touch ID to get faster access to the app data, tap **Enable**.

![MDK](img_026.1.png)

Since there is no app metadata deployed yet to Mobile Services, hence you will see an empty screen.

![MDK](img_027.png)

>You can always interrupt running process in terminal window by pressing `control + C`.

>To build an **IPA for an iOS device**, use `tns build ios --for-device --release`. This can also be accomplished in Xcode by opening the workspace and selecting the Archive option. More information about archiving can be found in Apple's documentation [here](https://developer.apple.com/library/content/documentation/IDEs/Conceptual/AppDistributionGuide/UploadingYourApptoiTunesConnect/UploadingYourApptoiTunesConnect.html).

[OPTION END]

[DONE]
[ACCORDION-END]

---
