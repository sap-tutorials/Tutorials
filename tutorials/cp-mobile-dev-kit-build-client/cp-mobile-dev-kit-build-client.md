---
title: Build Your Mobile Development Kit Client Using MDK SDK
description: Set up your development environment that enable MDK SDK so that you can begin building your branded Mobile Development Kit client.
auto_validation: true
primary_tag: software-product>mobile-development-kit-client
tags: [ tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, software-product>sap-business-technology-platform, software-product>mobile-development-kit-client, software-product>sap-mobile-services ]
time: 25
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- **Tutorial**: [Set Up Initial Configuration for an MDK App](cp-mobile-dev-kit-ms-setup)
- **Download the latest version of Mobile Development Kit SDK** either from the SAP community [download page](https://developers.sap.com/trials-downloads.html?search=Mobile%20development%20kit) or [SAP Software Center](https://launchpad.support.sap.com/#/softwarecenter/search/Mobile%2520development%2520kit) if you are a SAP Mobile Services customer


## Details
### You will learn
  - How to install a Mobile development kit client SDK on Mac and Windows OS
  - How to build a Mobile development kit client for iOS and Android
  - How to connect to SAP Mobile app

  There are 3 options for mobile development kit client:

  1. Install the SAP Mobile Services client from the public store which is meant for demo and development or learning purposes.
  2. Use the Cloud Build feature in SAP Mobile Services to generate a MDK Client (details are in [this](cp-mobile-dev-kit-cbs-client) tutorial)
  3. Build a client on your local machine in your organization development environment using MDK SDK

In this tutorial, you will learn creating a MDK client using option 3.  

For distribution to your users, you need to build a custom client. This enables you to:

-	Provide customer app icon
-	Use your signing profiles
-	Distribute custom extensions
-	Provide app-specific settings (Custom EULA texts, Add app assets)
-	Add demo mode
-	Customize languages (for onboarding screens)
-	Use your own distribution channels, like the mobile device management of your choice

---

[ACCORDION-BEGIN [Step 1: ](Run MDK Dependencies Installer)]

>Make sure you are choosing the right development platform tab above.

Make sure that you have download latest version of MDK SDK as described in Prerequisites.

[OPTION BEGIN [Mac]]

1. Extract the downloaded zip file on your Mac OS.

    !![MDK](img_1.1.png)

    >You will also find other files in the extracted folder. The README file contains information about the version requirements, dependencies and some getting started links.

2. MDK Dependencies Installer checks the status of the MDK dependencies and will install or upgrade the dependencies for you. Double click `MDK Dependencies Installer.app` file, click **Open**.

    !![MDK](img-1.2.png)

    >If you find some issues (for example: app cant be opened because the identity of the developer cannot be confirmed) while opening this file, go to System Preferences > Security & Privacy and click **Open Anyway**.

3. Enter Admin user password and click **OK**.

    !![MDK](img_1.3.png)

    >You may need to grant admin access via Privileges app.

    The installer will list all required components for iOS and Android platform and automatically check if they are already installed in the machine. Follow the installer UI to install the components you selected.

    !![MDK](img-1.4.gif)

    >You might see different software versions depending on MDK SDK version you are using.

    >You can look into console by clicking **Show Log** for execution of each dependencies.

[OPTION END]

[OPTION BEGIN [Windows]]

1. Extract the downloaded zip file on your Windows OS.

    !![MDK](img-1.5.png)

    >You will also find other files in the extracted folder. The README file contains information about the version requirements, dependencies and some getting started links.

2. Extract `MDKDependenciesInstallerWindows` zip file.

    MDK Dependencies Installer checks the status of the MDK dependencies and will install or upgrade the dependencies for you. Double click the  `MDK_Dependencies_Installer` application file to open it.

    !![MDK](img-1.6.png)

    The installer will list all required components for Android platform and automatically check if they are already installed in the machine. Follow the installer UI to install the components you selected.

    !![MDK](img_1.7.png)

    >If you encounter any issue while running the MDK Dependencies Installer then have a look at [this](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/troubleshooting/mdk/troubleshoot.html#loading-message-displays-while-running-mobile-development-kit-dependency-installer-on-windows-machine) troubleshooting guide.

    >You might see different software versions depending on MDK SDK version you are using.

    >You can look into console by clicking **Show Log** for execution of each dependencies.

[OPTION END]

Once you've installed these prerequisites, your machine is ready to generate and build an MDK project.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Installing the SDK dependencies)]

>Make sure you are choosing the right development platform tab above.

[OPTION BEGIN [Mac]]

1. To use the SDK to generate a mobile development kit client, the first step is to set up the SDK to create a client. Unzip `MDKClient_SDK.zip` if it is not already extracted.

    !![MDK](img_2.1.png)

2. From a terminal window, navigate to the `[path] -> MDKClient_SDK` folder and execute `./install.command`. If everything is fine, you will a success message in the console followed by next steps.

    !![MDK](img-2.2.1.png)

    !![MDK](img_2.2.2.png)

    Notice that the `create-client.command` file has appeared in the SDK directory.

    !![MDK](img_2.2.3.png)

[OPTION END]


[OPTION BEGIN [Windows]]

1. To use the SDK to generate a mobile development kit client, the first step is to set up the SDK to create a client. Unzip `MDKClient_SDK` if it is not already extracted.

    !![MDK](img_2.3.png)

2. From a command line window, navigate to the `[path] -> MDKClient_SDK` folder and execute `install.cmd`.

    !![MDK](img-2.4.png)

    If everything is fine, you will a success message in the console followed by next steps.

    !![MDK](img-2.5.png)

    !![MDK](img-2.5.1.png)    

    Once this completes, close the window. Notice that the `create-client.cmd` file has appeared in the SDK directory.

    !![MDK](img-2.6.png)    

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create your .mdkproject folder)]

>Make sure you are choosing the right development platform tab above.

[OPTION BEGIN [Mac]]

1. In the `MDKClient_SDK` folder, you will find the `template.mdkproject` folder.

    !![MDK](img-3.1.png)

    It is recommended that you copy this folder to another location so that you can to use it for future builds. Copy and paste it anywhere, and then rename template to `DemoSampleApp.mdkproject`.

    !![MDK](img-3.2.png)

2. Next, you will need to update the `BrandedSettings.json` and `MDKProject.json` files as needed for your client. Go into the `DemoSampleApp.mdkproject` folder.

    !![MDK](img-3.2.0.png)

3. Open the `MDKProject.json` file and update it as needed. This file has some build-time configurations such as the application name, version and bundle ID.

    !![MDK](img-3.3.png)

    >`AppDisplayName`: This is the name of the application on the home screen of the device.

    >`AppName`: This is the name of the folder where the client is created.

    >`BundleID`: It should be a unique identifier for your application. This controls if the client can be installed side by side with other applications on the device. Two applications with the same Bundle ID cannot be installed at the same time on a device.  For iOS this is the Identifier `(AppID)` that is registered in Apple Developer account since that determines if the application can be installed alongside other applications. If the `XCode` project is set up to use _Automatically manage signing_ then when building, `XCode` will automatically generate a signing profile for the specified bundle id. Without matching them, trying to run the custom client in iOS device will result in failure. In Android, it is known as [application ID](https://developer.android.com/studio/build/application-id).

    >`URLScheme`: Allows you to specify a custom URL scheme which opens the client. The default is `mdkclient`. This value also needs to be unique across applications on your device. If the value is not unique the wrong application may be referenced when redirecting.

    >You can find more details about configuration of `MDKProject.json` file in [this](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/custom-client/branding-custom-client.html) help documentation.        

4. Now, open the `BrandedSettings.json` file and update the `ConnectionSettings` with the values for your MDK application in Mobile Services.

    !![MDK](img-3.4.png)

    To find the correct URLs for your client, you should navigate to  [Mobile Services cockpit](cp-mobile-dev-kit-ms-setup) and find your MDK application that you want to link to this client.

    4.1 Click `com.sap.mdk.demo` > **Security** tab.

    Copy the Client ID, Redirect URL, OAuth Authorization, OAuth Token and paste to `ClientId`, `RedirectUrl`, `AuthorizationEndPointUrl` and `TokenUrl` parameters respectively.

    !![MDK](img-3.4.1.png)

    4.2 `AppId`: App ID from `Info` tab.

    !![MDK](img-3.4.2.png)

    4.3 `ServerUrl`: Server URL from `APIs` tab.

    !![MDK](img-3.4.3.png)

    >For Android (7 and below), screen sharing or taking screen shots are disabled by default. To enable it, add `"EnableScreenSharing": true` in `ConnectionSettings` section. Screen sharing is already enabled in Android 8 and above. You can find more information about `Enable Screen Sharing` in [this](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/custom-client/branding-custom-client.html#enable-screen-sharing) help documentation.

    Regarding other properties:
    **Debug settings**: The settings in the `DebugSettings` property are for development use and should not be enabled in a production setting.

    **Log Settings**: Set this to the log level to be used when the client is launched.

    **Demo**: If you want to access the app in the demo mode, you can configure required settings.        

    >If you are connecting to `AliCloud` accounts, you will also need to add your custom domains under `URLWhitelist` property in the same file. You can find more details in [documentation](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/custom-client/branding-custom-client.html#connection-settings-allowlist).

5. In the last section of `BrandedSettings.json` file, make these changes:

    | Field | Value |
    |----|----|
    | `DetailLabelViewText` | `My first custom MDK client` |
    | `SigninButtonText` | `Start` |

    !![MDK](img-3.5.png)        


[OPTION END]

[OPTION BEGIN [Windows]]

1. In the `MDKClient_SDK` folder, you will find the `template.mdkproject` folder.

    !![MDK](img-3.png)

    It is recommended that you copy this folder to another location so that you can to use it for future builds. Copy and paste it anywhere, and then rename the template to `DemoSampleApp.mdkproject`.

    !![MDK](img-3.2.1.png)

2. Next, you will need to update the `MDKProject.json` and `BrandedSettings.json` files as needed for your client. Go into the `DemoSampleApp.mdkproject` folder.

    !![MDK](img-3.2.3.png)

3. Open the `MDKProject.json` file and update it as needed. This file has some build-time configurations such as the application name, version and bundle ID.

    !![MDK](img-3.3.png)

    >`AppDisplayName`: This is the name of the application on the home screen of the device.

    >`AppName`: This is the name of the folder where the client is created.

    >`BundleID`: It should be a unique identifier for your application. This controls if the client can be installed side by side with other applications on the device. Two applications with the same Bundle ID cannot be installed at the same time on a device. In Android, it is known as [application ID](https://developer.android.com/studio/build/application-id).

    >`URLScheme`: Allows you to specify a custom URL scheme which opens the client. The default is `mdkclient`. This value also needs to be unique across applications on your device.  If the value is not unique the wrong application may be referenced when redirecting.    

    >You can find more details about configuration of `MDKProject.json` file in [this](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/custom-client/branding-custom-client.html#configuration-of-mdkprojectjson-file) help documentation.                    

  4. Now, open the `BrandedSettings.json` file and update the `ConnectionSettings` with the values for your MDK application in Mobile Services.

    !![MDK](img-3.4.png)

    To find the correct URLs for your client, you should navigate to  [Mobile Services cockpit](cp-mobile-dev-kit-ms-setup) and find your MDK application that you want to link to this client.

    4.1 Click `com.sap.mdk.demo` > **Security** tab.

    Copy the Client ID, Redirect URL, OAuth Authorization, OAuth Token and paste to `ClientId`, `RedirectUrl`, `AuthorizationEndPointUrl` and `TokenUrl` parameters respectively.

    !![MDK](img-3.4.1.png)

    4.2 `AppId`: App ID from `Info` tab.

    !![MDK](img-3.4.2.png)

    4.3 `ServerUrl`: Server URL from `APIs` tab.

    !![MDK](img-3.4.3.png)

    >For Android (7 and below), screen sharing or taking screen shots are disabled by default. To enable it, add `"EnableScreenSharing": true` in `ConnectionSettings` section. Screen sharing is already enabled in Android 8 and above. You can find more information about `Enable Screen Sharing` in [this](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/custom-client/branding-custom-client.html#enable-screen-sharing) help documentation.

    Regarding other properties:
    **Debug settings**: The settings in the `DebugSettings` property are for development use and should not be enabled in a production setting.

    **Log Settings**: Set this to the log level to be used when the client is launched.

    **Demo**: If you want to access the app in the demo mode, you can configure required settings.        

    >If you are connecting to `AliCloud` accounts, you will also need to add your custom domains under `URLWhitelist` property in the same file. You can find more details in [documentation](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/custom-client/branding-custom-client.html#connection-settings-allowlist).

5. In the last section of `BrandedSettings.json` file, make these changes:

    | Field | Value |
    |----|----|
    | `DetailLabelViewText` | `My first custom MDK client` |
    | `SigninButtonText` | `Start` |

    !![MDK](img-3.5.png)        


[OPTION END]


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create the MDK Client)]

>Make sure you are choosing the right development platform tab above.

[OPTION BEGIN [Mac]]

1. You can create a client by running `./create-client.command` and providing the path to a valid `.mdkproject` directory.

    !![MDK](img-4.1.png)

    >You can run the `create-client command` from any directory.  The resulting MDK client will be created in the directory where the `create-client command` is run from.

2. You will be asked whether you would like to build for iOS or Android or All?

    !![MDK](img-4.2.png)

    >**All** option was chosen in this tutorial as you will learn how to create the MDK client for iOS and Android.

    Once the `create-client.command` script executed successfully, you will see **Application ready** message in terminal console.

    !![MDK](img-4.3.1.png)

    You will also find your MDK Client app created under the `MDKClient_SDK` folder.

    !![MDK](img-4.3.2.png)

[OPTION END]

[OPTION BEGIN [Windows]]

1. You can create a client by running `create-client.cmd` and providing the path to a valid `.mdkproject` directory.

    !![MDK](img-4.4.png)

    >You can run the `create-client command` from any directory. The resulting MDK client will be created in the directory where the `create-client command` is run from.

2. Once the `create-client.cmd` script executed successfully, you will see **Application ready** message in terminal console.

    !![MDK](img-4.4.1.png)

    !![MDK](img-4.4.1.2.png)

     You will also find your app created under the `MDKClient_SDK` folder.

    !![MDK](img-4.4.2.png)

[OPTION END]

>This name of this folder is based on the `<App Name>` provided in the `MDKProject.json file` and this is the `NativeScript` project.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run the MDK Client)]

>Make sure you are choosing the right device platform tab above.

[OPTION BEGIN [Android]]

1. In this step, you will run the app on an android device. Attach the device to your Mac or Windows machine and run `tns device android` command to print a list of attached devices.

    !![MDK](img_5.1.png)

    >Make sure **Developer option** and **USB debugging** option is enabled in android device.

2. Copy the **Device Identifier** value for your device.

3. In terminal or command line window, navigate to the app name folder **`DemoSampleApp`** (in `MDClient_SDK` path) and use `tns run android --device <device identifier>` command to run the MDK client on android device.

    !![MDK](img_5.3.png)

    >To run the MDK client on Android emulator, you need to use `tns run android --emulator` command. Before trying to launch the client on Android emulator, make sure that you have already configured a virtual device (Android Studio>AVD Manager). Otherwise, you may get an error like No emulator image available for device identifier.

    Once, above command gets successfully executed, you will see new MDK client up and running in Android device.

4. Tap **Agree** on `End User License Agreement`.

    ![MDK](img-5.3.1.png)


5. In Welcome screen, you will notice that **app name**, **detailed label text** and **signing button text** have been updated as per changes done in step 3. Tap **Start** to connect MDK client to SAP Business Technology Platform (BTP).

    ![MDK](img-5.5.png)

6. Enter your credentials to login to SAP Business Technology Platform (BTP).

    ![MDK](img-5.7.png)
    ![MDK](img-5.7.1.png)

7. Choose a passcode with at least 8 characters for unlocking the app and tap **Next**.

    ![MDK](img-5.8.png)

8. Confirm the passcode and tap **Done**.

    ![MDK](img-5.9.png)

9. Optionally, you can enable biometric authentication to get faster access to the app data.

    ![MDK](img-5.9.1.png)

    If there is any app metadata already deployed to Mobile Services, you will see `Update Now?` dialog box Otherwise you will see an empty screen.

    ![MDK](img-5.10.png)

    >You can always interrupt running process in terminal window by pressing `control + C`.

    >To build an **`APK` for an Android device**, use `tns build android --release`. More information about archiving can be found in `NativeScript` documentation [here](https://v6.docs.nativescript.org/tooling/docs-cli/project/testing/build-android).

[OPTION END]

[OPTION BEGIN [iOS]]

1. In this step, In this step, you will run the app on an iOS device. Attach the device to your Mac and run `tns device ios` command to print a list of attached devices.

    !![MDK](img_5.11.png)

2. Copy the **Device Identifier** value for your device.

3. In terminal window, navigate to the app name folder **`DemoSampleApp`** (in `MDClient_SDK` path) and use `tns run ios --device <device identifier>` command to run the MDK client on iOS device.

    !![MDK](img_5.12.png)

    You can also run the app in Xcode. Open the project in Xcode with the command `open platforms/ios/<app name>.xcworkspace`, or open the workspace using the `File -> Open...` dialog in Xcode. Configure the application's code signing settings, then run the application for the target device.

    >To run the MDK client on iOS simulator, use `tns run ios --emulator` command.

    Once, above command gets successfully executed, you will see new MDK client up and running in Android device.


4. Tap **Agree** on `End User License Agreement`.

    ![MDK](img-5.13.png)

5. In Welcome screen, you will notice that **app name**, **detailed label text** and **signing button text** have been updated as per changes done in step 3.

    ![MDK](img-5.14.png)

    Tap **Start** to connect MDK client to SAP Business Technology Platform (BTP).

6. Enter your credentials to login to SAP Business Technology Platform (BTP).

    ![MDK](img-5.15.png)
    ![MDK](img-5.15.1.png)    

7. Choose a passcode with at least 8 characters for unlocking the app and tap **Next**.

    ![MDK](img-5.16.png)

8. Confirm the passcode and tap **Done**.

    ![MDK](img-5.17.png)

9. Optionally, you can enable biometric authentication to get faster access to the app data, tap **Enable**.

    ![MDK](img-5.18.png)

    If there is any app metadata already deployed to Mobile Services, you will see `Update Now?` dialog box Otherwise you will see an empty screen.

    ![MDK](img-5.19.png)

    >You can always interrupt running process in terminal window by pressing `control + C`.

    >To build an **IPA for an iOS device**, use `tns build ios --for-device --release`. This can also be accomplished in Xcode by opening the workspace and selecting the Archive option. More information about archiving can be found in Apple's documentation [here](https://developer.apple.com/library/content/documentation/IDEs/Conceptual/AppDistributionGuide/UploadingYourApptoiTunesConnect/UploadingYourApptoiTunesConnect.html).

[OPTION END]

[VALIDATE_3]
[ACCORDION-END]

---
