---
title: Installing the SAP Cloud Platform for iOS SDK
description: Installing the SAP Cloud Platform for iOS SDK files into your project.
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>beginner, topic>mobile, operating-system>ios, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Development machine:** Access to a Mac computer

## Next Steps
 - [Enable SAP Cloud Platform mobile service for development and operations](https://www.sap.com/developer/tutorials/fiori-ios-hcpms-setup.html)

## Details
### You will learn  
In this small tutorial, you will learn to add the SAP Cloud Platform for iOS SDK framework files to your project, and set the correct references.

> **NOTE: If you plan to use the *SDK Assistant* tool, you can skip this tutorial. The *SDK Assistant* will set the correct SDK configuration and references in the generated Xcode project**

### Time to Complete
**5 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Store SDK files locally)]

Download the `SAP-CP-SDK-for-iOS.dmg` disk image file. Double-clicking the file will mount it as a drive. Open the mounted image, and you will see it the following:

![Adding files dialog](fiori-ios-hcpms-install-sdk-07.png)

Drag the folder `SAP CP SDK for iOS` to the local `Applications` folder.

The folder you just copied contains 3 sub-folders and one application:

![Adding files dialog](fiori-ios-hcpms-install-sdk-01.png)

1.  Application `SAP Cloud Platform SDK for iOS Assistant` will be covered in tutorial [Creating an app using the SDK Assistant](http://www.sap.com/developer/tutorials/fiori-ios-hcpms-sdk-assistant.html).
2.  Folder `Documentation` contains the API docs for the SDK.
3.  Folder `Tools` contain scripts to generate OData proxy classes manually

    Open the folder `Tools > ProxyGenerator`. Drag the executable `sapcpsdk-proxygenerator` and folder `sapcpsdk-resources` to the `UsrLocalBin` folder (which is a symbolic link to your `/usr/local/bin` folder) so you can call the proxy generator from any location:

    ![Proxy Generator](fiori-ios-hcpms-install-sdk-08.png)

3.  Folder `Frameworks` contain the actual SDK framework files.

    Open the folder `Frameworks > Release-fat`. These contain the following 5 framework files:

    | Filename | Description |
    |---|---|
    | `SAPCommon.framework` | Includes fundamental components that integrate the application with SAP Cloud Platform mobile service for development and operations, such as logging, that are also used by other framework components|
    | `SAPFoundation.framework` | Includes components that integrate the application with SAP Cloud Platform mobile service for development and operations, including communication, authentication, caching, remote notifications, and so on |
    | `SAPOData.framework` | Use the `SAPOData` component to interact with an OData endpoint. `SAPOData` parses OData payloads, produces OData requests, and handles responses for OData versions 2 and 4. |
    | `SAPOfflineOData.framework` | Use the `SAPOfflineOData` component to add offline capability to your application. |
    | `SAPFiori.framework` | Includes a number of UI components which implement the Fiori Design Language. These inherit from the Apple `UIKit` components. |

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open or create Xcode project)]

Open your Xcode `com.sap.tutorial.demoapp.DemoApplication` project (or create a new one)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Open the project configuration)]

Select your **project file** from the **Project Navigator** and under the **General** tab, scroll down to the **Embedded Binaries** section:

![Adding files dialog](fiori-ios-hcpms-install-sdk-02.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add SDK files to group)]

Open a **Finder** window and navigate to `./<local_SDK_root>/Frameworks/Release-fat` folder.

Drag and drop the 5 framework files into the **Embedded Binaries** section in the **General** tab of your project configuration:

![Adding files dialog](fiori-ios-hcpms-install-sdk-03.png)

In the dialog that appears, make sure you have selected **Copy items if needed** as well as **Create folder references**:

![Adding files dialog](fiori-ios-hcpms-install-sdk-04.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Close dialog)]

Click **Finish** to close the dialog. Your project structure should now resemble the following:

![Adding files dialog](fiori-ios-hcpms-install-sdk-05.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Check if SDK files are linked)]

In the same editor, switch to the **Build Phases** tab.

Expand the **Link Binary With Libraries** panel, and make sure the three SDK files are referenced:

![Build Phases panel](fiori-ios-hcpms-install-sdk-06.png)

[DONE]
[ACCORDION-END]

## Next Steps
 - [Enable SAP Cloud Platform mobile service for development and operations](https://www.sap.com/developer/tutorials/fiori-ios-hcpms-setup.html)
