---
title: Generate a sample iOS application with SAP Cloud Platform SDK for iOS Assistant
description: Generate a sample iOS native application with SAP Cloud Platform SDK for iOS Assistant and enable passcode policy
auto_validation: true
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>intermediate, operating-system>ios, topic>mobile, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios ]
---

## Details

### You will learn

 - How to generate a sample iOS native application using SAP Cloud Platform SDK for iOS Assistant
 - How to enable passcode policy in that generated application

> Before you start, make sure you:

> - have downloaded SAP Cloud Platform SDK for iOS **version 2.2 (2.0 SP02)**.
> - have a trial account on SAP Cloud Platform. See [Sign up for a free trial account on SAP Cloud Platform](https://developers.sap.com/tutorials/hcp-create-trial-account.html) for more information.
> - enabled SAP Cloud Platform mobile service for development and operations. See [Enable SAP Cloud Platform mobile service for development and operations](https://developers.sap.com/tutorials/fiori-ios-hcpms-setup.html) for more information.

### Time to Complete
**20 Min**

---
[ACCORDION-BEGIN [Step 1: ](Create an Xcode project)]

Open SAP Cloud Platform SDK for iOS Assistant application. Click ![SAP Cloud Platform Mobile Services](fieldicon.png) to create a new project

![SAP Cloud Platform Mobile Services](img_0.png)

Click the Product Name and enter a name here. Avoid special characters and spaces.
![SAP Cloud Platform Mobile Services](img_000.png)

Click ![SAP Cloud Platform Mobile Services](fieldicon00.png)

![SAP Cloud Platform Mobile Services](img_001.png)

Click ![SAP Cloud Platform Mobile Services](fieldicon_38.png) to let the iOS Assistant create an application end point in the SAP Cloud Platform mobile services for your project.

![SAP Cloud Platform Mobile Services](img_002.png)

Click ![SAP Cloud Platform Mobile Services](fieldicon_40.png)
![SAP Cloud Platform Mobile Services](img_003.png)
Click ![SAP Cloud Platform Mobile Services](fieldicon_4000.png)
![SAP Cloud Platform Mobile Services](img_004.png)

Click ![SAP Cloud Platform Mobile Services](fieldicon_2.png)
![SAP Cloud Platform Mobile Services](img_006.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Build and run the Xcode project)]

Click on  **Build and Run** icon to let the iOS Assistant create an application end point in the SAP Cloud Platform mobile services for your project.

![SAP Cloud Platform Mobile Services](img_007.png)


Click  **Don't Allow**
![SAP Cloud Platform Mobile Services](img_008.png)

Click  **Start**

![SAP Cloud Platform Mobile Services](img_009.png)

Enter your SAP Cloud Platform Credentials

![SAP Cloud Platform Mobile Services](img_010.png)

Click ![SAP Cloud Platform Mobile Services](fieldicon_26.png)

![SAP Cloud Platform Mobile Services](img_011.png)

Click ![SAP Cloud Platform Mobile Services](fieldicon_23.png) for Privacy Consent Forms

![SAP Cloud Platform Mobile Services](img_012.png)

Click ![SAP Cloud Platform Mobile Services](fieldicon_30.png)

![SAP Cloud Platform Mobile Services](img_013.png)

Click ![SAP Cloud Platform Mobile Services](fieldicon_35.png)
![SAP Cloud Platform Mobile Services](img_014.png)


Click ![SAP Cloud Platform Mobile Services](fieldicon_37.png)
![SAP Cloud Platform Mobile Services](img_015.png)

Click `SalesOrderHeaders` collection to navigate to the list of Sales Orders from the OData Sample Service
![SAP Cloud Platform Mobile Services](img_016.png)


Click on any entry to navigate into the `salesorder` of the OData Sample Service
![SAP Cloud Platform Mobile Services](img_017.png)

Click ![SAP Cloud Platform Mobile Services](fieldicon01.png) ** ** to put application into background
![SAP Cloud Platform Mobile Services](img_018.png)


Click ![SAP Cloud Platform Mobile Services](fieldicon02.png) to bring application in foreground.
![SAP Cloud Platform Mobile Services](img_019.png)

![SAP Cloud Platform Mobile Services](info_word.png)

You noticed that there was no passcode/finger print required while restoring the application from background to foreground.
 **Note** :  *In an Assistant generated Xcode project, default passcode Policy is disabled in `OnboardingManager.swift` class.*  *But if you would like to have passcode policy get enabled in your application, you need to enable it manually from SAP Cloud Platform Mobile Services Cockpit. Then these policies will be pulled from Mobile Services at runtime.*

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Enable passcode policy in SAP Cloud Platform Mobile Services Cockpit)]

Switch back to Mobile Services cockpit to look into application created by iOS SDK Assistant. Click ![SAP Cloud Platform Mobile Services](fieldicon_9.png).

![SAP Cloud Platform Mobile Services](img_021.png)


Click ![SAP Cloud Platform Mobile Services](fieldicon03.png)
![SAP Cloud Platform Mobile Services](img_022.png)
Here you can see User registration details and then Click ![SAP Cloud Platform Mobile Services](fieldicon_5.png).
![SAP Cloud Platform Mobile Services](img_023.png)

Click ![SAP Cloud Platform Mobile Services](fieldicon_6.png)
![SAP Cloud Platform Mobile Services](img_024.png)

Click ![SAP Cloud Platform Mobile Services](fieldicon_8.png)
![SAP Cloud Platform Mobile Services](img_025.png)

Change Min. length to 5
![SAP Cloud Platform Mobile Services](img_026.png)

[VALIDATE_1]

Since application is already running on simulator, Finger print doesn't work here. Uncheck ![SAP Cloud Platform Mobile Services](fieldicon04.png).
![SAP Cloud Platform Mobile Services](img_027.png)


Click ![SAP Cloud Platform Mobile Services](fieldicon05.png)
![SAP Cloud Platform Mobile Services](img_028.png)


Click on  **Build and Run** icon to let the iOS Assistant create an application end point in the SAP Cloud Platform mobile services for your project.
![SAP Cloud Platform Mobile Services](img_029.png)


Click  **Continue**
![SAP Cloud Platform Mobile Services](img_030.png)


Choose a passcode
![SAP Cloud Platform Mobile Services](img_031.png)

Click ![SAP Cloud Platform Mobile Services](fieldicon_33.png)
![SAP Cloud Platform Mobile Services](img_032.png)

 re-enter same passcode
![SAP Cloud Platform Mobile Services](img_033.png)


Click ![SAP Cloud Platform Mobile Services](fieldicon_36.png)
![SAP Cloud Platform Mobile Services](img_034.png)

Click on simulator home icon to put application in background ![SAP Cloud Platform Mobile Services](fieldicon_900.png)
![SAP Cloud Platform Mobile Services](img_035.png)


Click on application icon to bring app in foreground.
![SAP Cloud Platform Mobile Services](img_036.png)

Enter the chosen passcode to application data
![SAP Cloud Platform Mobile Services](img_037.png)

Click ![SAP Cloud Platform Mobile Services](fieldicon06.png)
![SAP Cloud Platform Mobile Services](img_038.png)

![SAP Cloud Platform Mobile Services](info_word00.png)

Application is restored and now protected with a passcode.
![SAP Cloud Platform Mobile Services](img_039.png)

[ACCORDION-END]
