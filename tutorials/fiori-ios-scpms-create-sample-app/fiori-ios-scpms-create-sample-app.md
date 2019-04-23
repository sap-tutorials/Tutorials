---
title: Create a Sample App
description: Create and examine your first Fiori for iOS app using the SAP Cloud Platform SDK for iOS Assistant connecting against a sample service.
auto_validation: true
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>beginner, operating-system>ios, topic>mobile, topic>odata, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios software-product-function>sap-cloud-platform-mobile-services ]
time: 15
---

## Prerequisites  
- **Development environment:** Apple Mac running macOS High Sierra or higher with Xcode 10 or higher
- **SAP Cloud Platform SDK for iOS:** Version 3.0 SP01
- [Get a Free Trial Account on SAP Cloud Platform](https://developers.sap.com/tutorials/hcp-create-trial-account.html)
- [Enable SAP Cloud Platform Mobile Services](https://developers.sap.com/tutorials/fiori-ios-hcpms-setup.html)
- [Install the SAP Cloud Platform SDK for iOS](https://developers.sap.com/tutorials/fiori-ios-hcpms-install-sdk.html)
- [Configure Mobile Services in the iOS Assistant](https://developers.sap.com/tutorials/fiori-ios-hcpms-setup.html)

## Details
### You will learn  
  - How to create a Fiori for iOS application
  - What the SAP Cloud Platform SDK for iOS Assistant can generate.

---

[ACCORDION-BEGIN [Step 1: ](Run the SAP Cloud Platform SDK for iOS Assistant)]

> This app will be the foundation for most of the iOS tutorials on the [Tutorial Navigator](https://developers.sap.com/tutorial-navigator.html)


Double-click the **SAP Cloud Platform SDK for iOS Assistant** icon to start the application. If no applications have been generated previously, you will see the initial screen:

![SDK Assistant](fiori-ios-scpms-create-sample-app-01.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Create a new cloud application)]

For this tutorial, you will create a new cloud application configuration in Mobile Services, which will be configured to access data of the sample service.

Click the **Create new** button in the **First Steps** section. The first step of the workflow asks you to choose whether to create a sample app, use an existing cloud app configuration, or create a new one.

![Project Properties](fiori-ios-scpms-create-sample-app-02.png)

Click the **Create new Application** tile on the right side of the **Create a new App** step. This will allow us to create both a server-side configuration for the app, and it will generate a new Xcode project for an iOS app.

Next select your **SAP Cloud Platform Mobile Services account** and click **Next**.
![Project Properties](fiori-ios-scpms-create-sample-app-03.png)

In the **Add an SAP Cloud Platform Mobile Services application** step, add the needed application details to create an application in your mobile services account.

Enter the following details:

| Field | Value |
|----|----|
| Application Name | `MyProductApp` |
| Identifier | `com.example.product` |
| Authentication | `OAuth2` |

![Project Properties](fiori-ios-scpms-create-sample-app-04.png)

> **Note:** The name of the cloud application does not need to match your iOS app name, and often won't; your iOS app may access multiple cloud applications. The Application Identifier, however, must be unique across all of your configured cloud applications, and typically follows a reverse-DNS format.

Click **Next** to advance to the **Destinations** step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add the sample service as an destination endpoint)]

In the **Destinations** step, you can define the backend connection. You will select the `com.sap.edm.sampleservice.v2` destination as a backend endpoint.

![Destinations](fiori-ios-scpms-create-sample-app-05.png)

Click **Next** to advance to the **Features** step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Enable features)]

In the **Optional Features** step, you have the option to enable **logging** and **log uploads**, enable **remote notifications**, use **Discovery Service** and whether to use **Online** or **Offline** OData.

![Features](fiori-ios-scpms-create-sample-app-06.png)

Make sure the checkboxes **Enable Log Upload**, **Enable Remote Notifications** and **Use Discovery Service for Application bootstrapping** are selected, and the **OData Provider** radio button is set to **Enable Online OData** and click **Next** to proceed to the **Xcode Project** step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Set up the Xcode Project)]

In the **Xcode Project configuration** step you will set up your actual Xcode project for development later on.
Enter the following details:

| Field | Value |
|----|----|
| Product Name | `MyProductApp` |
| Organization Name | `<your company name>` |
| Organization Identifier | `com.sap.mobile.example` |
| Path | `<choose a local destination>` |

![Project Properties](fiori-ios-scpms-create-sample-app-07.png)

Click **Next** to advance to the **Proxy Classes** step. Here you can see what destination is going to be used to pull the Metadata of the OData Service to generate the OData Proxy Classes.

![Project Properties](fiori-ios-scpms-create-sample-app-08.png)

Click **Next** to go to the **UI Configuration** step. Set the **Destination** radio button to **Master / Detail View with Onboarding for destination:** and click finish.

![Project Properties](fiori-ios-scpms-create-sample-app-09.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Generate the Xcode project)]

After you have clicked **Finish** in the previous step, the SDK Assistant now loads the OData service's metadata. This metadata describes the data model, and can be accessed via `<service URL>$metadata`. For the sample service, the metadata URL is `https://hcmps-<your_user_id>trial/hanatrial/ondemand.com/mobileservices/origin/hcpms/ESPM.svc/v2/$metadata`
Based on this metadata, the OData proxy classes will be generated for the Xcode project.

In addition, the configuration settings you have provided in the SDK Assistant are now being sent to SAP Cloud Platform Mobile Services.

> **Note:** If you have already 5 native applications defined in SAP Cloud Platform Mobile Services, the SDK Assistant will give you an error.

> In that case, log on to your **SAP Cloud Platform mobile Services** account at `https://hcpmsadmin-<your_user_id>trial.dispatcher.hanatrial.ondemand.com/` and navigate to **Mobile Applications > Native/Hybrid**. Select one of the available application configurations and delete it in order for the SDK Assistant to add the new application configuration.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Examine the generated Xcode project)]

You can now select the `MyProductApp` application project under **Recent Projects** in the SDK Assistant's starting screen.

> If Xcode does not open automatically to show your project, click on the project name in the SDK Assistant and click **Open**.

In Xcode, the left-hand sidebar of the window shows various Navigators for exploring your project, source control, debugging, and so on. If it's not already selected, select the **Project Navigator** by clicking on the folder icon above the left-hand sidebar. This shows all of the files in the project.

![Xcode project overview](fiori-ios-scpms-create-sample-app-10.png)

The `Main.storyboard` file shows a split-view setup for the generated Master-Detail views.

The `MyProductApp/Onboarding` folder contains logic for the user onboarding, authentication and handling of `passcodes` and Touch ID.

The `MyProductApp/Proxy Classes` folder contains the OData proxy classes generated from the OData service. The `ESPMContainer.swift` file in the `Proxy Classes/public` folder acts as a data service provider to gain access to the OData entities. The `ESPMContainerMetadata` represents the metadata definition of the service.

The `ViewControllers` folders contain the master and detail view controllers as well as a storyboard for the entities, respectively.

[VALIDATE_1]
[ACCORDION-END]
