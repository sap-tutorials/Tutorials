---
title: Use OData Annotations to Add CRUD Functionality to an MDK App
description: Generate a fully functional CRUD multi-channel application based on OData annotations.
auto_validation: true
primary_tag: products>mobile-development-kit-client
tags: [ tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-business-technology-platform, products>mobile-development-kit-client, products>sap-mobile-services, products>sap-business-application-studio]
time: 25
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- **Tutorial group:** [Set Up for the Mobile Development Kit (MDK)](group.mobile-dev-kit-setup)
- **Download and install:** **SAP Mobile Services Client** on your [iOS](https://apps.apple.com/us/app/sap-mobile-services-client/id1413653544) or [Android](https://play.google.com/store/apps/details?id=com.sap.mobileservices.client) device (If you are connecting to `AliCloud` accounts then you will need to brand your [custom MDK client](cp-mobile-dev-kit-build-client) by allowing custom domains.)

## Details
### You will learn
  - How MDK editor parses OData Annotations for a given OData service
  - How to create a fully functional multi-channel application

You may clone an existing project from [GitHub repository](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/tree/master/4-Level-Up-with-the-Mobile-Development-Kit/5-Use-OData-Annotations-to-Add-CRUD-Functionality-to-an-MDK-App) and start directly with step 5 in this tutorial but make sure you complete step 2.

---

Mobile Development Kit brings OData annotations capabilities to your multi-channel applications. MDK editor supports generating List-Detail pages based on annotations. List-Detail pages are similar to a Master-Detail page, but it is two pages instead of one. The MDK editor parses existing annotations to give you a huge leap forward in your multi-channel applications.

![MDK](img_1.gif)

[ACCORDION-BEGIN [Step 1: ](Understand the SAP Fiori Elements)]

If you are a Fiori app designer, you may already be familiar with OData annotations and smart templates.

SAP Fiori elements provide designs for UI patterns and predefined templates for common application use cases. App developers can use SAP Fiori elements to create SAP Fiori applications based on OData services and annotations. With little or no coding, you can create SAP Fiori applications. UI5 has a Web solution, named smart templates, that builds a starter application by parsing the annotations in your OData service.

You can also check out more information on the Fiori elements [List Report](https://experience.sap.com/fiori-design-ios/article/list-report/) and [Smart templates](https://experience.sap.com/fiori-design-web/smart-templates/)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add annotation information in the backend destination)]

Sample backend in SAP Mobile Services provides annotation functionality for **Products**. If you add annotation path in given backend endpoint, the same annotation information can be leveraged by MDK editor to generate related CRUD pages.

Make sure you have already configured an app in Mobile Services cockpit and have added Sample service as per step 3 in [this](cp-mobile-dev-kit-ms-setup) tutorial.

1. In SAP MDK Demo App configuration, click **Mobile Connectivity**.

    !![MDK](img_2.1.png)

2. Click **Edit** icon to add annotation path to the `SampleServiceV2` destination.

    !![MDK](img_2.2.png)

3. In following steps, let the existing settings as it is.

    In **Annotations** step, click **Add Annotation URL** to add OData Annotations to the Sample service.

    Provide the below information and click **Next**:

    | Field | Value |
    |----|----|
    | `Annotation Name`| `Product` |
    | `Path/File` | `/annotations/Products` |

    !![MDK](img-2.6.png)

9. In the following screen, let the default settings as it is. Click **Finish**.

    Here you can see that OData Annotation information is updated in the `SampleServiceV2` destination.

    !![MDK](img-2.9.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a new MDK project in SAP Business Application Studio)]

This step includes creating the mobile development kit project in the editor.

1. Launch the [Dev space](cp-mobile-bas-setup) in SAP Business Application Studio.

2. Click **Start from template** on Welcome page.

    !![MDK](img-3.2.png)

    >If you do not see Welcome page, you can access it via **Help** menu.

3. Select **MDK Project** and click **Start**.

    !![MDK](img-3.3.png)  

4. In *Type* step, select or provide the below information and click **Next**:

    | Field | Value |
    |----|----|
    | `MDK template type`| Select `Base` from the dropdown |
    | `Your project name` | `MDK_Annotations` |
    | `Your application name` | <default name is same as project name, you can provide any name of your choice> |

    !![MDK](img-3.4.png)

    >More details on _MDK template_ is available in [help documentation](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/bas.html#creating-a-new-project-cloud-foundry).  

    >If you see *Cloud foundry token expired, continue without mobile services connection?* message, then set up the Cloud Foundry environment again by navigating to **View** menu > **Find Command**> **CF: Login to Cloud foundry** to initiate a valid session and click Start Over.    

5. In *Service Name* step, provide or select the below information and click **Next**:

    | Field | Value |
    |----|----|
    | `Service File Name`| `<Provide any name of your choice>` |
    | `OData Source` | Select `Mobile Services` from the dropdown |
    | `Mobile Services Landscape` | Select `standard` from the dropdown |
    | `Application Id` | Select `com.sap.mdk.demo` from the dropdown |
    | `Destination` | Select `SampleServiceV2` from the dropdown |
    | `Enter a path to the OData service` | Leave it as it is |
    | `Language URL` | Leave it with the default value |
    | `Enable Offline` | Choose `No` |   

    !![MDK](img-3.5.png)

    Regardless of whether you are creating an online or offline application, this step is needed for the app to connect to an OData service. When building an Mobile Development Kit application, it assumes the OData service created and the destination that points to this service is set up in Mobile Services and SAP Business Technology Platform.

    Since you will create an online based app, hence _Enable Offline Store_ option is unchecked.

6. After clicking **Next**, the wizard will generate your MDK Application based on your selections. You should now see the `MDK_Annotations` project in the project explorer.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add MDK Annotation component to MDK project)]

1. Right-click `Application.app` and select **MDK:New MDK Annotation Component**.

    !![MDK](img-4.1.png)

2. MDK editor fetches annotation details, select **Product** Annotation and click **Next**.

    !![MDK](img_4.2.png)

3. In **Template Customization** step, click **Next**.

    !![MDK](img_4.3.png)

4. Click **Finish** to create list detail from OData Annotation.

    In MDK project, you will see new pages, actions, rules have been generated for **Product**.

    !![MDK](img-4.4.1.png)

5. Next, you will add a Toolbar item on `Main.page` to navigate to `Product_List.page`.

    In `Main.page`, drag and drop **Toolbar Item** control on the page.

    !![MDK](img_4.5.gif)

6. Change its **Caption** to **Products**.

    !![MDK](img-4.6.png)

7. Now, you will set the `onPress` event to `NavToProduct_List.action`.

    In **Events** tab, click the 3 dots icon for the `OnPress` property to open the **Object Browser**.

    Double-click the `NavToProduct_List.action` and click **OK** to set it as the `OnPress` action.

    !![MDK](img-4.7.png)

    Pages, actions and rules created are a starting point. You can edit those pages and make it your own.  At this point the MDK editor is no longer reading the annotations from OData.

    For the List page, MDK supports the List Report. To create a Detail page, MDK requires `UI.LineItem` and `UI.HeaderInfo` in the annotation file.

    >If the OData designer updates the backend services data schema (annotations), the MDK pages will stay as originally created. It will not automatically update the pages or overwrite your changes. You are *disconnected* from the annotations at this point.

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Deploy the application)]

So far, you have learned how to build an MDK application in the SAP Business Application Studio editor. Now, you will deploy this application definition to Mobile Services and Cloud Foundry to consume it as Mobile and Web application respectively.

1. Right-click `Application.app` and select **MDK: Deploy**.

    !![MDK](img-5.1.png)

2. Select deploy target as **Mobile & Cloud**.

    MDK editor will deploy the metadata to Mobile Services (for Mobile application) followed by to Cloud Foundry (for Web application).

    !![MDK](img-4.2.gif)

    You should see successful messages for both deployments.

    !![MDK](img-4.3.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Run the app)]

[OPTION BEGIN [Android]]

>Make sure you are choosing the right device platform tab above. Once you have scanned and onboarded using the onboarding URL, it will be remembered. When you Log out and onboard again, you will be asked either to continue to use current application or to scan new QR code.

SAP Business Application Studio has a feature to generate QR code for app onboarding.

1. Click the `Application.app` to open it in MDK Application Editor and click **Application QR Code** icon to display the QR code.

    !![MDK](img-6.1.png)

    !![MDK](img-5.2.png)

2. Follow [these steps](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/blob/master/Onboarding-Android-client/Onboarding-Android-client.md) to on-board the MDK client.

    Once you accept app update, you will see the **Main** page with **LOGOUT** and **PRODUCTS** options at bottom of the page. Tap **PRODUCTS**, you will navigate to Product List page.

    ![MDK](img_7.1.png)

3. In following pages, you can create a new record, modify an existing record and even delete the record.

    ![MDK](img_7.2.png)
    ![MDK](img_7.3.png)

[OPTION END]

[OPTION BEGIN [iOS]]

>Make sure you are choosing the right device platform tab above. Once you have scanned and onboarded using the onboarding URL, it will be remembered. When you Log out and onboard again, you will be asked either to continue to use current application or to scan new QR code.

SAP Business Application Studio has a feature to generate QR code for app onboarding.

1. Click the `Application.app` to open it in MDK Application Editor and click **Application QR Code** icon to display the QR code.

    !![MDK](img-6.1.png)

    !![MDK](img-5.2.png)

2. Follow [these steps](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/blob/master/Onboarding-iOS-client/Onboarding-iOS-client.md) to on-board the MDK client.

    Once you accept app update, you will see the **Main** page with **Logout** and **Products** options at bottom of the page. Tap **Products**, you will navigate to Product List page.

    ![MDK](img-7.4.png)

3. In following pages, you can create a new record, modify an existing record and even delete the record.

    ![MDK](img-7.5.png)
    ![MDK](img-7.6.png)

[OPTION END]

[OPTION BEGIN [Web]]

1. Click the highlighted button to open the MDK Web application in a browser. Enter your SAP BTP credentials if asked.

    !![MDK](img-7.7.png)

    >You can also open the MDK web application by accessing its URL from `.project.json` file.
    !![MDK](img-7.8.png)

2. You will see the **Main** page with **Logout** and **Products** options at bottom of the page. Tap **Products**, you will navigate to Product List page.

    !![MDK](img-7.9.png)

3. In following pages, you can create a new record, modify an existing record and even delete the record.

    !![MDK](img-7.10.png)
    !![MDK](img-7.11.png)

[OPTION END]

[VALIDATE_2]
[ACCORDION-END]

---

Congratulations, you have successfully created a fully functional CRUD multi-channel application based on OData annotations and you can continue with the remaining tutorials in this mission.

---
