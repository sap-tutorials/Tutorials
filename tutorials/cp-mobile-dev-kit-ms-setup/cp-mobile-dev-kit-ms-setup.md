---
title: Set Up Initial Configuration for an MDK App
description: Configure a new app in the SAP Cloud Platform Mobile Services cockpit and assign some server side features.
auto_validation: true
primary_tag: products>mobile-development-kit-client
tags: [ tutorial>beginner, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>mobile-development-kit-client, software-product-function>sap-cloud-platform-mobile-services ]
time: 10
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- **Tutorial:** If you don't have an SAP Cloud Platform account, follow the tutorial to [set up a free developer account](hcp-create-trial-account).
- **Tutorial:** [Access SAP Cloud Platform Mobile Services](fiori-ios-hcpms-setup)

## Details
### You will learn
  - How to configure MDK apps in the Mobile Services cockpit

---

[ACCORDION-BEGIN [Step 1: ](Understand the mobile development kit feature)]

Mobile Development Kit (MDK) extends SAP Cloud Platform Mobile Services and Editor (SAP Web IDE/SAP Business Application Studio/Visual Studio Code extension) to provide you with a complete set of mobile application development and management tools including on-boarding, offline support, and central lifecycle management. It offers a metadata-driven approach to create native supported applications, so no experience with creating iOS or Android apps is required. Additionally, MDK now extends that capability to the [Web](https://blogs.sap.com/2020/10/27/moving-to-multi-channel-with-mobile-development-kit/) as well.

You can find more details in the [Mobile Development Kit developer page](https://developers.sap.com/topics/mobile-development-kit.html).

!![MDK](MDK.png)

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Configure a new MDK application in Mobile Services cockpit)]

Make sure that you have completed required pre-Prerequisites mentioned in this tutorial.

1. Navigate to [SAP Cloud Platform Mobile Services cockpit on Cloud Foundry environment](fiori-ios-hcpms-setup).

2. On the home screen, select **Create new app**.

    ![MDK](img_2.2.png)

3. Provide the required information and click **Next**.

    | Field | Value |
    |----|----|
    | `ID` | `com.sap.mdk.demo` |
    | `Name` | `SAP MDK Demo App` |

    !![MDK](img_2.3.png)

    >Other fields are optional. For more information about these fields, see [Creating Applications](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/admin/manage.html#creating-applications) in the SAP Cloud Platform documentation.

4. Select **Mobile Development Kit Application** from the dropdown and Click **Finish**.

    !![MDK](img_2.4.png)

    >If you see a _Confirm Finish_ window, click **OK**.

5. Once you have created your application, you see a list of default features have been automatically assigned to the app.

    !![MDK](img_2.5.png)

    >You can find more information on available features in SAP Cloud Platform Mobile Services in [help documentation](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/admin/features.html).

[VALIDATE_3]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Add a sample backend to your MDK app)]

A sample OData service is available for developers to use during development and testing. The sample OData service also lets you evaluate how delta tokens are handled in your test application.

>More details on _Sample Back End_ is available in [help documentation](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/backend-connectivity/sample.html).

1. Click the **+** icon to add **Mobile Sample OData ESPM** feature to your MDK app.

    Here, you can view the root service and metadata URLs, and generate sample sales orders and purchase orders for multiple entity sets. You can view the data for each entity in a separate text file, and even can reset the sample data.

    !![MDK](img_3.1.png)

    !![MDK](img_3.2.png)

    >If you see a _Confirm Save_ window, click **OK**.

2. Click `com.sap.mdk.demo` to navigate back to app configuration page.

[VALIDATE_1]
[ACCORDION-END]

---
