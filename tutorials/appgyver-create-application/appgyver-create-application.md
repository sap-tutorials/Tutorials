---
title: Create an Application with SAP AppGyver
description: Create an application with SAP AppGyver on SAP BTP. The application, created in a browser, can be used with the AppGyver Previewer App to scan physical barcodes on food packaging to display calorific information.
auto_validation: true
time: 10
tags: [ tutorial>beginner, tutorial>license, topic>mobile, products>sap-business-technology-platform]
primary_tag: products>sap-appgyver
author_name: Tom Beck
author_profile: https://github.com/heytombeck
---

## Prerequisites
- Access to an SAP BTP account in EU10 with Low-Code / No-Code entitlements
- Previously followed the steps provided in [Subscribe to the Low-Code / No-Code Service in SAP BTP](appgyver-subscribe-service)
 - Access to the AppGyver Previewer App on a smart phone or tablet: [iOS](https://apps.apple.com/us/app/sap-appgyver-preview/id1585856868) / [macOS](https://apps.apple.com/fi/app/appgyver/id1485395192)/ [Android](https://play.google.com/store/apps/details?id=com.sap.appgyver.preview.release)


## Details
### You will learn
  - How to create a no-code project with SAP AppGyver
  - How to add and edit visual elements in the AppGyver Composer tool

  In this mission you will learn how to create a no-code application using the SAP 'AppGyver' Composer tool on SAP BTP. The application you create will enable you to scan a barcode on a smartphone and retrieve information from a public API. To do this, you will need to download the 'AppGyver' Preview app (available through iTunes and Google Play store) and sign up for an account using the same email address that your SAP BTP account uses.

  The application you'll create across the tutorials will read barcodes from food packaging and display information about the product using the Open Food Facts API:

![Diagram of scanning a food item from a mobile app](OpenFoodFactsDiagram.png)

  The steps provided detail the process using an SAP BTP account within a desktop browser. It is also possible to replicate many of the steps using a free account from [AppGyver.com](http://www.appgyver.com).

  All applications are created, edited, and managed within what we call a project, with one application created per project. Projects, and their related applications, are not automatically saved in this service, so care should be taken before closing a browser.

---

[ACCORDION-BEGIN [Step 1: ](Create SAP AppGyver project)]

Projects are created from the application development lobby. To access this lobby from the SAP BTP Cockpit, click **Services - Instances and Subscriptions** and then select **SAP AppGyver**.

![Access application development lobby](access_lobby.png)

Within the application development lobby, click **Create** and then select ***AppGyver Project***.

![Create AppGyver Project](Create_AppGyver_Project.png)

Enter a `Project name` and an optional description, then click **Create**.

![Create AppGyver Project](Projectname.png)

Your AppGyver application is created, with the AppGyver Composer Pro view displayed.

For reference: Once created, projects can be accessed again at any time from your application development lobby.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Understand SAP AppGyver)]

![AppGyver Composer Pro](composerPro.png)

When working with AppGyver Composer Pro, the majority of your time will be spent in the App builder area. This area allows you to complete key tasks such as, but not limited to, the following:

- Define your app's structure and navigation logic
- Build pixel-perfect user interfaces
- Create complex logic with visual programming
- Integrate with external data resources
- Bind data to your components to create dynamic views, and more

For more detailed coverage of the features available in AppGyver's Composer Pro, view: [AppGyver Documentation](https://docs.appgyver.com/overview/introduction)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Edit app interface)]

You'll now start to create a basic layout for your application, starting with editing text.

Click the existing **Headline** field and edit the text to read:

- `Barcode Scanner`

![Edit UI Headline](EditHeadline.png)

Click the **Paragraph** field and edit the text to read:

- `Scan a barcode of a food product using your smartphone`

![Barcode Scanner](BarcodeScanner.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add scan button)]

Next you'll need to add a Scan button which, when tapped, will open the camera device on your smartphone.

To do this, locate the **Button** component (found under: Core – Forms) and drag and drop this underneath the paragraph field.

![Adding a button](AddButton.png)

Edit the Button text to read `Scan`.

![Edit button text](EditButtonText.png)

Click **Save**.

![Save Project](SaveProject.png)

The application is now saved in draft and available to preview using the AppGyver Preview app on your smartphone.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Preview app in AppGyver previewer)]

Once downloaded to your smartphone, you can login to your AppGyver account on the SAP BTP within the app using a QR code.

To scan the QR code, click **Launch**.

![Click launch](Launch_Preview.png)

Then using your AppGyver Preview app login screen, scan the available QR code.

![Scan QR to login to your app](scanQR.png)

The application refreshes and your project is available to view.

[DONE]
[ACCORDION-END]
