---
title: Install the SAP Cloud Platform SDK for iOS
description: Install the SAP Cloud Platform SDK for iOS locally on your machine.
auto_validation: true
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>beginner, topic>mobile, operating-system>ios, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios, software-product-function>sap-cloud-platform-mobile-services ]
time: 5
author_name: Sandeep TDS
author_profile: https://github.com/sandeep-tds
---

## Prerequisites  
- You completed the previous tutorial(s) in this group.
- **Development environment:** Apple Mac running macOS Catalina or higher with Xcode 11 or higher
- **SAP Cloud Platform SDK for iOS:** Download Version 5.1.0 or higher from [Trials and Downloads](https://developers.sap.com/trials-downloads.html?search=sdk%20for%20ios)

## Details
### You will learn  
  - How to add the SAP Cloud Platform SDK for iOS framework files to your project
  - How to set the correct references

---

[ACCORDION-BEGIN [Step 1: ](Store SDK files locally)]

Download the `SAP-CP-for-iOS.dmg` disk image file. Extracting and double-clicking the file will mount it as a drive. Open the mounted image, and you will see it the following:

![Adding files dialog](fiori-ios-hcpms-install-sdk-01.png)

Drag the application file `SAP Cloud Platform SDK for iOS Assistant` to the local `Applications` folder.

> The Assistant is a macOS app that rapidly generates object-oriented Swift proxy classes for OData services, eliminating exposure to low-level APIs. The Assistant also generates Mobile Services configuration and creates a ready to run Xcode project for iPhone or iPad.

The **SAP Cloud Platform SDK for iOS Assistant** will be covered in detail in one of the next tutorials.

>If you have used version 1.0 of the **SAP Cloud Platform for iOS SDK** before, you may have noticed the `Frameworks`, `Tools` and `Documentation` folders are no longer available. These can now all be accessed from the SDK Assistant's **App menu**.

>It is not necessary to extract the frameworks but if is needed then click **Export Frameworks**, select a folder where you wish the frameworks to be exported.

> ![Adding files dialog](fiori-ios-hcpms-install-sdk-02.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
