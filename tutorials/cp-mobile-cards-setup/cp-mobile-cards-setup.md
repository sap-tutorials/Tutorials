---
title: Set Up SAP Mobile Cards
description: Activate SAP Cloud Platform Mobile Services and connect the SAP Mobile Cards application to your trial account.
auto_validation: true
primary_tag: products>sap-mobile-cards
tags: [ tutorial>beginner, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>sap-mobile-cards ]
time: 10
---

## Prerequisites
- [Sign up for a free trial account on SAP Cloud Platform](hcp-create-trial-account)
- [Enable SAP Cloud Platform Mobile Services](fiori-ios-hcpms-setup)
- **Install SAP Mobile Cards Application:** Download and install on your [iPhone](https://itunes.apple.com/us/app/sap-content-to-go/id1168110623?mt=8) or [Android](https://play.google.com/store/apps/details?id=com.sap.content2go)

## Details
### You will learn
  - How to connect the SAP Mobile Cards application to your SAP Cloud Platform Mobile Services

---

[ACCORDION-BEGIN [Step 1: ](Understand the SAP Mobile Cards feature)]

SAP Mobile Cards is a feature within SAP Cloud Platform Mobile Services which provides our customers access to a micro application platform to publish data into a consumer grade wallet or passbook-style app. It allows companies to quickly create simple, yet highly valuable quick-win apps.

You can find more details in the [SAP Mobile Cards developer page](https://developers.sap.com/topics/mobile-cards.html).

![MobileCards](Markdown_files/MobileCards.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 2: ](Log into Mobile Services cockpit)]

Go to [https://cloudplatform.sap.com/index.html](https://cloudplatform.sap.com/index.html "") and click **Login**.

![MobileCards](Markdown_files/img_0.png)

Enter your SAP Cloud Platform account credentials and click **Log On**.

![MobileCards](Markdown_files/img_000.png)

Click **Neo Trial** to navigate into SAP Cloud Platform cockpit.

![MobileCards](Markdown_files/img_001.png)

Click **Services** to look for Mobile Services.

![MobileCards](Markdown_files/img_002.png)

Click **Mobile** from the list of available categories.

![MobileCards](Markdown_files/img_004.png)

Click **Mobile Services, users**.  

![MobileCards](Markdown_files/img_005.png)

Click **Go to Service** to open the SAP Cloud Platform Mobile Services Cockpit.

![MobileCards](Markdown_files/img_006.png)

Click **Close** to close this information window.

![MobileCards](Markdown_files/img_007.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Get familiar with default configuration)]

Navigate to **SAP Mobile Cards** to look into the Mobile Cards configuration.

![MobileCards](Markdown_files/img_009.png)

Click **Features**.

![MobileCards](Markdown_files/img_010.png)

Click **Connectivity** to look into destination endpoints, where you can create a new destination connecting to backend endpoint or select from any existing destinations.

![MobileCards](Markdown_files/img_011.png)

Below is a list of destinations from which the SAP Mobile Cards service can fetch data. For this tutorial, the `SAPCPMobileServices` destination is used. It's important that all destinations are only pointing to the root of the service.

![MobileCards](Markdown_files/img_012.png)

Click **SAP Mobile Cards** navigation bar to go back to Mobile Cards admin page.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Configure SAP Mobile Cards client)]

Follow the next steps to trigger the configuration of the SAP Mobile Cards client with this SAP Cloud Platform account.

Click **APIs**.

![MobileCards](Markdown_files/img_014.png)

Open your phone camera app and start scanning the QR code, as shown below. For Android devices, look for a QR code scanning app and scan with the same.

![MobileCards](Markdown_files/img_015.png)

Click the toast message to launch SAP Mobile Cards.

![MobileCards](Markdown_files/img_017.png)

Enter your SAP Cloud Platform credentials and click **Log On** to authenticate.

![MobileCards](Markdown_files/img_018.png)

You now have connected your SAP Mobile Cards client with your SAP Cloud Platform account.

![MobileCards](Markdown_files/img_019.png)

[VALIDATE_1]
[ACCORDION-END]

---
