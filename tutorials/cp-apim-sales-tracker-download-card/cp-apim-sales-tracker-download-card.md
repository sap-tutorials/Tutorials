---
title: Download SAP Mobile Cards to your Device
description: Download and register to the Mobile Cards service to consume data on an android or iOS device.
auto_validation: true
time: 15
tags: [ tutorial>beginner, products>sap-api-management, products>sap-cloud-platform]
primary_tag: products>sap-api-management
---

## Prerequisites

## Details
### You will learn
- How to download SAP mobile card onto your iOS device
- How to download SAP mobile card onto your android device
- How to register to SAP Mobile Cards service

Add additional information: Background information, longer prerequisites

---

[ACCORDION-BEGIN [Step 1: ](Install Mobile Cards app)]

1. Install Mobile Card app

  - [iOS](https://itunes.apple.com)
  
  ![Scan iOS](01-scan-code-IOS.png)

  - [Android](https://itunes.apple.com)

  ![Scan iOS](01-scan-code-IOS.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open Mobile Services)]

1. Return to the [Cockpit](https://account.hanatrial.ondemand.com ).

2. Under the side menu, Select **Services**.

3. Under Services, Select **Mobile Services, users**.

    It should be **Enabled**.

4. Click **Go to Service** to launch Mobile Cards.

    ![Navigate Mobile Services](03-go-to-mobile-service.png)

5. Click **Close**.

    ![Close popup](04-close-popup.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Register to Mobile Cards)]

1. Select and expand **Mobile Applications**.

    Click **SAP Mobile Cards** to launch the Mobile Cards Service.

    ![Close Popup](05-launch-mobile-card.png)

2. Navigate to **Features** and select **Connectivity**.

    ![Navigate Destination](22-connectivity.png)

3. Click **Create**.

    ![Navigate Destination](06-create.png)

    >Adds the Cloud Platform destination we just created

4. In the Destination wizard enter the following:

    ![Values Destination](07-destination-values.png)

    **Field** | **Value**
    ---- | ----
    Type |`API_Portal_Trial`
    Platform Destination Name |`HTTP`
    Destination Name |`API Portal on Trial`

5.  Click **Next** till you get **Finish**.

    ![click Finish](08-finish.png)

[DONE]
[ACCORDION-END]
