---
parser: v2
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
keywords: cap
auto_validation: true
time: 30
tags: [tutorial>intermediate, tutorial>license, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-api-management, software-product>sap-hana-cloud, software-product>sap-s-4hana-cloud]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

# Use Events from SAP S/4HANA Cloud to Update Your Deployed CAP Application
<!-- description --> This tutorial shows you how to create and change a Business Partner in your SAP S/4HANA Cloud system and see the updates in your CAP application.

## Prerequisites
 - [Enable Events from SAP S/4HANA Cloud to SAP BTP](btp-app-events-enable-s4hc)

## You will learn
 - How to create and change a Business Partner in your SAP S/4HANA Cloud system
 - How to check if the Business Partner creation and change events sent from your SAP S/4HANA Cloud system have updated your CAP application

---

### Create a Business Partner and check updates in your CAP application

1. In your SAP S/4HANA Cloud system, go to **Maintain Business Partner**. You can also use the **Search** field in the upper right corner of the screen.

    <!-- border -->![s4h20](s4h20.png)

2. Under **Organization**, add a name for the business partner and choose **Save**.

    <!-- border -->![BPa](Demo1.png)

3. Open the UI application. Choose the **Risks** tile and find the automatically created `Risk` for the newly saved `BusinessPartner`.

    <!-- border -->![Risk1](Demo16.png)

4. Open the object page of the new risk. Set **Impact** to `1000` and choose **Save**.

    <!-- border -->![Risk2](Demo6.png)

5. Refresh the **Risks** app and choose **Go**. The risk status is updated to `Assessed`.

    <!-- border -->![Risk1](Demo7.png)

5. In the SAP S/4HANA Cloud system, change the already created business partner. For example, add a salutation and choose **Save**.

    <!-- border -->![BP4](Demo8.png)

6. Refresh the **Risks** app and choose **Go** again. The status of the new risk is updated to `Changed`.

    <!-- border -->![Risk2](Demo11.png)

Congratulations! You have completed all tutorials.


---