---
title: Create a Dev Space for SAP Fiori Apps
description: Create an SAP Business Application Studio dev space, a preconfigured environment with the required tools and extensions tailored for a specific business scenario.
auto_validation: true
time: 5
tags: [ tutorial>beginner, topic>sapui5, products>sap-fiori, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-business-application-studio
author_name: Raz Korn
author_profile: https://github.com/raz-korn
---

## Prerequisites
 - You have access to SAP Business Application Studio (see [Set Up SAP Business Application Studio for Development](appstudio-onboarding)).
 - You configured a destination to SAP Gateway Demo System (ES5) (see [Connect SAP Cloud Platform to Your SAP Gateway Demo System Account (ES5)](cp-portal-cloud-foundry-gateway-connection)).


## Details
### You will learn
  - How to create an SAP Business Application Studio dev space for SAP Fiori apps

---

[ACCORDION-BEGIN [Step 1: ](Open SAP Business Application Studio)]
1. Go to your Cloud Foundry environment subaccount and click the **Subscriptions** tab.

    !![AppStudio Tile in SAP Cloud Platform Cockpit](01-01 SCP Subscriptions_.jpg)

2. Locate the **SAP Business Application Studio** tile.

3. Click **Go to Application**. 

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a preconfigured SAP Fiori dev space)]

1. Choose **Create Dev Space**.

    !![Open AppStudio](AppStudio Dev Space Manager_.jpg)

2. Enter `Demo_Fiori` for your dev space **name**.

3. Choose **SAP Fiori** as the application type.

4. Click **Create Dev Space**.

    !![Create Dev Space](AppStudio Create Dev Space Fiori_.jpg)

    >The dev space is in status **STARTING**. Wait until it is in status **RUNNING**.

    !![Dev Space Starting](AppStudio Dev Space Starting_.jpg)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Open the SAP Fiori dev space)]

**Click** the name of the dev space you created.


!![Open Dev Space](AppStudio Open Dev Space_.jpg)


The SAP Fiori dev space opens and the **Welcome** tab appears.

!![Open Dev Space](03-01-02 AppStudio Welcome Tab.jpg)

>The purple color of the status bar indicates that there is no open workspace.

[DONE]
[ACCORDION-END]


---
