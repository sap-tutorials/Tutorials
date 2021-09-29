---
title: Add a New Entitlement to Your Subaccount
description: Each subaccount in your global SAP BTP account is given a share of the resources through Entitlements. In this tutorial you will add additional entitlements (= resources) to your default subaccount.
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product-function>sap-btp-cockpit ]
primary_tag: products>sap-business-technology-platform
---


## Details
### You will learn
  - Where to find all entitlements of your subaccount
  - How to add a new entitlement to your subaccount

Entitlements are the heart of all subaccounts in SAP BTP. They define which service and runtime artifacts you are able to use. This tutorial will show you how you can set up the entitlements you need to develop success solutions on SAP BTP.

---

[ACCORDION-BEGIN [Step: ](Log into SAP BTP cockpit)]

1. Go to <https://account.hanatrial.ondemand.com> and log in to your SAP BTP cockpit.

2. Click on "Enter Your Trial Account" to access the SAP BTP Cockpit.

    !![accesstrial](accesstrial.png)

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step: ](Switch to entitlements view)]

1. **Select** the tile of the subaccount you want to add entitlements to.

    !![subaccount](selectsubaccount.png)

2. Click on **Entitlements** in the left-hand sidebar.

    !![openentitlements](openentitlements.png)

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step: ](Add SAP HANA Cloud services entitlement to the subaccount)]

In this step we will add a new service plan to a subaccount. Depending on when you created the SAP BTP account, this entitlement might already be assigned to the subaccount.

1. You can use the filter box if you see too many entries to find the entitlement you are looking for. Click on **Configure Entitlements** to switch the table into edit mode.

    !![findentitlement](findentitlement.png)

2. Click on **Add Service Plans** to see all available entitlements.

    !![addserviceplan](addserviceplan.png)

3. Use the filter bar the search for **`hana cloud`** to reduce the number of available entitlements. Click the checkbox to select the service plans **`hdi-data-connection`**, **`hana`**, and **`relational-data-lake`**. Finally, confirm your selection by clicking on **Add 3 Service Plan**.

    !![addplan](addplan.png)

4. In the table, you may see the quota you are going to assign to the subaccount depending on the selected service. As the trial landscape is for free, you can max out all your service plans.

> If you are not able to increase the quota of a given service plan, you might already consume this service plan in another subaccount. Please make sure to release the quota from other subaccounts, before reassigning it.


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step: ](Add more entitlements to subaccount)]

Congratulations, you just enabled the SAP HANA Cloud service in your subaccount. Depending on what you want to build, you might want to add one of more of the following service plans as well:

| Service | Usage Scenario |
|----|----|
|ABAP environment| For exploring the free trial of the SAP BTP, ABAP environment.|
|Kyma runtime| For exploring the free trial of the SAP BTP, Kyma runtime.|
|Cloud Foundry Runtime| Needed to run application on SAP BTP, Cloud Foundry runtime. The chosen quota defines the available amount of GB memory. |
|Enterprise Messaging|Messaging bus for inter-app communication within the Cloud Foundry environment.|
|Launchpad| Explore your SAPUI5 applications via the SAP Launchpad service.|
|SAP HANA Cloud| Manage all your data in a single solution with a multi-tier data storage including in-memory, warm disc memory and a petabyte scale data lake.|
|SAP HANA Schemas|The create HDI containers that can be connected to your apps.|
|Workflow|To use the SAP Workflow service within the Cloud Foundry environment.|
|SAP Business Application Studio|The development tools for cloud-native business applications.|

> This is an incomplete enumeration.

Follow the instructions from the previous step to add these new entitlements to your subaccount.


[VALIDATE_1]
[ACCORDION-END]
[ACCORDION-BEGIN [Step: ](Save the entitlements)]

Confirm you selection with **Save**.

!![save](save.png)


[DONE]
[ACCORDION-END]

---
