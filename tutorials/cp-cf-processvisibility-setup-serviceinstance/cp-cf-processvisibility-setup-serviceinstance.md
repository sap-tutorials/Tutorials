---
title: Create a Service Instance of SAP Cloud Platform Process Visibility
description: Create a service instance using SAP Cloud Platform Cockpit to enable the Process Visibility service.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>cloud, tutorial>license]
primary_tag: products>sap-cloud-platform
---

## Prerequisites
 - You are a global account administrator and your global account is entitled to use SAP Cloud Platform Process Visibility.
 - You have created a subaccount.
 - You have created a space within a subaccount in which Cloud Foundry is enabled. Within that space, the user is assigned to the **Space Manager** and **Space Developer** roles for the subaccount.
 - You have SAP Cloud Platform Web IDE Full-Stack enabled on your Neo account.

## Details
### You will learn
  - How to create a service instance of SAP Cloud Platform Process Visibility using SAP Cloud Platform cockpit

SAP Cloud Platform Process Visibility provides end-to-end visibility and intelligence to processes, which run on cloud, on-premise, and in hybrid environments. For more information on SAP Cloud Platform Process Visibility, refer to [SAP Cloud Platform Process Visibility](https://help.sap.com/viewer/62fd39fa3eae4046b23dba285e84bfd4/Cloud/en-US/2f72882f457a4b87a054bdf45d85fe52.html).

You must create a service instance to enable SAP Cloud Platform Process Visibility. You can create multiple service instances of process visibility across different spaces within the same organization. These instances share the same data.

---

[ACCORDION-BEGIN [Step 1: ](Assign service entitlements to global account)]
1. Log onto SAP Cloud Platform Control Center to assign service entitlements to your global account. Use your global account admin credentials to access the Control Center.

2. In the SAP Cloud Platform Control Center, navigate to your global account, click **Edit** and choose **Assign Services** from the dropdown list.

    ![Assign Entitlement](Entitlement-01.png)

3. Search the service by typing the keyword of the service in the search field, select the service, and click **Save**. In this tutorial, you require entitlements for **Process Visibility**, **Portal**, and **Application Runtime** services.

    ![Entitlement for Process Visibility](Entitlement-PV-02.png)

    ![Entitlement for Portal](Entitlement-Portal-03.png)

    ![Entitlement for Application Runtime](Entitlement-App-Runtime-04.png)

    Choose **Next** after selecting all the required services for entitlement.

4. Add **Process Visibility**, **Portal**, and **Application Runtime** entitlements to your global account with their quota for maximum allowed consumption. Choose the service, increase the quota based on your requirement, and click **Save**.

    ![Quota for Process Visibility](Quota-PV-05.png)

    ![Quota for Portal](Quota-Portal-06.png)

    ![Quota for Application Runtime](Quota-App-Runtime-07.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Assign service entitlements to subaccount)]

1. Open the SAP Cloud Platform cockpit in your web browser. Under **Entitlements**, navigate to **Subaccount Assignments**.

    ![Subaccount Entitlements](subaccount-entitlements-08.png)

2. Search your subaccount in the search field, choose your subaccount, and click **Go**.

    ![Subaccount](subaccount-09.png)

    You will be able to see the list of services and their entitlements.

3. Click **Configure Entitlements** to configure the service entitlements to your subaccount.

    ![Configure Entitlements](Configure-Entitlements-10.png)

4. Add service plans to your services by clicking **Add Service Plans**.

    ![Add service plan](Add-serviceplan-11.png)

    ![Add service plan for PV](Add-Service-Plan-PV-12.png)

    ![Add service plan for portal](Add-Service-Plan-Portal-13.png)

    ![Add service plan for application runtime](Add-service-plan-App-Runtime-14.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Create a service instance)]

1. Open the SAP Cloud Platform cockpit in your web browser. Navigate to **Spaces** and choose your space.

    ![Space](Spaces-15.png)

2. Under **Services** in the navigation pane, select **Service Marketplace**.

    ![Service marketplace](Service-Marketplace-16.png)

3. In **Service Marketplace**, search for process visibility and choose the **Process Visibility** tile.

    ![Process visibility tile](PV-Tile-17.png)

4. In the navigation area, choose **Instances** and then select **New Instance** to create a new instance.

    ![New instance](New-Instance-18.png)

5. Choose the service plan **standard** and click **Next**.

    ![Standard plan](Standard-Plan-19.png)

6. No parameters are needed as shown in the following image, so choose **Next**.

    ![Parameters](Parameters-20.png)

7. In the next screen, choose **Next** as we don't need to bind any application.

    ![Application](Application-21.png)

8. In the **Instance Name** field, provide **`pvservice`** as the instance name. You can provide a unique instance name of your wish.

    >**IMPORTANT:** Note the instance name, as it is required to bind the UIs. The new instance is displayed in the list and the status present under the **Last Operation** changes to **Created**.

    ![Instance name](Service-Instance-Name-22.png)

    ![Created instance](Created-23.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create service key for service instance)]

1. In the navigation area, choose **Instances**, then choose the **`pvservice`** instance from the list for which you are creating a service key.

    ![Instances](Instances-24.png)

2. Choose **Service Keys** in the navigation area and select **Create Service Key**.

    ![Service key](Service-Key-25.png)

3. In the **Name** field, provide **`pvservicekey`** as the service key name and click **Save**.

    ![Service key name](Service-Key-Name-26.png)

[VALIDATE_1]
[ACCORDION-END]
