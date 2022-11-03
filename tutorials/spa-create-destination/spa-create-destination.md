---
parser: v2
author_name: Chaitanya Priya Puvvada
author_profile: https://github.com/chaitanya-priya-puvvada
auto_validation: true
time: 10
tags: [ tutorial>beginner , software-product>sap-business-technology-platform , tutorial>free-tier]
primary_tag: software-product>sap-process-automation
---

# Configure BTP Destinations in SAP Process Automation
<!-- description --> Configure BTP Destinations in SAP Process Automation to securely store sensitive information.

## Prerequisites
 - Subaccount Administrator role to create Destinations in SAP Business Technology platform.

## You will learn
  - How to create destinations in BTP for SAP Process Automation
  - Add Destinations to Process Automation

---
Destinations are predefined endpoints used in the Business Technology Platform (or BTP). Destinations allow you to securely store sensitive information used to connect to a system, such as client credentials, keys, or certificates in the BTP.

### Create a Destination in SAP Business Technology Platform



1.  Navigate to **Settings** page in SAP Process tenant.

2.  Choose **Destinations**

3.  Choose **Open in BTP Cockpit**.

    >This Button is visible if you have an IRPA Officer role.

    <!-- border -->![Open BTP Cockpit](1.png)

    You will be navigated to the **Destinations** page of SAP Business Technology Platform.

4.  Choose **New Destination**.

5.  Enter the details for the Destination. Refer to [Managing Destinations](https://help.sap.com/docs/CP_CONNECTIVITY/cca91383641e40ffbe03bdc78f00f681/84e45e071c7646c88027fffc6a7bb787.html) for more information.

6.  Add the property **sap.processautomation.enabled** as **true** to view the destination in the SAP Process Automation tenant.    

    <!-- border -->![New Destination](1.1.png)



### Create a Destination in SAP Process Automation Tenant


1.  Navigate to Settings>Destinations>New Destination.

2.  Select the name of the destination that you created in **Step 1** and choose **Add**.

    <!-- border -->![New Destination](2.png)

    The Destination is successfully added to SAP Process Tenant.

    <!-- border -->![Successful Destination](3.png)


### Access Destination in the Business Process



The destination can be accessed in the **Business Process** project  by creating an Environment variable of the type Destination.

Create a **Business Process** from the Lobby.
After creating the Business Process , create the Environment variable of the type Destination.

1.  Navigate to Settings>Environment Variables.

2.  Create an Environment Variable of type **Destination**.

    <!-- border -->![Environment Variable](4.png)

    The Destination can be accessed successfully in your Business Process Project.


---
