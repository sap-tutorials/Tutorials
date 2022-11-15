---
parser: v2
author_name: Ilyes Yamoun
author_profile: https://github.com/shielddz
auto_validation: true
time: 20
tags: [ tutorial>intermediate, software-product>sap-business-technology-platform, tutorial>free-tier]
primary_tag: software-product>sap-process-automation
---

# Configure Launchpad
<!-- description --> Configure launchpad for visibility scenario

## Prerequisites
 - [Get an Account on SAP BTP to Try Out Free Tier Service Plans](btp-free-tier-account)
 - Space in BTP Cockpit subaccount created

## You will learn
  - How to configure launchpad in **SAP BTP Cockpit** to gain visibility in your dashboard

---
In this tutorial, you will configure everything needed for the creation of a launchpad dashboard in **SAP BTP Cockpit** to gain visibility of your processes.

### Add entitlement

1.  Navigate to **SAP BTP Cockpit** subaccount > **Entitlements**.

    <!-- border -->![navigate to entitlements](Step1-1.png)

2.  Search **Launchpad Service** in the search bar.
    > If it is already added, skip to the next step of this tutorial.

    <!-- border -->![search launchpad service](Step1-2.png)

3.  Choose **Add Service Plans**.

    <!-- border -->![add service plans button](Step1-3.png)

4.  Search **Launchpad Service**:
    - Select **Launchpad Service** from the options shown.
    - Select **standard (Application)**.
    - Choose **Add 1 Service Plan**.

    <!-- border -->![Add service plan](Step1-4.png)

5.  Choose **Save**.

    <!-- border -->![save changes](Step1-5.png)

**Launchpad Service** entitlement has now been added.


### Create a new subscription

1.  Navigate to **SAP BTP Cockpit** subaccount > **Services** > **Instances and subscriptions**.

2.  Choose **Create**.

    <!-- border -->![navigate to instances and subscriptions](Step2-1.png)

3.  For the new subscription:
    -  Select **Launchpad Service** as a **Service**.
    -  Select your plan from the **Plan** list.
    -  Choose **Create**.
    <!-- border -->![Subscription details](Step2-2.png)

The subscription is now added.

<!-- border -->![Result Subscription](Step2-3.png)



### Assign roles to users

1.  Navigate to **SAP BTP Cockpit** subaccount > **Security** > **Users**.

    <!-- border -->![navigate to Users](Step3-1.png)

2.  Select the user to whom you will give the roles then on **Enter Full-Screen Mode** .

    <!-- border -->![Select user](Step3-2.png)

3.  Choose **Assign Role Collection**.

    <!-- border -->![Assign Role Collection button](Step3-3.png)

4.  Select **Launchpad Admin** and **Launchpad External User** then choose **Assign Role Collection**.

    > Launchpad Admin is the role for whom is going to design the launchpad in the launchpad editor.
    >
    > Launchpad external user is the role for whom accesses the final launchpad.
    >
    > Note: Launchpad Admin shouldn't be given to all users for security purposes.

    <!-- border -->![Select Roles](Step3-4.png)

The roles are now assigned.

<!-- border -->![Result assignation](Step3-5.png)



### Create SAP process automation instance

1.  Navigate to **SAP BTP Cockpit** subaccount > **Services** > **Instances and subscriptions**.

2.  Choose **Create**.

    <!-- border -->![navigate to instances and subscriptions](Step2-1.png)

3.  For the new instance:
    -  Select **SAP Process Automation** as a **Service**.
    -  Select **Instance** plan from the **Plan** list.
    -  Select **Cloud Foundry** as **Runtime Environment**.
    -  Select a space from **Space** list.
    -  Set **Instance Name** as **SPA-instance**.
    -  Choose **Create**.

    <!-- border -->![Instance details](Step4-2.png)

The instance is now added.

<!-- border -->![Result instance](Step4-3.png)



### Create a new destination

1.  Navigate to **SAP BTP Cockpit** subaccount > **Connectivity** > **Destinations**.

    <!-- border -->![navigate to Destinations](Step5-1.png)

2.  To create a new destination:
    -  Choose **New Destination**.
    -  Select **Service Instance**.
    -  Set **Service Instance** to **SPA-instance** that you created in Step **4.3**.
    -  Set **Name** to **spa-launchpad**.
    -  Choose **Next**.

    <!-- border -->![create a new destination](Step5-2.png)

3.  Choose **Save**.

    <!-- border -->![Save destination](Step5-3.png)

The new destination is now created.


### Open launchpad editor

1.  Navigate back to **SAP BTP Cockpit** subaccount > **Services** > **Instances and Subscriptions**.

2.  Choose **Go to Application** on **Launchpad Service** in **Applications** tab.

    <!-- border -->![navigate to Instances and Subscriptions](Step6-1.png)

Your launchpad is now ready to be created and designed with **Launchpad Editor**.

<!-- border -->![Launchpad Editor](Step6-2.png)

1.  Click **Provider Manager**.

    <!-- border -->![Provider manager](Step6-3.png)

2.  Click **Update Content**.

    <!-- border -->![Update Content](Step6-4.png)

    > The content updates and the status changes to Activated.

3.  Click **Content Manager**.

4.  On the top two tabs select **Content Explorer**.

4.  Select **HTML5 Apps**.

    <!-- border -->![Content manager > content Explorer > HTML5 Apps](Step6-5.png)

5.  Select all items and click **+ Add to My Content**.

    <!-- border -->![Select and add to my content](Step6-6.png)

6.  Navigate to **Content Manager** > **My Content**.

7.  Click **New** and select **Group**.

    <!-- border -->![Add new Group](Step6-7.png)

8.  Set **Title** to **SAP Process Automation**.

9.  Assign **My Inbox** and **Process Workspace** to the group.

10.  Hit **Save** and go back.

    <!-- border -->![Add Assignments to group](Step6-8.png)

11.  Click **Everyone** role to edit it.

    <!-- border -->![Everyone Role from list](Step6-9.png)

12.  Hit **Edit** to edit role.

    <!-- border -->![Edit button](Step6-10.png)

13.  Assign **My Inbox**, **Process Workspace**, **Visibility Scenario Dashboard** and **Visibility Scenario Instances** to **Everyone** role then click **Save**.

    <!-- border -->![Assignments to Everyone Role](Step6-11.png)


---
