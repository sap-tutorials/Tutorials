---
title: Enable SAP BTP, Kyma Runtime
description: Kyma itself is a runtime deployed on a Kubernetes cluster and is using services to provide you an environment for your applications, services and serverless functions to run in. For example, the Kyma runtime is using `Istio` to provide a Service Mesh to you for better scaling.
time: 15
auto_validation: true
tags: [ tutorial>beginner, topic>cloud, products>sap-business-technology-platform]
primary_tag: products>sap-btp\\, kyma-runtime
---

## Prerequisites
 - **Tutorials:** [Get a Free Trial Account on Get a Free Account on SAP BTP Trial](hcp-create-trial-account)

## Details
### You will learn
  - How to enable the SAP BTP, Kyma runtime trial.

---

[ACCORDION-BEGIN [Step 1: ](Set entitlements)]
If the entitlement for the Kyma runtime trial has not been activated, the button **Enable Kyma** will not be shown. The following steps detail the process of enabling the **Kyma Runtime** entitlement. In case you have the necessary entitlements set, continue with **Step 2** of this tutorial.

1. In the left-hand menu, click **Entitlements**.

    !![SAP-BTP-Trial](kyma-getting-started-01.png)

2. Use the search field to search for **Kyma** in order to check if you have the needed entitlements enabled. If you have a new trial account, the needed entitlements should automatically be added to your account as seen in the screenshot below.

    !![SAP-BTP-Trial](kyma-getting-started-02.png)

> In case you don't have the entitlement in your account you need to add it in order to continue. Without the entitlement you won't be able to enable the Kyma runtime for your account.

3. Click **Configure Entitlements**.

    !![SAP-BTP-Trial](kyma-getting-started-02b.png)

4. Click **Add Service Plans** to add the Kyma runtime entitlement to your account.

    !![SAP-BTP-Trial](kyma-getting-started-02c.png)

5. Search for **Kyma**, check the trial checkbox and click **Add 1 Service Plan**.

    !![SAP-BTP-Trial](kyma-getting-started-02d.png)

    > In case you see the message **You have already used all the global account quota for this service plan. To add this service plan, purchase more quota or remove quota from other subaccounts.**, you already have the Kyma runtime entitlement used in another subaccount. Navigate to the subaccount where the Kyma runtime entitlement is assigned and delete the entitlement. You can also continue with this tutorial in the entitled subaccount.

6. Click **Save** to save the newly added service plan.

    > If you want to learn more about entitlements, visit the [Add a New Entitlement to Your Subaccount](cp-cf-entitlements-add) tutorial.

7. Go back to the **Overview** with help of the menu item on the left-hand-side. You should see the **Kyma Environment** tab.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Enable the Kyma environment)]

1. Now that you've made sure the Kyma entitlement is set, click **Enable Kyma** to trigger the enablement of the Kyma environment.

    !![SAP-BTP-Trial](kyma-getting-started-03.png)

2. In the popup, enter your cluster name of choice and click **Create**.

    !![SAP-BTP-Trial](kyma-getting-started-03b.png)

    In the background, a new Kubernetes cluster is being set up where the Kyma runtime and all its components will run on. This might take up 60 min.

    To continue with this tutorial, you need to wait until the enablement is finished. You can still use the SAP BTP trial or close the window as the setup process will continue automatically.

    !![SAP-BTP-Trial](kyma-getting-started-03c.png)

    To learn more about the Kyma environment and its functionality, see:

    - [kyma-project](https://kyma-project.io/docs/)
    - [SAP Help Portal - Kyma Environment](https://help.sap.com/viewer/3504ec5ef16548778610c7e89cc0eac3/Cloud/en-US/468c2f3c3ca24c2c8497ef9f83154c44.html)
    - [SAP BTP, Kyma runtime](https://discovery-center.cloud.sap/serviceCatalog/kyma-runtime)
    - [Kyma - YouTube](https://www.youtube.com/channel/UC8Q8bBtYe9gQN-dQ-_L8JvQ)
    - [project "Kyma" - Enablement Content](https://www.youtube.com/playlist?list=PL6RpkC85SLQC33__v6BFLDcV32uy5D3Rz)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Assign User to Role Collection)]

Once you've enabled the Kyma runtime, the last step is to assign your user the role needed to access the Kyma console UI. SAP BTP Cockpit has already created two different role collections for you in the process of Kyma runtime activation. You just need to assign your user to one or both of these collections.

The role collections available to you are:

- `KymaRuntimeDeveloper` role collection with the `KymaRuntimeDeveloper` role assigned
- `KymaRuntimeNamespaceAdmin` role collection with the `KymaRuntimeNamespaceAdmin` role assigned

In this tutorial, you will assign the `KymaRuntimeNamespaceAdmin` role collection as it allows you to have full access to the full feature set provided by the Kyma runtime trial.

1. If not already done, login to the SAP BTP Cockpit and choose the subaccount you want to assign roles to.

    !![SAP-BTP-Trial](kyma-getting-started-04.png)

2. In the left navigation panel, select the **Security** tab and choose **Role Collections**.

    > If you have followed this tutorial you should have the above mentioned role collections defined.

    !![SAP-BTP-Trial](kyma-getting-started-05.png)

 3. Click on the `KymaRuntimeNamespaceAdmin` role collection to get to the details.

    !![SAP-BTP-Trial](kyma-getting-started-06.png)

4. Click **Edit** to assign a user to this role collection.

    !![SAP-BTP-Trial](kyma-getting-started-07.png)

5. In the **Users** section, add the user you want to assign to the `KymaRuntimeNamespaceAdmin` role. Provide the email address in the **ID** field.

    !![SAP-BTP-Trial](kyma-getting-started-08.png)

6. Click **+** to add the user to the list.

    !![SAP-BTP-Trial](kyma-getting-started-09.png)

7. Click on **Save** to save your changes.

    !![SAP-BTP-Trial](kyma-getting-started-10.png)

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Open Kyma runtime console UI)]

To open the Kyma runtime console UI, navigate to **Services > Instances and Subscriptions**. Under the **Environments** section you can find the **Kyma Environment**.

!![SAP-BTP-Trial](kyma-getting-started-11.png)

Click on the **Kyma Environment** row to open the detail side panel.

!![SAP-BTP-Trial](kyma-getting-started-11a.png)

Click on the **Go to Dashboard** to open the Kyma runtime console UI.

The dashboard or Kyma console UI is your graphical playground for managing and deploying applications or services on the Kyma runtime. With the dashboard you can not only deploy or delete deployments but also manage them for scale, expose them over self-defined API Rules and much more.

To learn more about the capabilities and features of the SAP BTP, Kyma runtime, follow the Kyma tutorials, blog posts, read the documentation or check out the YouTube videos. If you aren't aware, there is also a [Kyma slack channel](https://kyma-community.slack.com/) where you can ask questions to the active community.

!![SAP-BTP-Trial](kyma-getting-started-12.png)

[DONE]
[ACCORDION-END]
