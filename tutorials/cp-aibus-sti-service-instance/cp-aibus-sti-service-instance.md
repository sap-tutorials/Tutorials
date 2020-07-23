---
title: Create Service Instance for Service Ticket Intelligence
description: Create a service instance and the associated service keys for Service Ticket Intelligence, one of the SAP AI Business Services, using the SAP Cloud Platform trial cockpit.
auto_validation: true
time: 15
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, products>sap-cloud-platform, products>sap-ai-business-services, products>service-ticket-intelligence]
primary_tag: topic>machine-learning
---

## Details
### You will learn
  - How to check your Service Ticket Intelligence entitlements
  - How to create a service instance of Service Ticket Intelligence
  - How to create service keys for your service instance

---

[ACCORDION-BEGIN [Step 1: ](Access your Cloud Foundry trial environment)]

Access the [SAP Cloud Platform Cockpit](https://cockpit.hanatrial.ondemand.com/cockpit/#/home/trial) and log in if necessary. Click **Enter Your Trial Account** to access your global account created in [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account).

![Trial Home](cockpit-home.png)

On the navigation sidebar, click **Subaccounts** and select your **trial** subaccount.

![Subaccounts](enter-trial-account.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Check your entitlements)]

To try out Service Ticket Intelligence, you need to make sure that your account is properly configured.

On the navigation sidebar, click **Entitlements** to see a list of all eligible services. You are entitled to use every service in this list according to the assigned service plan.

Search for **Service Ticket Intelligence Trial**. If you find the service in the list, you are entitled to use it. Now you can set this step to **Done** and proceed with step 3.

![Entitlements](check-entitlements.png)

If you do not find the service in your list, proceed as follows:

  1. Click **Configure Entitlements**.

    ![Configure Entitlements](configure-entitlements.png)

  2. Click **Add Service Plans** to add service plans to your entitlements.

    ![Add Service Plan](add-service-plans.png)

  3. Select **Service Ticket Intelligence Trial**, and choose the **standard** service plan. Click **Add 1 Service Plan**.

    ![Add Service Plan](add-entitlements.png)

  4. **Save** your entitlement changes.

    ![Add Service Plan](save-entitlements.png)    

You are now entitled to use the service and to create instances of the service.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Access your space)]

All applications and services live in spaces. By default, trial accounts only have the **dev** space available.

To access your spaces, click **Spaces** on the navigation sidebar and select the **dev** space to open it.

![Spaces](access-space.png)

In this space you will create your service instance.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Access service via Service Marketplace)]

The **Service Marketplace** is where you find the available services on SAP Cloud Platform.

To access it, click **Service Marketplace** on the navigation sidebar.

![Service Marketplace](access-service-marketplace.png)

Next, search for **Service Ticket Intelligence**. Click the tile named `service-ticket-intelligence-trial` to access the service.

![Service Ticket Intelligence in Service Marketplace](access-sti.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Create a service instance)]

You will now create an instance of your service.

To create an instance, click **Instances** on the navigation sidebar.

Next, click **New Instance** to start the creation dialog.

![Service Instances](create-instance.png)

  1. In the dialog, leave the default value for the service plan and click **Next**.

    ![Create Instance](create-instance-service-plan.png)

  2. Leave the parameters empty and click **Next**.

  3. Do not assign any application and click **Next**.

  4. Finally, enter the name `sti-demo` for your new instance. Click **Finish** to create the instance.

    ![Create Instance](create-instance-name.png)

You have successfully created a service instance for Service Ticket Intelligence.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Create service keys)]

You are now able to create service keys for your new service instance. Service keys are used to generate credentials to enable apps to access and communicate with the service instance.

To create service keys, first access your service instance by clicking its name.

![Service Instances](access-instance.png)

Now, select **Service Keys** on the navigation sidebar, and click **Create Service Key** to start the creation dialog.

![Service Keys](create-service-keys.png)

In the dialog, enter the name **`sti-demo-key`** for your service keys. Leave empty the **Configuration Parameters (JSON)** box.

 Click **Save** to create the service keys.

![Create Service Key](create-service-key-name.png)

You have successfully created service keys for your service instance. Make a local copy of the service keys. You will need the service keys values in [Set Up Postman Environment and Collection to Call Service Ticket Intelligence APIs](cp-aibus-sti-setup-postman).

![Service Key](service-key.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](List your service instances)]

To list and access your service instances, there is no need to access the service via the Service Marketplace every time. In your **Space**, you can see a list of all your service instances.

**Go back** to your **dev** space using the breadcrumbs at the top of the page.

As you navigate through the SAP Cloud Platform Cockpit and dig into more detail, the breadcrumbs at the top of the page show you the hierarchy of your navigation. You can use them to go back to previous steps.

![Breadcrumbs](nav-back-breadcrumbs.png)

Back in your space, click **Service Instances** on the navigation sidebar.

![Service Instances List](service-instances-list.png)

The list shows all your service instances across all services, including the service instance you have just created. You find here information on the service plan, referencing applications, created service keys and the current status of your instances. From here, you can manage your service instances, access or delete them, for example.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Validate your service key)]

In the text area below, paste the `tenantmode` value from the service keys you created for your service instance, then click **Submit Answer**.

[VALIDATE_1]
[ACCORDION-END]
