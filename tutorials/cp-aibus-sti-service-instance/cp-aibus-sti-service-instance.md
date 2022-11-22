---
parser: v2
auto_validation: true
time: 15
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, software-product>sap-business-technology-platform, software-product>sap-ai-business-services, software-product>service-ticket-intelligence]
primary_tag: topic>machine-learning
author_name: Juliana Morais
author_profile: https://github.com/Juliana-Morais
---

# Use Trial to Create a Service Instance for Service Ticket Intelligence
<!-- description --> Create a service instance and the associated service key for Service Ticket Intelligence, one of the SAP AI Business Services, using SAP Business Technology Platform (SAP BTP) Trial.

## Prerequisites
- You have created a trial account on SAP BTP: [Get a Free Account on SAP BTP Trial](hcp-create-trial-account)
- You have a subaccount and dev space with **US East (VA)** as region: [Manage Entitlements on SAP BTP Trial](cp-trial-entitlements). See also [Create a Subaccount](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/261ba9ca868f469baf64c22257324a75.html). You can also use old trial subaccounts created before October 2021 with **Europe (Frankfurt)** as region.

## You will learn
  - How to check your Service Ticket Intelligence entitlements
  - How to create a service instance of Service Ticket Intelligence
  - How to create a service key for your service instance
---

### Go to your trial account


1. In your web browser, open the [SAP BTP Trial cockpit](https://cockpit.hanatrial.ondemand.com/).

2. Navigate to the trial global account by clicking **Go to Your Trial Account**.

    <!-- border -->![Trial global account](01_Foundation20Onboarding_Home.png)

    >If this is your first time accessing your trial account, you'll have to configure your account by choosing a region. **Please select US East (VA)**. Your user profile will be set up for you automatically.

    >Wait till your account is set up and ready to go. Your global account, your subaccount, your organization, and your space are launched. This may take a couple of minutes.

    >Choose **Continue**.

    ><!-- border -->![Account setup](02_Foundation20Onboarding_Processing.png)

3. From your global account page, choose the `trial` tile to access your subaccount.

    <!-- border -->![Subaccounts](enter-trial-account.png)



### Check entitlements


To try out Service Ticket Intelligence, you need to make sure that your account is properly configured.

1. On the navigation sidebar, click **Entitlements** to see a list of all eligible services. You are entitled to use every service in this list according to the assigned service plan.

2. Search for **Service Ticket Intelligence Trial**. ***If you find the service in the list, you are entitled to use it. Now you can set this step to **Done** and proceed with Step 3.***

    <!-- border -->![Entitlements](check-entitlements.png)

***ONLY if you DO NOT find the service in your list, proceed as follows:***

  1. Click **Configure Entitlements**.

    <!-- border -->![Configure Entitlements](configure-entitlements.png)

  2. Click **Add Service Plans** to add service plans to your entitlements.

    <!-- border -->![Add Service Plan](add-service-plans.png)

  3. Select **Service Ticket Intelligence Trial**, and choose the **`blocks_of_100`** service plan. Click **Add 1 Service Plan**.

    <!-- border -->![Add Service Plan](add-entitlements.png)

  4. **Save** your entitlement changes.

    <!-- border -->![Add Service Plan](save-entitlements.png)    

You are now entitled to use the service and to create instances of the service.

>For more details on how to configure entitlements, quotas, subaccounts and service plans on SAP BTP Trial, see [Manage Entitlements on SAP BTP Trial](cp-trial-entitlements).



### Access service via Service Marketplace


The **Service Marketplace** is where you find the available services on SAP BTP.

To access it, click **Service Marketplace** on the navigation sidebar.

<!-- border -->![Service Marketplace](access-service-marketplace.png)

Next, search for **Service Ticket Intelligence**. Click the tile to access the service.

<!-- border -->![Service Ticket Intelligence in Service Marketplace](access-sti.png)



### Create service instance


You will now create an instance of your service.

Click **Create Instance** to start the creation dialog.

<!-- border -->![Service Instance](create-instance.png)

In the dialog, choose the **`blocks_of_100`** service plan. Enter a name for your new instance, for example, `stint-inst` and click **Create Instance**.

<!-- border -->![Create Instance](create-instance-dialog.png)

In the following dialog, click on **View Instance** to be navigated to the list of your service instances.

<!-- border -->![View Instances](view-instances.png)

You have successfully created a service instance for Service Ticket Intelligence.



### Create service key


You are now able to create a service key for your new service instance. Service keys are used to generate credentials to enable apps to access and communicate with the service instance.

  1. Click the dots to open the menu and select **Create Service Key**.

      <!-- border -->![Service Key](create-service-keys.png)

  2. In the dialog, enter `stint-inst-key` as the name of your service key. Click **Create** to create the service key.


      <!-- border -->![Create Service Key](create-service-key-name.png)

You have successfully created a service key for your service instance. You can now view the service key in the browser or download it.

<!-- border -->![View Service Key](view-service-key.png)

You will need the service key values in [Set Up Postman Environment and Collection to Call Service Ticket Intelligence APIs](cp-aibus-sti-setup-postman).


