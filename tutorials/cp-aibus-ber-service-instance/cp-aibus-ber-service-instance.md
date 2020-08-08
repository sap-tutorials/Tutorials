---
title: Create Service Instance for Business Entity Recognition
description: Create a service instance and the associated service keys for Business Entity Recognition, one of the SAP AI Business Services, using the SAP Cloud Platform trial cockpit.
auto_validation: true
time: 15
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, products>sap-cloud-platform, products>sap-ai-business-services, products>business-entity-recognition]
primary_tag: topic>machine-learning
---

## Details
### You will learn
  - How to check your Business Entity Recognition entitlements
  - How to create a service instance of Business Entity Recognition
  - How to create service keys for your service instance

---

[ACCORDION-BEGIN [Step 1: ](Enter your trial account)]

1. In your web browser, open the [SAP Cloud Platform trial cockpit](https://cockpit.hanatrial.ondemand.com/).

2. Navigate to the trial global account by clicking **Enter Your Trial Account**.

    !![Trial global account](png-files/01_Foundation20Onboarding_Home.png)

    >Business Entity Recognition is only available in the Europe (Frankfurt) region. Follow the steps described in [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account) and choose the Europe (Frankfurt) region. If this is not possible, create a new subaccount in the Europe (Frankfurt) region. You should then be able to find **Business Entity Recognition Trial** in your new subaccount **Entitlements**.

    >If this is your first time accessing your trial account, you'll have to configure your account by choosing a region. Your user profile will be set up for you automatically.

    >Wait till your account is set up and ready to go. Your global account, your subaccount, your organization, and your space are launched. This may take a couple of minutes.

    >Choose **Continue**.

    >!![Account setup](png-files/02_Foundation20Onboarding_Processing.png)

3. From your global account page, choose the `trial` tile to access your subaccount.

    !![Subaccounts](png-files/enter-trial-account.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Check entitlements)]

To try out Business Entity Recognition, you need to make sure that your subaccount is properly configured.

1. On the navigation side bar, click **Entitlements** to see a list of all eligible services. You are entitled to use every service in this list according to the assigned service plan.

2. Search for **Business Entity Recognition Trial**. If you find the service in the list, you are entitled to use it. Now you can set this step to **Done** and go to step 3.

    !![Entitlements](png-files/check-entitlements.png)

If you do not find the service in your list, proceed as follows:

  1. Click **Configure Entitlements**.

    !![Configure Entitlements](png-files/configure-entitlements.png)

  2. Click **Add Service Plans**.

    !![Add Service Plan](png-files/add-service-plans.png)

  3. Select **Business Entity Recognition Trial**, and choose the **standard** service plan. Click **Add 1 Service Plan**.

    !![Add Service Plan](png-files/add-entitlements.png)

  4. **Save** your **Entitlements** changes.

    !![Add Service Plan](png-files\save-entitlements.png)    

You are now entitled to use Business Entity Recognition and create instances of the service.

>For more details on how to configure entitlements, quotas, subaccounts and service plans on SAP Cloud Platform Trial, see [Manage Entitlements on SAP Cloud Platform Trial](cp-trial-entitlements).

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Access your space)]

You will find all applications and services in spaces. By default, trial accounts only have the **dev** space available.

Click **Spaces** on the navigation side bar and select the **dev** space to open it.

!![Spaces](png-files/access-space.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Access service via Service Marketplace)]

The **Service Marketplace** is where you find all the services available on SAP Cloud Platform.

1. To access it, click **Service Marketplace** on the navigation side bar.

    !![Service Marketplace](png-files/access-service-marketplace.png)

2. Next, search for **Business Entity Recognition** and click the tile to access the service.

    !![Business Entity Recognition in Service Marketplace](png-files/access-ber.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create service instance)]

You will now create an instance of your service.

1. Click **Instances** on the navigation side bar.

2. Click **New Instance** to start the creation dialog.

    !![Service Instances](png-files/create-instance.png)

    - In the dialog, leave the default value for the service plan and click **Next**.

      !![Create Instance](png-files/create-instance-service-plan.png)

      - Leave the parameters empty and click **Next**.

      - Do not assign any application and click **Next**.

      - Finally, enter a name for your new instance as `ber-inst`.

      - Click **Finish** to create the instance.

        !![Create Instance](png-files/create-instance-name.png)

You have successfully created a service instance for Business Entity Recognition.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create service keys)]

You are now able to create service keys for your new service instance. Service keys are used to generate credentials to enable apps to access and communicate with the service instance.

1. To create service keys, first access your service instance by clicking its name.

    !![Service Instances](png-files/access-instance.png)

2. Now select **Service Keys** on the navigation side bar.

3. Click **Create Service Key** to start the creation dialog.

    !![Service Keys](png-files/create-service-keys.png)

    - In the dialog, enter `ber-key` as the name for your service key. Leave the **Configuration Parameters (JSON)** box empty.

    - Click **Save** to create the service keys.

        !![Create Service Key](png-files/create-service-key-name.png)

You have successfully created service keys for your service instance. Make a local copy of the service keys. You will need the service keys values to create your `access_token` in the next tutorial: [Get OAuth Access Token for Business Entity Recognition Using Any Web Browser](cp-aibus-ber-web-oauth-token).

!![Service Key](png-files\service-key.png)

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](List your service instances)]

To list and access your service instances, there is no need to access the service via the Service Marketplace every time. In your space, you can see a list of all your service instances.

1. Go to your **dev** space using the breadcrumbs at the top of the page.

    As you navigate through SAP Cloud Platform trial and dig into more detail the breadcrumbs at the top of the page show you the hierarchy of your navigation. You can use them to go back to previous steps.

    !![Breadcrumbs](png-files/nav-back-breadcrumbs.png)

2. Back in your space, click **Service Instances** on the navigation side bar.

    !![Service Instances List](png-files/instance-list.png)

The list shows all your service instances across all services, including the service instance you have just created. Here, you find information on the service plan, referencing applications, created service keys and the current status of your instances. You can also manage your service instances, access or delete them, for example.

[DONE]
[ACCORDION-END]
