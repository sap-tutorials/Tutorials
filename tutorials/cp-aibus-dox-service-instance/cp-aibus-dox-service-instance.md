---
title: Create Service Instance for Document Information Extraction
description: Create a service instance and the associated service keys for the service.
auto_validation: true
time: 15
tags: [tutorial>beginner, topic>machine-learning, topic>cloud, products>sap-cloud-platform, products>sap-ai-business-services, products>document-information-extraction]
primary_tag: topic>machine-learning
---

## Details
### You will learn
  - How to check your Document Information Extraction entitlements
  - How to create a service instance of Document Information Extraction
  - How to create service keys for your service instance

With Document Information Extraction, you can [Use Machine Learning to Extract Information from Documents](cp-aibus-dox-swagger-ui). Before that, you need to create a service instance and the associated service keys for Document Information Extraction using the SAP Cloud Platform Trial.

---

[ACCORDION-BEGIN [Step 1: ](Access your SAP Cloud Platform trial)]

1. In case you do not have a global trial account on SAP Cloud Platform, follow the steps described in [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account).

2. Access the [SAP Cloud Platform Trial](https://cockpit.hanatrial.ondemand.com/cockpit/#/home/trial) and login if necessary.

3. Click **Enter Your Trial Account** to access your global trial account.

    ![Trial Home](cockpit-home.png)

4. On the navigation side bar, click **Subaccounts** and select your **trial** subaccount.

    ![Subaccounts](enter-trial-account.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Check entitlements)]

To try out Document Information Extraction, you need to make sure that your account is properly configured.

1. On the navigation side bar, click **Entitlements** to see a list of all eligible services. You are entitled to use every service in this list according to the assigned service plan.

2. Search for **Document Information Extraction**. If you find the service in the list, you are entitled to use it. Now you can set this step to **Done** and proceed with Step 3.

    ![Entitlements](check-entitlements.png)

If you do not find the service in your list, proceed as follows:

  1. Click **Configure Entitlements**.

    ![Configure Entitlements](configure-entitlements.png)

  2. Click **Add Service Plans** to add service plans to your entitlements.

    ![Add Service Plan](add-service-plans.png)

  3. Select **Document Information Extraction**, and choose the **default** service plan. Click **Add 1 Service Plan**.

    ![Add Service Plan](add-entitlements.png)

  4. **Save** your entitlement changes.

    ![Add Service Plan](save-entitlements.png)    

You are now entitled to use the service and to create instances of the service.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Access your space)]

All applications and services live in spaces. By default, trial accounts only have the **dev** space available.

To access your spaces, click **Spaces** on the navigation side bar and select the **dev** space to open it.

![Spaces](access-space.png)

You will create your service instance in this space.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Access service via Service Marketplace)]

The **Service Marketplace** is where you find the available services on SAP Cloud Platform.

1. To access it, click **Service Marketplace** on the navigation side bar.

    ![Service Marketplace](access-service-marketplace.png)

2. Next, search for **Document Information Extraction**, and click the tile to access the service.

    ![Document Information Extraction in Service Marketplace](access-dox.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create a service instance)]

You will now create an instance of your service.

1. Click **Instances** on the navigation side bar.

2. Click **New Instance** to start the creation dialog.

    ![Service Instances](create-instance.png)

    - In the dialog, leave the default value for the service plan and click **Next**.

      ![Create Instance](create-instance-service-plan.png)

      - Leave the parameters empty and click **Next**.

      - Do not assign any application and click **Next**.

      - Finally, enter a name for your new instance as `doc-info-extraction-inst`.

      - Click **Finish** to create the instance.

        ![Create Instance](create-instance-name.png)

You have successfully created a service instance for Document Information Extraction.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create service Keys)]

You are now able to create service keys for your new service instance. Service keys are used to generate credentials to enable apps to access and communicate with the service instance.

1. To create service keys, first access your service instance by clicking its name.

    ![Service Instances](access-instance.png)

2. Now select **Service Keys** on the navigation side bar.

3. Click **Create Service Key** to start the creation dialog.

    ![Service Keys](create-service-keys.png)

    - In the dialog, enter `doc-info-extraction-keys` as the name for your service keys. Leave empty the **Configuration Parameters (JSON)** box.

    - Click **Save** to create the service keys.

        ![Create Service Key](create-service-key-name.png)

You have successfully created service keys for your service instance. Make a local copy of the service keys. You will need the service keys values to create your `access_token` in [Get OAuth Access Token for Document Information Extraction Using a REST Client](cp-aibus-dox-rest-oauth-token).

![Service Key](service-key.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](List your service instances)]

To list and access your service instances, there is no need to access the service via the Service Marketplace every time. In your space, you can see a list of all your service instances.

1. Go to your **dev** space using the breadcrumbs at the top of the page.

    As you navigate through the SAP Cloud Platform Trial and dig into more detail the breadcrumbs at the top of the page show you the hierarchy of your navigation. You can use them to go back to previous steps.

    ![Breadcrumbs](nav-back-breadcrumbs.png)

2. Back in your space, click **Service Instances** on the navigation side bar.

    ![Service Instances List](instance-list.png)

The list shows all your service instances across all services, including the service instance you have just created. You find here information on the service plan, referencing applications, created service keys and the current status of your instances. From here, you can manage your service instances, access or delete them, for example.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Validate your service keys)]

In the text area below, paste the `tenantmode` value from the service keys you created for your service instance, then click **Submit Answer**.

[VALIDATE_1]
[ACCORDION-END]
