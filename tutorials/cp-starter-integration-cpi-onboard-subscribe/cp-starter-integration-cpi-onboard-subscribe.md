---
title: Set Up Your SAP Cloud Platform Integration Tenant
description: Subscribe to the Process Integration service, assign required roles, and use the self-service application to provision your own SAP Cloud Platform integration tenant.
auto_validation: true
time: 15
tags: [ tutorial>beginner, products>sap-cloud-platform, products>sap-cloud-platform-connectivity, products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-cloud-platform-integration-for-process-services
---

## Prerequisites
 - [Get a Free Trial Account on SAP Cloud Platform](https://developers.sap.com/tutorials/hcp-create-trial-account.html)
 - You have logged in to your SAP Cloud Platform trial account.
 - You have created a subaccount and an associated space.

## Details
### You will learn
  - How to subscribe to the Process Integration service and set-up your own SAP Cloud Platform Integration tenant

To provision your SAP Cloud Platform Integration tenant, you must first subscribe to the process integration service, assign the necessary roles to your user, and then provision your SAP Cloud Platform Integration tenant.

---

[ACCORDION-BEGIN [Step 1: ](Subscribe to Process Integration service)]
1. In your subaccount, select **Subscriptions** > **Process Integration**.

    ![Select process integration service](1.1.select-process-integration.png)

2. Select the **Subscribe** button.

    ![Select subscribe button](1.2.select-subscribe.png)

    Wait for the process to complete. You will see the status change to **Subscribed** in green.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Assign required roles)]
1. After you see the **Subscribed** icon that confirms your subscription, select your subaccount to navigate to the subaccount level.

    ![Navigate to subaccount](2.1.navigate-to-subaccount.png)

2. Select **Role Collections** > **New Role Collection**.

    ![Create role collection](2.2.create-role-collection.png)

3. In the **Name** field, enter **`Tenant_Administrator`** and select **Save**. Select the newly created **`Tenant_Administrator`** role collection.

    ![Create tenant admin role collection](2.3.create-tenant-admin-rc.png)

4. Select **Add Role**. In the **Role Template** dropdown list, select **`AuthGroup_Administrator`** and select **Save**.

    ![Add required roles](2.4.add-role.png)

    Similarly, add **`AuthGroup_BusinessExpert`** and **`AuthGroup_IntegrationDeveloper`** role templates and add these to the role collection.

[VALIDATE_7]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Configure trust)]

1. Select your subaccount name to navigate to your subaccount level. Select **Trust Configuration**.

    ![Access trust configuration](3.1.access-trust-configuration.png)

2. Select the default identity provider, **SAP ID Service**.

    ![Select default IDP](3.2.select-default-idp.png)

3. In the **E-Mail Address** field, enter your email ID and select **Show Assignments**. Since your user is not a part of the SAP ID Service, you get a prompt asking for the user to be added to SAP ID Service. Choose **Add User**.

    ![Assign user to IDP](3.3.assign-user.png)

4. Select **Show Assignments** > **Assign Role Collection**. In the **Assign Role Collection** prompt, choose **`Tenant_Administrator`** from the dropdown list and select **Assign Role Collection**.

    ![Assign Tenant_Administrator](3.4.assign-role-collection.png)

    These roles are required for you to create and model your integration flow.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Provision SAP Cloud Platform Integration tenant)]
1. Launch SAP Cloud Platform Integration provisioning application by choosing **<subaccount-name>** > **Subscriptions** > **Go to Application** in the **Process Integration** tile.

    ![Access cloud integration provisioning application](5.1.access-prov-app.png)

2. Login to the provisioning application by providing the same email ID and password that you used for creating the trial account. Choose **Log On**.

    ![Login to provisioning application](5.2.login-prov-app.png)

3. Choose **Provision** to provision a SAP Cloud Platform Integration tenant.

    ![Provision SAP Cloud Platform Integration tenant](5.3.provision-cpi.png)

4. You see a tenant URL after the provisioning is complete. This is the URL for your SAP Cloud Platform Integration tenant.

    ![Access tenant URL](5.4.access-tenant-url.png)

    Click on the URL to launch the SAP Cloud Platform Integration application. You can now create integration packages and integration flows.

    Please bookmark or save this URL. This will be the URL for your SAP Cloud Platform Integration application.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create service instance and key)]
1. Access your subaccount's space by choosing **Spaces** > **dev**. If you have intentionally specified any other space name, you can choose the space of your choice.

    ![Access space](4.1.access-space.png)

2. Choose **Service Marketplace** > **Process Integration Runtime**.

    ![Access PI Runtime](4.2.access-process-integration-runtime.png)

    >**TIP:** If you do not see the **Process Integration Runtime** service in the list, at the subaccount level, choose **Entitlements** > **Configure Entitlements** > **Add Service Plans**. Choose **Process Integration Runtime** and select the **integration-flow** service plan and choose **Add 1 Service Plan**.

3. Select **Instances** > **New Instance**.

    ![Create new instance](4.3.create.new.instance.png)

4. Choose **Next** to select the default service plan. In the **Specify Parameters** tab, enter:

    ```JSON
    {
     "roles":[
       "ESBMessaging.send"
     ]
    }
    ```

    ![Specify JSON for user role](4.4.specify-json-instance.png)

5. Choose **Next** until you have to specify the instance name. Provide any name of your choice and choose **Finish**.

    ![Specify instance name](4.5.specify-instance-name.png)

6. Select the newly created service instance.

    ![Select service key](4.6.select-service-instance.png)

    You create this service instance to enable inbound HTTP calls to your SAP Cloud Platform Integration tenant.

7. Choose **Create Service Key** to create a new service key.

    ![Create service key](4.7.create-service-key.png)

    This service key will provide you the credentials for making inbound HTTP calls to integration flows deployed on your SAP Cloud Platform Integration tenant.

8. Specify a name for the service key and choose **Save**.

    ![Enter service key name](4.8.create-service-key-2.png)

9. Copy the values of **`clientid`** and **`clientsecret`** parameters. These are the credentials that you use to make a request to your integration flow after you deploy it.

    ![Copy clientid and clientsecret](4.9.copy-clientid-secret.png)

    Please make a note of these credentials. You will use it when you make HTTP calls to integration flows deployed on your tenant with HTTP endpoints.


[DONE]
[ACCORDION-END]



--
