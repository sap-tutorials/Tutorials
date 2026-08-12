---
parser: v2
auto_validation: true
time: 20
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, software-product>sap-business-technology-platform, software-product>sap-ai-services, software-product>sap-document-ai]
primary_tag: topic>machine-learning
author_name: Juliana Morais
author_profile: https://github.com/Juliana-Morais
---

# Use Trial to Subscribe to the SAP Document AI Basic UI
<!-- description --> Get access to the SAP Document AI user interface application, using SAP Business Technology Platform (SAP BTP) Trial.

## Prerequisites
- You have created a service instance for SAP Document AI: [Use Trial to Create a Service Instance for SAP Document AI](cp-aibus-dox-service-instance).

## You will learn
  - How to get a trial tenant for Cloud Identity Services 
  - How to activate an account for Cloud Identity Services and establish trust
  - How to subscribe, assign role collection and access the SAP Document AI Trial application

---

### Get your trial tenant for Cloud Identity Services 


After completing the prerequisite tutorial [Use Trial to Create a Service Instance for SAP Document AI](cp-aibus-dox-service-instance), you can start with the steps to subscribe to the SAP Document AI Trial application.

1. From your global account page, choose the `trial` tile to access your subaccount.

    <!-- border -->![DOX-UI](enter-trial-account.png)

    The SAP Document AI Trial application requires a trial tenant for Cloud Identity Services. For more information, see [Get Trial Tenant](https://help.sap.com/docs/cloud-identity-services/cloud-identity-services/get-your-tenant?version=Cloud#get-trial-tenant). 

    The Service Marketplace is where you find all the services and applications available on SAP BTP.

2. On the navigation side bar, click **Service Marketplace**.

    <!-- border -->![DOX-UI](access-service-marketplace.png)
   
3. Search for **Cloud Identity Services** and click the tile. 

    <!-- border -->![DOX-UI](tile-ias.png)

4. In the top-right corner, click **Create**.

    <!-- border -->![DOX-UI](subscribe-ias.png)

4. In the dialog, choose the `default` plan and click **Create**.

  <!-- border -->![DOX-UI](dialog-create-ias.png)

The subscription is going to be created now. Click on **View Subscription** to go to the list of your existing subscriptions.

<!-- border -->![DOX-UI](dialog-in-progress.png)

>For more details on how to configure entitlements, quotas, subaccounts and plans on SAP BTP Trial, see [Manage Entitlements on SAP BTP Trial](cp-trial-entitlements).



### Activate your account for Cloud Identity Services


1. Go to your inbox and open the account activation email. Choose *Click here to activate your account*.

    <!-- border -->![DOX-UI](activate-account.png)

2. Enter a password and click **Continue**.

    <!-- border -->![DOX-UI](activate-account-continue.png)

3. You are informed that your account is successfully activated. Click **Continue**.

     <!-- border -->![DOX-UI](activate-account-acticated.png)

This opens the administration console of SAP Cloud Identity Services:

<!-- border -->![DOX-UI](console.png)



### Establish trust


1. Back on the SAP BTP Trial cockpit, under **Security**, click **Trust Configuration** and then in the top-right corner, click **Establish Trust**.

    <!-- border -->![DOX-UI](trust.png)    

2. Choose your trial tenant for Cloud Identity Services and click **Next** 3 times.

    <!-- border -->![DOX-UI](next.png) 

3. Click **Finish**.

    <!-- border -->![DOX-UI](finish.png)     

Your trial tenant for Cloud Identity Services is added to the list:
<!-- border -->![DOX-UI](list.png)



### Get subscribed to the SAP Document AI Trial application


1. On the navigation side bar, click **Service Marketplace**.

    <!-- border -->![DOX-UI](access-service-marketplace.png)

2. Search for **SAP Document AI** and click the tile.

    <!-- border -->![DOX-UI](tile.png)

3. Click **Create**.

    <!-- border -->![DOX-UI](subscribe.png)

4. In the dialog, choose the `default` plan and click **Create**.

  <!-- border -->![DOX-UI](dialog-create.png)

The subscription is going to be created now. Click on **View Subscription** to go to the list of your existing subscriptions.

<!-- border -->![DOX-UI](dialog-in-progress.png)



### Create user and assign role collection

1. Under **Security**, click **Users** and then click **Create**.

    <!-- border -->![DOX-UI](create-user.png) 

2. Enter your *User Name* and *E-mail*. In the *Identity Provider* dropdown, choose your trial tenant for Cloud Identity Services. Click **Create**.

    <!-- border -->![DOX-UI](create-user2.png) 

3. Click the **Navigation** arrow of your trial tenant for Cloud Identity Services.    

    <!-- border -->![DOX-UI](users.png)  

2. Click **Assign Role Collection**.

    <!-- border -->![DOX-UI](role-collection.png)

3. Choose **`Document_Information_Extraction_UI_Templates_Admin_trial`** to access all the features available in the UI application and click **Assign Role Collection**.

    <!-- border -->![DOX-UI](assign-role-collection.png)

Your trial tenant for Cloud Identity Services is now assigned to the **`Document_Information_Extraction_UI_Templates_Admin_trial`** role collection.

<!-- border -->![DOX-UI](roles.png)



### Go to application


Go back to **Instances and Subscriptions**, click the dots to open the menu and select **Go to Application** to open the app.

<!-- border -->![DOX-UI](go-to-app.png)

The SAP Document AI application is displayed:

<!-- border -->![DOX-UI](app.png)

You have successfully subscribed to the SAP Document AI basic UI. Find out how to use the application in the next tutorial: [Use Machine Learning to Extract Information from Documents with the SAP Document AI Basic UI](cp-aibus-dox-ui).