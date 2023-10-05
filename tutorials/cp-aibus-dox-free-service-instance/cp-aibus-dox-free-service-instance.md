---
parser: v2
auto_validation: true
time: 15
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, software-product>sap-business-technology-platform, software-product>sap-ai-business-services, software-product>document-information-extraction, tutorial>free-tier]
primary_tag: topic>machine-learning
author_name: Juliana Morais
author_profile: https://github.com/Juliana-Morais
---

# Use Free Tier to Create a Service Instance for Document Information Extraction
<!-- description --> Use the free tier service plan to create a service instance and the associated service key for Document Information Extraction using the free tier service plan.

## Prerequisites
- You have created an account on SAP BTP to try out free tier service plans: [Get an Account on SAP BTP to Try Out Free Tier Service Plans](btp-free-tier-account)
- You are entitled to use the Document Information Extraction service: [Manage Entitlements Using the Cockpit](btp-cockpit-entitlements)

## You will learn
  - How to check your subaccount entitlements and add Document Information Extraction
  - How to create a service instance of Document Information Extraction
  - How to create a service key for your service instance

---

### Access the SAP BTP cockpit


1. Open the [SAP BTP cockpit](https://account.hana.ondemand.com/cockpit#/home/allaccounts).

2. Access your global account.

    <!-- border -->![Access Subaccount](global-account.png)

3. Click the tile to access your subaccount.

<!-- border -->![Access Subaccount](access-subaccount.png)



### Check your entitlements


To use Document Information Extraction, you need to make sure that your account is properly configured.

1. On the navigation side bar, click **Entitlements** to see a list of all eligible services. You are entitled to use every service in this list according to the assigned service plan.

2. Search for `Document Information Extraction`. ***If you find the service in the list, you are entitled to use it. Now you can set this step to **Done** and proceed with Step 3.***

<!-- border -->![Check Entitlements](check-entitlements.png)

***ONLY if you DO NOT find the service in your list, proceed as follows:***

  1.  Click **Configure Entitlements**.

    <!-- border -->![Configure Entitlements](configure-entitlements.png)

  2.  Click **Add Service Plans**.

    <!-- border -->![Add Service Plans](add-service-plans.png)

  3.  In the dialog, select `Document Information Extraction`. Choose the `free` and `default (Application)` service plans. Click **Add 2 Service Plans**.

    >You can also perform this tutorial series using the `blocks_of_100` service plan. For that, choose the `blocks_of_100` plan in this step (instead of free). For more information on the service plans available for Document Information Extraction and their usage details, see [Service Plans](https://help.sap.com/docs/DOCUMENT_INFORMATION_EXTRACTION/5fa7265b9ff64d73bac7cec61ee55ae6/2c17b2aa8d6b4fa8a5c6259a33feb360.html).

    <!-- border -->![Add Service Plan](add-service-plan.png)

  4.  Click **Save** to save your entitlement changes.

    <!-- border -->![Save Entitlements](save-entitlements.png)

You are now entitled to use Document Information Extraction and create instances of the service.



### Access service via Service Marketplace


The Service Marketplace is where you find all the services available on SAP BTP.

1.  To access it, click **Service Marketplace** on the navigation side bar.

    <!-- border -->![Access Marketplace](access-marketplace.png)

2.  Next, search for **Document Information Extraction** and click the tile to access the service.

    <!-- border -->![Access Service](access-service.png)



### Create service instance


Next, you will create an instance of the Document Information Extraction service.

1. Click **Create** to start the service instance creation dialog.

    <!-- border -->![Create Instance](create-instance.png)

2. In the dialog, choose the `free` plan. Enter a name for your new instance, for example, `doc-info-extraction-inst` and click **Create**.

    >Choose `blocks_of_100` in this step (instead of free) if you're using the `blocks_of_100` plan to perform this tutorial series.

    <!-- border -->![Create Instance](create-instance-dialog.png)

3. In the following dialog, click on **View Instance** to navigate to the list of your service instances.

    <!-- border -->![View Instances](view-instances.png)

You have successfully created a service instance for Document Information Extraction.



### Create service key


You are now able to create a service key for your new service instance. Service keys are used to generate credentials to enable apps to access and communicate with the service instance.

1. Click the dots to open the menu and select **Create Service Key**.

    <!-- border -->![Service Key](create-service-key.png)

2. In the dialog, enter `doc-info-extraction-key` as the name of your service key. Click **Create** to create the service key.

    <!-- border -->![Create Service Key](create-service-key-name.png)

You have successfully created a service key for your service instance. You can now view the service key in the browser or download it.

<!-- border -->![View Service Key](view-service-key.png)


