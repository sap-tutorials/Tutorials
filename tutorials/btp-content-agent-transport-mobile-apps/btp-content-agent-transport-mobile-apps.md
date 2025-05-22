---
author_name: Gauravi Shukla
author_profile: https://github.com/GauraviShuklaSAP
keywords: tutorial
auto_validation: true
time: 45
tags: [ tutorial>beginner, software-product>sap-content-agent-service, software-product>sap-cloud-transport-management, software-product>sap-mobile-services ]
primary_tag: software-product>sap-content-agent-service
parser: v2
---
# Transport Mobile Development Kit (MDK) apps using SAP Content Agent UI

<!-- description --> Learn how to use SAP Content Agent UI to explore Mobile Development Kit (MDK) apps, select and transport to SAP Cloud Transport Management Service. 

## Prerequisites

 - You have an SAP BTP account as source where you have subscribed to SAP Mobile service. For more information, follow the tutorial [Create Your First MDK Application.](https://developers.sap.com/mission.mobile-dev-kit-get-started.html).
 - You've set up SAP Cloud Transport Management service in the central administrative subaccount. If you haven't done that, follow the tutorial [Get Started with SAP Cloud Transport Management](https://developers-qa-blue.wcms-nonprod.c.eu-de-2.cloud.sap/tutorials/btp-transport-management-getting-started.html).
 - You have an SAP BTP account as source where you have subscribed to SAP Mobile service and is empty. 


## You will learn
   - How to subscribe to SAP Content Agent service
   - How to set up role collections required for SAP Content Agent service
   - How to configure the destination targeted to SAP Cloud Transport Management by creating a service instance and a service key 
   - How to configure landscape in Cloud Transport Management
   - How to create the transport request using export option in SAP Content Agent service

## Scenario Overview

SAP Content Agent service acts as a local agent in the SAP BTP account that helps to standardize content management operation for various SAP BTP services.

In this tutorial, you will learn to create transport requests to move your apps from the source SAP BTP account where you’ve tested and finalized your development changes to other SAP BTP accounts.

For more information about SAP Content Agent Service, see the SAP Help Portal at [SAP Content Agent Service](https://help.sap.com/docs/content-agent-service).

![scenario](screenshots/scenario.png)

---
### Open the SAP BTP Cockpit

1. To access SAP BTP Cockpit of your enterprise account, choose [https://cockpit.btp.cloud.sap](https://cockpit.btp.cloud.sap).
   Depending on your own geo location, this URL will redirect you to the closest regional SAP BTP Cockpit URL.

2. In your global account, navigate to the Dev Source subaccount from where you want to transport Mobile apps. 

	![global-acc](screenshots/global-account.png)
---
### Subscribe To SAP Content Agent

To enable the usage of the user interface of SAP Content Agent service, subscribe to the free plan.

1. In your subaccount, go to **Services > Service Marketplace**.

2. Use the *Search* field to filter for *content agent*.

3. The *SAP Content Agent* tile is displayed.    

    ![Subscription 1](screenshots/service-marketplace.png)

4. Select the three dots (...) on the tile, and choose **Create**.

    ![Subscription 2](screenshots/create-subscription.png)

5. On the **New Instance or Subscription** dialog, from the **Plan** dropdown box, select **free** and choose **Create**.

    ![Subscription 3](screenshots/subscription-plan.png)

6. The subscription is in progress. Choose **View Subscription**. You've subscribed to the application. 

    ![Subscription 5](screenshots/subscription.png)

> See also on SAP Help Portal: [Initial Setup](https://help.sap.com/docs/content-agent-service/user-guide/subscribe-to-content-agent-service)

---
### Set Up Role Collections

After successful subscription, you need to configure user access to the application.  You can create role collections and assign roles to the role collections based on the application templates. Afterwards, you assign the role collections to users or user groups. 

1.  To use the delivered role collections, choose **Security > Role Collections** from the navigation on the left. Filter for role collections called *content agent*. The role collections are displayed. 

    ![Roles1](screenshots/delivered-roles.png)

2. To assign users to the desired role collection, choose **Content Agent Admin**, from the list. In the details of the role collection, choose **Edit**.

    ![Roles3](screenshots/roles-edit.png)

3. You can add individual users or user groups to the role collection. In the tutorial, add an individual user. To do this, select the identity provider (here: **Default identity provider**). In the ID field, enter an existing e-mail address and choose **Enter**. The **E-Mail** field is automatically filled with the selected e-mail address. Save your changes.

    ![Roles4](screenshots/user-edit.png)

Repeat the steps, if you want to add users other role collections for other tasks, such as the **Content Agent Import Operator** for import tasks. See also on SAP Help Portal: [Setting Up Role Collections](https://help.sap.com/docs/content-agent-service/user-guide/assign-user-roles-and-permissions?locale=en-US)

---
### Verify Access To SAP Content Agent Service

You should now be able to access the user interface of SAP Content Agent service. 

1. To check this, in your subaccount, choose **Services > Instances and Subscriptions**. In the **Subscriptions** section, choose the **Content Agent** link or the *Go to Application* icon to the right of it.

    ![openUI](screenshots/go-to-subscription.png)

2. In a new tab, you should now see the **Overview** page of your SAP Content Agent service. Currently, the *Activities Performed* tile is empty as expected from a new instance. 	

    ![overview](screenshots/cas-overview.png) 
    
---
### Get Details Of SAP Cloud Transport Management Service Instance 

Get the service key details of the SAP Cloud Transport Management instance that you created as part of tutorial [Step 5 in Get Started with SAP Cloud Transport Management](https://developers-qa-blue.wcms-nonprod.c.eu-de-2.cloud.sap/tutorials/btp-transport-management-getting-started.html).

1. Go to the central administrative SAP BTP subaccount. To do this, click on the **Central Services** tile.

    ![Find cTMS service key 1](screenshots/central-services.png)
        
2. Choose **Instances and Subscriptions** and filter for **Cloud Transport Management** to get the service instance. Click on the key link to view credentials.

    ![Find cTMS service key 2](screenshots/tms-instance.png)

3. The key looks as follows. Leave the service key open so that you copy the required values in the next step, or note down the values of `uri` (you have to scroll down), as well as `clientid`, `clientsecret`, and `url` from the `uaa` section.   
   
    ![Find cTMS service key 3](screenshots/tms-service-key.png)
    
---
### Create Destination To SAP Cloud Transport Management In Source Account 

The destination to SAP Cloud Transport Management service defines the endpoint of SAP Cloud Transport Management service that is used by SAP Content Agent for pushing the content to desired source node of transport route. The destination has the fixed name TransportManagementService.

1. Open the **source** subaccount in a new tab.

    ![Create cTMS Destination 1](screenshots/dev-source.png)
    
2. Under **Connectivity > Destinations** click on **Create Destination**.

    ![Create cTMS Destination 2](screenshots/create-dest.png)

3. In the **Destination Configuration** window, enter details for the following fields. 
    >Keep the values of fields not mentioned in the table unchanged.
    >For destination name, the value must have `TransportManagementService` as prefix. 

    | Field | Value |
    | ---------- | ------------- |
    | **Name** | `TransportManagementService` as default |
    | **Description** | For example: `Destination to SAP Cloud Transport Management` |
    | **URL** | Enter the value of the `uri` from the service key details (scroll down to the bottom of the service key). For example: `https://transport-service-app-backend.ts.cfapps.us10.hana.ondemand.com` |
    | **Authentication** | Select **OAuth2ClientCredentials**. |
    | **Client ID** | Enter the value of `clientid` from the `uaa` section of the service key details. |
    | **Client Secret** | Enter the value of the `clientsecret` from the `uaa` section of the service key details. |
    | **Token Service URL** | Enter the value of `url` from the `uaa` section of the service key details and append `/oauth/token` to the URL. For example: `https://cpi-dev-12345678.authentication.us10.hana.ondemand.com/oauth/token` |


    ![Create cTMS Destination 3](screenshots/create-tms-destination.png)

See also on SAP Help Portal: [Create TransportManagementService Destination](https://help.sap.com/docs/CONTENT_AGENT_SERVICE/ae1a4f2d150d468d9ff56e13f9898e07/eed66f35f9d148c8ae5b2d46ff097d8c.html)


---
### Create Target Node Destination In Central Services Account

You need to configure a transport destination so that SAP Cloud Transport Management knows about the target endpoint of the deployment process. This is usually the endpoint of the deploy service on Cloud Foundry.

1. You will use the Central Services subaccount that's subscribed to SAP Cloud Transport Management service.

	![Create Destination to Target Subaccount 1](screenshots/central-services.png)

2. In the Central Services subaccount, under Connectivity > Destinations click on *Create Destination*. 
	
	![Create Destination to Target Subaccount 2](screenshots/CreateTargDest-01.png)

3. In the **Destination Configuration** window, enter details for the following fields and save the entries:

    | Field | Value |
    | ---------- | ------------- |
    | **Name** | Enter a name, for example: `Quality_Target_Node` |
    | **Description** | For example: `Destination for Deploy Service targeted on target BTP account for Mobile Service` |
    | **URL** | Specify the URL to the SAP Cloud Deployment service as the deploy end point of the destination `https://deploy-service.cf.<domain>/slprot/<myorg>/<myspace>/slp`  `<domain>`: Domain of your target subaccount derived from the Cloud Foundry API endpoint that you can find in the SAP BTP Cockpit in the Overview of your subaccount. `<myorg>/<myspace>`: Names of your org and space. For example `https://deploy-service.cf.eu10-004.hana.ondemand.com/slprot/Example%20Company%20Test%20Org/Example%20Company%20Test%20Space/slp` |
    | **Authentication** | Select **BasicAuthentication**. |
    | *User** | Specify the user name (usually, an email address) of the user that is used for the deployment. User must be a valid platform user on Cloud Foundry environment and it must have the role SpaceDeveloper in the target space. |
    | **Password** | Specify the password of the user. |

    ![Create Destination to Target Subaccount 3](screenshots/CreateTargDest-02.png)
The destination was created.

> This destination name will be used later in target node configuration of the SAP Cloud Transport Management application.

---
### Set Up Landscape In SAP Cloud Transport Management

Configure the landscape in SAP Cloud Transport Management service using the transport destination.

1. Go to the central administrative SAP BTP subaccount. To do this, click on the **Central Services** tile.

	![Go to cTMS subaccount](screenshots/central-services.png)
        
2. Choose **Instances and Subscriptions** and filter for **Cloud Transport Management** and launch the application. The UI opens in a new browser tab or window.

	![Launch cTMS app](screenshots/go-to-tms.png)

3. To configure the landscape, select Landscape Visualization from the navigation on the left.

	![Set up the transport landscape 1](screenshots/CreateTranspLandsc-01.png)
4. Start by creating the transport node that acts as the logical representation of your SAP Integration Suite source subaccount, here **DEV_NODE**. To do this, choose the **+** icon (1). On the **Create Node** dialog, enter details for the following fields (2-3), and click **OK** (4):
    >You can keep the values of fields not mentioned in the table unchanged.  

    | Field | Value |
    | ---------- | ------------- |
    | **Name** (2)| Enter a name, here: `DEV_NODE` |
    | **Description**| This field is optional. For example: `source (development) node for Mobile Services` |
    | **Allow Upload to Node** (3)| Select the checkbox. |

    ![Set up the transport landscape 2](screenshots/CreateTranspLandsc-02.png)

5. To create the transport node that acts as the logical representation of your SAP Mobile services target subaccount, choose the **+** icon. On the **Create Node** dialog, enter details for the following fields. Afterwards, choose **OK** (4). 
    >You can keep the values of fields not mentioned in the table unchanged.

    | Field | Value |
    | ---------- | ------------- |
    | **Name** (1)| here `TARGET_NODE`|
    | **Description** | This field is optional. For example, `SAP Mobile services Quality target node`. |
    | **Content Type** (2) | Select the type of content as **Multi-Target Application**. |
    | **Destination** (3) | Select the destination created in the previous step. Here, this is **Quality_Target_Node**. |
    

   ![Create cTMS node 3](screenshots/CreateTranspLandsc-03.png)

6.  The target node is created and is also visible on the canvas. To create a transport route to connect the two nodes, choose the connector icon. sOn the **Create Route** dialog, enter a name for the transport route, here `Route_DEV_TARGET` (1), select the source node `DEV_NODE` (2) and the target node `TARGET_NODE` (3), and click **OK** (4).
    
    ![Set up the transport landscape 4](screenshots/CreateTranspLandsc-04.png)

You've configured the transport landscape. 

See also on SAP Help Portal: [Configuring the Landscape](https://help.sap.com/docs/TRANSPORT_MANAGEMENT_SERVICE/7f7160ec0d8546c6b3eab72fb5ad6fd8/3e7b04236d804a4eb80e42c6360209f1.html)


---
### Check App In Dev Account

Confirm if the app is ready to be exported.

1. Launch SAP Mobile Services Cockpit by clicking on the app from instances in DEV account.

	![Dev app](screenshots/dev-app.png)
	
2. Confirm the app in dev account

	![Dev app](screenshots/confirm-dev-app.png)

---
### Use SAP Content Agent UI To Export Mobile App To SAP Cloud Transport Management

1. Login to SAP Content Agent UI from the DEV account.

	![Export 1](screenshots/cas-overview.png)

2. Navigate to the **Content Types** page to check status of *SAP Mobile service*. You should see a warning message *SAP Mobile service is active and ready for export.* Ignore the *Action Recommended* as in this scenario we do not perform direct import from Content Agent UI.  

    ![Export 2](screenshots/content-type.png)
    
3. Check the status of SAP Cloud Transport Management is showing as "Connected" to verify the destination is configured correctly in the DEV account. Select the destination name. By default `TransportManagementService` is pre-selected. Select the source node as the entry node of the transport route you want to choose. 

    ![Export 3](screenshots/tms-check.png)

4. Start the export process by clicking on **Export** option listed in the *Side Navigation*. Use the options to filter type as *Mobile Application* and search text using name of your app, for example *test*. 

	![Export 4](screenshots/filter.png)

5. Select the mobile app from the list you want to export. Upon clicking the checkbox you can view the subcomponents of selected app where some mandatory components are pre-checked for you. Go to Step 2.

	![Export 5](screenshots/mobile-app-select.png)

6. Select the transport mode option as "SAP Cloud Transport Management" service. Choose the source node based on the entry node of the desired transport route. Enter a description for creating the transport request. This would be visible in the SAP Cloud Transport Management transport requests view.

	![Export 6](screenshots/mode.png)
		
7. Review your selection and proceed to the next step. 

	![Export 7](screenshots/review.png)

8. An asynchronous process is started and you can view the progress and messages. 

	![Export 8](screenshots/transport-success.png)
	
---
### View The Status Of Transport Request

You can also use the "Activities Performed" page to see the past activities - export/ import done along with logs, content selected and transport information to track if the changes are deployed in the target account. 

1. Navigate to *Activities Performed* and select the activity for which you want to view further details.

	![Activity 1](screenshots/activity1.png)
	
2. Select the tab *Activity Logs* to view the logs. You can also download them. 

	![Activity 2](screenshots/activity2.png)
	
3. Select the *Content Information* tab to view the details (subcomponent information) of the mobile app you have exported from the source account.

	![Activity 3](screenshots/activity3.png)
	
4. Select the *Transport Details* tab to see the transport request status. You can also use the *View Transport Request ##* button to go to SAP Cloud Transport Management UI to see further details.

	![Activity 4](screenshots/activity4.png)

### Verify The App Is Not Available In Target

1. Navigate to the target subaccount where you want the app to be imported.

	![Verify app 1](screenshots/verify-app-1.png)
2. Launch SAP Mobile Services Cockpit by clicking on the support link of SAP Mobile Services. 

	![Verify app 1](screenshots/verify-app-2.png)

3. To login, follow the steps of [tutorial.](https://developers.sap.com/tutorials/fiori-ios-hcpms-setup.html)

4. From the home screen, navigate to **Mobile Applications > Native/MDK**. Confirm there are no apps listed in the page.

	![Verify app 1](screenshots/verify-app-3.png)

5. Bookmark the Mobile Services admin UI URL for quick access.

### Import The Transport Request

In Cloud Transport Management, the content is automatically forwarded from the DEV_NODE to the TARGET_NODE. You can now import the content into your target subaccount.

1. Navigate to the transport request created in SAP Cloud Transport Management. You can do so by clicking on the *Transport Request* link from the *activities performed* page in Content Agent UI (as shown in previous step). To go to the import queue, click on the TARGET_NODE.
	
	![Import TR 1](screenshots/tr-in-tms-target-node.png)
2. To start the import select the checkbox in the table for the transport request and click on *Import Selected*.

	![TR Import 2](screenshots/select-TR-import.png)
2. Approve the import.

	![Approve](screenshots/approve-import.png)
3. When the import is successful, the status of the transport request changes to Succeeded. You can display more details about the import in the log of the transport request. To display the log, click on the log icon.
![Import Logs](screenshots/import-logs.png)

### Verify The Import In Target Account

1. Launch the Mobile Services admin UI by opening the URL bookmarked earlier.

2. From the home screen navigate to **Mobile Applications > Native/MDK**.

	![Verify After Import](screenshots/verify-after-import.png)
