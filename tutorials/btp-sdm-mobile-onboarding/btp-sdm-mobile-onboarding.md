---
auto_validation: true
time: 30
tags: [ tutorial>beginner, topic>mobile, topic>Document Management Service, software-product>sap-business-technology-platform, software-product>sap-mobile-services,  software-product>sap-business-technology-platform]
primary_tag: software-product>sap-document-management-service
author_name: Vikram Kulkarni
author_profile: https://github.com/Vikramkulkarni01
parser: v2
---
# Getting Started with SAP Document Management Mobile Application
<!-- description --> The SAP Document Management Service mobile app is available for both Android and iOS platforms. This robust application brings the functionality of the SAP Document Management Service to your fingertips, wherever you may be. This tutorial will guide you through the onboarding steps, enabling you to integrate the SAP Document Management Service mobile app within your own Document Management Service, Application Option user interface. Furthermore, you will learn how to effectively manage and control the application features using the SAP Mobile Services cockpit, enhancing overall efficiency and productivity.

## Prerequisites
 - You've access to the SAP BTP subaccount.
 - You've subscribed to the service **Document Management Service, Application Option** in the SAP BTP cockpit. For more information about the initial setup, see [Subscribing to Document Management Service, Application Option](https://help.sap.com/docs/document-management-service/sap-document-management-service/subscribing-to-document-management-service-application-option?locale=en-US&version=Cloud).
 - You've necessary administrator privileges and included the role **`SDM_MobileAdmin`** into your account.
 - You've assigned **`SDM_User`** role to the users who are required to use the mobile application. 
 
## You will learn
  - How to subscribe to SAP Document Management Service, Application Option
  - How to access the QR code from the SAP Mobile Services cockpit.
  - How to onboard mobile applications.
  - How to sync your repositories.

---
### Check entitlements
>Note: If you have already added the entitlements, you can skip this step. 

1. Enter your **Global Account** &rarr; **Account Explorer**.
   
2. In the **Subaccounts** tab, choose your subaccount. 
   
    <!-- border -->![Choose Subaccount](BTP_Global_Account_Explorer.png)

3. Click on **Entitlements** and choose **Configure Entitlements**(For new account) or **Edit** (For existing account). 

    <!-- border -->![Entitlements](BTP_Entitlements.png)

4. Click on **Add Service Plans** to see all available entitlements.

    <!-- border -->![Add Service Plan](BTP_Add_Service_Plan.png)

5. Use the filter bar to search for **`Document Management`** to reduce the number of available entitlements. Click the checkbox to select the service plan **'Standard'**. Finally, confirm your selection by clicking on **`Add 1 Service Plan`**.

    <!-- border -->![Add 1 Service Plan](Add1ServicePlan.png)

6. Choose **Save** to confirm your selection. 
    
    <!-- border -->![Save Entitlements](Save_Entitlements.png)


### Subscribe to SAP Document Management Service, Application Option
>Note: If you have already subscribed to the service, you can skip this step. 

1. Click **Service Marketplace** on the side navigation pane, search for **Document Management Service, Application Option**, and click tile.

    <!-- border -->![ServiceMarketPlace](Service Market Place.png)

2. Click **Create**.

    <!-- border -->![Creating App](CreateApplication.png)
    
3. In the dialog, choose the **`Standard`** subscription plan and click **Create**.

    <!-- border -->![CreateFinal Step](Create_Final.png)

4. Your subscription is being set up. To review your existing subscriptions, choose **View Subscription**.

    <!-- border -->![CreationProgress](CreationInProgress.png)


### Assign roles
To use the application, ensure that your user account is associated with a role collection that provides the necessary permissions.

1. Open the SAP BTP cockpit and navigate to your subaccount.

2. Choose **Security** &rarr; **Role Collections**, and then choose **Create**.
    
    <!-- border -->![Create RoleCollections](Create_RoleCollections.png)

3. In the **Create Role Collection** dialog, enter **`Mobile Admin`** in the **Name** field and choose **Create**.

    <!-- border -->![Creation popup](Creation_Dialog.png)

4. Choose the role collection **`Mobile Admin`** from the list of role collections and choose **Edit** on the right.

    <!-- border -->![Edit Role Collections](Edit_RoleCollections.png)

5. Open the value help in the **Role Name** field. 

    <!-- border -->![Role Name](Editing_Role_Name.png)

6. Search for the role **`SDM_MobileAdmin`** and **`SDM_User`**, select it, and choose **Add**.
   
    <!-- border -->![Role Name](Adding_Role_2.png)

7. Choose **Save**. 
   

### Assign role collection to a user

1. Choose **Security** &rarr; **Users**, and then choose a user from the available list. 

2. Under **Role Collections** on the right, choose **Assign Role Collection**.

    <!-- border -->![Role Collection to a User](Rolecollection_User.png)

3. In the **Assign Role Collection** dialog, select the **Mobile Admin** role collection and choose **Assign Role Collection**.

    You have now assigned the **Mobile Admin** role collection to your user.

    >Note: You might need to log out and log back in to make sure your new role collection is taken into account.

### Creating a repository

1. In your subaccount, navigate to **Services** &rarr; **Instances and Subscriptions**, click the button next to **Document Management Service, Application Option**.

    <!-- border -->![Go To Application in your subbacount](GoTo_Application.png)

2. Log on and click the **Document Management Service Admin**.

    <!-- border -->![Accessing Repo Creation Tile](Selecting_Repo_Tile.png)

3. Click **Add Repository**.

    <!-- border -->![Add Repository](Add_Repo.png)

4. Choose **Internal** as a Repository Type, enter the **Display Name** of your choice, and click **Add**. 

<!-- border -->![Adding Repository](Add_Repo_Details_Page.png)

>The repository you've set up will be available for access via the mobile application in the subsequent steps.


### Go to the application and access the QR code

1. In your subaccount, navigate to **Services** &rarr; **Instances and Subscriptions**, click the button next to **Document Management Service, Application Option**.

    <!-- border -->![Go To Application in your subbacount](GoTo_Application.png)

2. Log on to the application.

3. Click the **Document Management Service Mobile Admin** tile.
    
    <!-- border -->![Admin View](MobileAdminView_gettingStarted_Horizon.png)

4. On the Getting Started page, click **Start Onboarding**.

    <!-- border -->![Accessing Onboarding tile](Start_Onboarding.png)

5. Click **Mobile Services Cockpit**.

    <!-- border -->![Acccessing Mobile Service Cockpit](Access_Mobile_Services_Cockpit.png)

6. Navigate to the **APIs** tab.

    <!-- border -->![Accessing API tab](QR_Code_Scan_API_tab.png)

>**Results:** You've accessed the QR code. Keep this tab open and proceed with the next step.

### Installing mobile application
<!-- description --> In this step, you are installing the mobile application on your device.
>>**Note:** If you are planning to test the mobile app with the different users, it's required to assign the role of **`SDM_User`** to the respective users within the SAP BTP subaccount.

1.  Download the app. For iOS devices, go to the [App Store](https://apps.apple.com/in/app/sap-document-management/id6504613204). For Android devices, visit the [Google Play Store](https://play.google.com/store/apps/details?id=com.sap.sdm.mobileapp&hl=en_IN&gl=US).

2.  Start the app. 

3.  Click **Agree**. 

    <!-- border -->![End User License Agreement](Mobile_EULA_Agree.png)

4.  On your device, choose **Scan**. 

    <!-- border -->![Scan QR Code](Mobile_Scan_Screen.png)

5.  Scan the QR code from the previous step to proceed.
   
    <!-- border -->![Scanning QR Code](Mobile_Scan_Continue.png)

6. Choose **Continue**. 
   
    <!-- border -->![Continue Screen](Mobile_Scan_Continue.png)

    >**Results**: A repository that you created in the earlier steps is displayed.

7. To refresh the list manually, pull down on the list and release it.
    
    <!-- border -->![Repository Lists](List_of_Repostitories.png)

   
