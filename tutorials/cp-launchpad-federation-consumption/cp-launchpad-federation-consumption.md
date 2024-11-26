---
title: Add Federated SAP S/4HANA Roles to Your SAP Build Work Zone Site
description: Create a content provider for your SAP S/4HANA system in SAP Build Work Zone provider manager and add the exposed roles to the My Content area and to the Work Zone site, so that end users can access the federated apps and groups.
auto_validation: true
time: 20
tags: [ tutorial>intermediate, software-product>sap-business-technology-platform, software-product>sap-s-4hana, software-product>sap-fiori, topic>abap-connectivity, software-product>sap-launchpad-service, software-product>sap-build-work-zone--standard-edition, software-product>sap-build-work-zone--advanced-edition]
primary_tag: software-product>sap-build-work-zone--standard-edition
parser: v2
---

## Prerequisites
 - You have set up SAP Cloud Connector and runtime and design time destinations for your SAP S/4HANA system. 
 - You exposed roles in your SAP S/4HANA system. 
 - You have created an SAP Build Work Zone site (see tutorial group [Create your First Business Site with Apps](group.launchpad-cf-create-site)).


## You will learn
  - How to create a new content provider 
  - How to add federated content to the My Content area
  - How to make federated roles available in your SAP Build Work Zone site

---

### Create a new Content Provider

In a first step, you will add the SAP S/4HANA system as a content provider.

1. Open the Site Manager of SAP Build Work Zone, standard edition.

2. Click the Provider Manager tab.

    ![Go to Provider Manager](1-open-provider-manager.png)

3. Click **+New** to create a new Content Provider.

    ![New Content Provider](2-new-provider-manager.png)

4. Fill in the form using the destinations that you created in the second tutorial.

    |  Field Name     | Value
    |  :------------- | :-------------
    |  Name           | `SAP S/4HANA`
    |  Description    | `SAP S/4HANA demo system`
    |  ID             | Remove /, so the ID does not contain special characters except underscores.
    |  Design-Time Destination  | Select `s4hanadt` from drop-down list
    |  Runtime Destination  | Select `s4hanart` from drop-down list
    |  Runtime Destination for OData  | `Use default runtime destination`
    |  Content Addition Mode  | `Manual addition of selected content items`

5. Click **Save**. The content provider is added to the list and the content is loaded. This might take some seconds.

    ![Provider creation form](3c-new-content-provider.png)

    > If you select *Automatic addition of all content items*, all exposed content items will be automatically selected in the Content Explorer and added to the My Content tab, as soon as you create the content provider. In this exercise, you will add content items manually, so you can learn how this is done. In productive accounts, you would usually use automatic addition to reduce manual effort and allow for automatic content sync.

    > Using Identity Provisioning service is recommended for productive scenarios as it allows to reuse user-role assignments from the content provider and reduces manual effort.

    > If you select *Include group and catalog assignments to roles*, the relationship of groups and catalogs to roles from the SAP S/4HANA system is taken into account for the display of the groups and catalogs on SAP Build Work Zone. For more details see [the documentation](https://help.sap.com/docs/WZ_STD/8c8e1958338140699bd4811b37b82ece/215517e6d4f44c96b790bbcb56d63572.html). 


 6. Wait till the status says **Created**. If this takes long, try refreshing the browser page. If you want, click the **Report** link to see a summary of the created content.

 ![Provider created](4b-provider-created.png)



### Select roles to usage

1. Click the **Content Manager** icon to manually select roles from your SAP S/4HANA federated content, so you can add them to a site. These steps can be omitted when automatically selecting all exposed content.

    ![Content Manager Icon](5a-go-to-content-manager.png)

2. On the right side of the page, click **Content Explorer** to access content coming from content providers.

    ![Go to Content Explorer](6a-open-content-explorer.png)

3. Click the **SAP S/4HANA** tile to access the content provider that you just created.

    ![SAP S/4HANA tile](7a-open-s4hana.png)

4. Click the checkboxes to select both roles.

5. Then click **Add**.

    ![Add to My Content](8a-add-roles-to-my-content.png)

You see that both roles have been added.

>You can remove roles by clicking the *Remove* link.

   ![Roles Added](9a-roles-added.png)



### Check content items in selected roles 

1. In the breadcrumb on top of the page, click **Content Manager** to go back to the main view.

    ![Go to Content Manager](10a-open-my-content.png)

2. Click the **Accounts Payable Accountant** role to open it and view the apps that are part of this role.

    ![Open Accounts Payable Accountant](11a-open-accounts-payable-role.png)

You can see that there are 93 apps available in this role. When you navigate to the **Spaces** tab, you can see that there is also one space assigned to the role. In the next step, you will assign the role to your site.

![Role Accounts Payable Accountant](12a-ap-role-content.png)



### Assign roles to site

To make the space and the apps that come with the two federated roles available in your site, you need to assign the roles to the site.

1. Click the **Site Directory** icon to access your site.

    ![Go to Site Directory](13a-open-site-directory.png)

2. Click the **Site Settings** icon to open the ``JobCore`` site that you created in previous tutorials.

    ![Site Settings](14-open-site.png)

3. Click **Edit** to switch to Editing mode.

    ![Edit button](15a-edit-site.png)

4. Click into the **+ Search for items to assign** field to launch a search for all assignable roles.

    You get a list with the two roles that you just added.

5. Click the **Assign** icons next to both roles.

    ![Assign roles](16a-add-roles-to-site.png)

    The **Assign** icon switches to a red **Unassign** icon.

6. Click **Save**.

    ![Save](17a-save-assigned-roles.png)

7. Click the **Back to Site Directory** icon.

    ![Back to Site Directory](18-back-to-site-directory.png)


### Assign roles to your user

Apps are only displayed to users with the corresponding roles assigned. You assign those roles as role collections to users in the SAP BTP cockpit. In a productive system, you would usually automate this assignment using the Identity Provision service.

1. Open the SAP BTP cockpit of your trial account.

2. Navigate to **Security > Users** to assign the role collections to your user. When you added the federated content, one role collection for each federated role was automatically created.

    ![Users](19b-users.png)


3. In the list of users available in your subaccount, click on the name of your user.

    ![Select your user](20b-select-user.png)

4. In the user details panel on the right, click the three dots in the **Role Collections** section to open the menu. Then select ``Assign Role Collection``.

    ![Open menu and assign role collection](21f-open-and-assign-role-collection.png)


5. In the pop-up window select the checkboxes in front of the two federated roles ``~sap_s4hana_SAP_BR_MASTER_SPECIALIST_FIN`` and ``~sap_s4hana_SAP_BR_AP_ACCOUNTANT``. Then click **Assign Role Collection**.

    ![Select role collections](22c-select-role-collections.png)

Now you are done in SAP BTP cockpit.


### Access the federated content

1. Go back to the Site Directory to launch your ``JobCore`` site.

2. Click the **Go to Site** icon. The site opens in a new browser tab.


    ![Open Site](26-open-site.png)


3. Check the new SAP S/4HANA spaces, pages, and apps that have become available in the site. Depending on how much content you created in the site in previous exercises, you will see spaces created in SAP Build Work Zone first, then you see two spaces, one for each federated role.

    ![Site with SAP S/4HANA content](27a-site-with-s4-content.png)
    

4. Launch the SAPUI5 app **Manage Outgoing Checks** in the **Checks** group. Click **Go** to see some data.

    ![Click Go](28-go.png)

    ![App Manage outgoing checks](29-manage-checks.png)


5. Click the SAP icon to go back to the Work Zone homepage.


6. Launch the SAP GUI app **Maintain Business Partner** in the **Business Data Master** group.

    ![Maintain business partner](30-maintain-business-partner.png)


     > **Note:** If you select *Spaces and Pages - New Experience* as **View Mode** in the Site Settings, you can now also add the federated apps to pages you create on SAP Build Work Zone with the page editor.


---
