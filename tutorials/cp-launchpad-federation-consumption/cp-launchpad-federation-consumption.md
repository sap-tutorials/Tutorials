---
title: Add Federated SAP S/4HANA Roles to Your Launchpad Site
description: Create a content provider for your SAP S/4HANA system in the SAP Launchpad service provider manager and add the exposed roles to the My Content area and to the SAP Launchpad site, so that end users can access the federated apps and groups.
auto_validation: true
time: 20
tags: [ tutorial>intermediate, products>sap-business-technology-platform, products>sap-s-4hana, products>sap-fiori, topic>abap-connectivity]
primary_tag: products>sap-launchpad-service
---

## Prerequisites
 - You have set up SAP Cloud Connector and runtime and design time destinations for your SAP S/4HANA system.
 - You exposed roles in your SAP S/4HANA system.

## Details
### You will learn
  - How to create a new content provider
  - How to add federated content to the My Content area
  - How to make federated roles available in your SAP Launchpad site


---

[ACCORDION-BEGIN [Step 1: ](Create a new Content Provider)]

In a first step, you will add the SAP S/4HANA system as a content provider.

1. Open the SAP Launchpad Site Manager.

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

   5. Click **Save**.

       ![Provider creation form](3a-new-content-provider-form.png)

    > If you select *Automatic addition of all content items*, all exposed content items will be automatically selected in the Content Explorer and added to the My Content tab, as soon as you create the content provider. In this case, when updating a provider, all the new content items are automatically added. In this exercise, you will add content items manually.

    The content provider is added to the list and the content is loaded. This might take some seconds.

  6. Wait till the status says **Created** and **Partial content was created**. If this takes long, try refreshing the browser page.

    ![Provider created](4a-provider-created.png)

    > The message *Partial content was created* refers to the fact that currently Smart Business apps are not part of the exposed content. As soon as they are supported for federation, you will be able to update your content provider and this message will disappear.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add roles to My Content)]

1. Click the **Content Manager** icon to manually assign content from your SAP S/4HANA system to ``My Content``, so you can add it to a site.

    !![Content Manager Icon](5-go-to-content-manager.png)

2. On top of the page, click **Content Explorer** to access content coming from content providers.

    ![Go to Content Explorer](6-open-content-explorer.png)

3. Click the **SAP S/4HANA** tile to access the content provider that you just created.

    ![SAP S/4HANA tile](7-open-s4hana.png)

4. Click the checkboxes to select both roles.

5. Then click **Add to My Content**.

    ![Add to My Content](8-add-roles-to-my-content.png)

You see that both roles have been added.

>You can remove roles from My Content by clicking the *Remove* link.

![Roles Added](9-roles-added.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Check roles in My Content)]

1. On top of the page, click **My Content**.

    ![Go to My Content](10-open-my-content.png)

2. Click the **Accounts Payable Accountant** role to open it and view the apps that are part of this role.

    ![Open Accounts Payable Accountant](11-open-accounts-payable-role.png)

You can see that there are 84 apps available in this role. In the next step, you will assign the role to your site.

![Role Accounts Payable Accountant](12-ap-role-content.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Assign roles to site)]

To make the apps that come with the two federated roles available in your site, you need to assign the roles to the site.

1. Click the **Site Directory** icon to access your site.

    ![Go to Site Directory](13-open-site-directory.png)

2. Click the **Site Settings** icon to open the ``JobCore`` site that you created in previous tutorials.

    ![Site Settings](14-open-site.png)

3. Click **Edit** to switch to Editing mode.

    ![Edit button](15-edit-site.png)

4. Click into the **+ Search for items by their title** field to launch a search for all assignable items.

    You get a list with the two roles that you just added.

5. Click the **Assign** icons next to both roles.

    ![Assign roles](16-add-roles-to-site.png)

    The **Assign** icon switches to a red **Unassign** icon.

6. Click **Save**.

    ![Save](17-save-assigned-roles.png)

7. Click the **Back to Site Directory** icon.

    ![Back to Site Directory](18-back-to-site-directory.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Assign roles to your user)]

Apps are only displayed to users with the corresponding roles assigned. You assign those roles to users in the SAP BTP cockpit.

1. Open the SAP BTP cockpit of your trial account.

2. Navigate to **Security > Trust Configuration**.

    ![Trust Configuration](19-trust-configuration.png)

3. Click **Default Identity Provider** to access the role collection assignment for users coming from this identity provider.

    ![Default idp](20-default-idp.png)

4. Enter the ``E-Mail Address`` with which you login to the SAP Launchpad.

5. Then click **Show Assignments** to view already assigned role collections for your user.

    ![Show role assignments](21-show-assigned-roles.png)

6. To assign the new roles, click **Assign Role Collection**.

    ![Assign Role Collection](22-assign-role-collections.png)

    When adding the federated content to your account, one role collection for each role was automatically created.

    Now add the two role collections one by one,  ``~sap_s4hana_SAP_BR_AP_ACCOUNTANT`` and ``~sap_s4hana_SAP_BR_MASTER_SPECIALIST_FIN``.

7. Select the first role from the drop-down list.

8. Click **Assign Role Collection**.

    ![Assign first role collection](23-assign-ap-accountant.png)

9. Add the second role in the same way.

    ![Assign master data specialist](24-assign-master-data.png)

Now you are done in SAP BTP cockpit.

  ![Link text e.g., Destination screen](25-assigned-role-collections.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Access the federated content)]

1. Go back to the Site Directory to launch your ``JobCore`` site.

2. Click the **Go to Site** icon.

    ![Open Site](26-open-site.png)

    The site opens in a new browser tab.

3. Check the new SAP S/4HANA apps and groups that have become available in the site.

    ![Site with SAP S/4HANA content](27-site-with-s4-content.png)

4. Launch the SAPUI5 app **Manage Outgoing Checks** in the **Checks** group. Click **Go** to see some data.

    ![Click Go](28-go.png)

    ![App Manage outgoing checks](29-manage-checks.png)

5. Launch the SAP GUI app **Maintain Business Partner** in the **Business Data Master** group.

    ![Maintain business partner](30-maintain-business-partner.png)


[VALIDATE_7]
[ACCORDION-END]


---
