---
parser: v2
auto_validation: true
time: 25
tags: [ tutorial>beginner, programming-tool>sapui5, software-product>sap-business-technology-platform, topic>cloud, software-product>sap-build-work-zone--standard-edition]
primary_tag: software-product>sap-build-work-zone--standard-edition
author_name: Lindsay Bert
author_profile: https://github.com/LindsayBert
---

# Integrate Your UI Integration Card from the Content Package Into Your Site
<!-- description --> Integrate a card from your content package into your SAP Build Work Zone, standard edition site.  

## Prerequisites
 - You've created a content pakage that includes a UI integration card with its associated role
 - You've successfully deployed the card into a ZIP file and downloaded it on to your computer
 - You have a subscription to SAP Build Work Zone, standard edition
 
## You will learn
- How to integrate the UI integration card that you created

## Intro 
In this tutorial, you'll learn how to integrate the card and role from the content package that you created in SAP Business Application Studio.  

### Open your SAP Build Work Zone, standard edition site

From the SAP BTP cockpit, go to **Instances and Subscriptions** and click `SAP Build Work Zone, standard edition` to open the service.

<!-- border -->![Access Work Zone](1-access-work-zone.png)

The **Site Directory** opens.

<!-- border -->![Open Site Directory](2-open-site-directory.png)

### Upload your content package

1. Click the **Channel Manager** icon in the side navigation panel to open it.

    <!-- border -->![Open Channel Manager](3-open-channel-manager.png)

2. Click **+ New** and select **Content Package**.

    <!-- border -->![New content package](4-new-content-package.png)

3. In the **New Content Package** dialog box, browse for and select the ZIP file that you downloaded in the previous tutorial. You'll find the ZIP file in your **Downloads** folder on your computer. 

    <!-- border -->![Browse for ZIP file](5-browse-zip-file.png)

4. In the **Runtime Destination** field, select `ES5`, and leave the default values in the other fields. Click **Save**.

    <!-- border -->![Save content package](6-save-content-package.png)

    Once you've saved the content package properties, the ZIP file is created in the **Channel Manager**.  
    
5. Click **Report** to check the content of the package. It should look like this showing that your content package includes a role and an app:

    <!-- border -->![Create report](7-create-report.png)

### Assign yourself to the ContentPackageRole

The `ContentPackageRole` is part of the content in the content package ZIP file.

1. Go back to the SAP BTP Cockpit using the **Instances and Subscriptions** tab in your browser.

    <!-- border -->![Go to BTP Cockpit](8-instances-and-subscriptions.png)

2. From the side menu, go to **Security** --> **Role Collections** and search for the `ContentPackageRole`.

    <!-- border -->![ContentPackage role](9-content-package-role.png)

3. Click the arrow at the far right of the row displaying the `ContentPackageRole`.

    <!-- border -->![Open role collection details](9a-role-collection-details.png)

4. In the screen that opens, click **Edit**.
  
    <!-- border -->![Edit role collection](10-edit-role-collection.png)

4. In the **Users** section of the screen, add your ID (your email), and then your email again and add it to the list of users by clicking the **+** button. Save your changes.

    <!-- border -->![Add User](11-add-user.png)


### Ensure that the app is assigned to the ContentPackageRole

To do this, go to the Content Manager in SAP Build Work Zone, standard edition.

1. Click  **Services** --> **Instances and Subscriptions** in the side panel of the SAP BTP cockpit, select the **SAP Build Work Zone, standard edition** service and then **Go to Application**..

    <!-- border -->![Open work zone std](12-open-workzone-std.png)

2. From the side panel, open the **Content Manager**.

    <!-- border -->![Open Content Manager](13-open-content-manager.png) 

3. Click the `ContentPackageRole` to open the role editor.

    <!-- border -->![Select role](14-select-contentpackage-role.png)

4. Under the **Apps** tab, you should see the `Products by Category Card`. This means that the card is assigned to this role. 

    <!-- border -->![View card in the role editor](14a-view-card-in-role.png)


### Assign the ContentPackageRole to the site

1. Open the **Site Directory**.

    <!-- border -->![Open Site Directory](15-open-site-directory.png)

2. Open the **Site Settings** screen.

    <!-- border -->![Open Site Settings](16-open-site-settings.png)

3. In the **Assignments** panel, click inside the search box to open a list of available roles. You'll see the `ContentPackageRole`. Assign it to the site by clicking the **+** next to the role name and then click **Save**.

    <!-- border -->![Assign role to site](17-assign-role-to-site.png)    


### Add a header in your page

1. Go back to the Site Manager using the back arrow.

    <!-- border -->![Back to Site Manager](21-go-back.png)

2. Open the Content Manager.

     <!-- border -->![Open Content Manager](22-open-content-manager.png)

3. Click the `Overview` page to open the page editor.

    <!-- border -->![Open Overview page](23-open-overview-page.png)

4. Click **Edit**.

5. In the section where you've added the card, enter `My Card` as the **Section Title** and click **Save**.

    <!-- border -->![Name section](24-name-section.png)


### View the card in your runtime site

1. Click the **Site Directory** icon.

    <!-- border -->![Go back to Site Directory](18-open-site-directory.png)   

2. Click on the **Go to site** icon.

     <!-- border -->![Go to site](19-go-to-site.png) 

3. This is what you'll see:

    <!-- border -->![Final view](20-final-view.png) 