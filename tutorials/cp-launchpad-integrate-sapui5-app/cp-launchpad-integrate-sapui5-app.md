---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, products>sap-business-technology-platform, products>sap-build-work-zone--standard-edition, products>sap-fiori, programming-tool>html5, programming-tool>sapui5]
primary_tag: software-product>sap-build-work-zone--standard-edition
author_name: Lindsay Bert
author_profile: https://github.com/LindsayBert
---

# Integrate Your SAPUI5 App into Your Site
<!-- description --> Add an SAPUI5 app to a site in SAP Build Work Zone, standard edition.


## Prerequisites
 - You've already created the `JobCore` site
 - You've deployed your SAPUI5 app (including the navigation properties), to SAP BTP, Cloud Foundry environment


## You will learn
  - How to add a deployed, custom-developed, SAPUI5 app to your site

---
Once you've deployed your SAPUI5 app to SAP BTP, it becomes available to add to your site.


### Fetch updated content using the Channel Manager

In this step, you will find your custom developed app that you deployed to your subaccount in SAP BTP.


1. In the side navigation panel of your subaccount, click **Instances and Subscriptions** and then next to **SAP Build Work Zone, standard edition**, click the **Go to Application** icon.

    ![Open launchpad service](000-open-launchpad-service.png)

2. Click the **Channel Manager** icon to view any available content providers.

    ![Open Provider Manager](0-open-provider-manager.png)

3. Select the **HTML5 Apps** content provider.

    >The **HTML5 Apps** content provider is created automatically. Any app that you deploy to SAP BTP is automatically added as content to this provider.

    <!-- border -->![Select the HTML5 Provider](3-HTML5-provider.png)

4. Click the **Fetch updated content** icon.

    ![Fetch updated content](00-fetch-updated-content.png)

The **HTML5 Apps** content provider should now expose any newly deployed app for integration.


### Add your deployed SAPUI5 app to your content


1. Click the icon in the side panel to open the **Content Manager**.

    <!-- border -->![Open Content Editor](1-open-content-editor.png)

2. Click the **Content Explorer** button to explore content from the available content providers.

    ![Open Content Explorer](2-content-explorer.png)

3. Select the **HTML5 Apps** provider.

    ![Select the HTML5 tile](3a-select-HTML-tile.png)

4. You'll see that your `Suppliers` app that you've just created in SAP Business Application Studio, already exists in this provider. Select it and click **+ Add to My Content**.

    <!-- border -->![Add app to My Content](4-add-app-my-content.png)

5. Select the `Suppliers` app and click **Add**.

    ![Select app](5-select-app.png)

6. Using the breadcrumbs, go back to the Content Manager.

    ![Select app](6-go-to-content-manager.png)

    Note that your `Suppliers` app is in the list of content items.

    ![View app](7-view-app.png)


### Assign app to Everyone role


In this step, you'll assign the `Suppliers` app to the `Everyone` role. This is a default role - content assigned to the `Everyone` role is visible to all users.

1. In the **Content Manager**, click the `Everyone` role.

    ![Everyone role](9a-everyone-role.png)

2. Click **Edit**.

    ![Edit role](10-edit-role.png)

3. In the **Assignment Status** column, toggle the switch to assign the app to the `Everyone` role. 

    ![Assign role](11-assign-everyone-role.png)

4. Click **Save**.

### Add the app to your page

1. Using the breadcrumbs, go back to the Content Manager.

2. Select the `Overview` page. 

    ![Select page](12-select-page.png)

3. When the page opens you'll see other apps and a card that you added in the previous tutorials. Click **Edit**.

    ![Edit page](13-edit-page.png)

4. Hover in the section where your other apps are displayed, click the **+** icon.

    ![Add app](14-add-app.png)

5. Select the `Suppliers` app and click **Add**.

    ![Add Suppliers app](15-add-suppliers-app.png)

6. Click **Save**.

### Review your site


1. Click the **Site Directory** icon to open the Site Directory.

    ![Open site directory](16-open-site-directory.png)

2. Click **Go to site** on the site tile.

    ![Open site](17-go-to-site.png)

    You'll see all the apps that you have added to your site as well as the UI integration card.

    ![See all apps](18-final-view.png)


3. Go ahead and click the apps to launch them.

   
### Review your site


1. Click the **Site Directory** icon to open the Site Directory.

    ![Open site directory](11-open-site-directory.png)

2. Click **Go to site** on the site tile.

    ![Open site](12-go-to-site.png)

    You'll see all the apps that you have created in your site. In the `Our Suppliers` group, you'll see the `Suppliers` app that we've just created.

    ![See all apps](12a-view-launchpad.png)


3. Click the app to launch it.

    ![View app](13-suppliers-app.png)
