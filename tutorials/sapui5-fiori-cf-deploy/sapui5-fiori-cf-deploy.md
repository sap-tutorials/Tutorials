---
parser: v2
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>sap-fiori, programming-tool>odata, programming-tool>sapui5,   software-product>sap-build-work-zone--standard-edition, topic>user-interface, programming-tool>html5, topic>cloud, tutorial>free-tier]
primary_tag: software-product>sap-business-technology-platform
author_name: Nico Schoenteich
author_profile: https://github.com/nicoschoenteich
---

# Deploy the Fiori App to Cloud Foundry
<!-- description --> Deploy the application to the SAP BTP, Cloud Foundry environment to be able to share it with your coworkers.

## Prerequisites
- You have [Set Up a BTP Account for Tutorials](group.btp-setup). Follow the instructions to get an account, and then to set up entitlements and service instances for the following BTP services:
    - **SAP Build Work Zone, standard edition**
    - **Destination Service**
    - **Authorization and Trust Management Service (xsuaa)**
    - **HTML5 Application Repository Service**

## You will learn
  - How to build and deploy the application to the cloud.
  - How to interact with the SAPUI5 flex services to adapt the application to end-users.

---

### Build the project

Run the following command to package the project into one single archive. This archive contains the full source files (for debugging), as well as the bundled resources (for faster loading times):

```Terminal
npm run build:mta
```

> You can find the definition of this script in the `scrips` section of the `package.json` file.

### Deploy the built archive

Next, deploy the generated archive and track the deployment progress in the terminal with the following command:

```Terminal
cf deploy mta_archives/products_0.0.1.mtar
```

> Use `cf login` to establish a connection to the Cloud Foundry endpoint if you haven't done so yet. [This tutorial](cp-cf-download-cli) might help you if you are not sure how to do so.

The great thing about deploying a single `.mtar` file is that the Cloud Foundry environment will provision all required services for you. Here is a list of all services that are required for this project (you can see them enumerated in the `mta.yaml` file).


|  Service instance name     | service | service plan
|  :------------- | :-------------| :-------------
|  `products_destination` |  `destination` |  `lite`
|  `products_uaa` |  `xsuaa` |  `application`
|  `products_html5_repo_host` | `html5-apps-repo`  |  `app-host`


### Access the running web app

At the end of the deployment process log, you should see a line that looks similar to this one:
```[4]
Deploying content module "productsdestination-content" in target service "products_destination"...
Deploying content module "webapp_deployer" in target service "products_html5_repo_host"...
Skipping deletion of services, because the command line option "--delete-services" is not specified.
Process finished.
Use "cf dmol -i 0915a099-4130-11eb-b43e-eeee0a8f7188" to download the logs of the process.
```

This means you uploaded the app successfully. The URL of your web app will now follow this pattern: `https://<yourID>.launchpad.cfapps.<region>.hana.ondemand.com/productsservice.tutorialproducts`
With this URL, you are able to access the `flpSandbox.html`.

> You can also install [this plugin](cp-cf-install-cliplugin-html5) and run `cf html5-list -di products_destination -u --runtime launchpad` to print the full URL of your web app.


### Embed the application in SAP Build Work Zone, standard edition


Now it's time to embed the app in SAP Build Work Zone, standard edition:

1. Open the admin UI of the [SAP Build Work Zone, standard edition](cp-portal-cloud-foundry-getting-started). You can find the link to the UI under "Instances and Subscriptions" in the SAP BTP Cockpit.
2. Access the **Channel Manager** menu. You should now see the following screen and **hit the refresh button** to synchronize with the HTML5 application repository.

    <!-- border -->![channelManager](channelManager.png)

3. Open the **Content Manager** menu and click on the button **Content Explorer**.

    <!-- border -->![contentManager1](contentManager1.png)

4. Click on the tile that says **HTML5 Apps**.

    <!-- border -->![contentManager2](contentManager2.png)

5. You should now see the following screen from where you can select the web app you just deployed. **Select the checkbox** next to `Product List` and hit the **Add** button.

    <!-- border -->![contentExplorer](contentExplorer.png)

6. Go back to the **Content Manager** and click on the group **default**. In case you don't see this group, use the **+ New** button to create a new one.

    <!-- border -->![defaultGroup](defaultGroup.png)

7. Select the **Edit** button. You should be able to assign the app to the group via the **Assignment Status** switch. Don't forget to **Save** the group before navigating back.

    <!-- border -->![addToGroup](addToGroup.png)

8. Now you need to make this application visible to your users. To keep it simple, make the app visible to everyone. For this, select the **Everyone** role.

    <!-- border -->![everyoneRole](everyoneRole.png)

9. You are already familiar with the steps needed here. Select the **Edit** button to edit the role. Assign the app to the role via the **Assignment Status** switch and click **Save** before navigating back.

    <!-- border -->![addToRole](addToRole.png)

10. Go to the menu **Site Directory** and use the **Create Site** button to create a new site if you don't have an existing one.

    <!-- border -->![siteDirectory](siteDirectory.png)

11. You can choose any name for the new site, e.g. **`ProductsTutorial`**.

    <!-- border -->![createFLP](createFLP.png)

12. There's no need to edit this site, all you need comes with the default configuration. Click the button on the top right corner to open your new site.

    <!-- border -->![openFLP](openFLP.png)

### Save a custom filter variant

1. Click on the **Products** to open your web application. Also note that you see your initials in the top-right user menu. This shows that you are signed in (perhaps automatically via Single-Sign-On).

      <!-- border -->![FLP](./FLP.png)

3. Apply another filter with the following criteria:
    * The `ProductID` shall be less than 18.
    * The `CategoryID` shall be less than 60.

    <!-- border -->![New filter](./newFilter.png)

4. **Open** the views dialog and save the current filter variant by clicking **Save as**.

      <!-- border -->![View Popup](./myViews.png)

5. Name the view **`TutorialFilter`**, **check both checkboxes**, and hit **Save**. This user-specific variant is now stored in the backend and therefore persistent.

      <!-- border -->![Define name](./setName.png)

---
