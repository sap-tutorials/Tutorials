---
title: Set Up SAP Fiori launchpad in Cloud Foundry to Get Notifications
description: Create and configure an SAP Fiori launchpad site to receive notifications from SAP IoT using SAP Web IDE.
author_name: Jitendra Sharma
author_profile: https://github.com/JitendraSharma01
auto_validation: true
time: 40
primary_tag: topic>internet-of-things
tags: [ tutorial>beginner, tutorial>license, topic>internet-of-things, topic>cloud, products>sap-fiori, products>sap-internet-of-things, products>sap-business-technology-platform, products>sap-web-ide ]
---

## Details
### You will learn
  - How to create a new MTA project
  - How to add a SAP Fiori launchpad site to a MTA project

An SAP Fiori launchpad site provides a user with access to their notifications and applications deployed on the cloud.

---

[ACCORDION-BEGIN [Step 1: ](Create MTA project)]

  1. Launch SAP Web IDE

    - Log on to the SAP BTP cockpit

    - Select the **Neo** environment.

    - In the navigation pane, choose **Services**.

    - On the Services pane, select **SAP Web IDE Full-Stack**.

    - Click Open SAP Web IDE Full-Stack, and then **Go to Service**.

  2. Set Cloud Foundry Preferences

    Open the Preferences perspective in SAP Web IDE by clicking the Preferences icon and then select Cloud Foundry.

      ![SAP Web IDE Preferences](/images/webide_preference_cf1.png)

    In the pane on the right, select the API endpoint, organization and space for your project.

    ![SAP Web IDE Preferences Details](/images/webide_preference_cf2.png)

    Click **Save**.

  3. In the development perspective,  right-click on your workspace and create a new project from a template (there are other ways to do this, for example, from the home page).

    ![Select create new project from template](/images/webide_create_mta_1.png)

  4. Choose **Multi-Target Application** from the `Cloud Foundry` **Environment** and `Featured` **Category**, click **Next**.

    ![Select multi-target application](/images/webide_create_mta_2.png)

  5. Enter `iot-ds` as a project name, click **Next**.

    ![Enter project name](/images/webide_create_mta_3.png)

  6. Check the **Use HTML5 Application Repository** checkbox, click **Finish**.

    ![Enter MTA details](/images/webide_create_mta_4.png)

  7. SAP Web IDE will generate the new project in your workspace.  Click **Show Hidden Files** button to see all the files in the application.

    ![Show hidden files in project](/images/webide_create_mta_5.png)

  8. Create a new file by right clicking on `iot-ds` project.

    ![Create a new file](/images/webide_create_mta_6.png)

  9. Enter `xs-security.json` as **File Name** and click **OK**.

    ![Enter file name](/images/webide_create_mta_7.png)

  10. A blank `xs-security.json` file should be opened in the code editor.  

  11. Paste the following content into the file.

    ```JSON
    {
      "xsappname": "iot-ds-app",
      "tenant-mode": "dedicated",
      "description": "Security profile of called application",
      "scopes": [
        {
          "name": "uaa.user",
          "description": "UAA"
        }
      ],
      "role-templates": [
        {
          "name": "Token_Exchange",
          "description": "UAA",
          "scope-references": [
            "uaa.user"
          ]
        }
      ]
    }  
    ```

  12. Click **Save**.

    ![Save the file](/images/webide_create_mta_8.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create SAP Fiori launchpad site)]

  1. Enable SAP Fiori launchpad site Web IDE feature.  Go to Web IDE **Preference**, search for `portal` in **Extensions**. Turn on this feature.  Click **Save** to save the change.

    ![Enable SAP Fiori launchpad site SAP Web IDE extension](/images/create_portal_2.png)

  2. In **Development** perspective, right click on the project and add a new **SAP Fiori Launchpad Site Module**.

    ![Add new SAP Fiori launchpad site module](/images/create_portal_3_a.png)

  3. Enter `PortalSite` as **Module** name.

  4. Click **Finish**.

    ![Enter module name](/images/create_portal_4.png)

  5. SAP Fiori launchpad site has been added to the MTA project.

    ![New SAP Fiori launchpad site created](/images/create_portal_5_a.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Configure SAP IoT Notification service)]

  1. Open `CommonDataModel.json` in **Code Editor**.

    ![Open file CommonDataModel.json in code editor](/images/create_portal_9_2_a.png)

  2. Find the location of the JSON key **`payload.sites.payload.config`** in the file.

    ![Position in file where changes should be made](/images/create_portal_9_1_a.png)

  3. Update the value as follows:

    ```JSON
    "config": {
    	"ushellConfig": {
    		"renderers": {
    			"fiori2": {
    				"componentData": {
    					"config": {
    						"enableRecentActivity": false,
    						"enableNotificationsUI": true,
    						"applications": {
    							"Shell-home": {
    								"enableNotificationsPreview": true
    							}
    						}
    					}
    				}
    			}
    		},
    		"services": {
    			"Notifications": {
    				"config": {
    					"enabled": true,
    					"serviceUrl": "/ns/Consumer.svc",
    					"webSocketUrl": "/ns/WebSocket.svc",
    					"pollingIntervalInSeconds": 10
    				}
    			}
    		}
    	}
    }	  
    ```  

    ![File with changes made](/images/create_portal_9_a.png)

  4. **Save** the changes.

  5. Double click `xs-app.json` file in the folder `iot-ds_appRouter` to open it in the **Code Editor**.

  6. Add the following route for notification service:

    ```JSON
    {
      "source": "^/ns/(.*)",
      "target": "$1",
      "service": "com.sap.leonardo.iot",
      "endpoint": "notifications"
    }  	  
    ```

    ![File xs-app.json with changes made](/images/create_portal_10_a.png)

  7. **Save** the file.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add IoT service resource)]

  1. Right click the `iot-ds` project and add new SAP BTP service.

    ![Add new SAP BTP service](/images/service_binding_1_btp.png)

  2. If you were prompted to login, enter your user name and password for the Cloud Foundry space which you have configured in the SAP Web IDE Cloud Foundry preferences.

  3. Enter `iotae` in the **Search** field.

  4. Select the entry in the search result.

  5. Click **Next**.

    ![Enter search term](/images/service_binding_2.png)

  6. In the field **Add or Reuse an Existing Instance**, select **Reuse Service**.

  7. In the field **Service Name (Instance)**, select your service instance from the dropdown.  The service instance name is customer specific.

  8. In the field **Resource Name**, enter the same service instance name selected in the previous step.

  9. The field  **Plan** should be automatically filled.   

  10. Click **Next**.

    ![Enter service defintion](/images/service_binding_3.png)

  11. Click **Finish**.

    ![Confirm service creation](/images/service_binding_4.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add UAA service resource)]

  1. Open `mta.yaml` in **Code Editor**.

    ![Open mta.yaml file in code editor](/images/uaa_mta_1.png)

  2. Add the following UAA resource under **`resources`**. Please use the *Copy* button to copy the formatted configuration and paste it in the specified location in the `yaml` file.

    ```YAML
      - name: uaa_iot-ds
        parameters:
          path: ./xs-security.json
          service-plan: application
          service: xsuaa
        type: org.cloudfoundry.managed-service
    ```  

    ![Update file and save changes](/images/uaa_mta_2.png)

    If you see a red exclamation point on a line in the code editor, it usually indicates a formatting issue with the line.  Please check the indentation of the indicated line as well as the line above or below it.  In this example, the entry **`-name: uaa-iot-ds`** is misaligned with the entry **`-name: iot-ds-html5_repo_host`** .

    ![Sample indentation error in code editor](/images/yaml_format_error_1.png)

    **Note:** Please **use spaces** and **not tabs** when applying indentations in the mta.yaml file.

  3. **Save** the changes.

[VALIDATE_1]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Configure service bindings)]

  1. Open `mta.yaml` file in **MTA Editor**.

  2. Select `iot-ds_appRouter` in **Modules**.

    ![Select module in MTA editor](/images/service_binding_5.png)

  3. Scroll down to the **Requires** section.

  4. Click the **+** button.

  5. A new entry will be created in the table. Select the same **`iotae`** service instance name selected in step 4.

    ![Add iot resource to approuter module](/images/service_binding_6.png)

  6. Click the **+** button.

  7. A new entry will be created in the table. Select the **`uaa`** instance name created in step 5.

    ![Add uaa resource to approuter module ](/images/service_binding_8.png)

  8. Select **`PortalSite`** in **Modules**.

  9. Scroll down to the **Requires** section.

  10. Click the **+** button.

  11. A new entry will be created in the table. Select the **`iotae`** service instance name selected in step 4.

  12. **Save** the file.

    ![Add iot resource to portal module](/images/service_binding_7.png)


[DONE]
[ACCORDION-END]

> Testing notification is **optional** at this point. To test notification, complete Steps 7 to 12.
Otherwise, you can continue with the next tutorial [Build a Decision Support UI in SAP Web IDE](iot-ds-3-create-ui).

[ACCORDION-BEGIN [Step 7: ](Test notification in launchpad site)]

If you want to test notifications in SAP Fiori launchpad site at this point, you'll need to comment out the references to `UI Deployer` in `mta.yaml`. The `UI Deployer` is used for deploying UI components but you have not added any UI components to your project yet and it will cause the deployment to fail.

You'll add UI components to our project in the next tutorial [Build a Decision Support UI in the Web IDE](iot-ds-3-create-ui) and you will be able to test notifications without having to make these changes.

To comment out the references to `UI Deployer`:

1. Open `mta.yaml` in the code editor.

2. Type a `#` character in front of the lines shown below.

    ![Disable UI Deployer in mta.yaml file](/images/disable_uideployer.png)

3. **Save** the file.

> **Please remember to revert these changes before continuing to the next tutorial** [Build a Decision Support UI in the Web IDE](iot-ds-3-create-ui)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Build MTA project)]

  1. In SAP Web IDE, right-click the `iot-ds` project and click **Build** to start the build of the project.

    ![Build project is SAP Web IDE](/images/shared/webide_build_0_1.png)

  2. When the build is completed, a notification will be displayed at the top right corner of the SAP Web IDE.  

  3. You can also check the status of the build by viewing the logs in the SAP Web IDE console.

    ![Project is built successfully](/images/shared/webide_build_1.png)  

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Deploy MTA project)]

  1. You need to have the following entitlements in your Cloud Foundry sub-account to **deploy** and **run** our application.  You can find more information on how to configure entitlements in [Configure Entitlements and Quotas for Subaccounts](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/5ba357b4fa1e4de4b9fcc4ae771609da.html).

    Entitlement | Unit
    ------------ | -------------
    Portal|1 (or unlimited)
    HTML5 Application|2
    Application Runtime Memory| 1 Gib (Minimum)
    Destination|1

  2. To start a deployment, expand the `mta_archives` folder in your project, right click on the `mtar` file inside the folder and click **Deploy to SAP Business Technology Platform**.

    ![Start deployment of project to SAP Business Technology Platform](/images/shared/webide_build_3_btp.png)

  3. Select the appropriate **Cloud Foundry API Endpoint**, **Organization** and **Space** for your deployment.  If you cannot find your selections, please check your Cloud Foundry settings in Web IDE preference.

  4. Click **Deploy**.

    ![Enter SAP BTP information](/images/shared/webide_build_4.png)

  5. When the deployment is completed, a notification will be displayed at the top right corner of the SAP Web IDE.

  6. You can also check the status of the deployment by viewing the logs in the SAP Web IDE console.

    ![Project is deployed successfully](/images/shared/webide_build_5.png)  

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Retrieve thing id)]

  You will need to retrieve the thing id for the next Step. If you already have the thing id, please proceed to the next step.

  1. From SAP IoT launchpad, select the **Thing Modeler** tile.

    ![Select Thing Modeler in SAP Fiori launchpad](/images/shared/thing_modeler_tile.png)  

  2. Select the `greenhouse` package and select **Things** on the left panel.

  3. Click the **Connectivity Information** icon on the top right corner.

    ![Select greenhouse package and click connectivity information](/images/shared/thing_modeler_1.png)  

  4. Copy the thing id.  You'll use it in the next step.

    ![Copy thing id](/images/shared/thing_id.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Generate test notification)]

  1. From SAP IoT launchpad, select the **Actions** tile.

    ![Select Actions from SAP Fiori launchpad](/images/shared/launchpad_tile_actions_1.png)

  2. Select **Greenhouse Action**.

    ![Select greenhouse action](/images/shared/test_portal_1_2.png)

  3. Click **Test**.

    ![Click Test](/images/shared/test_portal_2_1.png)

  4. Enter the **Thing Id** in the **Test Action** dialog. Click **Test**.

    ![Enter thing id and click Test](/images/shared/test_portal_3_1.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Check notification in Portal site)]

  1. To find the URL of the SAP Fiori launchpad site, go to the SAP BTP Cockpit, navigate to the space of the sub-account where the MTA project is deployed.

  2. Navigate to the **Applications** screen.

  3. Click `iot-ds_appRouter`.

    ![Find deployed application in SAP BTP cockpit](/images/shared/find_portal_url_1_btp2.png)

  4. You should see the route displayed under **Application Routes**.

    ![Find application route](/images/shared/find_portal_url_2_btp2.png)

  5. Click on route to launch SAP Fiori launchpad site.

  6. Launch SAP Fiori launchpad site in a browser.  You should see a SAP Fiori notification alert.

    ![Notification alert displayed in SAP Fiori launchpad](/images/shared/test_notif_1_1.png)

  7. Click on the alert icon and the SAP Fiori notification panel will be opened.

    ![Open notification](/images/shared/test_notif_2_1.png)

[DONE]
[ACCORDION-END]
---
