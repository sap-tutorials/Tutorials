---
title: Add a List Report Interface to the CAP App (SAP HANA Cloud)
description: Run the CAP app locally and add an SAP Fiori elements list report to display the data
time: 20
auto_validation: true
tags: [ tutorial>beginner, products>sap-hana, products>sap-business-application-studio, topic>user-interface,  software-product-function>sap-cloud-application-programming-model]
primary_tag: products>sap-fiori
---

## Prerequisites
 - This tutorial is designed for SAP HANA Cloud. It is not intended for SAP HANA on-premise or SAP HANA, express edition.
 - You have created database artifacts and loaded data, as explained in [the previous tutorial](hana-cloud-cap-create-database-cds).


## Details
### You will learn
 - How to connect an HDI container during local development
 - How to create an SAP Fiori elements web interface
 - How to run the CAP application locally

---

[ACCORDION-BEGIN [Step 1: ](Start the development server)]

1. Open the **Run Configuration** panel from the left-hand toolbar and click the **+** button to trigger the prompts.

    !![add_run_config](add_run_config.png)

2. Select the suggested development profile **`MyHANAApp`**. You can provide a name for this run configuration in the next step - any name will do the job here.

    !![run_config_profile](run_config_profile.png)

3. You'll see that a new run configuration has been added to the list. Expand the configuration to see that it has one dependency: The HDI container where the data is stored. Click on the **plug** icon to trigger the binding.

    !![run_config_connect_hdi](run_config_connect_hdi.png)

4. Select the HDI container you created in the previous tutorial.

    !![run_config_select_hdi](run_config_select_hdi.png)

5. You'll see a few success messages once the binding has been established successfully.

    !![run_config_connected](run_config_connected.png)

6. Click on the green **play** button of the run configuration. This will switch the view to the debug panel, and scripts will be executed. A few moments later, a message will pop up and suggest exposing a new port. Accept this suggestion by selecting **Expose and Open**.

    !![run_config_expose](run_config_expose.png)

    If you accidentally close this dialog, you can always open the running services via **View > Find Command** and then choosing **Ports: Preview** and select the running service from the list.

7. A new browser tab should now open, and you'll see the list of entities you exposed.

    !![Welcome Page](welcome.png)

8. You can click on the entities to see the values in a JSON format being served from the SAP HANA Cloud database.    

    !![Service Test](service_test_json.png)                 

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Test the services)]

1. Choose the `$metadata` option from the *Welcome page*, and you can see a technical description of the service.

    !![metadata document](metadata.png)

2. You can use different methods in the OData v4 services you have created. Go back to the welcome page for the service and click **`Interactions_Items`**. Add the following to the URL:

    ```URL
    &$search=DE
    ```

    !![Play with the OData Service](search.png)

3. You can find out more about OData V4 at the [OData organization](https://www.odata.org/documentation/) and the [documentation for SAPUI5](https://sapui5.hana.ondemand.com/#/topic/5de13cf4dd1f4a3480f7e2eaaee3f5b8).    

[VALIDATE_1]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 3: ](Add a managed approuter)]

The managed approuter will make it easy for us to access web applications (such as the SAP Fiori elements app) in a [serverless manner](https://blogs.sap.com/2020/10/02/serverless-sap-fiori-apps-in-sap-cloud-platform/) later on.

1. Trigger the wizard with a **right-click** on the `mta.yaml` file and select **Create MTA Modules from Template**.

    !![Create MTA Module](create_module.png)

2. This will start a wizard to add the module to your project. Choose the Module Template type of **Approuter Configuration** and then press **Start**.

    !![approuter_config](approuter_config.png)

3. Choose the application runtime type of **Managed Approuter**, chose **`hana.app`** as the name of the business solution, and confirm that you want to **add a UI**. Then press **Next** to complete the wizard.

    !![approuter_param](approuter_param.png)        

4. This will complete the wizard and add a few new sections to the `mta.yaml` file at the root of your project.

    !![approuter_finished](approuter_finished.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 4: ](Add the Fiori elements list report)]

Now that the managed approuter is part of this project, it's time to add the web application. To be more precise, an SAP Fiori elements app that implements the [List Report Floorplan](https://experience.sap.com/fiori-design-web/list-report-floorplan-sap-fiori-element/) .  We will use the built-in wizard to generate the UI.

1. This wizard can also be triggered via a **right-click** on the `mta.yaml` file and then select **Create MTA Modules from Template** once again.

    !![Add module](fe_add_module.png)

2. This time you need to choose the **SAP Fiori application** tile and **Start** the wizard.

    !![fe_start](fe_start.png)

3. As mentioned before, we want to create a **List Report Object Page**. Select the corresponding tile and proceed with **Next**.    

    !![fe_list_report](fe_list_report.png)

4. This Fiori app shall retrieve its data from the **Local CAP Project**. Provide the path to this project so that you can select the **`CatalogService`** you defined in a previous step.

    !![fe_link_cap](fe_link_cap.png)    

5. For this tutorial, we want to visualize the **`Interactions_Items`** entity.

    !![fe_entity](fe_entity.png)

6. Fill in the properties with the following key-value pairs before you confirm with **Next**.

    | **Key**       | **Value**           
    | ------------- |:-------------:|
    | Module name      | **`frontend`**
    | Application title      | **`List Report`**     
    | Add deployment configuration | true      |
    | Add FLP configuration | true      |

    !![fe_attributes](fe_attributes.png)

7. Select **Cloud Foundry** as the target platform. Note that it's not necessary to enter a destination name. We will create the required destination in the following tutorial directly in the code.

    !![fe_deploy_config](fe_deploy_config.png)

8. Last but not least, enter the launchpad configuration. This information is needed for the navigation from the Launchpad to your Fiori app. Once you provided all the data from the screenshot, hit **Finish** to add the new files to the project.

    !![fe_launchpad](fe_launchpad.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 5: ](Restart the development server)]

So we added the Fiori resources now, but they are not yet visible in the running development server. This is expected as we started the process *before* the files were created.

1. Go to the **Debug** view and hit the **restart** button.

    !![restart_run](restart_run.png)


2. Go back to the running CAP application or open a new tab (if you already closed it). There should now be an `index.html` link under *Web Applications:*. Click this link.

    !![restarted_index](restarted_index.png)

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 5: ](Display the data)]

It's time to use the Fiori application!

1. You should only see one tile **List Report** in the Launchpad sandbox. Select this application.

    !![local_flp](local_flp.png)


2. Note that you need to select the visible columns manually before you can see any records.

    !![select_columns](select_columns.png)


2. Once all needed columns are selected, hit the **Go** button to display the data.

    !![local_lr](local_lr.png)


Congratulations! You have created a complete SAP Fiori elements application.

Now is an excellent time to commit your application to the local or remote Git.

[DONE]
[ACCORDION-END]

---
