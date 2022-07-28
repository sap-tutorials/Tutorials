---
title: Create a User Interface with CAP (SAP HANA Cloud)
description: Use services based on SAP Cloud Application Programming Model Node.js and use an SAP Fiori wizard to create a user interface.
time: 20
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
tags: [ tutorial>beginner, products>sap-hana, products>sap-business-application-studio, software-product-function>sap-cloud-application-programming-model]
primary_tag: products>sap-hana-cloud
---

## Prerequisites
 - This tutorial is designed for SAP HANA Cloud. It is not designed for SAP HANA on premise or SAP HANA, express edition.
 - You have created database artifacts and loaded data as explained in [the previous tutorial](hana-cloud-cap-create-database-cds).


## Details
### You will learn
 - How to create an SAP Fiori freestyle web interface
 - How to configure the `approuter`

Video tutorial version: </br>

<iframe width="560" height="315" src="https://www.youtube.com/embed/WMDpKa1QkFE" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
---

[ACCORDION-BEGIN [Step 1: ](Run the services)]

1. From the previous tutorial we have a `.env` file in the `/db` folder. This file contains the connection details to the SAP HANA Cloud instance and it was created when we performed the **bind** operation from the SAP HANA Projects view.

    !![.env file](env_file.png)

2. We can use this same configuration information from Cloud Foundry to start the CAP service layer and connect it to SAP HANA as well. Use the command `cds bind -2 MyHANAApp-dev:SharedDevKey` to tell CAP to bind to this same HANA Cloud HDI service instance that we bound to earlier in the SAP HANA Projects view.

    !![cds bind to db service](cds_bind.png)

3. Run the command `npm install` to install any Node.js dependent modules needed by the Cloud Application Programming Model.

    !![npm install](npm_install.png)

4. Now issue the command `cds watch --profile hybrid`. This will start the CAP service locally and use the binding configuration to connect to our remote HANA database instance. Once started you will see a dialog with a button that says **Open in New Tab**. Press this button to test the CAP service in a new browser tab.

    !![npm start](npm_start.png)

    If you accidentally close this dialog, you can always open the running services via **View > Find Command** and then choosing **Ports: Preview** and choosing the running service from the list

7. You should see the list of entities you exposed.

    !![Welcome Page](welcome.png)

8. You can click on the entities to see the values in a JSON format being served from the SAP HANA Cloud database.    

    !![Service Test](service_test_json.png)                 

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Test the services)]

1. Choose the `$metadata` option from the Welcome page and you can see a technical description of the service

    !![metadata document](metadata.png)

2. You can use different methods in the OData v4 services you have created. Go back to the welcome page for the service and click `Interactions_Items`. Different versions of the Cloud Application Programming Model preview page do different things at this point. Some add a $top limit to the generated URL for `Interactions_Items` automatically. Other and perhaps newer versions do not.  Have a look at the end of the URL when it opens. If it ends in `?$top=11` then add the following to the URL:

    ```URL
    &$search=DE
    ```

    Otherwise add the following to the URL:

    ```URL
    ?$search=DE
    ```

    !![Play with the OData Service](serach.png)

3. You can find out more about OData V4 at the [OData organization](https://www.odata.org/documentation/) and the [documentation for SAPUI5](https://sapui5.hana.ondemand.com/#/topic/5de13cf4dd1f4a3480f7e2eaaee3f5b8).    

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Configure routing)]

You will now create an application router module. This module is very important as it will become the entry point for your application. Every request coming to this module will be routed into the different backend services.

1. Open another Terminal instance (so that cds watch can continue to run in the other instance). Issue the command `cds add approuter`.

    !![Create MTA Module](create_module.png)

2. This will complete the wizard and generate a folder named `app` in the root of your project.

    !![New folder for App Router](new_folder_app_router.png)

3. Since the web module will be receiving the requests and routing them into the proper processing backend services, such as the OData service you have just tested, it will also be responsible for enforcing authentication.

    These routing logics are done by an application called `approuter`. You can see the Node.js module being called as the starting script for the web module as defined in the file `package.json`.

    !![package.json for app router](app_router_package_json.png)

4. We need to install the approuter dependency now as well.  From the terminal change to the `app` folder and issue the command `npm install`

    !![app router npm install](app_router_npm_install.png)

5. The `approuter` will scan the file `xs-app.json` to route patterns in the request to the right destinations. The xs-app.json that was generated by the wizard is ready to use real security settings, but our project isn't that far along yet.  Therefore, let's change the configuration to temporarily disable the security checks.

    Replace the content of `xs-app.json` with the following content

    ```json
    {
    "authenticationMethod": "none",
    "routes": [
        {
        "source": "^/app/(.*)$",
        "target": "$1",
        "localDir": ".",
        "cacheControl": "no-cache, no-store, must-revalidate"
        },
        {
        "source": "^/(.*)$",
        "target": "$1",
        "destination": "srv-api",
        "csrfProtection": true
        }
    ]
    }
    ```

    **Save** the changes.

    !![xs-app.json](xsapp_json.png)

6. Among other information, this configuration is declaring that requests containing the pattern `/catalog/(.*)` are routed to a destination called `srv-api`. This destination was defined by the wizard in the `mta.yaml` file.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a Fiori freestyle web interface)]

We want to create a Fiori freestyle UI for our CAP service.  We will use the wizards to generate most of the UI.

1. From the top menu select **View -> Find Command**. Then type `fiori` into the search box. Select **Fiori Open Application Generator**.

    !![Fiori Application Generator](fiori_app_gen.png)

2. Choose **SAPUI5 freestyle** as the application type, select **SAP Fiori Worklist Application** as the floor plan and press **Next**

    !![Fiori Application Type](application_type.png)

3. At the Data Source and Service Selection screen, choose **Use a Local CAP Node.js Project**. Select your project root as the **CAP project folder path**. Select `CatalogService` as your OData service. Press **Next**

    !![Data Source and Service Selection](data_source.png)

4. Choose `Interactions_Items` as the Object collection, `INTHeader_ID` for the remaining columns and press **Next**

    !![Entity Selection](entity_selection.png)

5. In the Project Attributes screen, match to the values shown in the following screenshot and press **Finish**

    !![Project Attributes](project_attributes.png)

6. The new project structure and content should look like This

    !![New Project Structure](new_structure.png)

7. From the terminal you should still have your `cds watch --profile hybrid` still running (if not restart it). This command watches for changes so your application is already to test with the new UI. Open the browser tab where you were testing it previously.   

8. The CAP test page now has a link to the newly generated application.

    !![CAP Test Page Link](cap_test_ui.png)

9. Clicking that link will launch the generated Fiori free style UI for the CAP service.

    !![Test UI](test_ui.png)

Congratulations! You have created your first, full application.

Now it is a good time to commit your application into the local or remote Git.

[DONE]
[ACCORDION-END]

---
