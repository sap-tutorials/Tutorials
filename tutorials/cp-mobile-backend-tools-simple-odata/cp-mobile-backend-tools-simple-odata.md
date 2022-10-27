---
title: Create a Simple OData Service with Mobile Back-End Tools
description: Create and deploy a simple OData Service with SAP Mobile Services, mobile back-end tools (MBT) using the CSDL graphical modeler.
auto_validation: true
time: 20
tags: [ tutorial>intermediate, products>sap-business-technology-platform, products>sap-mobile-services, products>sap-business-application-studio]
primary_tag: topic>mobile
author_name: Manuel Stampp
author_profile: https://github.com/manuel-stampp
---

## Prerequisites
  - [Get Ready to Develop on SAP Business Technology Platform](group.scp-1-get-ready)
  - [Setup your Business Application Studio environment for Mobile](cp-mobile-bas-setup)

## Details
### You will learn
  - How to use graphical editor to create your OData model
  - How to deploy an OData service with SAP Mobile Services, mobile back-end tools
  - How to create an app router and configure login

The mobile back-end tools is a set of tools that enable a developer to model an OData service and generate a Java EE web application to implement the OData service, along with automatic creation of the necessary database tables within a pre-existing database schema.

In this tutorial, you will rebuild a small part of the `GWSAMPLE_BASIC` OData service publicly available on the SAP Gateway Demo system (ES5), using the MBT graphical modeler in SAP Business Application Studio. In further tutorials you can therefore extend, connect and build an app on top of it.

---

[ACCORDION-BEGIN [Step 1: ](Create metadata file in SAP Business Application Studio)]

1. Launch the [Dev space](cp-mobile-bas-setup) in SAP Business Application Studio.

2. Open your workspace's default projects folder.

    Click **Files & Folders** on the **Welcome** screen or **File** &rarr; **Open**.

    Select **projects** in the popup window and click **Open**.

    ![Open projects folder](img_open_projects_folder.png)

3. Select **File** &rarr; **New Folder**.

    Name the folder `MBTEPMDemoService`.

    Select **File** &rarr; **Open**, select the folder, and click **Open**.

    > This will be the root folder of the OData service. It is important that this is specifically opened, because the following commands will deploy files to the currently opened folder.

3. Select **View** &rarr; **Find Command**.

4. Type `MBT` and select **MBT: New OData CSDL document (metadata)** and press **Enter**.

    ![Command selection](img_task_new_csdl.png)

5. Answer the prompts with the following values:

    | **Prompt** | **Value** |
    |----|----|
    | Choose OData version| `4.0` |
    | Schema namespace | `com.sap.mbtepmdemo` |
    | Namespace alias name | `Self` |
    | Metadata file name | `metadata.csdl.xml` |

The command creates a blank metadata file for you that you can now open and edit with the graphical editor.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add entities to the OData service)]

> In the end of this step, the full content of the CSDL file is linked, so you **do not have to build all the properties**. Nevertheless you are encouraged to create a complex type, the entities, some properties and especially the association yourself in Graphical Modeler.

> You can also open the CSDL file as XML document instead of the graphical editor - either by clicking **Switch Editor** (icon) button in the top left corner of the graphical modeler or by right-clicking the file &rarr; **Open with** &rarr; **Code Editor**. This is especially helpful if you want to import parts of existing models or perform advanced changes which exceed the graphical modeler's capabilities.

0. Click the file `metadata.csdl.xml` on the left panel in order to open it by default in the Graphical Modeler. Please note that the graphical modeler is only available when the file ending is `.csdl.xml`.

1. Having **complex Types** selected, click the **+** button to add a new complex type with name ``CT_Address``

    ![New complex type](img_new_complex_type.png)

2. From **Complex Types**, select the type ``CT_Address`` and click **+** to add the following properties:

    | **Name** | **Type** | **Max Length** |
    |----|----|----|
    | ``AddressType`` | *String* | *2* |
    | ``Building`` | *String* | *10* |
    | ``City`` | *String* | *40* |
    | ``Country`` | *String* | *3* |
    | ``PostalCode`` | *String* | *10* |
    | ``Street`` | *String* | *60* |

    ![Complex type properties](img_complex_type_attributes.png)

3. Having **Entity Types** selected, press the **+** button to add a new entity type with name ``BusinessPartner``

    ![New entity](img_new_entity.png)

4. Select the entity and click **+** to add the following properties:

    | **Name** | **Type** | **Max Length** |
    |----|----|----|
    | ``Address`` | ``CT_Address`` |  |
    | ``BusinessPartnerRole`` | *String* | *3* |
    | ``CompanyName`` | *String* | *80* |
    | ``LegalForm`` | *String* | *10* |
    | ``PhoneNumber`` | *String* | *30* |
    | ``FaxNumber`` | *String* | *30* |
    | ``EmailAddress`` | *String* | *255* |

5. Repeat sub-step **4** and **5** for the entity ``SalesOrder`` and the following attributes. Tick the checkbox **nullable** for the marked properties.

    | **Name** | **Type** | **Max Length** | **Nullable** |
    |----|----|----|----|
    | ``Note`` | *String* | *255* | *tick* |
    | ``NoteLanguage`` | *String* | *2* | *tick* |
    | ``CustomerID`` | *String* | *10* | *tick* |
    | ``CustomerName`` | *String* | *80* | *tick* |
    | ``CurrencyCode`` | *String* | *5* | *tick* |
    | ``GrossAmount`` | *Decimal* | *Precision: 16, Scale: 3* | *tick* |
    | ``NetAmount`` | *Decimal* | *Precision: 16, Scale: 3* | *tick* |
    | ``TaxAmount`` | *Decimal* | *Precision: 16, Scale: 3* | *tick* |
    | ``LifecycleStatus`` | *String* | *1* | *tick* |
    | ``LifecycleStatusDescription`` | *String* | *60* | *tick* |
    | ``BillingStatus`` | *String* | *1* | *tick* |
    | ``BillingStatusDescription`` | *String* | *60* | *tick* |
    | ``DeliveryStatus`` | *String* | *1* | *tick* |
    | ``DeliveryStatusDescription`` | *String* | *60* | *tick* |
    | ``CreatedAt`` | ``DateTimeOffset`` | | *tick* |
    | ``ChangedAt`` | ``DateTimeOffset`` | | *tick* |

6. Having ``BusinessPartner`` selected, click **+** and add the **Navigation to** ``SalesOrders`` with the following properties:

    - Tick *Navigation* and *Collection*
    - Name: ``ToSalesOrders``
    - Type: ``SalesOrder``
    - Partner: ``ToBusinessPartner``

    This will create the association as well as the bi-directional navigation properties.

    ![Animation create relationship](gif_add_navigation.gif)

> If you built not all the properties as described, please complete the CSDL file with the file content below.

  <details>
  <summary> **Click to expand** to see the full CSDL file.</summary>

```XML
<?xml version="1.0" encoding="utf-8"?>
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://docs.oasis-open.org/odata/ns/edmx http://docs.oasis-open.org/odata/odata/v4.0/os/schemas/edmx.xsd http://docs.oasis-open.org/odata/ns/edm http://docs.oasis-open.org/odata/odata/v4.0/os/schemas/edm.xsd">
    <edmx:Reference Uri="https://oasis-tcs.github.io/odata-vocabularies/vocabularies/Org.OData.Core.V1.xml">
        <edmx:Include Namespace="Org.OData.Core.V1" Alias="Core"/>
    </edmx:Reference>
    <edmx:DataServices>
        <Schema Namespace="com.sap.mbtepmdemo" Alias="Self" xmlns="http://docs.oasis-open.org/odata/ns/edm">
            <ComplexType Name="CT_Address">
                <Property Name="AddressType" Type="Edm.String" Nullable="false" MaxLength="2"/>
                <Property Name="Building" Type="Edm.String" Nullable="false" MaxLength="10"/>
                <Property Name="City" Type="Edm.String" Nullable="false" MaxLength="40"/>
                <Property Name="Country" Type="Edm.String" Nullable="false" MaxLength="3"/>
                <Property Name="PostalCode" Type="Edm.String" Nullable="false" MaxLength="10"/>
                <Property Name="Street" Type="Edm.String" Nullable="false" MaxLength="60"/>
            </ComplexType>
            <EntityType Name="BusinessPartner">
                <Key>
                    <PropertyRef Name="BusinessPartnerID"/>
                </Key>
                <Property Name="Address" Type="Self.CT_Address" Nullable="false"/>
                <Property Name="BusinessPartnerID" Type="Edm.Int64" Nullable="false"/>
                <Property Name="BusinessPartnerRole" Type="Edm.String" Nullable="false" MaxLength="3"/>
                <Property Name="CompanyName" Type="Edm.String" Nullable="false" MaxLength="80"/>
                <Property Name="FaxNumber" Type="Edm.String" Nullable="false" MaxLength="30"/>
                <Property Name="LegalForm" Type="Edm.String" Nullable="false" MaxLength="10"/>
                <Property Name="PhoneNumber" Type="Edm.String" Nullable="false" MaxLength="30"/>
                <NavigationProperty Name="ToSalesOrders" Type="Collection(Self.SalesOrder)" Partner="ToBusinessPartner"/>
            </EntityType>
            <EntityType Name="SalesOrder">
                <Key>
                    <PropertyRef Name="SalesOrderID"/>
                </Key>
                <Property Name="BillingStatus" Type="Edm.String" Nullable="true" MaxLength="1"/>
                <Property Name="BillingStatusDescription" Type="Edm.String" Nullable="true" MaxLength="60"/>
                <Property Name="BusinessPartnerID" Type="Edm.Int64" Nullable="false"/>
                <Property Name="ChangedAt" Type="Edm.DateTimeOffset" Nullable="true" Precision="7"/>
                <Property Name="CreatedAt" Type="Edm.DateTimeOffset" Nullable="true" Precision="7"/>
                <Property Name="CurrencyCode" Type="Edm.String" Nullable="true" MaxLength="5"/>
                <Property Name="CustomerID" Type="Edm.String" Nullable="false" MaxLength="10"/>
                <Property Name="CustomerName" Type="Edm.String" Nullable="true" MaxLength="80"/>
                <Property Name="DeliveryStatus" Type="Edm.String" Nullable="true" MaxLength="1"/>
                <Property Name="DeliveryStatusDescription" Type="Edm.String" Nullable="true" MaxLength="60"/>
                <Property Name="GrossAmount" Type="Edm.Decimal" Nullable="true" Precision="16" Scale="3"/>
                <Property Name="LifecycleStatus" Type="Edm.String" Nullable="true" MaxLength="1"/>
                <Property Name="LifecycleStatusDescription" Type="Edm.String" Nullable="true" MaxLength="60"/>
                <Property Name="NetAmount" Type="Edm.Decimal" Nullable="true" Precision="16" Scale="3"/>
                <Property Name="Note" Type="Edm.String" Nullable="true" MaxLength="255"/>
                <Property Name="NoteLanguage" Type="Edm.String" Nullable="true" MaxLength="2"/>
                <Property Name="SalesOrderID" Type="Edm.Int64" Nullable="false"/>
                <Property Name="TaxAmount" Type="Edm.Decimal" Nullable="true" Precision="16" Scale="3"/>
                <NavigationProperty Name="ToBusinessPartner" Type="Self.BusinessPartner" Nullable="false" Partner="ToSalesOrders">
                    <ReferentialConstraint Property="BusinessPartnerID" ReferencedProperty="BusinessPartnerID"/>
                </NavigationProperty>
            </EntityType>
            <EntityContainer Name="Com_sap_mbtepmdemoService">
                <EntitySet Name="BusinessPartnerSet" EntityType="Self.BusinessPartner">
                    <NavigationPropertyBinding Path="ToSalesOrders" Target="SalesOrderSet"/>
                </EntitySet>
                <EntitySet Name="SalesOrderSet" EntityType="Self.SalesOrder">
                    <NavigationPropertyBinding Path="ToBusinessPartner" Target="BusinessPartnerSet"/>
                </EntitySet>
            </EntityContainer>
        </Schema>
    </edmx:DataServices>
</edmx:Edmx>
```
</details>

&nbsp;

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Generate and run the service)]

1. Select **View** &rarr; **Find Command** (or press **Shift + CTRL/CMD + P**).

2. Type `cf login`, click **CF: Login to Cloud Foundry**

    Follow the prompts to select your organization and space by clicking on the home button in the lower-left corner.

    ![CF Login](img_cf_login.png)

2. Select **View** &rarr; **Find Command** (or press **Shift + CTRL/CMD + P**).

3. Type `MBT` and select **MBT: Create tasks.json file**.

4. In the input dialog, confirm the options as given in the following table:

    | Prompt | Value |
    |----|----|
    | `Generate odata service for Cloud Foundry`| *Yes, Cloud Foundry* |
    | `applicationName` | `MbtEpmDemoService` |
    | `application version` | *1.0.0* |
    | `Generate odata service with Spring Boot style` | *No (Java EE style)* |
    | `Select database type` | *H2 Database* |
    | `Target folder where generate odata service` | *Keep Default* |
    | `Do you want to add MTA support` | *Yes* |
    | `Enter the module name of MTA project` | `srv` |

    > Make sure to not use underscores in the application name

    > The selected database type will be corresponding, explicit database for the MBT OData service.  For the sake of simplicity, for this tutorial's purpose an embedded H2 database type is used, which is not supported for productive use.

5. In the last step of the wizard, an **Open File** dialog comes up. Locate `metadata.csdl.xml` file from your workspace and click **Open Metadata**.

    ![SAP Business Application Studio Wizard](img_open_csdl_file.png)

6. Select **Terminal** &rarr; **Run Task** and select the task `csdl-to-war-nodeploy` to generate and compile the service.

7. Open the file `manifest.yml` in `srv` folder and add a line with `random-route: true`, as in the following:

    ```YAML
    ---
    applications:
      - buildpack: sap_java_buildpack
        name: MbtEpmDemoService
        path: target/odata-service-1.0.0.war
        random-route: true
        env:    
          SET_LOGGING_LEVEL: '{odata: TRACE, sap.xs.console: TRACE, sap.xs.odata: TRACE}'
          TARGET_RUNTIME: tomee7
    ```

6. Select **Terminal** &rarr; **Run Task** and select the task `csdl-to-war` to generate, deploy and run the service to your space. You can observe in the Terminal if the run was successful.

    ![Deployment log](img_run_log.png)

7. (Optional) If you want your service to load test data, you can switch `TEST_MODE` to `true`. Therefore you execute task `csdl-to-war-test` or edit the variable in file `TestSettings.java` from your workspace at the path `srv` &rarr; `src` &rarr; `main` &rarr; `java` &rarr; `com` &rarr; `sap` &rarr; `mbtepmdemo` &rarr; `TestSettings.java`.

    You can also edit the generated test data inside the folder `srv` &rarr; `src` &rarr; `main` &rarr; `resources` &rarr; `test-data`. The test data is stored in the `.json` files. You will have to re-run the build task `csdl-to-war` again to reflect this change.

    ![Test Mode and Test Data](img_test_data.png)

>In case you struggle on generating the service, you might find the [documentation](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mbt/generating.html) helpful.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Configure authentication and app router)]

If the service shall be accessible independently and authentication is required, an Authorization and Trust Management Service (XSUAA) service binding as well as an app router will be required for your OData service. If you are looking for more details of this service, you might want to go through [Secure a Basic Node.js App with the Authorization and Trust Management Service (XSUAA)](cp-cf-security-xsuaa-create) tutorial and its references, already covering the same for another application type.

1. Open the file ``tasks.json`` from folder ``.vscode`` and uncomment the line `"-login", "XSUAA",` in **every** of the configurations as shown below.

    ![Uncomment for XSUAA security](img_uncomment_login.png)

    >Due to this change, after generating the service again it will not only require XSUAA authentication, but also create a default ``xs-security.json`` file.

2. To update the project and generate the file, select **Terminal** &rarr; **Run Task** and select the task `csdl-to-war-nodeploy`.

    >In your workspace a file ``xs-security.json`` will be generated inside `srv` folder. It contains two basic roles and corresponding scopes: ``Everyone`` (for users) and ``ViewMetrics`` (for administrators). Use this file to create a service instance via MTA.

    >If you do not find the file, try to close and re-open `tasks.json` file. This will refresh the file system cache. Then re-run the task.

4. Right-click the `srv` folder in your workspace, then click **New Folder**, name it `approuter` and confirm with **OK**.

5. Right-click the folder `approuter` in your workspace and select **New File** for two files `package.json` and `xs-app.json` with the content below.

    ```JSON package.json
    {
      "name": "approuter",
      "dependencies": {
        "@sap/approuter": "*"
      },
      "scripts": {
        "start": "node node_modules/@sap/approuter/approuter.js"
      }
    }
    ```

    ```JSON xs-app.json
    {
      "routes": [{
        "source": "^/",
        "target": "/",
        "destination": "odata"
      }]
    }
    ```

    ![Create new files](animation_create_approuter_files4.gif)

7. Finally, the XSUAA service binding need to be reflected for deployment. You can achieve this by adding them to the ``manifest.yml`` that was generated in your workspace.

    - Initially, your `manifest.yml` should look like the following:

    ```YAML
    applications:
      - buildpack: sap_java_buildpack
        name: MbtEpmDemoService
        path: target/odata-service-1.0.0.war
        random-route: true
        env:
          TARGET_RUNTIME: tomee7
    ```

    >When inserting these snippets, pay attention to the indentation of the lines, as YAML is indentation-sensitive. Before pasting, make sure that your cursor is set to beginning of an empty line.

    >**Hint:** You can indent multiple line back or forward by selecting them and pressing **(Shift + TAB)** or **(TAB)** on your keyboard.

    - To bind the XSUAA service instance, add the following lines:

    ```YAML
        services:    
          - MbtEpmDemoService-xsuaa
    ```

    >**Important:** Do not execute the ``csdl-to-war`` tasks anymore. First we need to create the service instances via the Mutli-Target-Archive. When they are created, you can work with ``csdl-to-war`` again in order to push just the OData service. This will be quicker than deploying the full MTA.

      <details>
      <summary> **Click to expand** - for reference check the full `manifest.yml` file content. </summary>

    ```YAML
    ---
    applications:
      - buildpack: sap_java_buildpack
        name: MbtEpmDemoService
        path: target/odata-service-1.0.0.war
        env:    
          SET_LOGGING_LEVEL: '{odata: TRACE, sap.xs.console: TRACE, sap.xs.odata: TRACE}'
          TARGET_RUNTIME: tomee7
        services:    
          - MbtEpmDemoService-xsuaa
    ```
      </details>

8. Create a MTA deployment descriptor.

    Right-click the file `mta.yaml` in the root project folder and click **Duplicate**.

    Rename the copy file to `mtad.yaml`.

    Replace the content with the following to include all required resources:

    ```YAML mtad.yaml
    ---
    ID: MbtEpmDemoService
    _schema-version: '3.3'
    version: 1.0.0
    modules:
      -
        # application
        name: MbtEpmDemoService
        # module
        path: srv/target/odata-service-1.0.0.war
        type: java
        parameters:    
          memory: 1G
          disk: 2G
          instances: 1
        properties:    
          SET_LOGGING_LEVEL: '{odata: TRACE, sap.xs.console: TRACE, sap.xs.odata: TRACE}'
          TARGET_RUNTIME: tomee7
        requires:
          - name: MbtEpmDemoService-xsuaa
        # Providing default-url to be re-used for the app router's destination
        provides:
          -
            name: mbtepmdemo-odata
            properties:
              url: ${default-url}
      -
        # approuter
        name: MbtEpmDemoService-approuter
        type: nodejs
        path: srv/approuter
        requires:
          - name: MbtEpmDemoService-xsuaa
          # require
          - name: mbtepmdemo-odata
        parameters:
          buildpack: nodejs_buildpack
          instances: 1
          memory: 128M
        properties:
        # Here we reference the provided URL for automatic linking
          destinations: >
            [
              {"name":"odata","url":"~{mbtepmdemo-odata/url}","forwardAuthToken": true}
            ]  
    resources:
      -
        name: MbtEpmDemoService-xsuaa
        type: org.cloudfoundry.managed-service   
        parameters:
          service: xsuaa
          service-plan: application
          path: srv/xs-security.json
    ```

9. Click **Terminal** &rarr; **New Terminal**, type `cf deploy` and press Enter.

    Wait for the deployment to be finished. MTA deployment may take a while, especially when it is initially deployed.

[VALIDATE_4]
[ACCORDION-END]

---

**Congratulations!** You have now created a stand-alone OData service with Mobile Back-End Tools. You can now continue with the next Tutorial of the mission to configure the service as Cache-DB and integrate data from another system.

---
