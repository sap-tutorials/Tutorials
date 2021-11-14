---
title: Create a Data Model and Expose It as Using a Service
description: Create a new application using low-code capabilities of SAP Business Application Studio.
auto_validation: true
time: 15
tags: [ tutorial>beginner, topic>mobile, software-product>sap-btp--cloud-foundry-environment, software-product>sap-fiori, software-product>sap-hana-cloud, tutorial>license, tutorial>free-tier]
primary_tag: software-product>sap-business-application-studio
author_name: Raz Korn
author_profile: https://github.com/raz-korn
---

## Prerequisites
 - You have access to your Application Development Lobby.

## Details
### You will learn
- How to create a project from scratch from the Application Development Lobby
- How to create a data model for your application
- How to expose the data using a service so that you'll be able to use it in your apps
- How to add sample data
- How to preview the service definition
- How to view the data before creating apps

---

[ACCORDION-BEGIN [Step 1: ](Create a project)]

1. Projects are created from the Application Development Lobby. You can access the Lobby using its bookmark (if you saved it) or from the SAP BTP cockpit: Select the subaccount you created using the [Set Up SAP Business Application Studio for Low-Code Development](appstudio-onboarding) tutorial, expand **Services**, click **Instances and Subscriptions**, and choose **SAP AppGyver**.

2. Within the Application Development Lobby, click **Create** and select **Business Application**.

    !![launch bas lcap](BAS-Create-Project-1-.png)

2. Provide a **Project Name**, e.g. `CapitalExpenditures`, and click **Create**.

    !![launch bas lcap](BAS-Create-Project-2-.png)

3. Wait for the homepage of SAP Business Application Studio for low-code development to load.

    >Loading SAP Business Application Studio for low-code development may take some time, especially if it's the first time in a day.

    !![bas lcap loaded](BAS-Create-Project-4-.png)

    >Close the notification icon at the bottom of the screen (optional).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Model your data: Entities)]

1. From the homepage, click the **+** of the Data Model tile to add a data model to your project.

    !![bas lcap open entity editor](BAS-LCAP-Data-Model-1-.png)

2. Provide an **Entity Name** for the entity e.g. **`Capex`**, and choose **Create**.

    !![bas lcap create entity](BAS-LCAP-Data-Model-2-.png)

3. Click the **+** to add a property to the **`Capex`** entity.

    !![bas lcap create entity](BAS-LCAP-Data-Model-3-.png)

4. Select the following:

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Property Name | **description** |
    | B | Property Type | **String** (default) |
    | C | Key Property | Leave unchecked (default) |
    | D | Null Value | **Yes** (default) |
    | E | Many | Leave unchecked (default) |
    | F | Other Facets (Max Length) | Leave empty (default) |

    !![bas lcap new property editor](BAS-LCAP-Data-Model-4-.png)

5. Add the following property (click **+**), and choose **Create**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Property Name | **`total_cost`** |
    | B | Property Type | **Integer** |
    | C | Key Property | Leave unchecked (default) |
    | D | Max Length | Leave empty (default) |
    | E | Null Value | **Yes** (default) |
    | F | Many | Leave unchecked (default) |

    !![bas lcap new property editor](BAS-LCAP-Data-Model-5-.png)

    The **`Capex`** entity appears in the **Data Model Editor** tab with a default **ID** property.

    >To edit an entity, click the entity's header and choose the **Edit Entity** icon.

    >!![bas lcap entity editor](BAS-LCAP-Data-Model-6-.png)

6. Access the **Home** tab.

    !![bas lcap return to home](BAS-LCAP-Data-Model-7-.png)

7. Click the **+** of the Data Model tile to add a data model to your project.

    !![bas lcap open entity editor](BAS-LCAP-Data-Model-8-.png)

8. Provide an **Entity Name** for the entity e.g. **Contractors**.

    !![bas lcap create entity](BAS-LCAP-Data-Model-9-.png)

9. Change the **Property Name** of the **ID** property to **contractor**, and its **Property Type** to **Integer**.

    !![bas lcap edit property](BAS-LCAP-Data-Model-10-.png)

10. Add the following property to the **Contractors** entity, and choose **Create**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Property Name | **name** |
    | B | Property Type | **String** (default) |
    | C | Key Property | Leave unchecked (default) |
    | D | Null Value | **Yes** (default) |
    | E | Many | Leave unchecked (default) |
    | F | Other Facets (Max Length) | Leave empty (default) |

    !![bas lcap add property](BAS-LCAP-Data-Model-11-.png)

    The **Contractors** entity appears in the **Data Model Editor**.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Model your data: Relationship between entities)]

1. Select the **`Capex`** entity, and choose the **Add Relationship** icon to add a relationship to the **Contractors** entity.

    !![bas lcap add relationship](BAS-LCAP-Data-Model-14-.png)

2. Select the following, and choose **Create**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Relationship | **Association** (default) |
    | B | Relationship Type | **To-One** (default) |
    | C | Property Name | **contractor** |
    | D | Target Entity Type | **CapitaExpenditures.Contractors** (default) |
    | E | Backlink Property | Leave empty (default) |

    !![bas lcap add relationship](BAS-LCAP-Data-Model-15-.png)

3. You can re-arrange the entities on the canvas manually or by clicking the **Auto Layout** icon to improve the data model visualization.

    !![bas lcap add relationship](BAS-LCAP-Data-Model-16-.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Expose your data)]

1. Access the **Home** tab.

    !![bas lcap return from data model editor to home tab](BAS-LCAP-Service-1-.png)

2. Click the **+** of the **Service** tile to add a service through which data from the data model can be accessed.

    !![bas lcap launch service editor](BAS-LCAP-Service-2-.png)

3. Define the following for your new service, and choose **Create**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Name | **`Capex`** |
    | B | Namespace | **`CapitalExpendituresService`** (default) |
    | C | Type | **`CapitalExpenditures.Capex`** |
    | D | &lt;all properties> | Leave checked (default) |
    | E | ID | Leave checked (default) |
    | F | description | Leave checked (default) |
    | G | `total_cost` | Leave checked (default) |
    | H | contractor | Leave checked (default) |

    !![bas lcap edit service](BAS-LCAP-Service-3-.png)

    The **`Capex`** service appears in the **Service Editor** tab.

4. Access the **Home** tab, and define the following new service.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Name | **Contractors** |
    | B | Namespace | **`CapitalExpendituresService`** (default) |
    | C | Type | **`CapitalExpenditures.Contracotrs`** |
    | D | &lt;all properties> | Leave checked (default) |
    | E | contractor | Leave checked (default) |
    | F | name | Leave checked (default) |

    !![bas lcap edit service](BAS-LCAP-Service-5-.png)

    The **Contractors** service is added to the **Service Editor**, with association to the **`Capex`** service.

    !![bas lcap service editor](BAS-LCAP-Service-6-.png)

5. Select the **`Capex`** service (Which gets highlighted with a blue rectangle indication), and check the **Draft Editing** property.

    !![bas lcap draft editing](BAS-LCAP-Service-7-.png)

6. You can re-arrange the entities on the canvas manually or by clicking the **Auto Layout** icon to improve the service visualization.

    !![bas lcap service editor auto layout](BAS-LCAP-Service-8-.png)

7. Access the **Home** tab. You can see that both the services are added to the **Service** tile.

    !![bas lcap service editor auto layout](BAS-LCAP-Service-9-.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Preview your data)]
It is a common practice in software development to test run any change to an app as soon as possible. Prior to having the services defined, you were not able to test the app. Since now you have defined the services, you can test them.

1. Choose the **Preview** option from the homepage.

    !![bas lcap launch preview](BAS-LCAP-Preview-Service-1-.png)

2. A new tab appears with the links which allow you to test the service definition, access data exposed by the service, and more.

    !![bas lcap launch preview](BAS-LCAP-Preview-Service-3-.png)

3. Choose the **$metadata** to view the service's XML.

    !![bas lcap launch preview](BAS-LCAP-Preview-Service-4-.png)

4. Choose the browser's **Back** button, and select the **`Capex`** or the **Contractors** links to view the data from the respective service.

    !![bas lcap launch preview](BAS-LCAP-Preview-Service-5-.png)

    >Here, the **value** appears as an empty list since the data model is empty.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add sample data and preview it)]
To test that the service works well, i.e. returns data when it is available in the data model, let's add sample data to your project. Sample data is a powerful tool as it allows you to (1) develop an app with no connectivity to a data source; (2) Debug your app.

1. Go back to the browser tab with SAP Business Application Studio for low-code development, and click the **+** of the **Sample Data** tile.

    !![bas lcap launch add sample data](BAS-LCAP-Add-Sample-Data-1-.png)

2. Choose **Create**, select **contractors**, and choose **Create**.

    !![bas lcap launch add sample data](BAS-LCAP-Add-Sample-Data-2-.png)

3. Add the sample data, e.g.

    ```csv
    CONTRACTOR;NAME
    1;SAP
    2;Backer Berlin
    3;Pear Computing Services
    4;C.R.T.U.
    ```

    !![bas lcap edit sample data](BAS-LCAP-Add-Sample-Data-4-.png)

4. Repeat steps 1 - 3 for the **`Capex`** data model. Add the following sample data:

    ```csv
    ID;DESCRIPTION;TOTAL_COST;CONTRACTOR_CONTRACTOR
    7bef2fdf-9b31-4e24-994a-81bb2c28c828;Solar panel connectors;384;4
    3e6ad791-d531-4547-b4ad-f546a3dcfe14;Laptop maintenance;12645;3
    7e7e667e-f52b-4aaa-8d8c-823d1d69f5f3;Bicycles;1723;2
    ```

5. Access the **Home** tab. You can see that the sample data is added to the **Sample Data** tile.

    !![bas lcap sample data](BAS-LCAP-Add-Sample-Data-5-.png)

6. Go back to the browser tab with the links to the services, and click either the link of the **Contractors** service or the link of the **`Capex`** service. You can view the sample data you have added.

    !![bas lcap view sample data from service](BAS-LCAP-Add-Sample-Data-6-.png)

[DONE]
[ACCORDION-END]

---
