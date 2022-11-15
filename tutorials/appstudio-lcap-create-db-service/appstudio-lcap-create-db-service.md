---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, topic>mobile, software-product>sap-btp--cloud-foundry-environment, software-product>sap-fiori, software-product>sap-hana-cloud, tutorial>license, tutorial>free-tier]
primary_tag: software-product>sap-business-application-studio
author_name: Raz Korn
author_profile: https://github.com/raz-korn
---

# Create a Data Model and Expose It as a Service
<!-- description --> Create a capital expenditures data model and expose it via a service, using the low-code capabilities of SAP Business Application Studio.

## Prerequisites
 - You have access to your Application Development Lobby.
 - If you are doing this as part of Devtoberfest 2022, you can create a trial account and set up SAP Business Application Studio according to [Set Up SAP Business Application Studio for Development](https://developers.sap.com/tutorials/appstudio-onboarding.html), create a low-code Dev Space, and launch the dev space. Finally, create a project.

## You will learn
- How to create a project from scratch from the Application Development Lobby
- How to create a data model for your application
- How to expose the data using a service so that you'll be able to use it in your apps
- How to add sample data
- How to preview the service definition
- How to view the data before creating apps

---

### Create a project


1. Projects are created from the Application Development Lobby. You can access the Lobby using its bookmark (if you saved it) or from the SAP BTP cockpit: Select the subaccount you created using the [Set Up SAP Business Application Studio for Low-Code Development](appstudio-onboarding) tutorial, expand **Services**, click **Instances and Subscriptions**, and choose **`SAP AppGyver`**.

2. Within the Application Development Lobby, click **Create** and select **Business Application**.

    <!-- border -->![launch bas lcap](BAS-Create-Project-1-.png)

2. Provide a **Project Name**, e.g. `CapitalExpenditures`, and click **Create**.

    <!-- border -->![launch bas lcap](BAS-Create-Project-2-.png)

3. Wait for the homepage of SAP Business Application Studio for low-code development to load.

    >Wait for the "We have finished installing all tools and templates for you, enjoy your work!" notification.

    >Loading SAP Business Application Studio for low-code development may take some time, especially if it's the first time in a day.

    <!-- border -->![bas lcap loaded](BAS-Create-Project-4-.png)

    >Close the notification icon at the bottom of the screen (optional).


### Model your data: Entities


1. From the homepage, click the **+** of the Data Model tile to add a data model to your project.

    <!-- border -->![bas lcap open entity editor](BAS-LCAP-Data-Model-1-.png)

2. Provide an **Entity Name** for the entity e.g. **`Capex`**, and choose **Create**.

    <!-- border -->![bas lcap create entity](BAS-LCAP-Data-Model-2-.png)

3. Click the **+** to add a property to the **`Capex`** entity.

    <!-- border -->![bas lcap create entity](BAS-LCAP-Data-Model-3-.png)

4. Select the following:

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Property Name | **description** |
    | B | Property Type | **String** (default) |
    | C | Key Property | Leave unchecked (default) |
    | D | Null Value | **Yes** (default) |
    | E | Many | Leave unchecked (default) |
    | F | Other Facets (Max Length) | Leave empty (default) |

    <!-- border -->![bas lcap new property editor](BAS-LCAP-Data-Model-4-.png)

5. Add the following property (click **+**), and choose **Create**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Property Name | **`total_cost`** |
    | B | Property Type | **Integer** |
    | C | Key Property | Leave unchecked (default) |
    | D | Max Length | Leave empty (default) |
    | E | Null Value | **Yes** (default) |
    | F | Many | Leave unchecked (default) |

    <!-- border -->![bas lcap new property editor](BAS-LCAP-Data-Model-5-.png)

    The **`Capex`** entity appears in the **Data Model Editor** tab with a default **ID** property.

    >To edit an entity, click the entity's header and choose the **Edit Entity** icon.

    ><!-- border -->![bas lcap entity editor](BAS-LCAP-Data-Model-6-.png)

6. Access the **Home** tab.

    <!-- border -->![bas lcap return to home](BAS-LCAP-Data-Model-7-.png)

7. Click the **+** of the Data Model tile to add a data model to your project.

    <!-- border -->![bas lcap open entity editor](BAS-LCAP-Data-Model-8-.png)

8. Provide an **Entity Name** for the entity e.g. **Contractors**.

    <!-- border -->![bas lcap create entity](BAS-LCAP-Data-Model-9-.png)

9. Change the **Property Name** of the **ID** property to **contractor**, and its **Property Type** to **Integer**.

    <!-- border -->![bas lcap edit property](BAS-LCAP-Data-Model-10-.png)

10. Add the following property to the **Contractors** entity, and choose **Create**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Property Name | **name** |
    | B | Property Type | **String** (default) |
    | C | Key Property | Leave unchecked (default) |
    | D | Null Value | **Yes** (default) |
    | E | Many | Leave unchecked (default) |
    | F | Other Facets (Max Length) | Leave empty (default) |

    <!-- border -->![bas lcap add property](BAS-LCAP-Data-Model-11-.png)

    The **Contractors** entity appears in the **Data Model Editor**.



### Model your data: Relationship between entities


1. Select the **`Capex`** entity, choose the **Add Relationship** icon, and select the **Contractors** entity to add a relationship to the **Contractors** entity.

    <!-- border -->![bas lcap add relationship](BAS-LCAP-Data-Model-14-.png)

    <!-- border -->![bas lcap add relationship](BAS-LCAP-Data-Model-14-2-.png)

2. Select the following, and choose **Create**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Relationship | **Association** (default) |
    | B | Relationship Type | **To-One** (default) |
    | C | Property Name | **contractor** |
    | D | Target Entity Type | **CapitaExpenditures.Contractors** (default) |
    | E | Backlink Property | Leave empty (default) |

    <!-- border -->![bas lcap add relationship](BAS-LCAP-Data-Model-15-.png)

3. You can re-arrange the entities on the canvas manually or by clicking the **Auto Layout** icon to improve the data model visualization.

    <!-- border -->![bas lcap add relationship](BAS-LCAP-Data-Model-16-.png)



### Expose your data


1. Access the **Home** tab.

    <!-- border -->![bas lcap return from data model editor to home tab](BAS-LCAP-Service-1-.png)

2. Click the **+** of the **Service** tile to add a service through which data from the data model can be accessed.

    <!-- border -->![bas lcap launch service editor](BAS-LCAP-Service-2-.png)

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

    <!-- border -->![bas lcap edit service](BAS-LCAP-Service-3-.png)

    The **`Capex`** service appears in the **Service Editor** tab.

4. Access the **Home** tab, and define the following new service.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Name | **Contractors** |
    | B | Namespace | **`CapitalExpendituresService`** (default) |
    | C | Type | **`CapitalExpenditures.Contractors`** |
    | D | &lt;all properties> | Leave checked (default) |
    | E | contractor | Leave checked (default) |
    | F | name | Leave checked (default) |

    <!-- border -->![bas lcap edit service](BAS-LCAP-Service-5-.png)

    The **Contractors** service is added to the **Service Editor**, with association to the **`Capex`** service.

    <!-- border -->![bas lcap service editor](BAS-LCAP-Service-6-.png)

5. Select the **`Capex`** service (Which gets highlighted with a blue rectangle indication), and check the **Draft Editing** property.

    >A draft is an interim version of a business entity that has not yet been explicitly saved as an active version. This allows users to be e.g. interrupted, and continue later on. For more information visit [SAP Fiori Design Guidelines - Draft Handling](https://experience.sap.com/fiori-design-web/draft-handling/).

    <!-- border -->![bas lcap draft editing](BAS-LCAP-Service-7-.png)

6. You can re-arrange the entities on the canvas manually or by clicking the **Auto Layout** icon to improve the service visualization.

    <!-- border -->![bas lcap service editor auto layout](BAS-LCAP-Service-8-.png)

7. Access the **Home** tab. You can see that both the services are added to the **Service** tile.

    <!-- border -->![bas lcap service editor auto layout](BAS-LCAP-Service-9-.png)



### Preview your data

It is a common practice in software development to test run any change to an app as soon as possible. Prior to having the services defined, you were not able to test the app. Since now you have defined the services, you can test them.

1. Choose the **Preview** option from the homepage, and select either **With Sample Data** or **With Live Data** option.

    <!-- border -->![bas lcap launch preview](BAS-LCAP-Preview-Service-1-.png)

2. The project preview page appears in a new tab with the links which allow you to test the service definition, access data exposed by the service, and more.

    <!-- border -->![bas lcap launch preview](BAS-LCAP-Preview-Service-3-.png)

3. Choose the **metadata** to view the service's XML.

    <!-- border -->![bas lcap launch preview](BAS-LCAP-Preview-Service-4-.png)

4. Select the **`Capex`** or the **Contractors** **view as code** to view the data from the respective service.

    <!-- border -->![bas lcap launch preview](BAS-LCAP-Preview-Service-5-.png)

    >Here, the **value** appears as an empty list since the data model is empty.


### Add sample data and preview it

To test that the service works well, i.e. returns data when it is available in the data model, let's add sample data to your project. Sample data is a powerful tool as it allows you to (1) develop an app with no connectivity to a data source; (2) Debug your app.

1. Go back to the browser tab with SAP Business Application Studio for low-code development, and click the **+** of the **Sample Data** tile.

    <!-- border -->![bas lcap launch add sample data](BAS-LCAP-Add-Sample-Data-1-.png)

2. Choose **Create**, select **Contractors**, and choose **Create**.

    <!-- border -->![bas lcap launch add sample data](BAS-LCAP-Add-Sample-Data-2-.png)

3. In the sample data editor add 4 rows.

    <!-- border -->![bas lcap edit sample data](BAS-LCAP-Add-Sample-Data-3-.png)

4. Update the sample data as follows:

    <!-- border -->![bas lcap edit sample data](BAS-LCAP-Add-Sample-Data-4-.png)

5. Access the **Home** tab, and click the **+** of the **Sample Data** tile.

    <!-- border -->![bas lcap edit sample data](BAS-LCAP-Add-Sample-Data-4-2-.png)

6. Choose **Create**, select **`Capex`**, and choose **Create**.

    <!-- border -->![bas lcap launch add sample data](BAS-LCAP-Add-Sample-Data-4-3-.png)

7. In the sample data editor add 3 rows, and update the **DESCRIPTION** and **CONTRACTOR** columns as follows:

    <!-- border -->![bas lcap edit sample data](BAS-LCAP-Add-Sample-Data-4-4-.png)

8. Access the **Home** tab. You can see that the sample data is added to the **Sample Data** tile.

    <!-- border -->![bas lcap sample data](BAS-LCAP-Add-Sample-Data-5-.png)

9. Go back to the **project preview** browser tab, and click either the link of the **Contractors** service or the link of the **`Capex`** service. You can view the sample data you have added.

    <!-- border -->![bas lcap view sample data from service](BAS-LCAP-Add-Sample-Data-6-.png)


---
