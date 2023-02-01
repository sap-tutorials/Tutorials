---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product>sap-fiori, software-product>sap-hana-cloud, software-product>sap-btp--cloud-foundry-environment, topic>mobile]
primary_tag: software-product>sap-business-application-studio
author_name: Raz Korn
author_profile: https://github.com/raz-korn
---

# Create a Data Model and Expose It as a Service
<!-- description --> Create a capital expenditures data model and expose it via a service, using the low-code capabilities of SAP Business Application Studio.

## Prerequisites
 - You have access to SAP Business Application Studio and created a Low-Code-Based Full-Stack Cloud Application dev space, as described in [Set Up SAP Business Application Studio for Low-Code Development in the Trial Environment](appstudio-lcap-onboarding-trial).

## You will learn
- How to create a project from scratch
- How to create a data model for your application
- How to expose the data using a service so that you'll be able to use it in your apps
- How to add sample data
- How to preview the service definition
- How to view the data before creating apps

---

### Create a project

1. Enter the **Dev Space** you created in [Set Up SAP Business Application Studio for Low-Code Development in the Trial Environment](appstudio-lcap-onboarding-trial).

2. If no project exists in a dev space the **Create Project** dialog box opens. Enter the **Project Name**, for example, `CapitalExpenditures`, and click **Create**.

    <!-- border -->![Create Project 1](BAS-Create-Project-2-.png)

    If a project already exists in your dev space, you can create an additional project by selecting **Project -> Create** in the Home tab.

    <!-- border -->![Create Project 2](BAS-Create-Project-1-.png)

3. Wait for the homepage of SAP Business Application Studio for low-code development to load with the new project.

    >Wait for the "We have finished installing all tools and templates for you, enjoy your work!" notification.

    >Loading SAP Business Application Studio for low-code development may take some time, especially if it's the first time in a day.

    <!-- border -->![bas lcap loaded](BAS-Create-Project-4-.png)

    >Close the notification icon at the bottom of the screen (optional).


### Model your data - Entities


1. From the homepage, click the **+** of the **Data Models** tile to add a data model to your project.

    <!-- border -->![bas lcap open entity editor](BAS-LCAP-Data-Model-1-.png)

2. Provide an **Entity Name** for the entity, for example, **`Capex`**.

    <!-- border -->![bas lcap create entity](BAS-LCAP-Data-Model-2-.png)

3. Click the **+** to add a property to the **`Capex`** entity.

    <!-- border -->![bas lcap create entity](BAS-LCAP-Data-Model-3-.png)

4. Select the following:

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Property Name | **description** |
    | B | Property Type | **String** (default) |
    | C | Key | Leave unchecked (default) |
    | D | Null | **Yes** (default) |
    | E | Arrayed | Leave unchecked (default) |
    | F | Max Length | Leave empty |
    | G | Default Value | Leave empty (default) |

    <!-- border -->![bas lcap new property editor](BAS-LCAP-Data-Model-4-.png)

5. Add the following property (click **+**). When done with adding the properties choose **Create**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Property Name | **`total_cost`** |
    | B | Property Type | **Integer** |
    | C | Key | Leave unchecked (default) |
    | D | Null | **Yes** (default) |
    | E | Arrayed | Leave unchecked (default) |
    | F | Default Value | Leave empty (default) |

    <!-- border -->![bas lcap new property editor](BAS-LCAP-Data-Model-5-.png)

    The **`Capex`** entity appears in the **Data Model Editor** tab with a default **ID** property.

    <!-- border -->![bas lcap new entity](BAS-LCAP-Data-Model-6-.png)

6. To add another data model to your project click the `Add Entity` button in the **Data Model Editor** tab.

    <!-- border -->![bas lcap add entity](BAS-LCAP-Data-Model-8-1.png)

    A new entity appears in the **Data Model Editor** tab.    

    To edit the entity, click the entity's header and choose the **Edit Entity** icon.

    <!-- border -->![bas lcap open entity editor](BAS-LCAP-Data-Model-8-.png)

7. Provide an **Entity Name** for the entity, for example, **Contractors**.

    <!-- border -->![bas lcap entity name](BAS-LCAP-Data-Model-9-.png)

8. Change the **Property Name** of the **ID** property to **contractor**, and its **Property Type** to **Integer**.

    <!-- border -->![bas lcap edit property](BAS-LCAP-Data-Model-10-.png)

9. Add the following property to the **Contractors** entity, and choose **Update**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Property Name | **name** |
    | B | Property Type | **String** (default) |
    | C | Key | Leave unchecked (default) |
    | D | Null | **Yes** (default) |
    | E | Arrayed | Leave unchecked (default) |
    | F |Max Length | Leave empty |
    | G | Default Value | Leave empty (default) |

    <!-- border -->![bas lcap add property](BAS-LCAP-Data-Model-11-.png)

    The **Contractors** entity appears in the **Data Model Editor**.



### Model your data - Relationship between entities


1. With relationships you can connect your entities. To do so, select the **`Capex`** entity, choose the **Add Relationship** icon, and select the **Contractors** entity to add a relationship to the **Contractors** entity.

    <!-- border -->![bas lcap add relationship](BAS-LCAP-Data-Model-14-.png)    

    <!-- border -->![bas lcap add relationship](BAS-LCAP-Data-Model-14-2-.png)

2. Select the following, and choose **Create**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Relationship Type| **Association** (default) |
    | B | Multiplicity | **To-One** (default) |
    | C | Key Property | **No** (default) |
    | D | Property Name | **contractor** |
    | E | Target Entity | **CapitalExpenditures.Contractors** (default) |
    | F | Backlink Property | Leave empty (default) |

    <!-- border -->![bas lcap add relationship](BAS-LCAP-Data-Model-15-.png)

3. You can re-arrange the entities on the canvas by clicking the **Auto Layout** icon to improve the data model visualization. In addition, you can also manually drag the entities over the canvas.

    <!-- border -->![bas lcap add relationship](BAS-LCAP-Data-Model-16-.png)



### Expose your data


1. Access the **Home** tab.

    <!-- border -->![bas lcap return from data model editor to home tab](BAS-LCAP-Service-1-.png)

2. Click the **+** of the **Services** tile to add a service through which data from the data model can be accessed.

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

4. To add another new service to your project click the `Add Entity` button in the **Service Editor** tab, define the following, and choose **Create**.

    | Step | Parameter | Value |
    |:-----|:----------|:------|
    | A | Name | **Contractors** |
    | B | Namespace | **`CapitalExpendituresService`** (default) |
    | C | Type | **`CapitalExpenditures.Contractors`** |
    | D | &lt;all properties> | Leave checked (default) |
    | E | contractor | Leave checked (default) |
    | F | name | Leave checked (default) |

    <!-- border -->![bas lcap add service](BAS-LCAP-Service-5-1.png)

    <!-- border -->![bas lcap edit service](BAS-LCAP-Service-5-.png)

    The **Contractors** service is added to the **Service Editor**, with association to the **`Capex`** service.

    <!-- border -->![bas lcap service editor](BAS-LCAP-Service-6-.png)

5. Select the **`Capex`** service (which gets highlighted with a blue rectangle indication), and check the **Draft Editing** property.

    >A draft is an interim version of a business entity that has not yet been explicitly saved as an active version. This allows users to pause, for example when being interrupted, and continue later on. For more information visit [SAP Fiori Design Guidelines - Draft Handling](https://experience.sap.com/fiori-design-web/draft-handling/).

    <!-- border -->![bas lcap draft editing](BAS-LCAP-Service-7-.png)

6. You can re-arrange the entities on the canvas by clicking the **Auto Layout** icon to improve the service visualization. In addition, you can also manually drag the entities over the canvas.

    <!-- border -->![bas lcap service editor auto layout](BAS-LCAP-Service-8-.png)

7. Access the **Home** tab. You can see that both the services are added to the **Services** tile.

    <!-- border -->![bas lcap service editor auto layout](BAS-LCAP-Service-9-.png)



### Preview your data service

It is a common practice in software development to test run any change to an app as soon as possible. Prior to having the services defined, you were not able to test the app. Since now you have defined the services, you can test them.

1. Choose the **Preview** option from the homepage, and select **Run CapitalExpenditures-1**. 

    >**Troubleshooting**: `CapitalExpenditures` is the name you have previously chosen for your project. If you have chosen a different name, this name will be displayed, for example, Run DifferentName-1. 
  
    <!-- border -->![bas lcap launch preview](BAS-LCAP-Preview-Service-1-.png)

2. The project preview page appears in a new tab with the links which allow you to test the service definition, access data exposed by the service, and more.

    <!-- border -->![bas lcap launch preview](BAS-LCAP-Preview-Service-3-.png)

3. In the preview page on the right side under **Services** choose the **metadata** link to view the service's XML. Close the XML file.

    <!-- border -->![bas lcap launch preview](BAS-LCAP-Preview-Service-4-.png)

4. Also in the preview page on the right side under **Services** select the button **view as code** next to **`Capex`** or **Contractors**  to view the data from the respective service.

    <!-- border -->![bas lcap launch preview](BAS-LCAP-Preview-Service-5-.png)

    >Here, the **value** appears as an empty list since the data model is empty.


### Add sample data and preview it

To test that the service works well, that is, returns data when it is available in the data model, let's add sample data to your project. Sample data is a powerful tool as it allows you to (1) develop an app with no connectivity to a data source; (2) Debug your app.

1. Go back to the browser tab with SAP Business Application Studio for low-code development, and click the **+** of the **Sample Data** tile.

    <!-- border -->![bas lcap launch add sample data](BAS-LCAP-Add-Sample-Data-1-.png)

2. Select **Contractors**, select **Create**, and press **Add**.

    <!-- border -->![bas lcap launch add sample data](BAS-LCAP-Add-Sample-Data-2-.png)

3. In the sample data editor add 4 rows.

    <!-- border -->![bas lcap edit sample data](BAS-LCAP-Add-Sample-Data-3-.png)

4. Update the sample data as follows:

    <!-- border -->![bas lcap edit sample data](BAS-LCAP-Add-Sample-Data-4-.png)

5. Access the **Home** tab, and click the **+** of the **Sample Data** tile.

    <!-- border -->![bas lcap edit sample data](BAS-LCAP-Add-Sample-Data-4-2-.png)

6. Select **`Capex`**, select **Create**, and press **Add**.

    <!-- border -->![bas lcap launch add sample data](BAS-LCAP-Add-Sample-Data-4-3-.png)

7. In the sample data editor add 3 rows, and update the **DESCRIPTION** and **CONTRACTOR** columns as follows:

    <!-- border -->![bas lcap edit sample data](BAS-LCAP-Add-Sample-Data-4-4-.png)

8. Access the **Home** tab. You can see that the sample data is added to the **Sample Data** tile.

    <!-- border -->![bas lcap sample data](BAS-LCAP-Add-Sample-Data-5-.png)

9. Go back to the **project preview** browser tab, and select the button **view as code** next to **`Capex`** or **Contractors**. You can view the sample data you have added.

    <!-- border -->![bas lcap view sample data from service](BAS-LCAP-Add-Sample-Data-6-.png)


---
