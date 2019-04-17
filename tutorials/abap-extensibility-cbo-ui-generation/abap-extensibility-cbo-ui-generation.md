---
title: Create the UI for a Custom Business Object
description: Create an own application based on a Custom Business Object and a Custom Catalog Extension
primary_tag: topic>abap-extensibility
tags: [  tutorial>beginner, topic>abap-extensibility, topic>cloud, products>sap-s-4hana ]
auto_validation: true
time: 15
---

## Prerequisites  
- **Authorizations:** Your user needs a business role with business catalog **Extensibility** (ID: `SAP_CORE_BC_EXT`) in your **S/4HANA Cloud** system

## Details

### You will learn
In the preceding tutorial you created a custom business object with a simple data structure and its persistence.
To be able to work with a business object you need a user interface. In this tutorial you will
<ul>
  <li>generate an User Interface (UI)
  <li>expose that UI as an application with the use of Custom Catalog Extensions
</ul>
At the end you will have a running application that you can create, update and delete custom business object entities with.

>**UI Generation** and later done **UI Adaptation** are so called **In-App Extensibility** features done completely inside a S/4HANA system. They are key user functionality with limited possibilities.
>
>Alternatively the more powerful **Side-By-Side Extensibility** via **SAP Cloud Platform** could be done to develop an UI with all SAPUI5 options (see tutorial group [Create Custom UI for S/4HANA Cloud](https://developers.sap.com/group.abap-custom-ui-with-webide.html)).

**Our Example**

A several tutorials spanning example will show extensibility along custom Bonus Management applications.

In the first parts a Manager wants to define business objects "Bonus Plan" for employees. A Bonus Plan is there to save employee specific rules for bonus entitlement.

### Additional Information
- **SAP S/4HANA Cloud Release** (tutorial's last update): 1808

---
[ACCORDION-BEGIN [Step 1: ](Start Editing Custom Business Object Bonus Plan)]

 **Start** the Custom Business Object's application by clicking its tile

![Custom Business Objects application tile](tile_CBO.png)

**Search** for Custom Business Object "Bonus Plan" (1+2) and **Open** its details by clicking its list item in the search result list (3).

![Open Custom Business Object from list](CBO_openFromList_decorated.png)

To be able to do changes to the lastly published version of the business object you need to start edit mode by executing the **Edit Draft** action.

![Press Edit Draft](CBO_editDraft.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Generate UI)]

**Check** the two boxes for UI Generation and Service Generation.

![Check UI and Service Generation](CBO_checkUiAndServiceGeneration.png)

**Publish** the business object to trigger the generation of UI (Master and Detail) and OData Service.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Expose UI as Application)]

Now you make the UI available as SAP Fiori Launchpad application by assigning it to a Business Catalog which corresponds to a group in Launchpad.
From the Business Object's overview go to Custom Catalog Extension application by clicking the **Maintain Catalogs** action.

![Maintain Custom Catalog Extension](CBO_maintainCCE.png)

A new window will open.

Start adding a catalog with the **Add** action.

![Add new Custom Catalog Extension](CCE_add.png)

In the opening value help narrow down the result list by searching for `Extensibility`, select the Catalog with role ID `SAP_CORE_BC_EXT` and press **OK**.

![Value Help for adding Custom Catalog Extension](CCE_addValueHelp.png)

>You could also choose another Catalog, but be aware that your user must have a Business Role containing the same to be able to view the created application.

**Select** the just added Catalog and **Publish** it.

![Publishing Custom Catalog Extension](CCE_publish.png)

This step takes some minutes, the screen refreshes automatically and once the status switches from unpublished to published, you can close this application's window and proceed.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Open Bonus Plan application)]

Back in the Custom Business Object application's window, go to the SAP Fiori Launchpad via **Home** action.

![Fiori Launchpad Home](LaunchpadHomeButton.png)

**Refresh** the Browser window with key **`F5`** for that the catalog extension becomes visible.
Now there is the Bonus Plan application's tile in the **Extensibility** group.

**Start** the application by clicking its tile.

![Bonus Plans application tile](tile_BonusPlans.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test Bonus Plan application)]

**Open** the Bonus Plan application.

**Create** an object.

![Creating a Bonus Plan](UI_Test_createBonusPlan.png)

**Enter** following data

| Field | Value |
| :------------- | :--------------------------- |
| ID | 1 |
| Validity Start Date | 01/01/2017 |
| Validity End Date | 31/12/2017 |
| Target Amount | 1000.00 EUR |
| Low Bonus Assignment Factor | 1 |
| High Bonus Assignment Factor | 3 |
| Employee ID | `<any>` |

Employee ID <any> shall be the one of a sales person that created sales orders with a Net Amount of more than 3000.00 EUR in 2017 and that are completed.

**Save** the Bonus Plan. The UI will automatically return from Bonus Plan Detail to Master, where you can see one entry in the list of bonus plans now.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
---
