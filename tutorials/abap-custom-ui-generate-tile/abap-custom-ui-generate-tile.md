---
title:  Add Custom UI Application Tile to SAP Fiori Launchpad
description:  Extend a business catalog with custom UI application.
auto_validation: true
primary_tag: topic>abap-development
tags: [  tutorial>beginner, topic>abap-development, topic>abap-extensibility ]
time: 15
author_name: Ulrike Liebherr
author_profile: https://github.com/Liebherr
---

## Prerequisites
- **Authorizations:** Your user needs a business role with business catalog **Extensibility** (ID: `SAP_CORE_BC_EXT`) in **SAP S/4HANA Cloud**

## Details
### You will learn
- How to make the custom UI visible as an application tile in SAP Fiori launchpad

As application availability in Fiori Launchpad and authorization for it is managed via Business Catalogs, you will see how to extend such a business catalog with your new app.

### Additional Information
- **SAP S/4HANA Cloud Release** (tutorial's last update): 1902

---

[ACCORDION-BEGIN [Step 1: ](Open custom catalog extensions)]
Open **Custom Catalog Extensions** application on your SAP Fiori launchpad (SAP S/4HANA Cloud system).

![Open custom catalog extensions](s4_customCatalogExtension_tile.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Find and select application)]
Search your application and select it.
The ID of your application is the combination of the name that you gave when deploying it from SAP Cloud Platform to S/4HANA Cloud including the prefix `YY1_` plus the suffix `_UI5R`. It is of type `Custom UI App`.  

![Select application](s4_customCatalogExtension_selectApp.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Extend business catalog)]
Press **Add** to start extending the business catalog that the new app shall be part of.

![add a catalog to become extended](s4_customCatalogExtension_add.png)

A pop up opens for catalog selection. Be aware, that users that shall be able to use the app must have a role with the same catalog assigned. As for this tutorial catalog `SAP_CORE_BC_EXT` is the essential prerequisite, we simply select that and press **OK**

![Find and select catalog ](s4_customCatalogExtension_chooseCatalog.png)

The pop up has closed and the catalog extension got an entry in its list of used catalogs. Select that entry and press **Publish**

![Published Catalog Extension for Extensibility catalog](s4_customCatalogExtension_publish.png)

![Published Catalog Extension for Extensibility catalog](s4_customCatalogExtension_published.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Open custom UI App)]
Go to the Fiori Launchpad by pressing the Home button.
Press **F5** to refresh your page and your new tile becomes visible.

Click it to open your new app developed with Web IDE.

![Open Custom App's tile](s4_BonusplanApp_tile.png)

The List Screen will open. As by default a list screen does not show the existing list entries you have to press the **Go** button to get these.

![Load list of Bonus Plans](s4_BonusplanApp_List_pressGoButton.png)

To get into the details screen of a Bonus Plan you can click its whole entry in the list.

![Open Custom App's tile](s4_BonusplanApp_List_pressListEntry.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
