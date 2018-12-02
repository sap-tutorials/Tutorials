---
title:  Add custom UI application tile to SAP Fiori launchpad
description:  Extend a business catalog with custom UI application.
primary_tag: topic>abap-development
tags: [  tutorial>beginner, topic>abap-development, topic>abap-extensibility ]
---

## Prerequisites  
- **Tutorials:** [`Create custom UI for S/4HANA on SAP Cloud Platform`](https://developers.sap.com/tutorials/abap-custom-ui-tile.html)


## Details
### You will learn
- How to make the custom UI visible as an application in SAP Fiori launchpad

The creation of a new tile will be explained with this tutorial. The adding of business catalogs to the catalog extension will be shown.

### Time to Complete
**15 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Open custom catalog extensions)]
Open **Custom Catalog Extensions** application on your SAP Fiori launchpad (SAP S/4HANA Cloud system).

![Open custom catalog extensions](catalog.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Select application)]
Search your application and select it.

![Select application](select.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add business catalog)]
Add the following business catalogs to your catalog extension:

  - `SAP_CORE_BC_SL_EXP`
  - `SAP_CORE_BC_SL_IMP`

![Add business catalog](add2.png)
Check both business catalogs and publish them.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Check tile)]
Press **F5** to refresh your page and your tile is created.

![Check tile](tile.png)

[ACCORDION-END]
