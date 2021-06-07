---
title: Map Fields Between Open Connectors and HubSpot
description: Use Common Resources templates to map fields from multiple sources to a single field in Open Connectors.
auto_validation: true
time: 20
tags: [ tutorial>intermediate, products>sap-business-technology-platform]
primary_tag: products>sap-integration-suite
---

## Details
### You will learn
  - How to clone and consume common resource templates

---

[ACCORDION-BEGIN [Step 1: ](Navigate to Common Resources)]
1. Navigate to  Open Connectors home or landing page.

    ![Home Page](01-open-connectors-home.png)

2. Click on the **Common Resources** tab and then select the **Common Resource Templates** tab to view and explore all the pre-shipped common resource templates.

    ![Click Common Resources](02-click-common-resources.png)

    >The pre-shipped Common Resource Templates are displayed in this tab.

3. Select the `basic-companies` tile.

    >The **My Resources** tab shows details about the exposed fields, field types, description, and list of mapped connectors for the selected templates.

    ![Basic Companies](03-basic-companies.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Clone Common Resources)]

1. Select the **Mappings** tab to view details about the mapped connectors of the selected common resource template.

    ![Common Resource Overview](04-cr-overview.png)

2. Select **Clone** to copy or clone the selected common resource template to your Open Connectors tenant.

    ![Clone Common Resource](05-cr-clone.png)

3. In the clone wizard, select the checkbox next to **Connector Name** to select all the connectors and select **Next**.

    ![Clone Connectors](06-cr-clone-connectors.png  )

4. Select the **Level > Account** and click **Next**.

    ![Common Resource Account](06-CR-account.png)

5. Provide a unique name for the cloned common resource and click **Save**.

    ![Common Resource Save](07-cr-save.png)

    >After the common resource is cloned, you would see the common resource cloned to your  Open Connectors tenant and it would show all the mapped connector instances that are present in your tenant.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Test Common Resources template)]

1. Select the connector instance for HubSpot that you have in your  Open Connectors tenant.

    ![HubSpot Instance](08-cr-hubspot-instance.png)

2. The mapping of the basic-companies to the selected non-SAP connector can be tested from Open Connectors.

    - Select the non-SAP CRM connector instance, say `myhubspotdemo`.

    - Select the **Play** button to test the common resources.

    ![Try Out](09-tryout-cr.png)

3. Click **Send** to test the Common Resource template.

    - Both the transformed response and raw response can be viewed in the test pane.

    ![Mapped Response](10-mapping-cr.png)

    ![Raw Response](10-raw-cr-response.png)

[DONE]
[ACCORDION-END]

---
