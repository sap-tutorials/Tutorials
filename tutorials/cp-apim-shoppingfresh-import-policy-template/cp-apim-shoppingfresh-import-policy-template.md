---
title: Import Policies from a Template into the API Proxy
description: Import policies from a template instead of handcrafting the entire policies one after the other into the API Proxy.
auto_validation: true
time: 15
tags: [ tutorial>beginner, products>sap-api-management, products>sap-cloud-platform]
primary_tag: products>sap-api-management
---

## Details
### You will learn
  - How to discover policy template in SAP API Business Hub.
  - How to import policies from template into the API Proxy.

---

[ACCORDION-BEGIN [Step 1: ](Discover policy template)]

1. Navigate to **Discover** from API Portal and select **ALL**.

    ![Discover Policy Template](01-discover-polic-template.png)

2. Search for `API Content - Recipes`.

    ![Search Template](02-search-template.png)

3. Click it and navigate to **Artifacts** tab.

    ![Navigate Artifacts](03-Nav-artifacts.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Copy policy template)]

1. From the **Actions** drop down select **Copy**.

    ![Copy Template](04-copy-template.png)

2. Navigate to **Develop** and select **Policy Templates** tab.

    ![Check Template](05-template-successfully-copied.png)

    >The copied policy template should be available

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Apply policy template)]
1. Navigate to **APIs** tab.

    ![Navigate APIs](06-Nav-APIs.png)

2. Go back to `SalesTrackerAPI` details screen and click **Policies**.

![Navigate Policy Editor](07-policy-editor.png)

3. In the Policy editor screen, click **Edit**.
    - Go to the **Policy Template** button and click **Apply**.

    ![Apply Template](08-policy-template.png)  
    ![Apply Template_2](08_2_Apply.png)

5. Click **Update**, to update the policy template.

    - Click **Save** to save the policy editor.

    ![Update Editor](09-update.png)

[VALIDATE_3]

[ACCORDION-END]


---
