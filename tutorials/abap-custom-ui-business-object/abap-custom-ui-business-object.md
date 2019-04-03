---
title: Expose Custom Business Object as External Web Service
description: Expose a custom business object as web service for integration of your solution with other systems.
primary_tag: topic>abap-development
tags: [  tutorial>beginner, topic>abap-development, topic>abap-extensibility  ]
time: 15
---

## Prerequisites  
- **Tutorials:** [Enable SAP Web IDE for S/4HANA system via OAuth](abap-custom-ui-communication-arrangement)
- **Authorizations:** The assignment of the business catalog **`SAP_CORE_BC_COM`**, **`SAP_CORE_BC_EXT`** to your user represents the prerequisites.


## Details
### You will learn  
- How to create service for a custom business object
- How to expose UI by the creation of a custom communication scenario
- How to expose UI by the creation of a communication arrangement

This tutorial describes how the service generation can be activated for an existing custom business object. Furthermore the addition of CDS views to custom communication scenario is explained. The creation of communication arrangements for `Bonusplan` scenarios also takes place in this tutorial.


---

[ACCORDION-BEGIN [Step 1: ](Open custom business objects application)]
Navigate to **Custom Business Objects** tile to start the application.

![Open custom business objects application](cbo.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Generate OData service)]
Open details of your custom business object **`Bonusplan`**, create draft and select the **Service Generation** checkbox. Click **Save** and **Publish**. With this step an OData service is created.

![Generate OData service](publish.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Open custom communication scenarios application)]
Open **Custom Communication Scenarios** application.

![Open custom communication scenarios application](scenario.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create communication scenario)]
A communication scenario is the basis definition for a communication between systems. It defines a solution to be made available for external systems. Click **New**, give your scenario a name, description and click **New** again.

![Create communication scenario](new.png)

Enter **`Bonusplan Scenario`** as description and click **New**.
![Click on new](new2.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add inbound service to scenario)]
Add your CDS view **`YY1_BONUSPLAN_CDS`** by clicking **+**. Check and publish it.

![Add inbound service to scenario](publish2.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Open communication arrangements application)]
Open **Communication Arrangements** application.

![Open communication arrangements application](arrangement.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create communication arrangement)]
Create a communication arrangement. This time you create it for your `Bonusplan` scenario.

![Create communication arrangement](create.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Set communication system in arrangement)]
Select **`SCP_DEV_SYSTEM`** as your communication system. Select **`SCP_DEV`** as communication system with **`OAuth 2.0`** as authentication method.

Click **Save**.

![Set communication system in arrangement](save2.png)

[ACCORDION-END]
