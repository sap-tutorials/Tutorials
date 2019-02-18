---
title: Make ABAP Custom Business Object Service Consumable via OAuth
description: Make an ABAP custom business object's service consumable on SAP Cloud Platform via OAuth. Enhancing the platform subaccount's destination to the services S/4HANA system.
primary_tag: topic>abap-development
tags: [  tutorial>beginner, topic>abap-development, topic>abap-extensibility ]
time: 15
---

## Prerequisites  
- **Tutorials:** [Expose custom business object as external web service](abap-custom-ui-business-object)
- **Authorizations:** The assignment of the business catalog **`SAP_CORE_BC_COM`**, **`SAP_CORE_BC_EXT`** to your user represents the prerequisites.


## Details
### You will learn
- How to add a service scope to your destination
- How to create a UI project

This tutorial describes how to make a custom business object's service consumable on SAP Cloud Platform in case of OAuth authentication. This will be done by enhancing the Platform subaccount's destination to the services S/4HANA system. The service will be added to the scope of the destination. At the end you cannot only choose the service in SAP Web IDE, but also retrieve its data.


---


[ACCORDION-BEGIN [Step 1: ](Search SAP Web IDE)]
Go to your SAP Cloud Platform subaccount and click on **Services**, search for **SAP Web IDE** and select it on your SAP Cloud Platform account.

![Open SAP Web IDE](webide1.png)

Click **Go to Service** to open SAP Web IDE.

![View service catalog](sapcp.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Select new project)]
Select **New Project from Template** to generate one.

![Select new project](webide.png)

If you don't see this welcome page then follow the alternative way:
File -> New -> Project from Template


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create UI project)]
Choose the **List Report Application** to create a new UI.

![Create UI project](next.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Enter basic information)]
Enter **`Bonusplan`** as project name and title.

![Enter basic information](bonusplan.png)

Click **Next**.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Explore business object's service in SAP Web IDE)]
Search for **`YY1_BONUSPLAN_CDS`** and open the list. Now you can see following:

![Explore business object's service in SAP Web IDE](list2.png)

Without `YY1_BONUSPLAN_CDS_0001` as parameter in your scope you wouldn't be able to select `YY1_BONUSPLAN_CDS_0001` as service to create your UI project. All other services still cannot be selected.  

[ACCORDION-END]
