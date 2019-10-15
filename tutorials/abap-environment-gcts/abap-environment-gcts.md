---
auto_validation: true
title: Transport a Software Component Between Two ABAP Instances
description: Create and import a new software component into an SAP Cloud Platform ABAP Environment instance, add ABAP sources and export it to a new SAP Cloud Platform ABAP Environment instance.
primary_tag: products>sap-cloud-platform--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-cloud-platform, tutorial>license ]
time: 15
author_name: Niloofar Naseri
author_profile: https://github.com/niloofar-naseri
---

## Prerequisites  
 - Create a developer user in a SAP Cloud Platform ABAP Environment system.
 - Download Eclipse Photon or Oxygen and install ABAP Development Tools (ADT). See <https://tools.hana.ondemand.com/#abap>.

## Details
### You will learn  
  - How to create and import a new software component into a SAP Cloud Platform ABAP Environment instance
  - How to add ABAP sources and export software component
  - How to import software component into a new SAP Cloud Platform ABAP Environment instance

---


[ACCORDION-BEGIN [Step 1: ](Create software component via SAP Fiori launchpad)]
  1. Login as administrator and open **Manage Software Components** application on your first instance's Fiori Launchpad.

      ![manage SC](SC1.png)

  2. Press **GO** button to see the list of all already available software components.

      ![all SC](SC2.png)

  3. Press **+** button to create a new software component.

      ![add a new SC](SC3.png)

  4. Enter a **Software Component Name** and a **Description** and press **Save**.

      ![enter data](save.png)
     You can use your own namespace.

  5. Select your created software component and press **Import**. Click **OK** on **Import** popup.

     As software component creation creates a repository for it in a distributed version-control system only and not in the development instance, you have to import that before starting real development.

      ![import popup](SC6.png)

  6. Check the popup.

      ![import SC](check.png)

      With selecting **Import History** you can view the current status of the import.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Add ABAP sources and export component)]

  1. Open Eclipse and connect with your developer user to your first ABAP system. Right-click on **Favorite Packages** and select **Add Package**.
      ![add to favorites](favorite.png)

  2. Enter `Z_MY_APP_XXX`, select it and click **OK**.

      ![add to favorites](favorite2.png)

  3. Check your result.  

      ![add to favorites](eclipsenew.png)

  4. Create package:


  4. Release your transport request and its subtasks. Therefore, open view **Transport Organizer**.

      ![release](release1.png)

  5. Release all your subtasks by right click on it and select **Release**.

      ![release tasks](release2.png)

  6. Afterwards release your transport request by right click on it and also select **Release**.

      ![release tasks](release3.png)


      Hint: Your software component root package is a structure package, hence it's not allowed to create objects directly in this package. You need to add an additional development package and then you're able to add your objects.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Update or import software component again)]
  1. Open **Manage Software Components** app on your second instance's Fiori launchpad.

      ![manage SC](SC1.png)

  2. Press **GO** button to load all available software components.

      ![all SC](SC2.png)

  3. Select your software component and press **Import**. This will either update the software component if it is already imported into the second system or import the software component if it is not present in the second system.

     As software component creation creates a repository for it in a distributed version-control system only and not in the development instance, you have to import that before starting real development.
      ![import SC](check.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Check the imported software component)]
  1. Open Eclipse and connect with your developer user to your second ABAP system and add your imported software component to your **Favorite Packages**. Check if all objects are imported.

      ![add to favorites](eclipsenew.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
---
