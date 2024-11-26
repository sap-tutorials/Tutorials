---
title: Expose Federation Content from SAP S/4HANA
description: Select and expose roles in SAP S/4HANA to make them available with their assigned apps, groups, catalogs, and spaces in SAP Build Work Zone.
auto_validation: true
time: 10
tags: [ tutorial>intermediate, software-product>sap-business-technology-platform, software-product>sap-s-4hana, software-product>sap-fiori, software-product>sap-launchpad-service, software-product>sap-build-work-zone--standard-edition, software-product>sap-build-work-zone--advanced-edition]
primary_tag: software-product>sap-build-work-zone--standard-edition
parser: v2
---

## Prerequisites
 - You have access to an SAP S/4HANA system with SAP Fiori Front-end Server 2020 or higher that was configured for connectivity with your SAP BTP trial account. The screenshots in this tutorial were taken from an SAP S/4HANA 2023 FPS01 system, but differences in older systems are also explained. 


## You will learn
  - Create a federation scope
  - Expose the scope for consumption by SAP Build Work Zone

---

### Login to your SAP S/4HANA system

Login to your SAP S/4HANA system with SAP Logon. 

![Login to SAP S/4HANA](1-s4-login.png)



### Select SAP Fiori Content for Exposure

1. Enter transaction code ``/n/ui2/cdm3_exp_scope``.

    ![Enter transaction](2a-s4-enter-transaction.png)

2. In the **Launchpad Content Exposure** screen, click the  **Multiple Selection** icon to open role selection.
    ![Select roles](3c-s42023fps01.png)



    > **Note**: When using an older SAP S/4HANA system, the screen will look a bit different, as the option to select between exposure version 1 and version 2 only became available with SAP S/4HANA 2023 FPS01. In this tutorial, simply keep version 1. To learn more about the exposure versions, please check the [documentation](https://help.sap.com/docs/build-work-zone-standard-edition/sap-build-work-zone-standard-edition/federation-of-remote-content-providers).


3. Enter the two roles ``SAP_BR_AP_ACCOUNTANT`` and ``SAP_BR_MASTER_SPECIALIST_FIN`` either by typing them in or using the value help.

2. Then click the **Copy** icon.

    ![Multi select screen](4b-s4-select-roles.png)

3. Click  **Save Configuration** to keep the roles for future exposure.

    ![Save selected roles](5a-s4-save-roles.png)



### Expose Selected Roles

Click **Expose Content** to start exposing the selected roles and their assigned content like apps, catalogs, spaces and pages.

![Expose](6a-s4-expose.png)

The exposure process starts. In the status message you can see the progress. Preparation of the content usually takes 1 to 2 minutes.

![Status Reading catalogs](7b-s4-status.png)




### Check exposure log

Once the exposure process is finished, the exposure log is displayed. In the status bar, you also see a success message.

![Exposure log](8b-s4-success.png)

   Scroll down to the bottom of the log.

Now you are done with exposing the content and can go to SAP Build Work Zone to consume the exposed content.





---
