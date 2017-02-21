---
title: Creating simple Fiori launchpad tiles based on an CDS OData Service
description: Creating a tile within Fiori Launchpad based on an CDS OData Service
tags: [ tutorial>intermediate, products>sap-s-4hana ]
---
## Prerequisites  
 - **Proficiency:** Intermediate
 - **Tutorials:** [Creating OData based on CDS Views. ](http://sap.com/developer/tutorials/s4hana-cds-creating-odata-service.html)

## Next Steps
 [Building simple tile](http://sap.com/developer/tutorials/s4hana-cds-building-simple-tile.html)

## Details
### You will learn  
- How to create a tile within Fiori Launchpad

### Time to Complete
**15 Min**.

---

1. The first thing you have to do is to access the Fiori Launchpad Designer by opening the welcome page that came with S/4HANA trial landscape.
    ![Launchpad1 ](Lauchnpad1png.png)

2. From the above welcome page, click on the link (1) to access the Login screen of the Fiori Launchpad Designer.
    ![Launchpad2](Lauchnpad2.png)
Start the Fiori Launchpad Logon and logon to the preconfigured system with:
 (1) User:     `S4H`
 (2) Password: `Welcome1`
 (3) Client:   `100`
and afterwards(4) click on Log On button.

3. Now you have to append the `url` to the Fiori Launchpad Designer to SAP Launchpad `url`. The part that needs to be appended to the `url` is this:
`/sap/bc/ui5_ui5/sap/arsrvc_upb_admn/main.html`

    ![Launchpad3](Lauchnpad3.png)

Afterwards hit the Enter taste from your keyboard.
We are now able to see lots of tiles because new profiles were already assigned to the user S4H.

    ![Launchpad4](LauchpadDesigner1.png)    

4. You can open the customization scope by appending  the following to the `url` :
     `?scope=CUST`


    ![Launchpad5](LauchpadDesignerCUSTscope.png)

5. Another scope is called Configuration. It is for system specific settings. It is accessible by appending the following to the `url`: `?scope=CONF`

    ![Launchpad6](LauchpadDesignerCONFscope.png)

6. Now let's assign some roles to the Business user. To achieve this you have to login in the SAP GUI (User: `SH4`, Client: `100`, Password:`Wecome1`)

    ![Launchpad7](LauchpadDesignerRoleAssignt1.png)
After successfully login , enter the ABAP transaction `su01` (1) for user maintenance and hit enter from the keyboard.

    ![Launchpad8](LauchpadDesignerRoleAssignt2.png)

     While in the transaction `su01` maintain the field User with `SH4` (1)
     Now click on the change button (2) to maintain fields and add the roles `SAP_BR_PURCHASER` and `SAP_BR_PURCHASING_MANAGER` in the tab Roles:

     6.1. Tab Roles(1)
      ![Launchpad9](LauchpadDesignerRoleAssignt4.png)

     6.2. Status Roles
      ![Launchpad10](LauchpadDesignerRoleAssignt5.png)

     6.3  Press the F4 help
      ![Launchpad11](LauchpadDesignerRoleAssignt6.png)

     6.4 Role Name window
     a) Here switch to the tab Roles (1) and maintain the field Single Role

      ![Launchpad12](LauchpadDesignerRoleAssignt7.png)    
     b) Maintain the field single role with `SAP_BR*` (1) and click on the OK button (2)   

      ![Launchpad13](LauchpadDesignerRoleAssignt8.png)

     c) On the Tab Single Roles of the window Roles Name, check the lines (1) :
        `SAP_BR_PURCHASER`
        `SAP_BR_PURCHASING_MANAGER`
      and click on the OK button (2)
      ![Launchpad14](LauchpadDesignerRoleAssignt10.png)
  You should have something like this:
  ![Launchpad15](LauchpadDesignerRoleAssignt11.png)
  7. Add additional role following the steps described above: `SAP_BR_ANALYTICS_SPECIALIST`
      ![Launchpad16](LauchpadDesignerRoleAssignt13.png)
  <br>
  8. Add additional role following the the steps described above : `SAP_BR_EMPLOYEE`
      ![Launchpad17](LauchpadDesignerRoleAssignt14.png)
      ![Launchpad18](LauchpadDesignerRoleAssignt15.png)
<br>
  9. Add additional role following the the steps described above : `ZSAP_UI2_ADMIN_700`
      ![Launchpad19](LauchpadDesignerRoleAssignt16.png)
      ![Launchpad20](LauchpadDesignerRoleAssignt17.png)
<br>
  10. Add additional role following the the steps described above: `ZSAP_UI2_USER_700`
      ![Launchpad21](LauchpadDesignerRoleAssignt18.png)                                                                            
      ![Launchpad22](LauchpadDesignerRoleAssignt19.png)
<br>  
  11. Add additional role following the steps described above : `Z_RDS_BR`
      ![Launchpad23](LauchpadDesignerRoleAssignt20.png)
      ![Launchpad24](LauchpadDesignerRoleAssignt21.png)
  <br>
  12. Add additional role following the steps described above: `Z_RT_ADMIN`
      ![Launchpad25](LauchpadDesignerRoleAssignt22.png)  
      ![Launchpad26](LauchpadDesignerRoleAssignt23.png)  
      Save the changes you made and close the SAP Logon.
<br>
  13. Go back to Fiori Launchpad  Designer restart and logon to it by clicking on the link (1) to access the Login screen of the Fiori Launchpad Designer
      ![Launchpad27](Lauchnpad1png.png)
    While in the Fiori Launchpad Designer you would be able to see several groups related to purchasing Business Roles as well as some groups for creating KPI.
    You will also see the group `Z_RDS_BCG`.
        ![Launchpad28](LauchpadDesignerRoleAssigntFinal.png)


## Next Steps
 - [Building simple tile](http://go.sap.com/developer/tutorials/s4hana-cds-building-simple-tile.html)
