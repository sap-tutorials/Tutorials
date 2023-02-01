---
parser: v2
auto_validation: true
primary_tag: software-product>sap-btp--abap-environment
tags: [  tutorial>beginner, programming-tool>abap-development, software-product>sap-business-technology-platform, tutorial>license]
time: 10
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

# Maintain Business Roles and Access Restrictions in SAP BTP, ABAP Environment
<!-- description --> Create business roles and maintain access management in SAP BTP, ABAP environment.

## Prerequisites  
  - You need a SAP BTP, ABAP environment license.

## You will learn
  - How to create business roles
  - How to add business catalogs
  - How to add business users
  - How to maintain access management

## Intro
In this tutorial, wherever `XXX` appears, use a number (e.g. `000`).

---


### Create business role

1. Logon to your ABAP system.

      ![Create authorization field](role.png)

2. Search **Identity and Access Management** and select **Maintain Business Roles**.

    ![Create authorization field](role2.png)

2. Click **New**.

    ![Create authorization field](role3.png)

3. Create your **business role**:
     - Business Role ID: **`BR_ROOM`**
     - Business Role Description: Room

     Click Save.

    ![Create authorization field](role4.png)


### Add business catalog

1. Click **General Role Details** and **Edit**.

      ![Create authorization object](catalog.png)

2. Select **Assigned Business Catalogs** and click **Add**.

    ![Create authorization object](catalog2.png)

3.  Search for **`Z_ROOM_BC_XXX`**, select **Room** and click **Apply** and **OK**.

       ![Create authorization object](catalog3.png)


### Add business user

  1. Select **Assigned Business Users** and click **Add**.

      ![Create Access Control](user.png)

  2. Search for your user name, select it and click **Apply** and **OK**.

      ![Create Access Control](user2.png)


### Maintain access management

  1. Select your user and click **Maintain Restrictions**.
      ![Edit authorization default values](restrictions.png)

  2. Set your write restrictions and click **OK**.

      ![Edit authorization default values](restrictions2x.png)

  3. Go back to general role details and click **Save**.

      ![Edit authorization default values](restrictions3x.png)

Save and activate.


### Test yourself



