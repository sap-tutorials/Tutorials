---
parser: v2
auto_validation: true
primary_tag: products>sap-btp--abap-environment
tags: [  tutorial>beginner, programming-tool>abap-development,  products>sap-business-technology-platform, tutorial>license ]
time: 10
author_name: Niloofar Flothkoetter
author_profile: https://github.com/niloofar-flothkoetter
---

# Create a Developer User in SAP BTP ABAP Environment
<!-- description --> Create a developer user with the developer role in SAP Business Technology Platform ABAP Environment.

## Prerequisites  
- You must have an administrator user.

## You will learn
- How to create an employee user
- How to assign business roles to an employee user
## Time to Complete
**10 Min**.


---

### Overview


To be connected to your system in ADT, expose an ABAP service and consume this service to create a Fiori Application, you will need to have a Developer User with developer role.

![Overview](Picture21.png)


### Log in to SAP Fiori Launchpad as administrator


If you do not have already an existing ADT project, you need to find the URL to the launchpad in the BTP cockpit. For more information see the [link] (https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/11e765e8af6d476f99ce014b3f02bd64.html). Otherwise you can use the existing project to find out the launchpad link as follows:

 1. Open Eclipse and do right click on your system and navigate to **Properties**.

    ![Open Eclipse](Picture17.png)

 2. Navigate to **ABAP Development** and copy the **System URL**.

    ![System URL](Picture18.png)

 3. Copy this URL in a browser and change the URL like this:

    Add `-web` after `.abap` and `/ui` at the end of URL.
    `https://<your-system>.abap-web.eu10.hana.ondemand.com/ui`

    ![Change URL](Picture20.png)

 4. Login with admin user and password in SAP Fiori Launchpad.

    ![Login to SAP Fiori Launchpad](Picture19.png)


### Navigate to Maintain Employees application

Navigate to **Maintain Employees** application.

![Maintain Employees](Picture2.png)



### Create a new employee

Create a new employee by clicking **New**

![Create a new employee](Picture3.png)


### Enter user data

  1. Enter user data and a valid Email address .
      - Employee ID: `DEVELOPER_XXX`
      - Last Name: `DEVELOPER_XXX`
      - E-Mail: `developer_xxx@example.com`

  2. **Save** your changes.

      ![Enter user data](Picture4.png)


### Create business user

  1. Select the newly created entry in the employee list.

  2. Press **Create Business User**.

      ![Create business user](Picture5.png)


### Add business roles

  1. Press **Add** Business Roles.

      ![Add business Roles](Picture6.png)

  2. Select business role **Developer** and press **OK**.

      ![Add business Roles](Picture7.png)

  3. Save all changes with click on **Save**.

      ![save](Picture8.png)


### Test yourself

In which application on your SAP Fiori launchpad can you create developer user as an administrator?


---
