---
parser: v2
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-data-warehouse-cloud]
primary_tag: products>sap-data-warehouse-cloud
---

# Add Users and Assign Roles in SAP Data Warehouse Cloud
<!-- description --> Add new users and assign roles to them in SAP Data Warehouse Cloud.

## Prerequisites
 - You have [signed up for the SAP Data Warehouse Cloud trial.](data-warehouse-cloud-1-begin-trial)
 - You have [familiarised yourself with the SAP Data Warehouse Cloud interface.](data-warehouse-cloud-2-interface)


## You will learn
  - How to add users into your SAP Data Warehouse Cloud tenant
  - How to assign roles to users
  - How to edit users in bulk

  You can also follow the steps in this tutorial by watching this video

  <iframe width="560" height="315" src="https://www.youtube.com/embed/7i0rUE5d78s" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

---

### Go to the security page


1.	To add users, first click on the security icon on the bottom left, and click on **Users**.

    ![Users](T03-Picture1.png)

2. This takes you to the **Security** page, where you can see an overview of all the users in your SAP Data Warehouse Cloud tenant.

![UsersPage](T03-Picture2.png)


### Add users


To start adding users, follow these steps:

1. Click on the + icon or just start filling in the blank row along the bottom.

2.	Then enter the User ID. This must be unique, and no two users can have the same ID.

3.	Next, enter the person's first and last name, followed by the display name. This is the name that will be displayed throughout the system.

4.	Now fill in the user's e-mail. This is the e-mail address that notifies the user they have an account in your organisation's data warehouse. Additionally, this will also be the new user's login email ID.

![Add Users](T03-Picture3.png)


### Assign roles


The next step is to assign roles to your users. The following standard roles are available in SAP Data Warehouse Cloud.

•	**System Owner** - Includes all user privileges to allow unrestricted access to all areas of the application. Only one user in the system can be assigned to this role, and it must always be assigned to a user.


•	**DW Administrator** - Can create users and Spaces and has full privileges across the whole of the SAP Data Warehouse Cloud tenant.


•	**DW Space Administrator** - Manages all aspects of their Spaces and can create data access controls and use the Content Network


•	**DW Integrator** - Can create and edit connections, database users, and associate HDI containers in Spaces of which they are a member.


•	**DW Modeler** - Can create and edit objects in the Data Builder and Business Builder in Spaces of which they are a member.


•	**DW Viewer** - Can view objects in Spaces of which they are a member.

You can either click on the two squares icon to see all the available user roles or start typing in the role in order to select it. Once you select all the relevant roles for your user, click on **OK** to confirm.

![User Roles](T03-Picture4.png)

Once you're done, don't forget to click on the save icon on the top right of your screen. You can also delete any users from your warehouse by selecting the user and clicking on the delete icon. Further along, we have a resend email button. This is in case your users missed their access email and need another one. And finally, there is the button to assign a system owner.

![Save](T03-Picture6.png)


> You can only assign one system owner in your SAP Data Warehouse Cloud tenant.



### Edit users in bulk


If you wish to add multiple users at once, you have the option to import from a CSV. Simply click on the **Import Users** icon on the top right and upload your CSV file. It is also possible to export multiple users into a CSV file by clicking on the Export Users icon.

![Bulk Users](T03-Picture5.png)

> **Well done!**

> You have completed this tutorial and now know how to add users and assign them roles in SAP Data Warehouse Cloud.


### Test yourself




---
