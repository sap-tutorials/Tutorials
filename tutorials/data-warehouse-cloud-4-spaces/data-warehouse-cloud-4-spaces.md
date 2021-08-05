---
title: Create a Space in SAP Data Warehouse Cloud
description: A Space is like a virtual team or office space. These virtual workspaces are designed for an individual or a group of users to perform their data modeling and data integration. Spaces are totally isolated from each other and can be assigned quotas for available disk space, CPU usage, runtime hours, and memory usage.
auto_validation: true
time: 5
tags: [ tutorial>beginner, products>sap-data-warehouse-cloud]
primary_tag: products>sap-data-warehouse-cloud
---

## Prerequisites
 - You have [familiarised yourself with the SAP Data Warehouse Cloud interface.](data-warehouse-cloud-2-interface)

## Details
### You will learn
  - How to create a Space
  - How to manage and monitor your Space

  This tutorial is part of a mission, in which you try to help Best Run Bikes to to get a holistic view of their sales data by using the power of SAP Data Warehouse Cloud. You will get the sales data of Best Run Bikes and it is your mission to use the features of SAP Data Warehouse Cloud to help the bike suppliers make the best possible business decisions.

  This mission consists of 8 modules that contain the necessary steps you need to follow in your mission to help Best Run Bikes:

  1. [Sign up for trial.](data-warehouse-cloud-1-begin-trial)
  2. [Get to know the SAP Data Warehouse Cloud interface](data-warehouse-cloud-2-interface)
  3. [Add users and assign roles](data-warehouse-cloud-3-add-users)
  4. **You are here ->** [Create your Space](data-warehouse-cloud-4-spaces)
  5. [Import your datasets](data-warehouse-cloud-5-import-dataset)
  6. [Create an entity relationship model](data-warehouse-cloud-6-entityrelationship-model)
  7. [Create a graphical view model](data-warehouse-cloud-7-graphicalview)
  8. [Define measures, business semantics and preview your data](data-warehouse-cloud-8-define-measures)

---

[ACCORDION-BEGIN [Step 1: ](Create a Space)]

1.	To create a space, click on the space management icon on the bottom left, and click on the plus symbol on the top right.

    ![Create Space](T04-Picture1.png)

2.	Enter a name for your space. The Space ID will auto populate. In this example, let's call our space Best Run Bikes.

    ![Space Name](T04-Picture2.png)

3.	Click on **Create**, and you've successfully created your Space in SAP Data Warehouse Cloud.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Understand your Space)]

In the **Space Management** screen, each Space is represented by a tile. The Spaces tiles can show a red, green, or blue colour table.

•	Green spaces are performing optimally.

•	Red spaces are overactive and need more resources allocated to perform optimally.

•	Blue spaces are under active.

Some resources that aren't being used by a blue space could be reallocated to a red space, for example. You also have the option to monitor a space and see detailed information about disk use and in memory usage. This is done by clicking on the three dots next to your space name.

!![Space Overview](T04-Picture3.png)

To see an overview of your space settings, simply select the Space, and this takes you to the overview, where you can monitor your space and see a more detailed view of the same. You can fine tune your disk-space and memory allocations as per your requirements if you wish. For this example, use the minimum configuration.

![Space Details](T04-Picture4.png)

Now that you've created your Space, you can now move ahead and assign users to your Space.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add users into your Space)]

1.	Once in your Space, click on the Members tab. You can also alternatively scroll down to the members heading.
2.	Click on **Add** to allocate a member from your user list.

    ![Add Users](T04-Picture5.png)

3.	You can now select the users you'd like to add to your space by checking them off. When you're happy with your choices, click on **Add** to confirm. Don't forget to add yourself as a user!

![Select User](T04-Picture6.png)

You have successfully added your team members as users into your Space.

[VALIDATE_1]
[ACCORDION-END]

---
