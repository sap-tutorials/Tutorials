---
title: Create an application using SAP HANA and the Cloud Application Programming Model
description: Use SAP Web IDE and the Cloud Application Business wizard
auto_validation:
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
time: 25
tags: [ tutorial>beginner, products>sap-hana, products>sap-hana\,-express-edition]
primary_tag: products>sap-hana
---

## Prerequisites
 - This tutorial is designed for SAP HANA on premise and SAP HANA, express edition. It is not designed for SAP HANA Cloud.
 - [Getting started with SAP HANA, XS Advanced Development](group.hana-xsa-get-started)

## Details
### You will learn
  - How to create a simple application based on the SAP Cloud Application Business wizard
  - You will be introduced to basics on cloud-native development and multi-target applications

---

[ACCORDION-BEGIN [Step 1: ](Create a Project)]

Right-click on workspace and choose `Project from Template`

![New project](1.png)

Make sure `Cloud Foundry` is the selected environment. Select **SAP Cloud Platform Business Application** and click **Next**

![New project](2.png)

Call the project `APP` and click **Next**

![New project](3.png)

Mark **Use HTML5 application repository** and click **Next**

![New project](4.png)

Change the database version to **2.0 SPS02**

![New project](5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create database artifacts)]

Expand the `db` folder and double-click on **`data-model.cds`** to expand it

![New project](6.png)

Replace the content in `data-model.cds` with the following:

```text

namespace my.app;

entity Recipes{
	key ID : Integer;
	instructions : String;
	dish : String;
	time : Integer;
	unit : String;
	ingredients : association to many Ingredients on ingredients.recipe = $self;
};


entity Ingredients{
	key ID : Integer;
	recipe : association to Recipes;
	quantity : Double;
	unit : String;
	ingredient : String;
};

```
For example:

![New project](7.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Create services to expose data)]

Navigate into the `srv` folder. Double-click on **`cat-service.cds`**.

![New project](8.png)

Replace the existing content in `cat-service.cds` with the following

```text
using my.app from '../db/data-model';
service CatalogService {

 entity Recipes
	@readonly
	as projection on app.Recipes;

 entity Ingredients
	@readonly
	as projection on app.Ingredients;

}

```

**Save all files**

![New project](9.png)


> ## What is going on?
>
> You can see Core Data and Services generating design time artifacts based on SAP HANA (`.hdbcds`) under a new folder called `db\src\gen`. You can also follow progress by opening the console.
>
>  ![New project](10.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Build the database module)]

The `.cds` file in which you defined the entities for persistence was used to create files with extension `.hdbcds`. These files will be used by the deployment infrastructure to create physical objects, such as tables, in the database.

Right-click on the `db` folder and choose **Build**

![Build database](11.png)

You can follow progress in the Console:

![Build database](build.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Insert data into the ingredients table)]

Now that the database module has been built, you can explore the HDI container and the physical tables.

Right-click on `db` and choose **Open HDI Container**.

![Build database](12.png)

Click on **Tables**. Right-click on `MY_APP_INGREDIENTS` and choose **Generate INSERT statement**.

![Build database](13.png)

**Make a note of the name of the schema and table, as you may need to adjust the code.**

Use the following code to insert values into the table:

```sql
INSERT INTO "APP_HDI_CONTAINER"."MY_APP_INGREDIENTS" VALUES(1,175,'G','Softened Butter',1);
INSERT INTO "APP_HDI_CONTAINER"."MY_APP_INGREDIENTS" VALUES(2,3,'UN','Egg',1);
INSERT INTO "APP_HDI_CONTAINER"."MY_APP_INGREDIENTS" VALUES(4,4,'TBSP','Nutella',1);
INSERT INTO "APP_HDI_CONTAINER"."MY_APP_INGREDIENTS" VALUES(5,250,'G','Nishiki Rice',2);
INSERT INTO "APP_HDI_CONTAINER"."MY_APP_INGREDIENTS" VALUES(6,750,'ML','Water',2);
INSERT INTO "APP_HDI_CONTAINER"."MY_APP_INGREDIENTS" VALUES(8,800,'G','Fresh salmon',2);
INSERT INTO "APP_HDI_CONTAINER"."MY_APP_INGREDIENTS" VALUES(9,50,'G','Macaroni',3);
INSERT INTO "APP_HDI_CONTAINER"."MY_APP_INGREDIENTS" VALUES(10,150,'ML','Water',3);
INSERT INTO "APP_HDI_CONTAINER"."MY_APP_INGREDIENTS" VALUES(11,50,'G','Cheddar cheese',3);

```

> Hint: `Ctrl+H` allows you to replace the schema in the code above if necessary.

Press `F8` or use the **Run** button to insert the records

![Build database](15.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Insert data into the recipes table)]

Repeat the previous steps for the table `MY_APP_RECIPES` and use the following SQL statements to create data:

```sql
INSERT INTO "APP_APP_HDI_CONTAINER"."MY_APP_RECIPES" VALUES(1,'Mix the egg, butter and flour into a cup. Incorporate Nutella. Bake in microwave for 2 minutes','Nutella cupcake',15,'min');
INSERT INTO "APP_APP_HDI_CONTAINER"."MY_APP_RECIPES" VALUES(2,'Lay a piece of nori on sushi mat. Spread rice. Add salmon. Roll','Salmon roll',1,'h');
INSERT INTO "APP_APP_HDI_CONTAINER"."MY_APP_RECIPES" VALUES(3,'Mix the macaroni, water, and salt in a microwaveable mug. Microwave for 2-3 minutes','Mac and Cheese',7,'min');
```
Right-click on `MY_APP_RECIPES` and choose **Generate SELECT statement**.

Change the select clause to count the number of records to complete the validation below

![Build database](17.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Build and run the OData services)]

Go back to the code editor.

Right-click on the `srv` folder and choose **Build**

![Build Java](18.png)

Wait until the process has finished and make sure the build has finished successfully in the console:

![Build Java](19.png)

Right-click on the module and choose **Run as Java Application**

![Run Java](20.png)

Once the application is running, click on the URL for the services.

![Run Java](21.png)

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Add a Web module)]

Right-click on your app and choose **HTML5 module**

![Build web](22.png)

Choose **SAP Fiori `Worklist` Application**. Click **Next**

![Run Java](23.png)

Call the module `web`. Fill in the **Title** and **Namespace** and click **Next**

![Run Java](24.png)

Choose `CatalogService` and click **Next**

![Run Java](25.png)

Complete the data binding as follows and click **Finish**

![Run Java](26.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Run the application)]

Right-click on the web module and choose **Run as Web Application**

![Run Java](27.png)

Choose `index.html`

![Run Java](28.png)

If prompted, enter your credentials:

![Run Java](29.png)

> Note: Your credentials are the same you used to log in to the SAP Cloud Platform trial account

**Congratulations!** You have created your first multi-target application using the Cloud Application Programming Model. You can read more about Multi Target applications and SAP HANA [in this series of blogs posts](https://blogs.sap.com/2017/09/04/xs-advanced-for-not-so-dummies/) or about the Cloud Application Programming Model [in the official SAP Documentation](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/00823f91779d4d42aa29a498e0535cdf.html)

> If you look into the SAP Cloud Platform Cockpit, you will see the Java micro-service running as an application:
>
> ![Run Java](30.png)
> And the HDI container, with the SAP HANA database artifacts, listed in the services:
>
> ![Run Java](31.png)


[VALIDATE_3]
[ACCORDION-END]


---
