---
title: DAT365 - Native development with SAP HANA (SAP TechEd)
description: DAT365
time: 120
tags: [ tutorial>beginner, products>sap-cloud-platform--sap-hana-service, products>sap-cloud-platform--sap-hana-service]
primary_tag: products>sap-hana
---

## Prerequisites
 - This is supporting material for the hands-on session at SAP TechEd, [DAT365](https://sessioncatalog.sapevents.com/go/agendabuilder.sessions/?l=220&sid=91314_504388&locale=en_US) and [HOL365](https://sessioncatalog.sapevents.com/go/agendabuilder.sessions/?l=220&sid=97169_505070&locale=en_US)
 - This tutorial depends on access to pre-configured accounts in SAP Cloud Platform


## Details
### You will learn
You will develop a complete, end-to-end application using SAP HANA extended application services, SAP Web IDE, and the deployment infrastructure of SAP HANA. Start from scratch and build a complete application that can be deployed both on premise and in the cloud. Take advantage of this opportunity to see the connective tissue, security setup, and deployment options for building real-world applications – not demos.

Created by [Thomas Jung (SAP)](https://sessioncatalog.sapevents.com/go/agendabuilder.speakers/?l=220&speaker_id=818&locale=en_US) and [Lucia Subatin (SAP)](https://sessioncatalog.sapevents.com/go/agendabuilder.speakers/?l=220&speaker_id=43744&locale=en_US)

---

[ACCORDION-BEGIN [Step 1: ](Log in to SAP Cloud Platform)]

Navigate to the  [SAP Cloud Platform cockpit](https://account.hana.ondemand.com/) in a new tab.

Replace **XXX** with the number assigned to you

**User**:
```text
dat365-XXX@teched.cloud.sap
```

**Password**: Welcome19

![Log in ](1.png)

Click **Log On**.

If prompted, accept the disclaimers for the temporary user

![Log in ](2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Log in to SAP Web IDE for Full Stack)]

SAP Web IDE for Full Stack is a service running in the Neo stack in SAP Cloud Platform. You will use it to create your application.

Navigate into the global account **TechEd2019**

![Log in ](3.png)

Click **DAT365neo**

![Log in ](4.png)

Click **Services** and the tile for **SAP Web IDE Full-Stack**

![Log in ](5.png)

Click **Go to Service**

![Log in ](6.png)

Use the same credentials as before

![Log in ](7.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Configure SAP Web IDE)]

You will configure SAP Web IDE for Full Stack to access the space in Cloud Foundry assigned to your user for this session. You will also enable extensions for SAP HANA.

Go into **Settings**

![Log in ](8.png)

Click **Cloud Foundry**

![Log in ](9.png)

Choose the endpoint that says **eu10** from the list

![Log in ](10.png)

Use the same credentials as before to log in

![Log in ](11.png)

The space will populate automatically. Click **Save**

![Log in ](12.png)

Navigate to **Extensions** and enable **SAP HANA Database Development Tools**. Click **Save**.

![Log in ](13.png)

When prompted, click **Refresh**.

> ##  What is going on?
> You will develop an application in SAP Cloud Platform, in Cloud Foundry. An organization has been setup for SAP TechEd and a specific subaccount has been setup for this session. Your user has been assigned to a space, in which you will build and deploy your application.
> SAP Web IDE Full Stack is service that runs in the Neo stack.
> A space is part of this organization and an organization can have multiple spaces. Different users may have different levels of access to a space. Multiple applications and resources can be scoped into a space

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create your first project)]

You will start by creating your first project using the Cloud Application Programming model.

In the development ![dev](dev.png) perspective, right-click on the **Workspace** and choose **New > Project from Template**

![Create app ](14.png)

Switch the environment to **Cloud Foundry** and choose **SAP Cloud Platform Business Application**. Click **Next** to start the wizard.

![Create app ](15.png)

Call the app `HANAapp` and click **Next**

![Create app ](16.png)

Flag **Use HTML5 Repository**

![Create app ](17.png)

Click **Next**.

Choose the following for your project:

1.  **Node.js** for the service
2.  **SPS 04** for the persistence with SAP HANA
3.  Enable **UAA**

![Create app ](18.png)  

Click **Finish**. The wizard will generate a Multi Target Application with two default modules: one for persistence (`db`) and one for services (`srv`).

> ##  What is going on?
> The Cloud Application Programming model provides a base to develop end-to-end applications in SAP Cloud Platform faster...

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create a basic web module)]

Start by creating a basic web module. Right-click on the project and choose **New > HTML5 module**

![Create app ](H1.png)  

Choose **SAPUI5 application** and click **Next**

![Create app ](h2.png)  

Call the module `basicUI` and use the namespace `dat365`. Click **Next**

![Create app ](h3.png)  

Keep the defaults and click **Finish**

![Create app ](h4.png)  

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Configure User Authentication)]

Open the file `mta.yaml`. Click **Resources** and choose the UAA service.

![Add UAA ](1uaa.png)  

Use **+** to add the following key-value pair under **Parameters**

|  Key     | Value
|  :------------- | :-------------
|  path          | `./xs-security.json`

As follows:

![Create app ](57.png)

Also change the `service-plan` from default to `application`

![Add UAA ](5uaa.png)  

Add a dependency to the user authentication service in the app router. You will learn more about this application later.

![Add UAA ](3uaa.png)  

**Save** the changes

![Add UAA ](4uaa.png)  

You can now test the project. Right-click on the `basicUI` module and choose **Run > Run as Web Application**.

![Create app ](71.png)

Choose `index.html` when prompted and wait for the application to be built and deployed:

![Create app ](72.png)

A new tab should open automatically. Enter `dat365-XXX@teched.cloud.sap` and the provided password to log in, replacing XXX with your number:

![Create app ](73.png)

You should see your very basic app:

![Create app ](74.png)

> ##  What is going on?
> You have just deployed your first micro-service into SAP Cloud Platform and bound it to two services. If you looked into your space in the Cloud Foundry account, you would see the UAA service that was automatically created for you, together with the instance of the HTML5 repository you chose to create at the beginning of the wizard:\
>
> [Services](wio1.png)
>

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Configure cross-container access)]

You will be using an instance of the SAP HANA service in Cloud Foundry in a different space. This instance contains an HDI container loaded with data about food. The schema and tables in this HDI container have been shared with limited access across all spaces.

Cross-container access is a very common scenario, especially when data is replicated using Smart Data Integration or SAP SLT into a classic schema, outside an HDI container. In order for the HDI container in your application to access the data, you need to create a user-provided service that provides the credentials for access and synonyms for the objects you want to access.

A user-provided service has already been created in your space and it contains the necessary information to access the target database and schema with data.

Start by declaring this user-provided service as a resource needed by your application.

Open the `mta.yaml` file and click **Resources**

![Create app ](19.png)  

You can see the resources the wizard has created. Click **+** to add a new resource.

Use the following name:

```text
cross-container-service
```

![Create app ](20.png)  

Use the following as type:

```type
org.cloudfoundry.existing-service
```

![Create app ](70.png)  

Click **+** to add a new parameter.

Use the following key:
```text
service-name
```

Use the following value:
```text
CC_GRANT
```

That is the name of the user-provided service that has been created in your space.

![Create app ](21.png)

Under **Properties**, add a new key-value pair.

Use the following for **Key**
```text
cc-service-name
```
Use the following for **Value**
```text
${service-name}
```
![Create app ](24.png)

**Save** the changes in this file.

![Create app ](22.png)

Click on **Modules** and choose the `db` module.

Under **Requires**, use the **+** sign and add the service you have just declared as a resource.

![Create app ](23.png)

In the **Group** field for the cross container resource, enter:

```text
SERVICE_REPLACEMENTS
```
Use the following as key-values, under properties:

|  Key     | Value
|  :------------- | :-------------
|  key          | `cc_service`
|  service           | `~{cc-service-name}`

This is what the configuration should look like:

![Create app ](25.png)

Finally, you want to make sure the default target schema for your database module is your own HDI container, not the shared one. Add the following properties to the HDI container resource:

![Create app ](26.png)

|  Key     | Value
|  :------------- | :-------------
|  `TARGET_CONTAINER`          | `~{hdi-container-name}`

**Save** the file.

![Create app ](27.png)

In the code editor, this is what the modified parts should look like:

![Create app ](28.png)

The technical users from your HDI container will need permissions to execute SELECT and INSERT statements on the shared HDI container. That container has two roles created in it, called `admin` and `adminGrant`.

For further reference, this is what the roles in the shared HDI container look like:

![Create app ](29.png)

The user-provided service has the credentials of a user with enough rights to grant those roles to the technical users in your HDI container.

Under `db`, create a new file

![Create app ](30.png)

Use the following name:

```text
cfg/central.hdbgrants
```

Add the following contents to the file:

```json
{
  "cc_service": {
    "object_owner" : {
      "container_roles":["adminGrant#"]
    },
    "application_user" : {
      "container_roles":["admin"]
    }
  }
}
```

**Save** ![save](save.png) the file.

You can now create synonyms for the table with food data in the shared HDI container. In the new folder `db/cfg`, create a file with the following name:

```text
food-hdi.hdbsynonymconfig
```

Open this file in editor mode:

![Create app ](32.png)

Add the following content into it:

```json
{
  "COMM_FOODS": {
    "target": {
      "object": "COMM_FOODS",
      "grantor": "cc_service"
    }
  },
  "NUTRITION_SCORES": {
    "target": {
      "object": "NUTRITION_SCORES",
      "grantor": "cc_service"
    }
  }
}
```

**Save** ![save](save.png) the file.


In the `src` folder, create a new database artifact:

![Create app ](89.png)

Copy the following name:

```text
synonyms/food-hdi
```

And choose type `.hdbsynonym`

![Create app ](90.png)

In the **Code Editor**, add the following content:

```json
{
  "COMM_FOODS": {},
  "NUTRITION_SCORES": {}
}
```

**Save all the files**. Make sure the structure of your folder looks like this:

![Create app ](33.png)

Before you can build, you want to make sure all changes you make, including deleting artifacts, are reflected in the database. Open the file `package.json` in the `db` folder and
add `--auto-undeploy` to the HDI deploy call:

![Create app ](38.png)

**Save** ![save](save.png) the changes.

Right-click on the `db` module and choose **Build**.

![Create app ](75.png)

Once the build has been successful, right-click on the `db` folder and choose **Open HDI container**

![Create app ](76.png)

> If prompted to add a database, answer **No**

You should see the synonyms for the tables in the shared HDI container and the data in them

![Create app ](77.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Modify Core Data and Services for persistence)]

Within `db/src`, create a  **New > Database Artifact**

![Create app ](78.png)

Call the view `FOODS` and choose `hdbview`:

![Create app ](79.png)

Add the following content to it:

```sql
VIEW FOODS COMMENT 'Foods and Nutrition Data'
   ( "ID",
	 "CODE",
	 "URL",
     "PRODUCT_NAME",
     "BRANDS",
     "COUNTRIES",
     "INGREDIENTS_TEXT",
     "SERVING_SIZE",
     "SERVING_QUANTITY",
     "NUTRITION_CLASS") AS SELECT
	 "FOOD"."ID" ,
	 "FOOD"."CODE" ,
	 "FOOD"."URL",
	 TO_NVARCHAR( "FOOD"."PRODUCT_NAME" ),
	 TO_NVARCHAR( "FOOD"."BRANDS" ),
	 TO_NVARCHAR( "FOOD"."COUNTRIES" ),
	 TO_NVARCHAR( "FOOD"."INGREDIENTS_TEXT" ),
	 "FOOD"."SERVING_SIZE" ,
	 "FOOD"."SERVING_QUANTITY" ,
	 "SCORES"."CLASS" as "NUTRITION_CLASS"
FROM ( "COMM_FOODS" AS "FOOD"
	INNER JOIN "NUTRITION_SCORES" AS "SCORES" ON ( "FOOD"."ID" = "SCORES"."CODE" ) ) WITH READ ONLY
```

Under `srv`, open the file `cat-service.cds` and clear the contents:

![Create app ](82.png)

You will also fix the `cds` module to version `3.13.0` to avoid a future error. Open the file `package.json`

![Create app ](srv1.png)

And make sure the versions is set to `3.13.0`, without the character `^`:

![Create app ](srv2.png)

Under `db`, create a new file called `import.cds` with the following content. Do **NOT save** the file yet.

> If you save the file accidentally, you will get an error in the console. You can ignore the error for now.

```text
@cds.persistence.exists
entity FOODS {
  key ID: Integer  @(title: '{i18n>id}', Common.FieldControl: #Mandatory, Search.defaultSearchElement, Common.Label: '{i18n>id}');
  CODE: Integer64 @title: '{i18n>code}';
  URL: String(500) @title: '{i18n>url}';
  PRODUCT_NAME: String(500) @title: '{i18n>product_name}';
  BRANDS: String @title: '{i18n>brands}';
  COUNTRIES: String @title: '{i18n>countries}';
  INGREDIENTS_TEXT: String @title: '{i18n>ingredients}';
  SERVING_SIZE: String(500) @title: '{i18n>serving_size}';
  SERVING_QUANTITY: String(500) @title: '{i18n>serving_quantity}';
  NUTRITION_CLASS: String(1) @title: '{i18n>nutrition_class}';
}
```

Replace the contents of the file `data-model.cds` with the following content.

```text
namespace teched.dat365;
using FOODS from '../db/import';

entity NUTRITION_GRADE_DESC {
  key CLASS : String(1) @(title: '{i18n>class}', Common.FieldControl: #Mandatory, Search.defaultSearchElement, Common.Label: '{i18n>class}');
  DESCRIPTION  : String @title: '{i18n>description}';
}

entity user {
	key id : String;
	givenName: String;
	familyName: String;
	email: String;
	locale: String;
}

extend FOODS with {
	DESC: association to NUTRITION_GRADE_DESC on DESC.CLASS = NUTRITION_CLASS;
}

```

**Save All** files now:

![Create app ](80.png)

You will see the building process in the console. After it has finished, you will see the design-time artifacts that have been generated for SAP HANA from your `CDS` definition.

![Create app ](35.png)

> If you get an error while building, right-click on `data-model.cds` and choose **Build > Build CDS**
>
> ![Create app ](cds.png)

Right-click on the `db` folder and choose **Build** for them to be created in the database:

![Create app ](36.png)

Look at the logs from the build. You should have confirmation of a successful build:

![Create app ](37.png)

You can now explore the database artifacts. Right-click on the `db` module and choose **Open HDI Container**.

![Create app ](39.png)

> If you are asked if you want to add a database, click **No**.

Scroll down and go into **Tables**. Right-click on the `NUTRITION_GRADE_DESC` table and choose **Generate INSERT Statement**.

![Create app ](40.png)

You will see the name of the schema and table that has been generated for you.

![Create app ](41.png)

Replace that statement with the following to insert records into the table:

```sql
INSERT INTO "TECHED_DAT365_NUTRITION_GRADE_DESC" VALUES('a', 'High nutrition');
INSERT INTO "TECHED_DAT365_NUTRITION_GRADE_DESC" VALUES('b', 'Moderate nutrition');
INSERT INTO "TECHED_DAT365_NUTRITION_GRADE_DESC" VALUES('c', 'Low nutrition');
INSERT INTO "TECHED_DAT365_NUTRITION_GRADE_DESC" VALUES('d', 'Bad nutrition');
```

**Execute** the statements

![Create app ](42.png)

You can also check the view that you created using data form the cross-container access. Use **Open Data** to see the data from the shared HDI container.

![Create app ](43.png)

[DONE]
[ACCORDION-END]



[ACCORDION-BEGIN [Step 9: ](Expose an OData services using Core Data and Services)]

In the previous exercise, you deleted the content of the file `srv/cat-service.cds`. This file contained sample code referring to database artifacts that you replaced with the food table. You deleted those contents to avoid an error when building the artifacts for persistence.

Open the file `srv/cat-service.cds` and add the following content into it:


```text
using teched.dat365 as dat365 from '../db/data-model';
using FOODS from '../db/import';

service CatalogService {
    @readonly entity GradeDesc as projection on dat365.NUTRITION_GRADE_DESC;
    @readonly entity User as projection on dat365.user;
    annotate User with @(requires: 'authenticated-user');
    @readonly entity Foods as projection on FOODS;    
}
```
**Save** the file.

Just like the `cds` file for persistence, this file will generate some artifacts automatically from the definition you entered. Right-click on the file and choose **Build CDS**

![Create app ](44.png)

You can now see new artifacts generated from your definition. These are new views that will be created in the database. Right-click on `db` and choose **Build**

![Create app ](46.png)

Once the build has finished successfully, you can run the `srv` module. Right-click on the folder and choose **Run > Run as Node.js application**

![Create app ](45.png)

When you see the application is running, click on the URL to test it

![Create app ](47.png)

You can see the metadata for the three services being exposed, and data from some of them.

![Create app ](84.png)

Take a look at the definition you inserted in the file `cat-service.cds` and click on the User service. You will notice an annotation there which should be consistent with the message you are getting when trying to access the service with a user that is not authenticated.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Enhance Node.js service)]

In the `srv` folder, create a file called `server.js` with the following content

```javascript
/*eslint no-console: 0, no-unused-vars: 0, no-undef:0, no-process-exit:0*/
/*eslint-env node, es6 */

"use strict";
const https = require("https");
const port = process.env.PORT || 3100;
const server = require("http").createServer();

const cds = require("@sap/cds");
//Initialize Express App for XSA UAA and HDBEXT Middleware
const xsenv = require("@sap/xsenv");
const passport = require("passport");
const xssec = require("@sap/xssec");
const xsHDBConn = require("@sap/hdbext");
const express = require("express");
xsenv.loadEnv();

https.globalAgent.options.ca = xsenv.loadCertificates();
global.__base = __dirname + "/";
global.__uaa = process.env.UAA_SERVICE_NAME;

//logging
var logging = require("@sap/logging");
var appContext = logging.createAppContext();

//Initialize Express App for XS UAA and HDBEXT Middleware
var app = express();

//Build a JWT Strategy from the bound UAA resource
/*passport.use("JWT", new xssec.JWTStrategy(xsenv.getServices({
	uaa: {
		tag: "xsuaa"
	}
}).uaa)); */

//Add XS Logging to Express
app.use(logging.middleware({
	appContext: appContext,
	logNetwork: true
}));

//Add Passport JWT processing
//app.use(passport.initialize());

var hanaOptions = xsenv.getServices({
	hana: {
		plan: "hdi-shared"
	}
});

hanaOptions.hana.pooling = true;
//Add Passport for Authentication via JWT + HANA DB connection as Middleware in Express
app.use(
	xsHDBConn.middleware(hanaOptions.hana)
/*,
	passport.authenticate("JWT", {
		session: false
	}),	*/
);

//CDS OData V4 Handler
var options = {
	kind: "hana",
	logLevel: "error"
};

//Use Auto Lookup in CDS 2.10.3 and higher
/*Object.assign(options, hanaOptions.hana, {
	driver: options.driver
});*/

cds.connect(options);
var odataURL = "/catalog/";
// Main app
cds.serve(
	"gen/csn.json", {
		crashOnError: false
	})
	.to("fiori")
	.at(odataURL)
	//.with(require("./cat-service"))
	.in(app)
	.catch((err) => {
		console.log(err);
		process.exit(1);
	});

console.log(`CDS Build Target: ${cds.env.build.target}`);
console.log(`CDS SQL Mapping: ${cds.env.sql_mapping}`);
//console.log(`CDS Requires: ${JSON.stringify(cds.env.requires)}`);

app.get("/node", (req, res) => {
	res.redirect(odataURL);
});

//Setup Additonal Node.js Routes
//require("./router")(app, server);

//Start the Server
server.on("request", app);
server.listen(port, function () {
	console.info(`HTTP Server: ${server.address().port}`);
});
```


Edit `package.json` to add the following dependencies required by the code you have just pasted (add a `,` comma after the last module, before you insert the new modules):

```js
"@sap/xssec": "2.2.2",
"@sap/xsenv": "2.0",
"@sap/hdbext": "~6.0",
"@sap/logging": "~5.0",
"passport": "~0.4.0"
```

Also replace the start script with `node server.js` as follows:

![Create app ](48.png)

Edit `mta.yaml`, to increase the disk quota of the `srv` module to make room for the additional packages:

![Create app ](49.png)

**Save all** files ![save all](save_all.png)

You can now right-click the `srv` module and choose **run script start**

![Create app ](50.png)

You will once again get a URL in the `Run` console. Append `/catalog/` to call the service definition:

![Create app ](51.png)

> The aesthetics of the JSON output may be different depending on the browser.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Add a user interface)]

Right-click on the project and choose **New > HTML5 module**

![Create app ](53.png)

Choose **SAP Fiori Worklist Application OData V4**

![Create app ](52.png)

Call the module `worklistUI` and fill in the rest of the fields. Click **Next**

![Create app ](54.png)

You will see the services exposed by your project. Choose `Foods` and click **Next**

![Create app ](55.png)

Configure the application settings and template and click **Finish**

![Create app ](56.png)

Open the `mta.yaml` file in **Code Editor**

![Create app ](85.png)

Since you already have the dependency for UAA, remove the one generated automatically in the `mta.yaml` file

![Create app ](58.png)

**Save** the file ![save all](save.png)

Finally, navigate into the `Worklist.view.xml` file. Change the sorting of the products to `true` so that the list does not start with items without a product name:

![Create app ](86.png)

**Save** the file ![save all](save.png)

You can run the first test of your application

![Create app ](59.png)

Choose `index.html`

![Create app ](60.png)

Log in with your user (replace the XXX in the log in example below with your number):

![Create app ](61.png)

You can see your first app displaying data from your services.

![Create app ](68.png)
[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Enable user authentication)]

On step 9, you were able to call the OData services without authentication. However, the web module is asking you to authenticate before it can run.

You will add authentication to the service call and modify your application so that the authentication for your web module is passed over to the Node.js application exposing the services when the web module is called.

Start by changing the authentication to UAA in the route to the OData services catalog in `xs-app.json`.  Also set the CSRF protection to true.

This is what the route for the OData services should look like:

![Create app ](63.png)

**Save** ![save](save.png) the file.

> If you are wondering where `srv_api` comes from, open the file `mta.yaml`:
>
> ![Create app ](64.png)
>
> You will see the `approuter` module is referred here. You will see the application by showing hidden files
>
> ![Create app ](65.png)

Open the file `server.js`. Uncomment the lines for user authentication using the passport module:

![Create app ](67.png)

**Save all** files ![save all](save_all.png)

Run the `srv` module again and click on the endpoint.

![Create app ](87.png)

You should get an `Unauthorized` error message. Your application now expects a JWT token, which is provided by your UAA service when you log in through the user interface.

Run the `worklistUI` module again. Replace `index.html` in the new tab with `/srv_api/catalog`. You will see the metadata from the `srv` module exposed by the app router now:

![Create app ](88.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Create a free-style Fiori application)]

Start by creating a new **HTML5 module**.

![Create app ](91.png)

Choose **SAPUI5 application**.

![Create app ](92.png)

Use the following values for the name and namespace:

|  Field     | Value
|  :------------- | :-------------
|  Name          | `advUI`
|  Namespace           | `teched.dat365.ui`

![Create app ](93.png)

Keep the defaults and click **Finish**.

![Create app ](94.png)

You will now replace the empty shell with an implementation of the UI we have prepared for you.

Download the archive from this [GitHub repository](https://github.com/SAP-samples/teched2019-hana-cloud-dat365-native-development/files/3617784/advUI.zip)

Right-click on the `advUI` module in SAP Web IDE and choose **Import > File or Project**...

![Create app ](95.png)

...and import the file you downloaded

![Create app ](96.png)

Choose to overwrite the existing files. You can now explore the interface you have just imported. The main part of the logic uses templates (just like Fiori Elements) to dynamically adapt the UI based upon OData Annotations.

![Create app ](97.png)

 You can now extend the OData Service to include Annotations. Create a new file in the `srv` module...

![Create app ](98.png)

 ... with the following name:

```name
 fiori-annotations-cat-service.cds
```

Use the following code in this new file:

```cds
using CatalogService as cats from './cat-service';

annotate cats.GradeDesc with @( // header-level annotations
// ---------------------------------------------------------------------------
// List Report
// ---------------------------------------------------------------------------
	// Grade Desc List
	UI: {
		LineItem: [
			{$Type: 'UI.DataField', Value: CLASS, "@UI.Importance":#High},
			{$Type: 'UI.DataField', Value: DESCRIPTION, "@UI.Importance": #High}
 		],
 		PresentationVariant: {
			SortOrder: [ {$Type: 'Common.SortOrderType', Property: CLASS, Descending: false}, {$Type: 'Common.SortOrderType', Property: DESCRIPTION, Descending: false} ]
		}
	}
);


annotate cats.Foods with @( // header-level annotations
// ---------------------------------------------------------------------------
// List Report
// ---------------------------------------------------------------------------
	// Foods List
	UI: {
		LineItem: [
			{$Type: 'UI.DataField', Value: ID, "@UI.Importance":#High},		
			{$Type: 'UI.DataField', Value: CODE, "@UI.Importance":#Low},		
			{$Type: 'UI.DataField', Value: URL, "@UI.Importance":#High},
			{$Type: 'UI.DataField', Value: PRODUCT_NAME, "@UI.Importance":#High},
			{$Type: 'UI.DataField', Value: BRANDS, "@UI.Importance":#High},
			{$Type: 'UI.DataField', Value: COUNTRIES, "@UI.Importance":#Low},
			{$Type: 'UI.DataField', Value: INGREDIENTS_TEXT, "@UI.Importance":#Medium},		
			{$Type: 'UI.DataField', Value: SERVING_SIZE, "@UI.Importance":#High},
			{$Type: 'UI.DataField', Value: SERVING_QUANTITY, "@UI.Importance":#High},			
			{$Type: 'UI.DataField', Value: NUTRITION_CLASS, "@UI.Importance":#Low},
			{$Type: 'UI.DataField', Value: DESC.DESCRIPTION, "@UI.Importance": #High}
 		],
 		PresentationVariant: {
			SortOrder: [ {$Type: 'Common.SortOrderType', Property:  DESC.DESCRIPTION, Descending: false}, {$Type: 'Common.SortOrderType', Property: ID, Descending: false} ]
		}
	}
);
```

You also want translatable texts in our annotations (for column headers and other UI descriptions).  

Create a new file in the `srv` module with the following name:

```name
 _i18n/i18n.properties
```
Add the following content:

```text
class=Nutrition Grade Class
description=Nutrition Grade Description
id=Food Identifier
code=Unique Food Code
url=Food Details Link
product_name=Food Name
brands=Brands
countries=Country of Origin
ingredients=Ingredents
serving_size=Serving Size
serving_quantity=Serving Quantity
nutrition_class=Nutrition Class
nutrition_description=Nutrition Description
```
This is what the structure looks like so far:

![Create app ](99.png)

You also want to expose user profile data to the client side via your OData service. However this data doesn't come from the underlying HANA database. Instead you will use an exit to populate this data from the UAA service.  

Create a new file with the following name in the `srv` module...

```name
 cat-service.js
```
... and paste the following content in it:

```javascript
/*eslint no-console: 0, no-unused-vars: 0, no-undef:0, no-process-exit:0*/
/*eslint-env node, es6 */
"use strict";
/**
 * Implementation for CatalogService defined in ./cat-service.cds
 */

module.exports = (srv) => {

	function getSafe (fn, defaultVal) {
		try {
			return fn();
		} catch (e) {
			return defaultVal;
		}
	}

	srv.on("READ", "User", async(req) => {
		try {
			console.log(`Data: ${JSON.stringify(req.query)}`);
			console.log(`Auth Info: ${JSON.stringify(req.user)}`);
			let data = [];
			data.push({
				"id": getSafe(() => req.user.id),
				"givenName": getSafe(() => req.user.name.givenName),
				"familyName": getSafe(() => req.user.name.familyName),
				"email": getSafe(() => req.user.emails[0].value),
				"locale": getSafe(() => req.user.locale),
			});
			req.reply(data);
		} catch (err) {
			console.error(err.toString());
		}
	});
};
```

To activate these changes, open the file `server.js` and uncomment the following line:

![Create app ](100.png)

**Save all** changes.

Re-run the `srv` modules to pick up these changes.

![Create app ](101.png)

Run the `advUI` module to test the new user interface and service extensions.

![Create app ](102.png)

Choose `index.html`.

![Create app ](103.png)

You will see the new interface:

![Create app ](104.png)

You can also check the console output from the exit showing user details:

![Create app ](105.png)

[DONE]
[ACCORDION-END]


---
