---
title: Manually Create a Data Model for SAP Web IDE's Mock Data Server
description: Create a data model in Common Schema Definition Language (CSDL) using SAP Web IDE.
primary_tag: products>sap-cloud-platform
auto_validation: true
author_name: Marius Obert
author_profile: https://github.com/iobert
tags: [products>sap-cloud-platform, products>sap-web-ide, topic>cloud, topic>mobile, topic>odata, tutorial>intermediate ]
time: 20
---

## Prerequisites  
- **Tutorials:** While not required, it would be useful to complete the [An Open Data Protocol (OData) Primer for Developers](https://developers.sap.com/tutorials/hcp-webide-odata-primer.html) and be familiar with SAP Web IDE before beginning this tutorial.

## Details
### You will learn  
In most cases, a live OData service will be available when building an application. For times when a service is not available, it is still possible to build apps with SAP Web IDE with a file-based data model and then run on simulated data (referred to as "mock data" in SAP Web IDE).

Once the data service is available, the app can then be configured to run against the service rather than the mock data with no other changes. The mock data approach is also useful if you want to prototype an app and have realistic data appear in the UI.

In this tutorial, you will create an OData model with Sales Order-related data fields in two parts:

  - **Part 1:** Create the minimum data model needed to build a basic app based on one OData collectionץ

  - **Part 2:** The second part adds an additional collection as a `NavigationProperty` to your primary collection similar to what you used in the earlier Mobile Guides.

Both versions of the metadata document will allow you to use the SAP Web IDE template wizard to create an app in the next tutorial (and then switch it to a live service).

Additionally, the full data model is provided at the bottom of this document if you want to refer to it later for use in your projects.

### Background

A metadata document is defined in the Common Schema Definition Language (CSDL). There is an exhaustive description of [CSDL](http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part3-csdl.html) here, but this tutorial will focus on a subset of the full language to build the metadata needed for this tutorial.  

The metadata document you will build for part 1 (one OData collection) will have the following structure:

Component         | Description
:--------------   | :-------------
XML declaration   | Not necessary for `.edmx` files, but useful to include if you want to view the file in an editor that supports XML syntax highlighting
`Edmx` and `DataServices` elements | The "wrapper" for your data model
`Schema`          | Container for the `EntityTypes`, `Associations` and `EntityContainer` elements
`EntityType`      | Defines the data model for the OData collection
`EntityContainer` | Exposes the OData collection

![Part 1 OData Model Structure](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part1_intro.png)

Part two of this tutorial adds in a second `EntityType` and the required elements to set up the `NavigationProperty`. In the SAP Web IDE template you have been using the data from the `NavigationProperty` (Suppliers) has been displayed in the Info or Suppliers tab.


---


[ACCORDION-BEGIN [Step 1: ](Open SAP Web IDE and create new folder)]

Open SAP Web IDE, select the Local folder and create a new folder called `Metadata`.

Right-click the `Metadata` folder, and create a new file named `m104metadata_no_nav.edmx`.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create entity model wrapper)]

To start with, paste in the text below as the entity model wrapper of the metadata file. The wrapper consists of the following:

Item          | Description
:------------ | :-------------
`<?xml ...?>` | XML encoding declaration. Here you specify the XML version, the encoding used in the file, and that it is stand-alone (does not rely on information from an external source)
`edmx:DataServices` | A CSDL formatted document requires `edmx:Edmx` as the root element, and that it contains a single `edmx:DataServices` element

Text to enter:

```XML
<?xml version="1.0" encoding="utf-8" standalone="yes"?>
<edmx:Edmx Version="1.0" xmlns:edmx="http://schemas.microsoft.com/ado/2007/06/edmx">
<edmx:DataServices m:DataServiceVersion="2.0" xmlns:m="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata">

</edmx:DataServices>
</edmx:Edmx>
```

![wrapper](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part1_3.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Add Schema element)]

Next, paste the `Schema` element text inside of the `DataServices` element. After you paste in the text, select **Beautify** from the **Edit** menu (**Edit > Beautify**) to clean up the indents (you will want to select Beautify each time you paste in text during this tutorial). It is suggested you then add some blank lines before the `</Schema>` tag to make it easier to insert text in later steps (as shown in the image below).

Item          | Description
:------------ | :-------------
`<Schema...`  | A CSDL document requires that each `edmx:DataServices` element contain one or more `Schema` elements. Here, you will also specify the namespace (Sales) for the service.
`</Schema>`   | This is the closing tag for the `Schema` element. The closing tag is used in all multi-line XML elements (there are two in the previous step).

Text to enter:

```XML
<Schema Namespace="SalesModel" xmlns="http://schemas.microsoft.com/ado/2008/09/edm" xmlns:d="http://schemas.microsoft.com/ado/2007/08/dataservices" xmlns:m="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata">

</Schema>
```

![schema](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part1_4.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Review exposed data items)]

Before continuing, with the `EntityType`, it will be useful to understand the data items that the service will expose. Imagine that you have completed your Design Thinking sessions with the users and have determined that the app should display the sales order relevant fields below (and their data types):


Field Name                   | Data Type
:--------------------------- | :-------------
`SalesOrderID`               | String
`CreatedAt`                  | Date and Time
`CreatedByCustomerName`      | String
`CreatedByEmployeeFirstName` | String
`CreatedByEmployeeLastName`  | String
`CreatedByEmployeeUserID`    | String
`NetSum`                     | Decimal
`Tax`                        | Decimal
`TotalSum`                   | Decimal
`Currency`                   | String
`CurrencyCodeDescription`    | String
`BillingStatus`              | String
`BillingStatusDescription`   | String
`ChangedAt`                  | Date and Time
`ChangedByCustomerName`      | String
`ChangedByEmployeeFirstName` | String
`ChangedByEmployeeLastName`  | String
`ChangedByEmployeeUserID`    | String
`CustomerID`                 | String
`CustomerName`               | String
`DeliveryStatus`             | String
`DeliveryStatusDescription`  | String
`Status`                     | String
`Note`                       | String
`StatusDescription`          | String


Additionally, the Data Architects involved in the project require two additional fields for linking the tables in the back-end:

Field Name                   | Data Type
:--------------------------- | :-------------
`CustomerKey`                | String
`SalesOrderKey`              | String




There are sixteen different primitive data types supported in OData ([more information can be found on the `odata.org` website](http://www.odata.org/documentation/odata-version-2-0/overview/)), but you will only need a few to capture the data you need:

- `String`
- `Decimal`
- `DateTime`


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Add entity type)]

The next step is to add the `EntityType` element into your document which defines the fields in your data model. Within the `EntityType` element you will include a `Name` attribute which is a simple identifier for the element.

Copy and paste the text below within the `Schema` element in your document.

```XML
<EntityType Name="SalesOrder">


</EntityType>
```

![Entity Type](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part1_7.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Populate entity type element)]

With the field names and data types known, it is simple to populate the `EntityName` element. When doing this on your own, you will need to make two decisions:

- Which field to use as a `Key`
- The field length for the strings

The `Key` is used as the default sorting key the service will use when returning the data, and the field length for strings is the maximum number of characters allowed. For this tutorial, these items have been taken care of for you (`SalesOrderKey` and the string lengths vary by field).

Copy and paste the following within the `SalesOrder` `EntityType` elements.

```XML
<Key>
	<PropertyRef Name="SalesOrderKey"/>
</Key>
<Property Name="SalesOrderID" Type="Edm.String" MaxLength="10" />
<Property Name="CreatedByEmployeeLastName" Type="Edm.String" MaxLength="40" />
<Property Name="Status" Type="Edm.String" MaxLength="1" />
<Property Name="ChangedByEmployeeLastName" Type="Edm.String" MaxLength="40" />
<Property Name="ChangedByEmployeeUserID" Type="Edm.String" MaxLength="12" />
<Property Name="NetSum" Type="Edm.Decimal" Precision="15" Scale="2" />
<Property Name="CustomerKey" Type="Edm.String" MaxLength="32" />
<Property Name="CreatedByEmployeeUserID" Type="Edm.String"  MaxLength="12" />
<Property Name="DeliveryStatus" Type="Edm.String" MaxLength="1" />
<Property Name="CurrencyCodeDescription" Type="Edm.String" MaxLength="255" />
<Property Name="StatusDescription" Type="Edm.String" MaxLength="60" />
<Property Name="ChangedByCustomerName" Type="Edm.String" MaxLength="80" />
<Property Name="CreatedByEmployeeFirstName" Type="Edm.String" MaxLength="40" />
<Property Name="DeliveryStatusDescription" Type="Edm.String" MaxLength="60" />
<Property Name="Note" Type="Edm.String" MaxLength="255" />
<Property Name="CreatedAt" Type="Edm.DateTime" Precision="7" />
<Property Name="Tax" Type="Edm.Decimal" Precision="15" Scale="2" />
<Property Name="TotalSum" Type="Edm.Decimal" Precision="15" Scale="2" />
<Property Name="CreatedByCustomerName" Type="Edm.String" MaxLength="80" />
<Property Name="ChangedByEmployeeFirstName" Type="Edm.String" MaxLength="40" />
<Property Name="ChangedAt" Type="Edm.DateTime" Precision="7" />
<Property Name="CustomerID" Type="Edm.String" MaxLength="10" />
<Property Name="CustomerName" Type="Edm.String" MaxLength="80" />
<Property Name="SalesOrderKey" Type="Edm.String" Nullable="false" MaxLength="32" />
<Property Name="BillingStatus" Type="Edm.String" MaxLength="1" />
<Property Name="BillingStatusDescription" Type="Edm.String" MaxLength="60" />
<Property Name="Currency" Type="Edm.String" MaxLength="5" />
```

![Entity Type Content](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part1_8.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Finally, add entity container)]

The last element to add is the `EntityContainer` which will expose the `SalesOrder` model as the `SalesOrders` collection. Copy and paste the following between the `</EntityType>` tag and the `</Schema>` tag.

```XML
<EntityContainer Name="SalesEntities" m:IsDefaultEntityContainer="true" xmlns:p7="http://schemas.microsoft.com/ado/2009/02/edm/annotation">
	<EntitySet EntityType="SalesModel.SalesOrder" Name="SalesOrders"/>
</EntityContainer>
```

![EntityContainer](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part1_9.png)


Your basic OData model is now complete and you could use it to build a basic app with the SAP Web IDE template wizard. If you would like to do this before continuing, you can jump to the next tutorial now.


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Review requirements for linking second collection)]

Most OData services you will use will have collections that are linked to one or more other related collections through a `NavigationProperty` element. In this section, you will add a second collection and the elements to enable the navigation. The table and graphic below describe the changes you make and provide a graphical view of the part 2 OData model.

Component                  | Description
:------------------------- | :-------------
XML declaration, `Edmx` and `DataServices` elements | Same as part 1
`EntityType` #1            | Same as in part 1, but with a `NavigationProperty` element added
`EntityType` #2            | Similar to part 1, but defines the data for the second collection
`Association`              | Defines the nature of the connection between the two collections
`EntityContainer`          | Similar to part 1, but adds the second collection as well as the `AssociationSet` to expose the connection between the two collections

![Part 2 OData Model Structure](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part2_intro.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 9: ](Copy existing file)]

To begin, right-click the `m104metadata_no_nav.edmx` file and select **Copy**.

Right-click the `Metadata` folder and select **Paste** (to duplicate the file). You will see a naming conflict dialog box open – name the new file `m104metadata_nav.edmx` and click **OK**.

![Rename file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part2_2.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 10: ](Add navigation property element)]

The first edit to make is to add a `NavigationProperty` element to the `SalesOrder` entity. Copy and paste the following text just below the Currency property in `m104metadata_nav.edmx`. The attributes you specify here are:

Attribute                  | Purpose
:------------------------- | :-------------
`Name`                  | `BusinessPartner` – Label for the `NavigationProperty`
`FromRole`     | `SalesOrder` – refers to a destination of the `NavigationProperty` and matches the name specified in the `Association` element
`ToRole`       | `BusinessPartner` - refers to a destination of the `NavigationProperty` and matches the name specified in the `Association` element
`Relationship` | `SalesModel.FK\_SalesOrder_BusinessPartner` – This string's format is \<Schema Namespace>.\<Association Name>. The `Association` will be added later.

Text to enter:

```XML
<NavigationProperty FromRole="SalesOrder" Name="BusinessPartner" Relationship="SalesModel.FK_SalesOrder_BusinessPartner"
ToRole="BusinessPartner"/>
```

![NavigationProperty](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part2_3.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 11: ](Add entity type)]

For the second collection (`BusinessPartner`), you will add another `EntityType` element with the fields below (again, imagine this is based on your Design Thinking sessions).

Field Name                   | Data Type
:--------------------------- | :-------------
`Address`                    | String
`BusinessPartnerID`          | String
`BusinessPartnerKey`         | String
`BusinessPartnerRoleCode`    | String
`BusinessPartnerRoleText`    | String
`Company`                    | String
`CountryCode`                | String
`CurrencyCode`               | String
`CurrencyText`               | String
`EmailAddress`               | String
`FaxNumber`                  | String
`LegalForm`                  | String
`TelephoneNumber`            | String
`WebAddress`                 | String



To add the `EntityType` for `BusinessPartner`, copy and paste the following text immediately below the `SalesOrder` `</EntitySet>` closing tag:

Text to enter:

```XML
<EntityType Name="BusinessPartner">
	<Key>
		<PropertyRef Name="BusinessPartnerKey"/>
	</Key>
	<Property Name="Address" Nullable="false" Type="Edm.String"/>
	<Property MaxLength="10" Name="BusinessPartnerID" Type="Edm.String"/>
	<Property MaxLength="80" Name="Company" Type="Edm.String"/>
	<Property MaxLength="60" Name="BusinessPartnerRoleText" Type="Edm.String"/>
	<Property MaxLength="32" Name="BusinessPartnerKey" Nullable="false" Type="Edm.String"/>
	<Property MaxLength="60" Name="CurrencyText" Type="Edm.String"/>
	<Property MaxLength="60" Name="WebAddress" Type="Edm.String"/>
	<Property MaxLength="3" Name="BusinessPartnerRoleCode" Type="Edm.String"/>
	<Property MaxLength="30" Name="FaxNumber" Type="Edm.String"/>
	<Property MaxLength="10" Name="LegalForm" Type="Edm.String"/>
	<Property MaxLength="255" Name="EmailAddress" Type="Edm.String"/>
	<Property MaxLength="30" Name="TelephoneNumber" Type="Edm.String"/>
	<Property MaxLength="5" Name="CurrencyCode" Type="Edm.String"/>
	<Property MaxLength="3" Name="CountryCode" Type="Edm.String"/>
	<Property MaxLength="1" Name="GenderCode" Type="Edm.String"/>
</EntityType>
```

![BusinessPartner EntityType](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part2_5.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 12: ](Add association element)]

The next addition is the `Association` element which (as the name implies) represents an association in an entity model. The `Association` element must contain a `Name` and two `End` elements – and can also contain referential constraints. The attributes you will use are explained below.

Attribute                  | Purpose
:------------------------- | :-------------
`Name`                     | `FK\_SalesOrder_BusinessPartner` – must match the `Association` Name in the `NavigationProperty` element
`End #1`                     | Multiplicity: "0..1" – indicates that when a `SalesOrder` exists, it can have zero or one `BusinessPartner` entities associated <ul><li>Role: Allows the association to be bound to a `NavigationProperty`</li><li>Type: Must point to an entity type in the entity model</li></ul>
`End #2`                     | Multiplicity: * – indicates that when a `BusinessPartner` exists, it can have zero or more `SalesOrder` entities associated<ul><li>Role: Allows the association to be bound to a `NavigationProperty`</li><li>Type: Must point to an entity type in the entity model</li></ul>
`ReferentialConstraint`     | This element asserts that the entity identified in the Principal Role must exist in order for the entity in the Dependent Role to exist. In this case, a `BusinessPartner` must exist in order to create a `SalesOrder`.  Also specified here are the unique identifiers (`BusinessPartnerKey` and `SalesOrderID`).

Copy and paste the following text immediately after the `BusinessPartner` `</EntityType>` closing tag.

```XML
<Association Name="FK_SalesOrder_BusinessPartner">
	<End Multiplicity="0..1" Role="BusinessPartner" Type="SalesModel.BusinessPartner"/>
	<End Multiplicity="*" Role="SalesOrder" Type="SalesModel.SalesOrder"/>
	<ReferentialConstraint>
		<Principal Role="BusinessPartner">
			<PropertyRef Name="BusinessPartnerKey"/>
		</Principal>
		<Dependent Role="SalesOrder">
			<PropertyRef Name="SalesOrderID"/>
		</Dependent>
	</ReferentialConstraint>
</Association>
```



[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 13: ](Modify entity container element)]

The last step is to modify the `EntityContainer` element to add the `BusinessPartners` collection as well an the association between the two collections.

Copy and paste the following text after the `SalesOrders` `<EntitySet>` element within the `EntityContainer` element.

```XML
<EntitySet EntityType="SalesModel.BusinessPartner" Name="BusinessPartners"/>
<AssociationSet Association="SalesModel.FK_SalesOrder_BusinessPartner" Name="SO_TO_BP_AssocSet">
	<End EntitySet="BusinessPartners" Role="BusinessPartner"/>
	<End EntitySet="SalesOrders" Role="SalesOrder"/>
</AssociationSet>
```

![EntitySet](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part2_7.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 14: ](Model is complete)]

Your model is now complete. When working on your own OData model, you can take advantage of the [OData Model Editor](https://help.hana.ondemand.com/webide_odatamodeler/frameset.htm?e5c9289506a7493189948b55c69097db.html) (which is a plug-in to SAP Web IDE). To enable it, select **Tools > Preferences** in SAP Web IDE, click **Plugins**, locate the **OData Model Editor** plugin, click the slider to **On**, then **Save** and SAP Web IDE will reload.

![Enable OData modeler](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part2_8.png)


[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 15: ](Load graphical view)]

With the `m104metadata_nav.edmx` file open, you will see the **Source** and **Design** tabs at the bottom of your editor pane. These show up when you are working on an `.edmx` file. Click the **Design** tab to switch to the graphical view.

![Design tab](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part2_9.png)



The graphical view will open. If the model appears very small (as shown below), click the **Fit to Screen** button to zoom in.

![Fit to screen](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part2_10.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 16: ](View full entity model)]

In this view you can see the full entity model, associations, multiplicity and clicking on one of the members of the model will show the attributes for that member.

![View member attributes](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part2_11.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 17: ](View console)]

Click the **Source** tab, and then select **View > Console** (if the console is not currently displayed). SAP Web IDE also has a CSDL validator that is activated when you edit an `.edmx` file. Since your model is complete – it will show **this is a valid OData model file**.

![Valid model](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part2_12.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 18: ](View changes in real time)]

The validation happens during editing, so you can see the effect of changes right away. To test this out, simulate a typographic error by inserting a character into the first `EntitySet` line (change `SalesModel` to `SalesModels`). The validator points out the error right away.

![Validator](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part2_13.png)

Remove the extra character to remove the error.

![fixed typo](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-create-odata-model/mob4-1_part2_14.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 19: ](Future modifications)]

If you want to modify the entity model used in this tutorial for your own projects, the full entity model text is provided below. Just copy and paste it into an `.edmx` file in SAP Web IDE and modify the Name, Type fields and others as needed.

```XML
<?xml version="1.0" encoding="utf-8" standalone="yes"?>
<edmx:Edmx Version="1.0" xmlns:edmx="http://schemas.microsoft.com/ado/2007/06/edmx">
	<edmx:DataServices m:DataServiceVersion="2.0" xmlns:m="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata">
		<Schema Namespace="SalesModel" xmlns:d="http://schemas.microsoft.com/ado/2007/08/dataservices"
xmlns:m="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata" xmlns="http://schemas.microsoft.com/ado/2008/09/edm">
			<EntityType Name="SalesOrder">
				<Key>
					<PropertyRef Name="SalesOrderKey"/>
				</Key>
				<Property MaxLength="10" Name="SalesOrderID" Type="Edm.String"/>
				<Property MaxLength="40" Name="CreatedByEmployeeLastName" Type="Edm.String"/>
				<Property MaxLength="1" Name="Status" Type="Edm.String"/>
				<Property MaxLength="40" Name="ChangedByEmployeeLastName" Type="Edm.String"/>
				<Property MaxLength="12" Name="ChangedByEmployeeUserID" Type="Edm.String"/>
				<Property Name="NetSum" Precision="15" Scale="2" Type="Edm.Decimal"/>
				<Property MaxLength="32" Name="CustomerKey" Type="Edm.String"/>
				<Property MaxLength="12" Name="CreatedByEmployeeUserID" Type="Edm.String"/>
				<Property MaxLength="1" Name="DeliveryStatus" Type="Edm.String"/>
				<Property MaxLength="255" Name="CurrencyCodeDescription" Type="Edm.String"/>
				<Property MaxLength="60" Name="StatusDescription" Type="Edm.String"/>
				<Property MaxLength="80" Name="ChangedByCustomerName" Type="Edm.String"/>
				<Property MaxLength="40" Name="CreatedByEmployeeFirstName" Type="Edm.String"/>
				<Property MaxLength="60" Name="DeliveryStatusDescription" Type="Edm.String"/>
				<Property MaxLength="255" Name="Note" Type="Edm.String"/>
				<Property Name="CreatedAt" Precision="7" Type="Edm.DateTime"/>
				<Property Name="Tax" Precision="15" Scale="2" Type="Edm.Decimal"/>
				<Property Name="TotalSum" Precision="15" Scale="2" Type="Edm.Decimal"/>
				<Property MaxLength="80" Name="CreatedByCustomerName" Type="Edm.String"/>
				<Property MaxLength="40" Name="ChangedByEmployeeFirstName" Type="Edm.String"/>
				<Property Name="ChangedAt" Precision="7" Type="Edm.DateTime"/>
				<Property MaxLength="10" Name="CustomerID" Type="Edm.String"/>
				<Property MaxLength="80" Name="CustomerName" Type="Edm.String"/>
				<Property MaxLength="32" Name="SalesOrderKey" Nullable="false" Type="Edm.String"/>
				<Property MaxLength="1" Name="BillingStatus" Type="Edm.String"/>
				<Property MaxLength="60" Name="BillingStatusDescription" Type="Edm.String"/>
				<Property MaxLength="5" Name="Currency" Type="Edm.String"/>
				<NavigationProperty FromRole="SalesOrder" Name="BusinessPartner" Relationship="SalesModel.FK_SalesOrder_BusinessPartner" ToRole="BusinessPartner"/>
			</EntityType>
			<EntityType Name="BusinessPartner">
				<Key>
					<PropertyRef Name="BusinessPartnerKey"/>
				</Key>
				<Property Name="Address" Nullable="false" Type="Edm.String"/>
				<Property MaxLength="10" Name="BusinessPartnerID" Type="Edm.String"/>
				<Property MaxLength="80" Name="Company" Type="Edm.String"/>
				<Property MaxLength="60" Name="BusinessPartnerRoleText" Type="Edm.String"/>
				<Property MaxLength="32" Name="BusinessPartnerKey" Nullable="false" Type="Edm.String"/>
				<Property MaxLength="60" Name="CurrencyText" Type="Edm.String"/>
				<Property MaxLength="60" Name="WebAddress" Type="Edm.String"/>
				<Property MaxLength="3" Name="BusinessPartnerRoleCode" Type="Edm.String"/>
				<Property MaxLength="30" Name="FaxNumber" Type="Edm.String"/>
				<Property MaxLength="10" Name="LegalForm" Type="Edm.String"/>
				<Property MaxLength="255" Name="EmailAddress" Type="Edm.String"/>
				<Property MaxLength="30" Name="TelephoneNumber" Type="Edm.String"/>
				<Property MaxLength="5" Name="CurrencyCode" Type="Edm.String"/>
				<Property MaxLength="3" Name="CountryCode" Type="Edm.String"/>
				<Property MaxLength="1" Name="GenderCode" Type="Edm.String"/>
			</EntityType>
			<Association Name="FK_SalesOrder_BusinessPartner">
				<End Multiplicity="0..1" Role="BusinessPartner" Type="SalesModel.BusinessPartner"/>
				<End Multiplicity="*" Role="SalesOrder" Type="SalesModel.SalesOrder"/>
				<ReferentialConstraint>
					<Principal Role="BusinessPartner">
						<PropertyRef Name="BusinessPartnerKey"/>
					</Principal>
					<Dependent Role="SalesOrder">
						<PropertyRef Name="SalesOrderID"/>
					</Dependent>
				</ReferentialConstraint>
			</Association>
			<EntityContainer Name="SalesEntities" m:IsDefaultEntityContainer="true" xmlns:p7="http://schemas.microsoft.com/ado/2009/02/edm/annotation">
				<EntitySet EntityType="SalesModel.SalesOrder" Name="SalesOrders"/>
				<EntitySet EntityType="SalesModel.BusinessPartner" Name="BusinessPartners"/>
				<AssociationSet Association="SalesModel.FK_SalesOrder_BusinessPartner" Name="SO_TO_BP_AssocSet">
					<End EntitySet="BusinessPartners" Role="BusinessPartner"/>
					<End EntitySet="SalesOrders" Role="SalesOrder"/>
				</AssociationSet>
			</EntityContainer>
		</Schema>
	</edmx:DataServices>
</edmx:Edmx>
```


[DONE]
[ACCORDION-END]

### OData Model Editor
- Read up on the [OData Model Editor](https://help.hana.ondemand.com/webide_odatamodeler/frameset.htm)
