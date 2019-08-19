---
title: An Open Data Protocol (OData) Primer for Developers
description: Learn how to explore the data in an OData service, and the functionality included in the service.
primary_tag: topic>odata
auto_validation: true
author_name: Marius Obert
author_profile: https://github.com/iobert
tags: [ products>sap-cloud-platform, products>sap-web-ide, topic>cloud, topic>mobile, topic>odata, tutorial>intermediate ]
time: 15
---


## Details
### You will learn
  - The basics of OData

OData (Open Data Protocol) is an [OASIS open industry standard](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=odata) covering building and consuming RESTful APIs. The standard was initially created by Microsoft, and the committee is now chaired by a Microsoft and an SAP employee.

There is a wealth of information available at [http://www.odata.org](http://www.odata.org/), but this tutorial will focus on a few practical aspects that will help you in future projects. The topics covered are:

- Browser extension to view JSON easily
- OData URI format
- Service Document and metadata
- Query options:
    - `$format`
    - `$top`
    - `$skiptoken`
    - `$orderby`
    - `$filter`
    - `$select`
    - `$expand`



---

[ACCORDION-BEGIN [Step 1: ](Get browser extension)]

To help visualize an OData feed, it is useful to install a formatting extension in your browser. There are a few options available to select 7#151; the one used in this tutorial is "**JSONView**" and is available for Chrome, Firefox and Safari browsers. The process for installing an extension is similar across browsers, the Chrome steps are shown below.

In Google Chrome, click the **menu** button and then select **Settings**.

![Open Chrome Settings](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_1.png)

Click **Extensions** and then at the bottom of the page, click **Get more extensions**.

![Get more extensions](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_2.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 2: ](Install extension)]

In the chrome web store, type **`JSONView`** in the search box, hit Enter and scroll to the Extensions part of the results to find `JSONView`. Click **Add to Chrome**.

![Find JSONView](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_3.png)

In the dialog box, click **Add extension**.

![Add JSONView](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_4.png)

JSONView is now installed and enabled.

![JSONView installed](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_5.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 3: ](Review OData URLs)]

The format of an OData URL is shown below.

![OData URL format](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_6.png)

For the Northwind service you have been using the URI (from host to `ServiceRoot`) is:

- <http://services.odata.org/V2/Northwind/Northwind.svc/>

This URL points to a Service Document, which for OData, exposes two key things:

- The entity sets, functions and singletons that can be retrieved
- A metadata document (which shows the packaging format of the data)

To view the entity sets, open the link above in a new browser tab.

> If you would like to access an SAP Gateway server, see the Optional section at the end of this tutorial for the free Gateway trial sign up link and OData service URL.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Review collections)]

As you scroll through the page, you will see all of the entity sets (or collections) listed. You have been using the `Products` and `Suppliers` collections.

![Northwind Service Document](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_7.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 5: ](Review metadata)]

The metadata document will show the individual fields, formats and navigation properties of all the collections. To view the metadata for *any* OData service, append `$metadata` at the end of the URL in your browser:

<http://services.odata.org/V2/Northwind/Northwind.svc/$metadata>

With the metadata displayed, scroll down to `<EntityType Name="Product">` which is the collection you are using. You may notice that there is a slight change in the naming. The collection `href` (or the external name) in step 7 is `Products`, and in the metadata the same collection's name is `Product`. The mapping of an `EntityType` to a collection name is defined in the `EntityContainer` elements. You will learn more about the OData model structure in the OData model tutorial.

![Products Collection](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_8.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 6: ](Review and add fields)]

In your app, you have used the fields displayed including the `NavigationProperty` entry that points to `Suppliers`.

In OData parlance, a `NavigationProperty` is a link from an Entry to one or more related Entries. In your app, Web IDE used the link from `Products` to `Suppliers` to display some supplier information in the information tab. Specifically, the Supplier `CompanyName`, `Phone` and `Address` fields.

![Supplier Collection](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_9.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Review data)]

Viewing the metadata is a quick way to see what data is available, but it is also useful to view the data itself. To see a set of data from a collection, simply enter the URL to the service document followed by the collection of interest. For the `Products` collection, it is:

<http://services.odata.org/V2/Northwind/Northwind.svc/Products>

By adding `Products` to the URL here, you are specifying the `ResourcePath` portion of the URL (refer to step 6 above). The first set of data (20 records) is displayed in XML format.

![Data from the Products collection](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_10.png)



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Run app to see paging)]

Scroll to the bottom of the page and look for the `<link rel="next"` entry. The Northwind service enforces server-side paging and will only pass 20 records at a time. The paging size (20) can be seen in the `$skiptoken=20` query option at the end of the "next" URL. A Web IDE generated app will automatically issue the "next" URL when you scroll to the bottom of the master list in your app. You can run your app and try this now. Look for the spinning "busy" UI element to appear briefly as the new request is sent and set loaded in.

![Next URL](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_11.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Enhance format for readability)]

The XML format of the OData response includes a lot of extra characters that makes the output difficult to read. You can add a query option to the URL to change the format to JSON, and with JSONView installed, some formatting and color-coding makes it more "human-readable".

After the resource path, multiple query options can be added. Following standard HTTP query string formats, the first option will be pre-pended with a `?`, and any successive ones will be pre-pended with a `&`. To request the response in JSON, use the query option: `$format=json` pre-pended with a `?`.

<http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json>

You can see how there is less text shown, along with the color-coding and formatting of JSONView improves the readability.

![JSON format](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_12.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Set number of records to return)]

Some OData services do not enforce server-side paging, and will send a large amount of data for each request. To limit the set of data sent, include the `$top=x` query option, where x is any integer. `$top=1` will request only the first record, `$top=5` requests the first five.

Enter `$top=1` to view the first record only (be sure to pre-pend it with a `&` since it is the second query option).

<http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$top=1>

![top query option](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_13.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Skipping over records)]

To see the 6th and 7th records, add the `$skiptoken=5` query option and change `$top` to 2. The `$skiptoken` will make the service skip over the `skiptoken` many records before it sends data. Changing `$top` to `2` will return two records, rather than just one.

<http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$top=2&$skiptoken=5>

![6th and 7th records](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_14.png)



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Create URL for individual records)]

A shorthand way of referring to individual records is to put the record number in parenthesis after the Collection name. To view the 22nd record in `Products`, use the URL below:

<http://services.odata.org/V2/Northwind/Northwind.svc/Products(22)?$format=json>

![Shorthand-22](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_15.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Set sort-by field)]

In the metadata, the `ProductID` is set as the key for the collection. This means that any data returned will be sorted by `ProductID`, as evidenced in the screenshots above (`ProductID` of 1, 6, 7, 22 etc.).

![Key field](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_16.png)


You can specify that the results should be returned sorted on a different field by using the `$orderby=<FieldName>` query option. It is easier for a person to find a desired product if the list was sorted alphanumerically by `ProductName`. To see this in action, use the URL below that includes the `$orderby=ProductName` query option.

<http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$top=2&$orderby=ProductName>

You can see that the records are returned in alphanumerical order (`ProductID`s 17 and 3 respectively).

![orderby ProductName](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_17.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Set sort order)]

By default, the `$orderby` option sorts in ascending order. To sort by descending order, append "` desc`" (with the space) after the `orderby` field. The browser will encode the space as `%20` and the results are returned in descending alphanumerical order.

<http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$top=2&$orderby=ProductName%20desc>

![orderby ProductName descending](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_18.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 15: ](Add filters)]

A very useful query option to highlight is the filter expression (`$filter`). A `filter` expression can be simple logical operators (`equal`, `greater than`, `less than`, etc.) include basic math (`add`, `subtract`, `multiply` and `delete`) and functions (`string`, `data`, `math` and `type` functions), and combinations of all.

A simple example of filtering would be to see which products in the Northwind service are discontinued (the link below will show eight results). The query option string is: `$filter=Discontinued eq true`

<http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$orderby=ProductName&$filter=Discontinued%20eq%20true>

To exclude those from an app, you would simply change the `eq` (equal) to `ne` (not equal).

![filter Discontinued = true](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_19.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 16: ](Add other query parameters)]

To get a list of products with a `UnitPrice` greater than 100 and not discontinued, the query option string would be: `$filter=Discontinued eq false and UnitPrice gt 100`. Here, `gt` stands for "greater than".

<http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$orderby=ProductName&$filter=Discontinued%20eq%20false%20and%20UnitPrice%20gt%20100>

![UnitPrice > 100 and not discontinued](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_20.png)

>For a full list of filter options with examples, please see the URI conventions link shown in the last step of this tutorial.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 17: ](Get subset of collection)]

The `$select` query option specifies a subset of the full collection properties to return which is useful is you want to minimize the amount of data sent or received to the app. For example, to return only the `ProductNam`, `UnitPrice` and `Supplier` use this URL:

<http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$select=ProductName,UnitPrice,Supplier>

![selecting a subset of fields](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_21.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 18: ](Expand the query)]

Since `Supplier` is a `NavigationProperty`, it is returned as a URI. You can use the `$expand` query option to have the server include those properties in the response (as opposed to just the URI):

<http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$select=ProductName,UnitPrice,Supplier&$expand=Supplier>

![Expand a NavigationProperty field](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_22.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 19: ](Combine query options)]

Building on the last example, in this final query you will combine a few of the query options together:

- `$format` – return the results in JSON format
- `$select` – return three fields (`ProductName`, `UnitPrice` and `Supplier`)
- `$expand` – expand the Supplier `NavigationProperty`
- `$filter` – filter the response based on a field name in the `Supplier` collection where (`CompanyName` starts with `Grand`)

<https://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$select=ProductName,UnitPrice,Supplier&$expand=Supplier&$filter=startswith(Supplier/CompanyName,%20%27Grand%27)>

![Complex query results](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-odata-primer/mob3-4_23.png)


[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 19: ](Wrap-up)]

As you have seen, there is quite a bit of capability exposed in an OData service. The advantage to the developer is the application logic that you would otherwise have to create and maintain in code can be delivered by the OData service. You will learn how to do this in the next tutorial.


There are many OData resources available on the web. A few are listed below:

- OData tutorial: <http://www.odata.org/getting-started/basic-tutorial/>
- OData URI conventions: <http://www.odata.org/documentation/odata-version-2-0/uri-conventions/>
- OData viewer and query builder: <http://pragmatiqa.com/xodata/>

>To use the OData viewer, select **Metadata URL** under **Choose Access Option** and enter `http://services.odata.org/V2/Northwind/Northwind.svc/$metadata`


[DONE]
[ACCORDION-END]

### Cheat Sheet

Here is a short summary of the various URLs and query options covered in this tutorial:

Comments                   |  URL
:-------------------------| :-------------
Service Document             |  <http://services.odata.org/V2/Northwind/Northwind.svc/>
Metadata Document            |  <http://services.odata.org/V2/Northwind/Northwind.svc/$metadata>
View a Collection            |  <http://services.odata.org/V2/Northwind/Northwind.svc/Products>
Collection with `$skiptoken` |  <http://services.odata.org/V2/Northwind/Northwind.svc/Products?$skiptoken=20>
Use `$format` to output JSON  |  <http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json>
`$top=1` to return the first record only (as JSON) |  <http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$top=1>
`$skiptoken=5` to skip the first five records, `$top=2` to return the next two records (# 6 and #7) |  <http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$top=2&$skiptoken=5>
Use Collection(XYZ) to return a specific record number |  <http://services.odata.org/V2/Northwind/Northwind.svc/Products(22)?$format=json>
`$orderby=<EntityType>` to return records sorted by a field other than the key |  <http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$top=2&$orderby=ProductName>
`$orderby=<EntityType> desc` to return records sorted by a field other than the key in descending order | <http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$top=2&$orderby=ProductName%20desc>
`$filter` to return only records that satisfy a single logical test |  <http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$orderby=ProductName&$filter=Discontinued%20eq%20true>
`$filter` to return records that satisfy two logical tests |  <http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$orderby=ProductName&$filter=Discontinued%20eq%20false%20and%20UnitPrice%20gt%20100>
`$select` to define a subset of fields to return |  <http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$select=ProductName,UnitPrice,Supplier>
`$select` to define a subset of fields to return and `$expand` to include a `NavigationProperty` linked entity in the results |  <http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$select=ProductName,UnitPrice,Supplier&$expand=Supplier>
A combination example with `$format`, `$select`, `$expand` and `$filter` |  <http://services.odata.org/V2/Northwind/Northwind.svc/Products?$format=json&$select=ProductName,UnitPrice,Supplier&$expand=Supplier&$filter=startswith(Supplier/CompanyName,%20%27Grand%27)>

### Make Your Own App

If you would like to build an app similar to what you have done in this tutorial series but with SAP-like data, you can register for a free SAP Gateway trial. See the [Create an Account on the Gateway Demo System](gateway-demo-signup) tutorial.

The two OData service document URLs are:

- `https://sapes5.sapdevcenter.com/sap/opu/odata/IWBEP/GWDEMO/`
- `https://sapes5.sapdevcenter.com/sap/opu/odata/IWFND/RMTSAMPLEFLIGHT/`

To build an app like what you have now, but with data from SAP Gateway, you simply need to:

- Create an SAP Cloud Platform destination pointing to `https://sapes5.sapdevcenter.com` following an [earlier tutorial procedure](hcp-create-destination)
- Enter the remaining part of the URL in the Data Connection portion of the SAP Web IDE template customization. For the two URLs above, they would be:
- `/sap/opu/odata/IWBEP/GWDEMO`
- `/sap/opu/odata/IWFND/RMTSAMPLEFLIGHT`
