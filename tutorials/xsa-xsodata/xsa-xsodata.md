---
title: Creating a Simple OData Service
description: Creating a Simple OData Service
tags: [  tutorial>intermediate, topic>odata, products>sap-hana, products>sap-hana\,-express-edition ]
---
## Prerequisites  
 - **Proficiency:** Intermediate
 - **Tutorials:** [SAP HANA XS Advanced, Creating a Node.js Module](http://www.sap.com/developer/tutorials/xsa-xsjs-xsodata.html)

## Next Steps
 - [Create and OData Service with Entity Relationship](http://www.sap.com/developer/tutorials/xsa-xsodata-entity.html)

## Details
### You will learn  
Create a simple OData service connecting to your table and data.

### Time to Complete
**10 Min**.

---

1. Right mouse click on the `js/lib/xsodata` folder and choose `New->File`. 

	![New file](1.png)
	
2. Enter the name as `businessPartners.xsodata` and click "OK".

	![file name](2.png)

3. You want to define an OData service to expose the business partner table. The syntax of the XSODATA service is relative easy for this use case. You need only define a namespace (your package path), the name of the HANA Table you will base the service from (`dev602.data::MD.BusinessPartner`) and the name of the OData entity (`BusinessPartners`). Therefore the content of the XSODATA file would be. Note: if you don't want to type this code, we recommend that you cut and paste it from this web address `http://<hostname>:51013/workshop/admin/ui/exerciseMaster/?workshop=dev602&sub=ex3_10`
	
	![odata service](3.png)
	
4. Save the file. Run the Node.js module. You will receive the unauthorized message as expected. Then run the `html5` module (defined as `web` earlier). Change the URL path to `/xsodata/businessPartners.xsodata` to test this new service. The resulting document describes the service entities.  You have the one entity named `BusinessPartners`. 

	![save file](4.png)

5. You can now adjust the URL slightly and add the `/$metadata` parameter to the end of it. 
	
	For Example: `/xsodata/businessPartners.xsodata/$metadata`
	
	You can see the field descriptions for all the attributes of the OData service.

	![access metadata](5.png)
	
6. In order to view the data of the entity, you would append `BusinessPartners` to the end of the URL:	For Example:	`/xodata/businessPartners.xsodata/BusinessPartners?$format=json`	You are now able to see the data from the `businessPartner` table.  	![Business Partner data](6.png)
7. You can also experiment with standard OData URL parameters like $top, $skip, or $filter.  These options are interpreted and handled by the OData service of the `XSEngine` for you.  You get complex service handling without any coding. For example the following URL would return only three business partner records and would skip the first five records.  Such parameters are helpful when implementing server side scrolling, filtering, or sorting in table UI elements.	For Example:	`/xsodata/businessPartners.xsodata/BusinessPartners?$top=3&$skip=5&$format=json`
	![output](7.png) 

## Next Steps
 - [Create and OData Service with Entity Relationship](http://www.sap.com/developer/tutorials/xsa-xsodata-entity.html)
