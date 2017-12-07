---
title: Use oData Metadata to dynamically create the columns
description: Use oData Metadata to dynamically create the columns
primary_tag: products>sap-hana
tags: [  tutorial>intermediate, topic>html5, topic>odata, topic>sapui5, products>sap-hana, products>sap-hana\,-express-edition   ]
---
## Prerequisites  
- **Proficiency:** Intermediate
- **Tutorials:** [Consume a Basic OData Service](http://www.sap.com/developer/tutorials/xsa-sapui5-odata.html)

## Next Steps
- [Consume an OData Service with Create Option](http://www.sap.com/developer/tutorials/xsa-sapui5-consume.html)

## Details
### You will learn  
Use the Metadata to dynamically create columns in your table.


### Time to Complete
**10 Min**.

---


[ACCORDION-BEGIN [Step 1: ](Check the metadata from the service)]

In the previous tutorial, you hard-coded the columns to be displayed from your OData service. However, OData services expose all their meta data and we can use this feature to build the columns dynamically. You can test this by running the web module and calling the OData service and adding `$metadata`

![view file](1.png)

You can see the service exposes the names of the fields and other properties such as the length.



[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Adapt the controller)]

Return to the controller and replace the following lines:

![view file](2.png)

With the following code:

```javascript

function fnLoadMetadata() {
	oTable.setModel(bpModel);
	oTable.setEntitySet("BP");
	var oMeta = bpModel.getServiceMetadata();
	var headerFields = "";
	for (var i = 0; i < oMeta.dataServices.schema[0].entityType[0].property.length; i++) {
	var property = oMeta.dataServices.schema[0].entityType[0].property[i];
	headerFields +=  property.name + ",";
	}
	oTable.setInitiallyVisibleFields(headerFields);
}
bpModel.attachMetadataLoaded(bpModel, function() {
	fnLoadMetadata();
 });

```

You can see the new function that retrieves the metadata from the service, loops at the results and concatenates the names of the fields separated by a comma. The fields are  then attached to the `bpModel` component.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Save and run)]

Save the files you have modified and run the web module:

![view file](3.png)



[ACCORDION-END]



## Next Steps
- [Consume an OData Service with Create Option](http://www.sap.com/developer/tutorials/xsa-sapui5-consume.html)
