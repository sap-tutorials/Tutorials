---
parser: v2
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
primary_tag: products>sap-hana
tags: [  tutorial>intermediate, topic>html5, topic>odata, topic>sapui5, products>sap-hana, products>sap-hana\,-express-edition   ]
---
# Use OData Metadata to dynamically create the columns
<!-- description --> Use OData Metadata to dynamically create the columns

## Prerequisites  
- This tutorial is designed for SAP HANA on premise and SAP HANA, express edition. It is not designed for SAP HANA Cloud.
- **Proficiency:** Intermediate
- **Tutorials:** [Consume a Basic OData Service](https://developers.sap.com/tutorials/xsa-sapui5-odata.html)

## Next Steps
- [Consume an OData Service with Create Option](https://developers.sap.com/tutorials/xsa-sapui5-consume.html)

## You will learn  
Use the Metadata to dynamically create columns in your table.

## Time to Complete
**10 Min**.

---


### Check the metadata from the service


In the previous tutorial, you hard-coded the columns to be displayed from your OData service. However, OData services expose all their meta data and we can use this feature to build the columns dynamically. You can test this by running the web module and calling the OData service and adding `$metadata`

![view file](1.png)

You can see the service exposes the names of the fields and other properties such as the length.


### Adapt the controller


Return to the controller of `odataBasic` from the previous tutorial and replace the following lines:

![view file](2.png)

With the following code:

```javascript
function fnLoadMetadata() {
	try {
		oTable.setModel(bpModel);
		oTable.setEntitySet("BusinessPartners");
		var oMeta = bpModel.getServiceMetadata();
		var headerFields = "";
		for (var i = 0; i < oMeta.dataServices.schema[0].entityType[0].property.length; i++) {
			var property = oMeta.dataServices.schema[0].entityType[0].property[i];
			headerFields += property.name + ",";
		}
		oTable.setInitiallyVisibleFields(headerFields);
	} catch (e) {
		console.log(e.toString());
		oDataFailed();
	}
}
bpModel.attachMetadataLoaded(bpModel, function () {
	fnLoadMetadata();
});
fnLoadMetadata();
```

You can see the new function that retrieves the metadata from the service, loops at the results and concatenates the names of the fields separated by a comma. The fields are  then attached to the `bpModel` component.


### Save and run


Save the files you have modified and run the web module:

![view file](3.png)

