---
title: Internet of Things (IoT) Viewing your Tessel data from IoT Services through SAP HANA XS
description: Part 10 of 10, Now connect your IoT Services to an SAP HANA XS shared instance and show the data using SAP HANA XS
tags: [products>sap-hana, products>sap-hana-cloud-platform, topic>big-data, topic>internet-of-things, tutorial>beginner ]

---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Internet of Things (IoT) Viewing your Tessel data from IoT Services](http://go.sap.com/developer/tutorials/iot-part9-hcp-services-viewdata.html)

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://go.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://go.sap.com/developer/tutorials.html)


## Details
### You will learn  
Now that your IoT Services are collecting data and you were able to view it your deployed Java application, now how about redirecting the data to a shared SAP HANA XS instance and making a small SAP HANA XSC application to show the data.  


### Time to Complete
**20 Min**.

---

1. To be on the safe side go ahead and stop your `iotmms` Java application.


2. From within the cockpit now choose Databases & Schemas and create a new SAP HANA XS Shared instance.

    ![HANA instances](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part10-hcp-services-hanaxs/1.png)

    ![HANA instances](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part10-hcp-services-hanaxs/2.png)
    
3. Under the first one, our `XXXXXXtrial.iotmms.web` you’ll notice we have an existing binding in place. We need to remove that one. Then we will need to add the new binding to our new HANA XS (shared) instance.

4. Once you have that complete you will need to start your “IOTMMS” Java Application and send some data through your device which will then generate the proper tables in your HANA XS (shared) instance ([see tutorial](http://go.sap.com/developer/tutorials/iot-part7-add-device.html)). So provided you received the “200” status in your messages then we should now have data in our tables and can begin working on our XS application.

    ![bindings](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part10-hcp-services-hanaxs/3.png)

5. Choose the “SAP HANA Web-based Development Workbench,” now right click on your package name, `XXXXXXXTrial`, and choose “New Application”. We’ll choose the “Blank Application” option and the “sub package” - `codejam.iotmmsxs`

    ![new application](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part10-hcp-services-hanaxs/4.png)

6. Now we’ll want to create a new file in this new sub package called `iotmmsxs` called `.xsprivileges` (remember the . in the front)

	```
	 	{
		 	"privileges": [
		 		{
					"name":"Basic",
		 			"description":"Basic IoT MMS privilege"
				}  
		  	]
		}
	```
	
7. Now switch to the catalog view to determine the next values you will need.

    ![catalog](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part10-hcp-services-hanaxs/5.png)
    
8. Now check under the `NEO_` schema, you will need to make a note of that specific schema name as you will need it in a moment. Under this schema you will find your tables. These are the tables created by the IoT Services for our devices.

    ![catalog tables](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part10-hcp-services-hanaxs/6.png)
    
9. Your table table `T_IOT_1_XXXXXXXXXXXXX` will be the other item you need to make note of for use in a moment. Now back in our other window the “Editor” should still be open and running and there we need to make another new file in our sub package `iotmmsxs`. This file will be called `iotaccess.hdbrole`.

	```
	 role <user_id>trial.codejam.iotmmsxs::iotaccess {
	 application privilege: <user_id>trial.codejam.iotmmsxs::Basic;
	 catalog schema "NEO_<schema_id>": SELECT;
	}
	```
	
	This will give a specific user read access to our table. As soon as you save it and it’s successfully activated that is. 

10. Now you will need to modify your `.xsaccess` file.

	```
	{
	 "exposed" : true ,
	 "authentication" : [{"method" : "Basic"}],
	 "authorization": ["<user_id>trial.codejam.iotmmsxs::Basic"]
	}
	```
	Save these changes. At this point go back over to the “Catalog” window and give your user access to the application. 

11. Within the “Catalog” across the top menu there is a button for “SQL” go ahead and click it. This will open an SQL console.

	```
	call "HCP"."HCP_GRANT_ROLE_TO_USER"('<user_id>trial.codejam.iotmmsxs::iotaccess', '<user_id>')
	```
	
	Here you will assign your user access to the role you just created.

12. Back in the “Editor” it’s time to select your sub package `iotmmsxs` and then add a new file. This will be `iotservice.xsodata`.
     
	```
	service {
	  "NEO_<schema_id>"."T_IOT_<table_postfix>" key generate local "GEN_ID";
	```
	
	So this file we can open in our web browser right now and have full access to all of the built in odata functionality.
	
	This will point you to OData service.
	
	```
	https://<system_id>hanaxs.hanatrial.ondemand.com/<user_id>trial/codejam/iotmmsxs/iotservice.xsodata
	```
	
	The next link shows OData service metadata
	
	```
	https://<system_id>hanaxs.hanatrial.ondemand.com/<user_id>trial/codejam/iotmmsxs/iotservice.xsodata/$metadata
	```
	
	The next link shows entity content in XML format
	
	```
	https://<system_id>hanaxs.hanatrial.ondemand.com/<user_id>trial/codejam/iotmmsxs/iotservice.xsodata/T_IOT_<table_postfix>
	```
	
	The next link shows entity content in JSON format
	
	```
	https://<system_id>hanaxs.hanatrial.ondemand.com/<user_id>trial/codejam/iotmmsxs/iotservice.xsodata/T_IOT_<table_postfix>?$format=json
	```

13. You will use this when modifying the `index.html` file. 

	![default page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part10-hcp-services-hanaxs/7.png)

14. You will replace the existing code with the following, which is quite a bit but should be easily readable and understandable as we are adding a table to a page. This is not the only way to do but that is a matter for you to explore and discover!

	```
	 <!DOCTYPE HTML>
	 <html>
	 <head>
		 <meta http-equiv="X-UA-Compatible" content="IE=edge" />
		 <meta charset="UTF-8"/>
		 <title>My Sensor Data</title>
		 <script id='sap-ui-bootstrap'
		 	src='/sap/ui5/1/resources/sap-ui-core.js'
		 	data-sap-ui-theme='sap_goldreflection'
		 	data-sap-ui-libs='sap.ui.core,sap.ui.commons,sap.ui.table'></script>
		 <script language="JavaScript">
			 var oModel = new sap.ui.model.odata.ODataModel("/<userid>trial/codejam/iotmmsxs/iotservice.xsodata/", false);
			 var arrayHeaders = new Array();
			 	oTable = new sap.ui.table.Table("test",{tableId: "tableID", visibleRowCount: 10});
			 //Bring the table onto the UI
			 oTable.placeAt("sensor_table");
			 //Table Column Definitions
			 var oMeta = oModel.getServiceMetadata();
			 var oControl;
			 for ( var i = 0; i < oMeta.dataServices.schema[0].entityType[0].property.length; i++) {
			 var property = oMeta.dataServices.schema[0].entityType[0].property[i];
			  oControl = new sap.ui.commons.TextField().bindProperty("value",property.name);
			  oTable.addColumn(new sap.ui.table.Column({label:new sap.ui.commons.Label({text: property.name}), template: oControl, sortProperty: property.name, filterProperty: property.name, filterOperator: sap.ui.model.FilterOperator.EQ, flexible: true, width: "125px" }));
			    }
			 oTable.setModel(oModel);
			 var sort1 = new sap.ui.model.Sorter("C_TIMESTAMP");
			 <body>
			oTable.bindRows("/<table name>",sort1);
		 </script>
	 </head>
	  <div id="sensor_table"/>
	 </body>
	 </html>
	 ```

	![new page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part10-hcp-services-hanaxs/8.png)

    
## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://go.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://go.sap.com/developer/tutorials.html)
