---
title: SAP HANA XS Advanced, Creating an HDI Module
description: Part 3 of 3, Create your first HDI module for database content within your XSA application
tags: [products>sap-hana, products>sap-hana,-express-edition, topic>big-data, tutorial>beginner ]

---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [SAP HANA XS Advanced Creating an HTML5 Module](http://go.sap.com/developer/tutorials/xsa-html5-module.html)

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://go.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://go.sap.com/developer/tutorials.html)


## Details
### You will learn  
You will now create the HTML5 module to add basic web based content to your XSA application.


### Time to Complete
**20 Min**.

---

New to HANA in SPS 11 is the HANA Deployment Infrastructure or HDI. The goal of HDI is to manage database artifacts from design time objects but in a way that allows multiple copies/versions of the same core objects to be used on the same HANA database at the same time.

HDI introduces the concept of the container as an abstraction of the Schema. The container in turn dynamically generates the Schema, a container-specific Database User who owns all objects, and a password for that database user. XS Advanced based services then only need access to the container and never need to know the actual Schema, technical user, or password. All of that information is stored within the container definition. 

1. Begin by selecting your project and then choosing `New -> HDB Module`

    ![New Module](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/1.png)

2. Name this new module `db`. Then press Next. 
    
    ![Create module](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/2.png)
    
3. Although the namespace for database artifacts is optional in HDI input the namespace `dev602`. Press Next.

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/3.png)
    
4. Press Finish to complete the creation of the HDB Module. 

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/4.png)
    
5. The wizard has created the `db` folder as well as the `hdi-container` resource and the `db` module in the `mta.yaml` file for you. 

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/5.png)
    
6. You will be able to see some of the additional files that the module creation wizard created if you choose `View->Show Hidden Files`  

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/6.png)
    
7. The `db/src` folder is where your actual database development objects belong. There are two configuration files in the root of this folder. The `.hdiconfig` file maps the file extensions to the specific server side activation plug-ins. This way you can choose any file extensions you wish to use as long as you map them to the correct plug-ins. However we will use the default mappings for now. 

	The `.hdinamespace` file configures the package namespace for your development objects. As we no longer use the HANA Repository to hold design time objects, this file provides the same service as the folder structure in the Repository used to. 

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/7.png)
    
8. In the `src` folder we will create several development objects. First create a folder called data in the `src` folder. 

	Then `New->CDS Artifact` to create the core database tables and views in our application. 

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/8.png)	

9. Name the new CDS file `PurchaseOrder` and press Create
	
    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/9.png)	
	
10. `hdbcds` is the new file extension replacing `hdbdd`. It contains the table and view definitions. We are creating a simple Purchase Order Header and Item data model. 

	The syntax is the same as `CDS-based` development objects previously.

	```
	namespace dev602.data;
	
	context PurchaseOrder {
	type BusinessKey : String(10);
		type SDate : LocalDate;
		type CurrencyT : String(5);
		type AmountT : Decimal(15,2);
		type QuantityT : Decimal(13,3);
		type UnitT: String(3);
		type StatusT: String(1);
		
	 Type HistoryT {
	          CREATEDBY : BusinessKey;
	          CREATEDAT : SDate;
	          CHANGEDBY : BusinessKey;
	          CHANGEDAT : SDate; 
	        };
	
	    Entity Header {
	        key  PURCHASEORDERID: BusinessKey;
	        ITEMS: Association[*] to Item on ITEMS.PURCHASEORDERID = PURCHASEORDERID;        
	        HISTORY: HistoryT;
	        NOTEID: BusinessKey null;
	        PARTNER: BusinessKey;
	        CURRENCY: CurrencyT;
	        GROSSAMOUNT: AmountT;
	        NETAMOUNT: AmountT;
	        TAXAMOUNT: AmountT;
	        LIFECYCLESTATUS: StatusT;
	        APPROVALSTATUS: StatusT;
	        CONFIRMSTATUS: StatusT;
	        ORDERINGSTATUS: StatusT;
	        INVOICINGSTATUS: StatusT;
	      } technical configuration {
	          column store;
	      };
	
	    Entity Item {
	        key  PURCHASEORDERID: BusinessKey;
	        key  PURCHASEORDERITEM: BusinessKey;
	        HEADER: Association[1] to Header on HEADER.PURCHASEORDERID = PURCHASEORDERID; 
	        PRODUCT:  BusinessKey;
	        NOTEID: BusinessKey null;
	        CURRENCY: CurrencyT;
	        GROSSAMOUNT: AmountT;
	        NETAMOUNT: AmountT;
	        TAXAMOUNT: AmountT;
	        QUANTITY: QuantityT;
	        QUANTITYUNIT: UnitT;
	        DELIVERYDATE: SDate;
	      } technical configuration {
	          column store;
	      };
	
	      
	   define view ItemView as SELECT from Item {
	      PURCHASEORDERID as "PurchaseOrderItemId", 
	      PURCHASEORDERITEM as "ItemPos",
	      HEADER.PARTNER as "PartnerId",
	      PRODUCT as "ProductID",
	      CURRENCY as "CurrencyCode",
	      GROSSAMOUNT as "Amount",
	      NETAMOUNT as "NetAmount",
	      TAXAMOUNT as "TaxAmount",
	      QUANTITY as "Quantity",
	      QUANTITYUNIT as "QuantityUnit",
	      DELIVERYDATE as "DeliveryDate1"
	   } with structured privilege check; 
	};
	```	

11. Look at the syntax you just entered into the `hdbcds` file in more detail. 

	First you need to define some reusable elemental types. These will later be used to define the data type of individual columns in our tables. Within the `PurchaseOrder` context, create element types for `BusinessKey`, `SDate`, `CurrencyT`, `AmountT`, `QuantityT`, `UnitT`, and `StatusT`.
	
	```
	context PurchaseOrder {
	type BusinessKey : String(10);
		type SDate : LocalDate;
		type CurrencyT : String(5);
		type AmountT : Decimal(15,2);
		type QuantityT : Decimal(13,3);
		type UnitT: String(3);
		type StatusT: String(1);
	```
	
12. You can also create reusable structures with multiple fields. This is useful when the same sets of fields are repeated in multiple tables. Create a reusable structure for History – with `CREATEDBY`, `CREATEDAT`, `CHANGEDBY`, and `CHANGEDAT` fields.
	

	```
	Type HistoryT {
          CREATEDBY : BusinessKey;
          CREATEDAT : SDate;
          CHANGEDBY : BusinessKey;
          CHANGEDAT : SDate; 
        };
	```

13. The syntax for creating Entities is similar to types. Entities will become database tables when activating the `hdbcds` file.

	```
	Entity Header {
        key  PURCHASEORDERID: BusinessKey;
        ITEMS: Association[*] to Item on ITEMS.PURCHASEORDERID = PURCHASEORDERID;        
        HISTORY: HistoryT;
        NOTEID: BusinessKey null;
        PARTNER: BusinessKey;
        CURRENCY: CurrencyT;
        GROSSAMOUNT: AmountT;
        NETAMOUNT: AmountT;
        TAXAMOUNT: AmountT;
        LIFECYCLESTATUS: StatusT;
        APPROVALSTATUS: StatusT;
        CONFIRMSTATUS: StatusT;
        ORDERINGSTATUS: StatusT;
        INVOICINGSTATUS: StatusT;
      } technical configuration {
          column store;
      };
	``` 
	
14. With the tables we created, you use a unique order id number as the primary key. Therefore you need a sequence to have an auto incrementing unique id generated when new data is inserted. Create a new sequence by right-clicking on the data folder and choosing “New”, then “File”.

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/10.png)

15. Enter the name of the file as `orderId.hdbsequence`. Click “OK”.

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/11.png)

16. Create a non-cycling sequence within your schema starting from 200000000 and ending with 299999999. Make it dependent upon your header table with the full package id on the front of the header table name. Save your sequence file.

	```
	SEQUENCE "dev602.data::orderId" 
	INCREMENT BY 1 START WITH 200000000 
	MINVALUE 1 MAXVALUE 2999999999
	NO CYCLE
	RESET BY
	SELECT IFNULL(MAX(PURCHASEORDERID), 0)+1 
	FROM "dev602.data::PurchaseOrder.Header"
	```

17. You may want to deliver an initial set of data within a table – particular a configuration table. In this exercises we will learn how to create automatic data load configuration and the accompanying `CSV` files for just such a situation. 

	The data load for table requires two files – 1. An `csv` (comma separated) file which holds the data you want to load. 2. An `hdbtabledata` file which specifies the target table for a source `csv` file.

18. You also have the `hdbtabledata` development object. This is the replacement for the old `hdbti` development object. Although the syntax of this object is new, the purpose is the same – to allow the loading of initial data from `CSV` files it target tables during their creation. 

	Create a file named `Purchase.hdbtabledata` and enter this text into it. Don’t forget to save the file afterwards. 

	```
	{
	"format_version": 1,
	"imports": [{
	"target_table": "dev602.data::PurchaseOrder.Header",
	"source_data": {
	“data_type": "CSV",
		"file_name": "dev602.data::header.csv",
		"has_header": false,
		"dialect": "HANA",
		"type_config": {
		"delimiter": ","
		}
	},
	"import_settings": {
	"import_columns": [
	"PURCHASEORDERID",
		"NOTEID",
		"PARTNER",
		"CURRENCY",
		"GROSSAMOUNT",
		"NETAMOUNT",
		"TAXAMOUNT",
		"LIFECYCLESTATUS",
		"APPROVALSTATUS",
		"CONFIRMSTATUS",
		"ORDERINGSTATUS",
	
	"INVOICINGSTATUS"]
	},
	"column_mappings": {
		"PURCHASEORDERID": 1,
		"NOTEID": 6,
		"PARTNER": 7,
		"CURRENCY": 8,
		"GROSSAMOUNT": 9,
		"NETAMOUNT": 10,
		"TAXAMOUNT": 11,
		"LIFECYCLESTATUS": 12,
		"APPROVALSTATUS": 13,
		"CONFIRMSTATUS": 14,
		"ORDERINGSTATUS": 15,
		"INVOICINGSTATUS": 16
		}
	},
	{
	"target_table": "dev602.data::PurchaseOrder.Item",
	"source_data": {
		"data_type": "CSV",
		"file_name": "dev602.data::item.csv",
		"has_header": false,
		"dialect": "HANA",
		"type_config": {
		"delimiter": ","
		}
	},
	"import_settings": {
	"import_columns": [
	"PURCHASEORDERID",
		"PURCHASEORDERITEM",
		"PRODUCT",
		"NOTEID",
		"CURRENCY",
		"GROSSAMOUNT",
		"NETAMOUNT",
		"TAXAMOUNT",
		"QUANTITY",
		"QUANTITYUNIT" ]
		},
	"column_mappings": {
		"PURCHASEORDERID": 1,
		"PURCHASEORDERITEM": 2,
		"PRODUCT": 3,
		"NOTEID": 4,
		"CURRENCY": 5,
		"GROSSAMOUNT": 6,
		"NETAMOUNT": 7,
		"TAXAMOUNT": 8,
		"QUANTITY": 9,
		"QUANTITYUNIT": 10
		}
	}]
	}
	```

19. You need some `CSV` files to hold some initial test data to be loaded by the `hdbtabledata` configuration file. Enter this data into a file named `header.csv` and save it.

	```
	0500000000,0000000033,20120101,0000000033,20120101,9000000001,0100000000,EUR,13224.47,11113,2111.47,N,I,I,I,I
	0500000001,0000000033,20120102,0000000033,20120102,9000000001,0100000002,EUR,12493.73,10498.94,1994.79,N,I,I,I,I
	```

20. And data for the item table named `item.csv`.  Don’t forget to save. 

	```
	0500000000,0000000010,HT-1000,,EUR,1137.64,956,181.64,1,EA,20121204
	0500000000,0000000020,HT-1091,,EUR,61.88,52,9.88,2,EA,20121204
	0500000000,0000000030,HT-6100,,EUR,1116.22,938,178.22,2,EA,20121204
	0500000000,0000000040,HT-1000,,EUR,2275.28,1912,363.28,2,EA,20121204
	0500000000,0000000050,HT-1091,,EUR,92.82,78,14.82,3,EA,20121204
	0500000000,0000000060,HT-6100,,EUR,1116.22,938,178.22,2,EA,20121204
	0500000000,0000000070,HT-1000,,EUR,2275.28,1912,363.28,2,EA,20121204
	0500000000,0000000080,HT-1091,,EUR,61.88,52,9.88,2,EA,20121204
	0500000000,0000000090,HT-6100,,EUR,1674.33,1407,267.33,3,EA,20121204
	0500000000,0000000100,HT-1000,,EUR,3412.92,2868,544.92,3,EA,20121204
	0500000001,0000000010,HT-1100,,USD,213.96,179.8,34.16,2,EA,20121204
	0500000001,0000000020,HT-2026,,USD,35.69,29.99,5.7,1,EA,20121204
	0500000001,0000000030,HT-1002,,USD,3736.6,3140,596.6,2,EA,20121204
	0500000001,0000000040,HT-1100,,USD,213.96,179.8,34.16,2,EA,20121204
	0500000001,0000000050,HT-2026,,USD,71.38,59.98,11.4,2,EA,20121204
	0500000001,0000000060,HT-1002,,USD,3736.6,3140,596.6,2,EA,20121204
	0500000001,0000000070,HT-1100,,USD,320.94,269.7,51.24,3,EA,20121204
	0500000001,0000000080,HT-2026,,USD,107.06,89.97,17.09,3,EA,20121204
	0500000001,0000000090,HT-1002,,USD,3736.6,3140,596.6,2,EA,20121204
	0500000001,0000000100,HT-1100,,USD,320.94,269.7,51.24,3,EA,20121204
	```
21. You also need synonyms now to access any table or view outside of our container.  Therefore you will create an `hdbsynonym` to allow us to access the dummy view. Enter this code into the file `general.hdbsynonym` and save.

	```
	{
		"dev602.data::DUMMY" : {
			"target" : {
				"schema" : "SYS",
				"object" : "DUMMY"
			}
		}	
	}
	```

22. Create a procedures folder in the `src` folder. In the procedures folder you can create an `hdbprocedure` file via `New->Procedure`

	>The syntax for stored procedures hasn’t changed from previous levels of HANA.

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/12.png)

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/13.png)

23. Here is the source of `getPOItems.hdbprocedure`

	```
	PROCEDURE "dev602.procedures::getPOItems" ( 
	   OUT ex_addresses "dev602.data::PurchaseOrder.Item" )
	   LANGUAGE SQLSCRIPT
	   SQL SECURITY INVOKER
	   --DEFAULT SCHEMA <default_schema_name>
	   READS SQL DATA AS
	BEGIN
	   /*************************************
	       Write your procedure logic 
	   *************************************/
	   ex_addresses = 
	     select *
	              from "dev602.data::PurchaseOrder.Item";
	END
	```

24. All access to our HDI database objects from XSA is done automatically by the HDI container technical user. However if you want to allow access via other database users (for use cases such as external reporting tools) you must create a database role. Create another folder called roles.

25. First you need to create a structured privilege. This is the logical successor to the analytic privilege and allows us to perform instance filtering for our `CDS` view you created earlier. Enter this code into the file `PurchaseOrder.hdbstructuredprivilege` and save.

	This will limit the access to your view to only allow users with this privilege to see items for Euros. 

	```
	STRUCTURED PRIVILEGE 
	    "dev602.roles::PO_VIEW_PRIVILEGE"
	    FOR SELECT ON 
	    "dev602.data::PurchaseOrder.ItemView"
	    WHERE "CurrencyCode" = 'EUR'
	```

26. Now create a role named `dev602.hdbrole` and enter this code. Don’t forget to save.

	```
	{
	"role":{
	"name": "dev602.roles::dev602",
		"object_privileges":[
		{
		"name": "dev602.data::PurchaseOrder.Header",
		"type": "TABLE",
		"privileges": [ "SELECT" ]
		},
		{
		"name": "dev602.data::PurchaseOrder.Item",
		"type": "TABLE",
		"privileges": [ "SELECT" ]
		},
		{
		"name": "dev602.procedures::getPOItems",
		"type": "PROCEDURE",
		"privileges": [ "EXECUTE" ]
		},
		{
		"name": "dev602.data::PurchaseOrder.ItemView",
		"type": "VIEW",
		"privileges": [ "SELECT" ]
		}		
		],
		"schema_analytic_privileges": [
	            {
	             "privileges":[ "dev602.roles::PO_VIEW_PRIVILEGE" ]
	            }
	        ]
	}
	}
	```

27. Now that you have your database development objects, you are ready to build the module which will create them in the HANA database. This process technically executes a `node.js` application which will call over to HANA and deploy these database artifacts into their container. Right mouse click on the `db` folder and choose Build.

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/14.png)

28. Similar to the run activity of the web module earlier; the status of the build will be displayed in a window in the lower right side of the IDE. If everything worked correctly, you should see that the build completed successfully.

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/15.png)

29. There is a new database tool called the HANA Runtime Tools (or `HRTT`) that can be used to view and interact with HDI created content. Open a new browser tab and navigate to `http://vhcalhdb:51006` to access this tool.

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/16.png)

30. You need to bind to our container in order to view the database objects in it. Click on the Search HDI Containers button. 

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/17.png)

31. Choose the container in the list that begins with your user name and end with your project name and then `hdi-container`. The string in the middle is the generated ID of your workspace. Click the Bind button. 

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/18.png)

32. You now have a folder for your container which you can expand to see the various catalog objects you just created. 

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/19.png)
    
33. You can view the definition of the objects

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/20.png)
    
34. And you can query the data in these objects. 

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/21.png)

35. If you try to access the data in the view, however, even the technical user of the container doesn’t have access because of the structured privilege we placed upon it. 

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/22.png)

36. In order to access the structure privilege based view or for a database user other than the technical user to access any of the catalog objects they need the database role granted to their user.  

    ![Login](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/xsa-hdi-module/23.png)


## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://go.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://go.sap.com/developer/tutorials.html)
