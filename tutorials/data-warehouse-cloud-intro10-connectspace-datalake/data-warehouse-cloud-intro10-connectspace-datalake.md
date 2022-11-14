---
parser: v2
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-data-warehouse-cloud]
primary_tag: products>sap-data-warehouse-cloud
---

# Connect Your Space to an SAP HANA Cloud, Data Lake
<!-- description --> Connect your SAP Data Warehouse Cloud Space to the SAP HANA Cloud, data lake to gain more storage to handle petabytes of data.

## Prerequisites
 - You have [familiarised yourself with the SAP Data Warehouse Cloud interface.](data-warehouse-cloud-2-interface)
 - You have [created at least one Space in SAP Data Warehouse Cloud.](data-warehouse-cloud-4-spaces)

## You will learn
  - How to connect your SAP Data Warehouse Cloud Space to the SAP HANA Cloud, data lake

## Intro
To get familiar with the SAP HANA Cloud, data lake, please see the [SAP HANA Cloud product page.](https://www.sap.com/products/hana/cloud.html) You can also discover the data lake capabilities in detail with this [SAP HANA Cloud, data lake mission.](mission.hana-cloud-data-lake-get-started)

To use a data lake associated with SAP Data Warehouse Cloud tenant, you need to open a support ticket. Please see more details here in the [technical documentation.](https://help.sap.com/viewer/9f804b8efa8043539289f42f372c4862/cloud/en-US/93d0b5d4faa24777a4b78513f7ed6172.html?q=data%20lake)

If you are accessing the data lake via SAP Data Warehouse Cloud, you can only use a single user in the SAP HANA Cloud, data lake.


---

### Connect your Space to SAP HANA Cloud, data lake


You can access tables in the data lake via virtual tables in the open SQL schema in SAP Data Warehouse Cloud, once the data lake is connected to your SAP Data Warehouse Cloud tenant. These tables can also be consumed in the Data Builder.

> DDL statements in your data lake are currently not audited.

In order to connect your Space to the data lake, follow the steps below:

1. Go to **Space Management** and select the Space that you want to give access to data lake. Alternatively, you could create a dedicated Space for the data lake.

2. Under **Storage Assignment**, select the check box Use this Space to access the data lake. If another Space already has access to the data lake, you won't be able to assign it to your Space.

3. Click Save.

You can now use your preferred SQL tool to create tables in data lake and access these tables as virtual tables in your open SQL schema.


### Create and access your virtual tables in data lake


1. SAP Data Warehouse Cloud offers two stored procedures that you can use to easily create and access the tables. For more information and examples on the stored procedures, see the [technical documentation.](https://help.sap.com/viewer/9f804b8efa8043539289f42f372c4862/cloud/en-US/12b6825ac6d34db9902460f665cfcb88.html)

2. In your SQL tool, use the `"DWC_GLOBAL"."DATA_LAKE_EXECUTE" ( IN STMT NCLOB )` stored procedure in the schema `DWC_GLOBAL` to create tables in data lake. Please note, that the statements issued via this procedure are not audited.

3. Then create virtual tables in your open SQL schema that refer to the tables in data lake. The virtual tables are used to query data stored in data lake.




### Create virtual tables using the data builder


Use the following procedure to create a virtual table in your open SQL schema:

```SQL
"DWC_GLOBAL"."DATA_LAKE_CREATE_VIRTUAL_TABLE"
(IN VIRTUAL_TABLE_NAME NVARCHAR(256), IN DATA_LAKE_TABLE_NAME NVARCHAR(256), IN TARGET_SCHEMA_SYS BOOLEAN DEFAULT false).
```

You can then work with your virtual tables in the **Data Builder** by selecting the open SQL schema used for the data lake.

> **Well done!**

> You have completed the Introduction to SAP Data Warehouse Cloud Administration tutorial group. Now it's time to continue learning. Please check out the other SAP Data Warehouse Cloud tutorials [available here](https://developers.sap.com/tutorial-navigator.html?tag=products:technology-platform/sap-data-warehouse-cloud), and don't forget to follow the SAP Data Warehouse Cloud tag in the SAP Community to hear about the most up to date product news.



### Test yourself




---
