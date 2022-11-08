---
parser: v2
auto_validation: true
time: 10
tags: [ tutorial>intermediate, products>sap-data-warehouse-cloud]
primary_tag: products>sap-data-warehouse-cloud
---

# Connect Microsoft Power BI to SAP Data Warehouse Cloud
<!-- description --> In this tutorial, you will learn how to connect your SAP Data Warehouse Cloud tenant to Microsoft Power BI.

## Prerequisites
  - You have installed the [ODBC Data Source Administrator (64-bit)](https://docs.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server?view=sql-server-ver15)
  - You have [installed an SAP HANA ODBC Driver](data-warehouse-cloud-bi4-install-odbc)
  - You have [created a database user](data-warehouse-cloud-intro8-create-databaseuser)


## You will learn
  - How to connect Microsoft Power BI to SAP Data Warehouse Cloud

---

### Add your IP address to IP allowlist


To allow an external BI client in your local network to connect to the database of SAP Data Warehouse Cloud, you need to add the external (public) IPv4 address of the client to an allowlist.

To do so, navigate to the **Configuration** screen on the bottom left of your SAP Data Warehouse Cloud tenant. Here, you can find the **IP Allowlist** tab, where you can simply click on **Add** and enter the IPv4 address of your system to add it to the allowlist.

  ![IP Allowlist](T05 7.png)


### Ensure entities in SAP Data Warehouse Cloud are consumable


In order to ensure the data entities and models created in your SAP Data Warehouse Cloud tenant are consumable by the BI tool of your choice, it is important to expose them for consumption.
If the entity was created in the data builder, simply click on the entity and set the **Expose for Consumption** toggle to **ON** as below.

  ![Consumable](T05 5.png)

If the entity was created in the business builder, the toggle shows up as a **Allow public data access** checkbox in it's properties tab. Simple click on the data entity to open it, and ensure the checkbox is ticked.

  ![Public Data Access](T05 6.png)



### Create an ODBC data source


The next step to connecting Microsoft Power BI to SAP Data Warehouse Cloud is creating an ODBC data source. This data source must point to the SAP Data Warehouse Cloud schema using the database user credentials you already have created. To create an ODBC data source, follow the steps below:

1. Open the ODBC Data Source Administrator (64 bit) application. Click on **Add** to add a new data source. Select and add the SAP HANA ODBC driver you have earlier installed. The driver is named **HDBODBC**.

    ![ODBC](T06 1.png)

2. On the configuration screen, enter the same connection credentials as that of your database user. Ensure that the **Multitenant** box is unchecked, and the **Connect to the database using TLS/SSL** box is checked.

    ![Connect](T06 2.png)

In case the driver cannot be found, re-install it from the [SAP HANA development tools website.](https://tools.eu1.hana.ondemand.com/#hanatools) The ODBC driver used in this example is version 2.4.177.



### Connect Microsoft PowerBI to SAP Data Warehouse Cloud


1.	With the data now ready to be imported, open the Microsoft Power BI application from your system, and click on the **Get Data** icon.

    ![Get Data](T07 1.png)

2. Select **ODBC** from the list of connections and click on **Connect**.

    ![ODBC](T07 2.png)

3.	Select the data source you have connected to ODBC from SAP Data Warehouse Cloud, and enter your database user credentials. Click on **Connect** once done.

4.	Your data now appears in the navigator. Select the data entities you wish to work with, and click on **Load**.

And that's it! The data is now available in Microsoft Power BI and can be used to create reports and charts for analysis.

  ![Charts](T07 3.png)

>**Well done!**

> You have completed the connection to BI tools tutorial group. Now it's time to continue learning. Please check out the other SAP Data Warehouse Cloud tutorials available here, and don't forget to follow the SAP Data Warehouse Cloud tag in the [SAP Community](http://community.sap.com) to hear about the most up to date product news.


### Test yourself




---
