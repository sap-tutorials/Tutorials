---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, products>sap-hana-cloud, software-product-function>sap-hana-cloud\,-sap-hana-database, products>sap-business-application-studio]
primary_tag: products>sap-hana-cloud
---

# Create Calculation View in SAP HANA Cloud
<!-- description --> Create your own calculation views in SAP HANA Cloud, SAP HANA database with SAP Business Application Studio.

## Prerequisites
 - You have [Set Up a BTP Account for Tutorials](group.btp-setup). Follow the instructions to get an account, and then to set up entitlements and service instances for the following BTP services.
    - **SAP Business Application Studio**
    - **SAP Analytics Cloud trial**

  ## You will learn
- How SAP HANA modeling is performed using SAP Business Application Studio
- How to create and use HDI containers


## Intro
In this tutorial, you will learn about the creation of a calculation view in SAP HANA Cloud. The created calculation view is later consumed in SAP Analytics Cloud using Live Data Connection to create a story for detailed data analysis.


---



### Start SAP Business Application Studio and create Dev Space

1.  Login to your BTP trial subaccount, navigate to subscriptions and select SAP Business Application Studio.

    <!-- border -->![SAP Business Application Studio](step1-I1.PNG)

    This will open the dev spaces.

2.  Click **Create Dev Space**.

    <!-- border -->![DevSpace Creation](step2-I2.PNG)

3.  Select SAP HANA Native Application for the Dev Space type and specify a name.

    We have provided the name as `HANADevSpace`.

    <!-- border -->![DevSpace Creation](step3-I3.PNG)

4. Click **Create Dev Space**.

    In a minute, the Dev Space will get created. You need to start the Dev Space, which will by default be in stopped state.

    <!-- border -->![DevSpace Starting](step4-I4.PNG)

    After starting, the state of the Dev Space changes to Running.

    <!-- border -->![DevSpace Starting](step5-I5.PNG)


### Launch the studio and log into Cloud Foundry

1. Click on the Dev Space name to launch SAP Business Application Studio.

    <!-- border -->![BAS Starting](step2-I3.png)

    After launching SAP Business Application Studio, you need to log in to Cloud Foundry.

2. To log in, click **View > find command** and enter `CF`. From the available options, select **Login to Cloud Foundry**.

    <!-- border -->![CF Login](step2-I4.png)

3. Specify your Cloud Foundry endpoint.

    <!-- border -->![CF Login](step2-I5.png)

4. Enter your e-mail address.

    <!-- border -->![CF Login](step2-I6.png)

5. Select the space from the CF Subaccount. This will set the CF Organization and Space for the application we are building

    <!-- border -->![CF Login](step2-I7.png)

6. A confirmation message will be shown at the bottom of the screen.

    <!-- border -->![CF Login](step2-I8.png)


### Create SAP HANA database project

You will start the creation of the project now.

1. Select **File > Project from Template**, which shows the list of available templates to create applications using BAS.

    <!-- border -->![HANA Project Creation](step3-I4.png)

2. For this scenario, select SAP HANA Database project and then click **Start**.

    <!-- border -->![HANA Project Creation](step3-I5.png)

3. Specify a name to the project

    <!-- border -->![HANA Project Creation](step3-I6.png)

4. Leave the basic properties as is (for example, module name is `db`).

    <!-- border -->![HANA Project Creation](step3-I7.png)

5. Leave the namespace and schema fields empty. Check 'Yes' for the binding the database module to a Cloud Foundry service instance property.

    <!-- border -->![HANA Project Creation](step3-I8.png)

6. Leave default values for HDI Container service and then click **Finish**.

    <!-- border -->![HANA Project Creation](step3-I9.png)

    The SAP HANA database project generation will start.

    <!-- border -->![HANA Project Creation](step3-I10.png)

    Now you will be able to see the project in the Explorer section of SAP Business Application Studio.

    <!-- border -->![HANA Project Creation](step3-I11.png)


### Create SAP HANA tables

1. Click **View > find command**, type `HANA` and then select the option **Create SAP HANA Database Artifact**.

    <!-- border -->![HANA Table Creation](step4-I5.png)

2. Select the source folder in the project to create the artifact and choose the artifact type as `Table (hdbtable)`.

    Name the table `customers`.

    <!-- border -->![HANA Table Creation](step4-I6.png)

3. Include the following code for the table definition:

    ```SQL
    COLUMN TABLE "customers"(custid integer, custname varchar(40), custcountry varchar(3), custregion varchar(4));
    ```

    <!-- border -->![HANA Table Creation](step4-I7.png)

4. Click ![HANA Table Creation](step4-I10.png) button on the top-right corner to deploy the table.

    <!-- border -->![HANA Table Creation](step4-I8.png)

5. Similarly, you can create another table called `sales` by simply right-clicking the source folder and selecting the option new file.

    Enter the name `sales.hdbtable`.

    <!-- border -->![HANA Table Creation](step4-I9.png)

6. Add the following definition to the sales.hdbtable file.

    ```SQL
    COLUMN TABLE "sales" ( sid integer, pid varchar(5), sdate date, samt integer, custid integer )
    ```

7. Click <!-- border -->![HANA Table Creation](step4-I10.png)  button on the top-right corner to deploy the sales table.

  <!-- border -->![HANA Table Creation](step4-I11.png)


### Insert data into tables


1. Click the SAP HANA projects in explorer, then click  the database explorer ![HANA Table Creation](step4-I12.png) icon.

    <!-- border -->![HANA Table Creation](step5-I6.png)

    This opens SAP HANA Database Explorer in a new window where you can see the details of the HDI container.

    <!-- border -->![HANA Table Creation](step5-I7.png)

3. Select the tables folder in the HDI Container to see the customers and sales tables that you created.

    <!-- border -->![HANA Table Creation](step5-I8.png)

4. Click the customers table. This shows the table definition.

    <!-- border -->![HANA Table Creation](step5-I9.png)

5. Click open data and then click ![HANA Table Creation](step5-I10.png) to insert a few sample records into the table.

    <!-- border -->![HANA Table Creation](step5-I11.png)

6. Follow similar steps to insert a few sample records into the sales table.

    <!-- border -->![HANA Table Creation](step5-I12.png)

Now you can go back to the project in SAP Business Application Studio to create a CUBE calculation view.


### Create calculation view


1. Select the source folder in the project to create the calculation view and choose the artifact type as Calculation View.

    Then, specify for the name `customerwisesales`, and then click **Create**.

    <!-- border -->![HANA CV Creation](step6-I1.png)

2. Open the file `customerwisesales.hdbcalculationview`.

    <!-- border -->![HANA CV Creation](step6-I2.png)

3. Add a Join node into the canvas.

    <!-- border -->![HANA CV Creation](step6-I3.png)

4. Click the following icon:

    <!-- border -->![HANA CV Creation](step6-I4.png)

    Search for customers and sales tables and add them to the join node.

    <!-- border -->![HANA CV Creation](step6-I5.png)

    After adding both the tables, the join node looks like the following.

    <!-- border -->![HANA CV Creation](step6-I6.png)

6. Double-click the join node.

    <!-- border -->![HANA CV Creation](step6-I7.png)

7. Drag the `CUSTID` field of the customers table onto the `CUSTID` field of the sales table. This will add a join condition between the tables.

    <!-- border -->![HANA CV Creation](step6-I9.png)

8. Navigate to the mapping tab of the join node and map the fields from both customers and sales into output columns.

    <!-- border -->![HANA CV Creation](step6-I101.png)

9. Link the join node to the aggregation node by using the following button:

    ![HANA CV Creation](step6-I10.png)

    It looks like this.

    <!-- border -->![HANA CV Creation](step6-I11.png)

10. Double-click the aggregation node. Move all the columns from the join node to the output columns.

    <!-- border -->![HANA CV Creation](step6-I12.png)

11. Double-click the semantics node in the calculation view. Select `CUSTID` and `SID` fields and mark them as attributes.

    <!-- border -->![HANA CV Creation](step6-I21.png)

12. Maintain all the other semantics as needed.

13. Click the <!-- border -->![HANA Table Creation](step4-I10.png)  button to deploy the calculation view.

    <!-- border -->![HANA CV Creation](step6-I22.png)

15. Navigate to the database explorer. Select the HDI container and choose column view. This shows the calculation view.

    <!-- border -->![HANA CV Creation](step6-I23.png)

16. Select the calculation view and click **Open Data**. This presents the data from the calculation view.

    <!-- border -->![HANA CV Creation](step6-I24.png)

17. You can go to the **Analysis** tab to view the data in the form of graphs, charts and other elements.

    Here the country wise sales is shown in a donut chart.

    <!-- border -->![HANA CV Creation](step6-I25.png)


### Test yourself




---
