---
time: 20
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
tags: [ tutorial>beginner, products>sap-hana, products>sap-business-application-studio, software-product-function>sap-cloud-application-programming-model]
primary_tag: products>sap-hana-cloud
parser: v2
---

# Create Calculation View and Expose via CAP (SAP HANA Cloud)

<!-- description --> Learn how to combine HANA native artifacts, like calculation views, with SAP Cloud Application Programming Model (CAP)

## You will learn

- The basics of creating HANA Native Artifacts within a Cloud Application Programming Model project
- How to create SAP HANA calculation views
- How to integrate SAP HANA native artifacts, like calculation views, within the SAP Cloud Application Programming Model

## Prerequisites

- This tutorial is designed for SAP HANA Cloud. It is not designed for SAP HANA on premise or SAP HANA, express edition.
- You have created database artifacts and loaded data as explained in [the previous tutorial](hana-cloud-cap-create-database-cds).

## Video Version

Video tutorial version:

<iframe width="560" height="315" src="https://www.youtube.com/embed/Zb_9ccGXIRk" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

### Create calculation view

With the SAP Cloud Application Programming Model and its implementation of Core Data Services (CDS), we don't  directly import a Calculation View or other native DB artifact. This import, however, is important if you want to expose these artifacts via OData V4 services, since that requires a CDS entity or view.

But CDS does have an annotation called  `@cds.persistence.exists`. This annotation allows you to re-define an existing DB object and CDS won't attempt to create or alter it. It will just assume it already exists in the matching state.

This feature does require you to completely redefine the DB artifact with exactly the same name, columns and column names.

In this exercise, lets see how we can create a calculation view and other artifacts and expose them to CDS.

For more details on this topic, including advanced features such as parameters and quoted names, please refer to the online documentation here: [https://cap.cloud.sap/docs/advanced/hana#create-sap-hana-native-object](https://cap.cloud.sap/docs/advanced/hana#create-sap-hana-native-object)

Calculation Views and other HANA native artifacts allow you to leverage HANA specific features and optimizations that might not otherwise be available at the abstraction layers within the SAP Cloud Application Programming Model. Calculation Views are especially good at aggregation and filtering of large datasets. In this exercise will create a simple join Calculation View based upon or small data set and data model. This is done so we focus on the mechanics of combining HANA native with CAP without needing the typical large data set where the technical advantages of Calculation Views become more apparent.

1. Create a new **Calculation View** via **View > Command Pallette** and then **SAP HANA: Create SAP HANA Database Artifact** command pallet entry.

    ![New calculation view](new_db_artifact.png)

1. Create a calculation view called `V_INTERACTION` of Data Category **DIMENSION** and Dimension Type of **STANDARD**. Press **Create**

    ![Create calculation view](create_calc_view.png)

1. The new artifact is created in the `/db/src` folder alongside the `/gen` content created by CAP.  This way you can have a single HANA database model that contains both HANA native content and CAP generated content.

    ![View created in the /db/src folder](folder_view.png)

1. Click on the `V_INTERACTION.hdbcalculationview` to load the graphical calculation view editor.

    ![Calculation View editor](calc_view_editor.png)

### Model the join relationship

1. Drop a `join` node into the modeling space

    ![Join Node](join_node.png)

1. Use the ![plus sign](plus.png) sign to add tables to the node.

    ![Add data source](add_data_source.png)

1. Type in `HEADER` and then select the table you created earlier via CDS called `APP_INTERACTIONS_INTERACTIONS_HEADER` and press **Finish**.

    ![Add header table](header_table.png)

1. Repeat the process to add the `APP_INTERACTIONS_INTERACTIONS_ITEMS` table to the same join node.  You should see both artifacts in the join node.

    ![Both tables in join](both_in_join.png)

1. Double-click on the join node. A panel will open on the right.

    ![Join Definition on the right](join_definition.png)

1. Drag and drop the `ID` field to the `INTHEADER_ID` field.  

    ![Create Join](create_join.png)

1. Set the cardinality to `1..n`

    ![Cardinality](cardinality.png)

1. In the **Mapping** tab, add all the columns except `INTHEADER_ID` as output columns.

    ![Mapping](mapping.png)

1. Connect the join node with the Projection node using the ![arrow](arrow.png)

    ![Connect to Projection #1](connect_projection1.png)

    ![Connect to Projection #2](connect_projection2.png)

1. Click on the **Projection** node and double-click on the join parent to add all the columns to the output

    ![Projection Join](projection_join.png)

1. From the SAP HANA Projects view, press the Deploy button

    ![Deploy](deploy.png)

1. Check the deployment log to make sure everything was successfully created in the database.

    ![Check Log](check_log.png)

1. Open the HDI Container in the Database Explorer

    ![Open Database Explorer](open_db_explorer.png)

1. Under **Column Views** you will find your Calculation View.  Choose **Open Data**

    ![Test Calculation View](open_data.png)

1. Go to the **Raw Data** and you should see the header and item data joined together.

    ![Raw Data of view](raw_data.png)

### Create calculation view proxy entity

We now want to expose our Calculation View to the Cloud Application Programming model by creating a "proxy" entity for the view in the CDS data model.

1. Return to the Business Application Studio and open `interactions.cds`.

    ![Edit interactions.cds](interactions_cds.png)

1. We need our proxy entity to be created without the namespace in our current `interactions.cds`.  Therefore comment out the namespace line and add all the existing content except the `using ...` line in a new context for `app.interactions`.

    ![Add Context](add_context.png)

1. We need to add a matching entity definition for the Calculation View. This means redefining all the column names and data types / lengths. Doing so manually would be error prone, but the `hana-cli` has a utility that will help.  Open a terminal and change to the `db` folder with the command `cd db`.  Now issue the command:

    ```shell
    hana-cli inspectView -v V_INTERACTION -o cds
    ```

    ![inspectView](inspect_view.png)

    With this command you are looking up the definition of the view but asking for the output (-o) in the CDS format.

1. Copy this block from the terminal and paste it into the `interactions.cds` file at the end outside the context block.

    ![Add Proxy entity](add_proxy_entity.png)

1. CDS does have an annotation called `@cds.persistence.exists`. This annotation allows you to re-define an existing DB object and CDS won't attempt to create or alter it. It will just assume it already exists in the matching state.There is also the annotation `@cds.persistence.calcview`. This will further tell the Cloud Application Programming Model that this target entity is also a Calculation View.

1. Now open the `interactions_srv.cds` file from the `/srv` folder. Add this new Calculation View based entity to the CAP service as read-only.

    ![Add Entity to Service](entity_to_service.png)

1. From the terminal return to the root of the project and issue the command: `cds build`

    ```shell
    cds build
    ```

    ![CDS build](cds_build.png)

1. Although we didn't add any new database artifacts to the project, the addition of an entity to the service layer causes new views to be generated within SAP HANA. Therefore we need to deploy to the database using the SAP HANA Projects view before we can test.

    ![Deploy](deploy.png)

1. From the console in the project root hopefully you still have the `cds watch ...` running. Otherwise start it again with `cds watch --profile hybrid` to start the CAP service layer for testing.  If you have performed the tutorial [SAP HANA Cloud, Add User Authentication to Your Application](hana-cloud-cap-add-authentication), remember you must also run the application router to test your service with authentication.

1. You can test your view via the service layer by adding `/odata/v4/catalog/V_Interaction` to the path.

    ![Test](test_view.png)

Congratulations! You have now successfully combined HANA native artifacts with the SAP Cloud Application Programming Model and learned the modern HANA way to expose Calculation Views via OData. 
