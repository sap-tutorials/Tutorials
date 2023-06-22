---
parser: v2
auto_validation: true
time: 15
tags: [tutorial>beginner, software-product-function>sap-hana-cloud--data-lake, software-product>sap-hana-cloud]
primary_tag: software-product-function>sap-hana-cloud--data-lake
---

# Build an SAP CAP Application to Access SAP HANA Cloud, data lake Relational Engine
<!-- description --> Create an SAP Cloud Application Programming Model (CAP) application in SAP Business Application Studio that queries data from an existing data lake, Relational Engine table. The data lake, Relational Engine instance must be SAP HANA-managed.
To learn about SAP CAP and application development on SAP BTP, see [Introduction to Application Development Using CAP and Node.js | Tutorials for SAP Developers](https://developers.sap.com/tutorials/btp-app-introduction.html).


## Prerequisites
 - An SAP HANA Cloud data lake instance managed by an SAP HANA Cloud instance
 - Experience working in SAP Business application Studios with HDI-based tables
 - Experience building and running SAP Cloud Applications Programming (CAP) model applications in SAP Business Application Studios

## You will learn
  - How to connect to an existing HANA-managed data lake Relational Engine instance using CAP 


## Intro

---

### Create database objects

1. From SAP HANA Cloud Central, open your SAP HANA database in Database Explorer by choosing **Open in SAP HANA Database Explorer**. 

    If you have not previously opened the database in Database Explorer, you will need to login using the `DBADMIN` credentials specified upon the creation of your HANA database. 
    ![open-in-database-explorer](open-in-database-explorer.png)

2. Within Database Explorer as `DBADMIN`, run the following SQL script to create a new user `HDB_BOOKSHOP_USER` and a relational container `BOOKSHOP_CONTAINER` in the attached HANA-managed data lake Relational Engine instance owned by `HDB_BOOKSHOP_USER`. and a new schema `BOOKSHOP`. This script also grants privileges on the `BOOKSHOP` SCHEMA to the `HDB_BOOKSHOP_USER`. 

    ```SQL
    -- Create a new user called HDB_BOOKSHOP_USER
    CREATE USER HDB_BOOKSHOP_USER PASSWORD Password1 NO FORCE_FIRST_PASSWORD_CHANGE SET USERGROUP DEFAULT;
    -- Create a Relational Container in the attached HANA-managed HDLRE instance
    CALL SYSHDL.CREATE_CONTAINER('BOOKSHOP_CONTAINER', 'HDB_BOOKSHOP_USER');
    ```
    ![create-user-sql](create-user-sql.png)

    Then, create a new schema `BOOKSHOP`, and grant schema privileges on `BOOKSHOP` schema to the `HDB_BOOKSHOP_USER`. 
    ```SQL
    -- Create a new schema
    CREATE SCHEMA BOOKSHOP;
    -- Grant all BOOKSHOP schema privileges to HDB_BOOK_USER
    GRANT ALL PRIVILEGES ON SCHEMA BOOKSHOP TO HDB_BOOKSHOP_USER WITH GRANT OPTION;
    ```
    ![create-schema-sql](create-schema-sql.png)

3. You will now create database objects in the `BOOKSHOP` schema in the HANA-managed data lake Relational Engine that map to objects in the HANA database. These commands will be executed as the `HDB_BOOKSHOP_USER` as the owner of the relational container created in the previous step. 

    ```SQL
    -- Connect as the HDB_BOOKSHOP_USER
    CONNECT HDB_BOOKSHOP_USER PASSWORD Password1;

    -- Create a virtual table in HANA that maps to a table in the attached HANA-managed HDLRE instance, also created here
    CREATE VIRTUAL TABLE BOOKSHOP.BOOK_REVIEWS (
        REVIEW_ID INTEGER PRIMARY KEY,
        BOOK_ID INTEGER NOT NULL,
        RATING INTEGER NOT NULL,
        REVIEW VARCHAR(500) NOT NULL
    ) AT "SYSHDL_BOOKSHOP_CONTAINER_SOURCE"."NULL"."SYSHDL_BOOKSHOP_CONTAINER"."BOOK_REVIEWS" WITH REMOTE;

    -- Insert sample data into the virtual table
    INSERT INTO BOOKSHOP.BOOK_REVIEWS(REVIEW_ID, BOOK_ID, RATING, REVIEW) VALUES(1, 1, 5, 'I loved reading this novel since I love mysteries.');
    INSERT INTO BOOKSHOP.BOOK_REVIEWS(REVIEW_ID, BOOK_ID, RATING, REVIEW) VALUES(2, 12, 4, 'This was a good fantasy book, but not as realistic as I had hoped.');
    INSERT INTO BOOKSHOP.BOOK_REVIEWS(REVIEW_ID, BOOK_ID, RATING, REVIEW) VALUES(3, 7, 3, 'This book was slow but an interesting read overall.'); 
    INSERT INTO BOOKSHOP.BOOK_REVIEWS(REVIEW_ID, BOOK_ID, RATING, REVIEW) VALUES(4, 3, 5, 'I could not put this book down. I truly get why this is a classic.');
    INSERT INTO BOOKSHOP.BOOK_REVIEWS(REVIEW_ID, BOOK_ID, RATING, REVIEW) VALUES(5, 12, 3, 'I wish there was more action and less romance.');
    ```
    ![create-table-sql](create-table-sql.png)

    For additional details consult [Creating Virtual Tables](https://help.sap.com/docs/SAP_HANA_PLATFORM/6b94445c94ae495c83a19646e7c3fd56/91e5edbfb2144301abc0085984dce8a7.html?version=1.0.12)

4. Query the local SAP HANA table and the equivalent SAP HANA Cloud, data lake Relational Engine table. Note that both results should be identical. 
   
    ```SQL
    SELECT * FROM BOOKSHOP.BOOK_REVIEWS;
    SELECT * FROM SYSHDL_BOOKSHOP_CONTAINER.BOOK_REVIEWS;
    ```
    ![select-query](select-query.png)

### Create a dev space in SAP Business Application Studio

1.  In SAP BTP Cockpit, navigate to the **Service Marketplace** at the subaccount level. 

    ![service-marketplace](service-marketplace.png)

    Search for SAP Business Application Studio and launch the application by clicking the **Go to Application** button.
    
    ![launch-bas](launch-bas.png)

2. Within Business Application Studio, create a **Full Stack Cloud Application** dev space with the following three additional SAP Extensions: SAP HANA Calculation View Editor, SAP HANA Performance Tools, SAP HANA Tools.

    ![create-dev-space](create-dev-space.png)

    Your dev space is created instantly and automatically started, which can take a minute or so. 

3. Once it is running, click on the name of your dev space to open it. 

    ![open-dev-space](open-dev-space.png)

### Create a CAP application in SAP Business Application Studio 

1. Before you create an application, you will need to sign in to Cloud Foundry. Navigate to the **Cloud Foundry** page using the left menu bar. 
    ![cloud-foundry-menu-item](cloud-foundry-menu-item.png)

    Expand the **Services** directory and click the circled arrow icon to open the Cloud Foundry sign in page. 

    ![cf-login-arrow](cf-login-arrow.png)

    Sign in using SSO and select the correct organization and space where your HANA database is located. 

    ![cf-login-sso](cf-login-sso.png)

    Ensure to click **Apply** to save your organization and space targets.

    ![cf-login-targets](cf-login-targets.png)

2. Navigate to the **Explorer** page using the left menu bar and select **Create Project** to create a new project using a template. 

    ![create-project](create-project.png)

3. Use the **CAP Project** template and select **Start** to open the project creation wizard using the CAP template. 

    ![cap-project-type](cap-project-type.png)

4. Name your project `My_HDL_CAP_Project` and ensure your project uses the Node.js runtime environment, all available features, and includes basic sample files. Click **Finish** to create your project. 

    ![project-details](project-details.png)

### Connect to data lake, Relational Engine tables through a CAP application

1. To specify the specific pre-existing SAP HANA database you would like to configure your CAP application with, you will need to provide the instance ID, which can be found in SAP HANA Cloud Central.    
   In HANA Cloud Central, copy the **Instance ID** of your SAP HANA database. 
   
    ![copy-instance-id](copy-instance-id.png)

2. In your project workspace in Business Application Studio, right click on the `mta.yaml` file and select **Open With...**. 
    
    ![open-yaml](open-yaml.png)

    Select **Text Editor**. 

    ![open-yaml-with-text-editor](open-yaml-with-text-editor.png)

3. Under the resources header, edit the file to add your instance ID from your clipboard. 
   
    ![config-yaml](config-yaml.png)
    
4. Next, you will create the service instance which will connect to our specified HANA database. 
   
    In your project workspace under **SAP HANA PROJECTS**, select the icon to add a new database connection. 

    ![add-db-connection](add-db-connection.png)

5. Create a user-provided service instance and provide the information for the `HDB_BOOKSHOP_USER` created in a previous step. 
   
    Ensure you use the `BOOKSHOP` schema since this is where the `BOOK_REVIEWS` table is located in our HANA database. 
   
    Additionally, check the box to generate hdbgrants file which will give the runtime user the same permissions as `HDB_BOOKSHOP_USER` in the `BOOKSHOP` schema. 

    ![db-connection-wizard](db-connection-wizard.png)

6. Once the service is created and bound, you will be able to see it in the workspace under **SAP HANA PROJECTS**. 

    ![db-connection-success](db-connection-success.png)

7. Next you will create an HDI container within your existing SAP HANA database. 
   
    ![bind-create-hdi](bind-create-hdi.png)

    Select **Bind to an HDI container** and **Create a new service instance**. Call the container `My_CAP_HDI_Container`.

    You will receive the following two notifications once your service instance has been created and bound to your project. 

8. Create an hdbsynonym by opening the **Command Palette**.

    ![command-palette](command-palette.png)

9. Search for **SAP HANA: Create SAP HANA Database Artifact**. Select the artifact type as **Synonym (hdbsynonym)** and name the synonym `MY_BOOKSHOP_BOOK_REVIEWS_SYNONYM`. 

    ![hdbsynonym](hdbsynonym.png)

10. Open the file `MY_BOOKSHOP_BOOK_REVIEWS_SYNONYM.hdbsynoym` and click on **...** under the **Object Name** column.  
11. Ensure the connection `My_HANA_Connection` is selected, and select the `BOOKSHOP.BOOK_REVIEWS` table. 

    ![connection-selected](connection-selected.png)
    ![table-selected](table-selected.png)

12. Open the `data-model.cds` file and replace the contents with the following code. 
    
    ```data-model.cds
    namespace MY.BOOKSHOP;

    entity Books {
    key ID : Integer;
    title  : String;
    stock  : Integer;
    }

    @cds.persistence.exists
    entity BOOK_REVIEWS_SYNONYM {
    key REVIEW_ID  : Integer;
        BOOK_ID    : Integer;
        RATING     : Integer;
        REVIEW : String(500)
    }
    ```
    ![data-model](data-model.png)

13. Open the `cat-service.cds` file and replace the contents with the following code.

    ```cat-service.cds
    using MY.BOOKSHOP as my from '../db/data-model';

    service CatalogService {
        @readonly entity Books as projection on my.Books;
        @readonly entity BOOK_REVIEWS_SYNONYM as projection on my.BOOK_REVIEWS_SYNONYM;
    }
    ```
    ![cat-service](cat-service.png)

14. Deploy the project using the rocket icon.
    ![deploy](deploy.png)

15. In the left navigation menu, navigate to **Run Configurations** and create a new configuration. Select your current project and name the configuration using the default name. 
    ![create-new-run-config](create-new-run-config.png)

16. Bind the configuration to your HDI container, `My_HANA_HDI_Container`, by clicking the bind icon and selecting the HDI container. 

    ![bind-config](bind-config.png)

17. Then, run the configuration by clicking the bind icon. 

    ![run-config](run-config.png)

18. You will be prompted to open a service in a new tab. Click **Open in a New Tab**.
    
    ![new-tab](new-tab.png)

19. Select **Fiori preview** for the `BOOK_REVIEWS` table. 
    
    ![fiori-preview](fiori-preview.png)

20. Click the gear icon to open the table settings. 

    ![table-settings](table-settings.png)

    Ensure all the columns are selected. 

    ![column-select](column-select.png)

21. You are now able to view the data in the `BOOK_REVIEWS` table. 

    ![view-table](view-table.png)
    
### Knowledge check

Congratulations! You have now created a CAP application that connects to and queries an SAP HANA-managed, SAP data lake, Relational Engine database.  

---
