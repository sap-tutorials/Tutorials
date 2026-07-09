---
parser: v2
auto_validation: true
time: 15
tags: [software-product>sap-hana-cloud, software-product-function>sap-hana-cloud--sap-hana-database, programming-tool>sql, tutorial>beginner, tutorial>license]
primary_tag: software-product-function>sap-hana-cloud--sap-hana-database
---

# Set up and use the Data Exploration Tools

<!-- description -->This tutorial explains what the data exploration tools are, shows how they can be set up, and provides a few examples of using them in the SQL Console.

## Prerequisites

- A productive SAP HANA Cloud instance that has enabled the Knowledge Graph (also known as a triple store) and the Natural Language Service (NLS)  
- Access to an AI Core instance or the ability to deploy and configure an instance of one

## You will learn

- How to set up the data exploration tools
- How you can use these tools
- How the data exploration tools make use of the SAP HANA Cloud Vector engine and Knowledge Graph to return answers to questions about your tables, views, and functions and the data returned by them using natural language.

## Overview

SAP HANA Cloud provides many features that make it well suited as a database for business AI.  The [data exploration tools](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-administration-guide/data-exploration-with-generative-ai) make use of a few of these features which are further described below.  The data exploration tools provide a database object discovery stored procedure that can be used to find objects and a data retrieval procedure that can be used to return query results from a prompt.

SAP HANA Cloud provides a [vector engine](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-vector-engine-guide/introduction?locale=en-US) and a set of [vector functions](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-vector-engine-guide/vector-function-reference?locale=en-US) to work with vectors.  The ability to embed data as a vector data type and to provide a method to perform a similarity search on the embedded data is helpful in providing grounding data to an LLM.  Retrieval Augmented Generation (RAG) is a pattern where instead of asking a large language model (LLM) to answer a question from memory alone, you first retrieve relevant context from a knowledge base (in this case the tables, views, and functions in your database) and then augment the LLM's prompt with that context. The LLM generates a response grounded in your data rather than generating its response based only on the data it's been trained with.

SAP HANA Cloud also provides the [SAP HANA Database Knowledge Graph](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-knowledge-graph-guide/sap-hana-cloud-sap-hana-database-knowledge-graph-engine-guide?locale=en-US) which can be used to store the relationships between objects.  The data exploration tools as part of its configuration will create a knowledge graph containing information about the relationships between the database table, views, and functions that are specified during its configuration.

---

### SAP AI Core

The data exploration tools require access to an orchestration model running in [SAP AI Core](https://help.sap.com/docs/sap-ai-core/sap-ai-core-service-guide/what-is-sap-ai-core).  The following steps demonstrate how an SAP AI Core instance can be created and how SAP HANA Cloud can be configured to connect to the SAP AI Core instance.  

1. Open the [SAP BTP Cockpit](https://amer.cockpit.btp.cloud.sap/) and in a [region](https://discovery-center.cloud.sap/serviceCatalog/sap-ai-core?region=all&tab=service_plan) that supports the SAP AI Core, create an instance of it using the extended plan.

    ![create SAP AI Core](create-ai-core.png)

    ![specify values](create-ai-core2.png)

2. Once the instance has been created, add a service key of type X.509 certificate.  We will use this service key to enable SAP HANA Cloud to access the AI Core instance.  

    ![create service key](create-service-key.png)

    Specify that the key should be an X.509 certificate by including the JSON below.

    ```JSON
    {
        "xsuaa": {
            "credential-type": "x509",
            "x509": {
            "key-length": 2048,
            "validity": 3,
            "validity-type": "MONTHS"
            }
        }
    }
    ```

    ![providing parameters to the service key](create-service-key2.png)

    Additional details can be found at [Create a Service Key](https://help.sap.com/docs/sap-ai-core/sap-ai-core-service-guide/create-service-key).

3. Download the service key.

    ![Download the service key](download-key.png)

4. **The following are optional steps** for illustrative purposes only.  The data exploration wizard shown in step 4 can be used to access the deployment ID and to create a remote source connection to the AI Core instance.  Feel free to jump to step 4 Configure the data exploration tools.  

    1. Create a second service key this time without providing the JSON.  This will create a user name and password based service key that we will use to make a REST request against the SAP AI Core instance to determine its deployment ID.

    2. In the SAP BTP Cockpit, a subscription to the SAP AI Launchpad can be created.  It can be used to examine one or more SAP AI Core instances and to start, stop, and delete models.  It is shown below so that the deployment ID of the default orchestration model can be determined.

        ![SAP AI Launchpad subscription](launchpad.png)

        After opening the UI, a connection can be made to the SAP AI Core instance.

        ![connect to the AI Core instance from the SAP AI Launchpad](create-an-ai-core-connection.png)

        To view the list deployments first select a resource group.  The data exploration tools use the default resource group.

        ![default resource group](resource_group.png) 

        Next select the deployments which includes the deploymentID.  By default a deployment of an orchestration model is included in each SAP AI Core instance.  This is the model required by the data exploration tools.  

        ![finding the deployment ID in the SAP AI Launchpad](deployments-in-ai-launchpad.png)

        Above an additional model was added to support [SAP RPT-1](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-sql-reference-guide/ai-tabular-prediction-procedural).

    3. Additionally, there is a [REST API](https://api.sap.com/api/AI_CORE_API) that can be used to interact with the SAP AI Core instance.  The below shows an alternate method to access the deployment ID.  If not already installed, add the REST Client extension from Marketplace after opening the extensions view.

        ![Install Rest client ](REST-client-install.png)

        To try it out, create an HTTP file with the following content and open it in Visual Studio Code.  Update the variable values to match those from the previously created user name and password service key.

        ```HTTP
        #REST CLIENT VARIABLES ---------------------------
        # instructions at https://help.sap.com/docs/sap-ai-core/sap-ai-core-service-guide/create-service-instance?locale=en-US
        # https://help.sap.com/docs/sap-ai-core/generative-ai/get-auth-token?locale=en-US
        # https://api.sap.com/api/AI_CORE_API

        @ai_api_url = https://api.ai.prod.us-east-1.aws.ml.hana.ondemand.com
        @auth_url = https://dan-van-leeuwen-aws.authentication.us10.hana.ondemand.com
        @client_id = sb-fccb40af...
        @client_secret = 0f12e678-6...
        @resource_group = default

        @oauth = oauth/token?grant_type=client_credentials

        #Generated by the request bearer token call.  Copy the access_token value from the result without the quotes
        @bearer = 
        #AUTHORIZATION REST API CALL ---------------------

        #Request Bearer Token
        GET {{auth_url}}/{{oauth}}
        Authorization: Basic {{client_id}}:{{client_secret}}

        ###

        #Request deployments
        GET {{ai_api_url}}/v2/lm/deployments
        Authorization: Bearer {{bearer}}
        AI-Resource-Group: {{resource_group}}
        ```

    4. Request a bearer token and copy that value into the bearer variable.

        ![request a bearer token](request-bearer-token.png)

    5. Request the deployment ID from the AI Core instance.  Write down the deployment ID.  This can be used in a later optional step.

        ![request the deployment id](deployment-id.png)

### Connect SAP HANA Cloud to SAP AI Core

**The following are optional steps for illustrative purposes only**.  The data exploration wizard in step 4 can be used to create the remote source.  Feel free to jump to step 4 now.

1. Create a Personal Security Environment (PSE).  In a SQL Console connected to your SAP HANA Cloud, SAP HANA database instance, execute the below SQL after replacing the values from the service key created in the previous step.  

    ```SQL
    SELECT * FROM PSES;
    CREATE PSE AI_CORE_PSE;
    ```

2. Copy the JSON from the service key created on the AI Core instance and paste it into Visual Studio Code.  Perform a search and replace (ctrl-F) for \n and replace it with ctrl enter.

    ![replace \n](searchAndReplace.png)

3. Use the 4 certificates in the previous sub-step in the SQL below.

    ```SQL
    ALTER PSE AI_CORE_PSE SET OWN CERTIFICATE '
    -----BEGIN CERTIFICATE-----
    -----END CERTIFICATE-----
    -----BEGIN CERTIFICATE-----
    -----END CERTIFICATE-----
    -----BEGIN CERTIFICATE-----
    -----END CERTIFICATE-----
    -----BEGIN RSA PRIVATE KEY-----
    -----END RSA PRIVATE KEY-----';
    ```

    ![add certificates to the pse](alter-pse.png)

4. Create a remote source from SAP HANA Cloud to the AI Core instance by executing the SQL below.  Use the deploymentId that was obtained in step 1 sub step 8.  The remainder of the values can be found in the service key created previously on the AI Core instance.  **Note that the authUrl below is labeled certurl in the service key**.

    ```SQL
    CREATE REMOTE SOURCE AI_CORE_RS ADAPTER "sapgenaihub" CONFIGURATION
    'aiApiUrl=https://api.ai.prod.us-east-1.aws.ml.hana.ondemand.com;
     authUrl=https://dan-van-leeuwen-aws.authentication.cert.us10.hana.ondemand.com;
     resourceGroup=default;
     deploymentId=d1359bdbe93ddf8c;
     clientId=sb-fccb40af-2194-4353-b290-...'
    WITH CREDENTIAL TYPE 'X509' PSE AI_CORE_PSE;
    CALL GET_REMOTE_SOURCE_AI_MODELS('AI_CORE_RS', ?);
    SELECT * FROM REMOTE_SOURCES;
    ```

    ![create remote source](create-remote-source.png)

    Additional details can be found at [Creating Text Embeddings with SAP AI Core](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-vector-engine-guide/creating-text-embeddings-with-sap-ai-core).

### Configure the data exploration tools

1. Open the data exploration tools wizard.

    ![open the manage data exploration tools](open-manage-data-exploration-tools.png)

2. Create a new tool.

    ![create new tool](create-new-tool.png)

    Select the remote source created in the previous step, or choose to create a new one.

    ![specify the name and details](create-new-tool2.png)

3. Once the tool has been created (you may need to trigger a refresh), it can then be opened in the SQL Console via the action menu.

    ![successfully created](create-new-tool3.png)

    The discovery and retrieval procedures are now ready to be used.

    ```SQL
    CALL "DBADMIN"."DATABASE_OBJECT_DISCOVERY_TOOL_HTLS_DET"( 'Any work orders?', ? );
    CALL "DBADMIN"."DATA_RETRIEVAL_TOOL_HTLS_DET"( 'Which hotels were the most visited in 2026.  Display the result in a table.  Provide the SQL statement used, the tools called, and the tool execution plan.', ? );
    ```

    ![result 1](result1.png)

    ![result 2](result2.png)

    The following is the full result for the call using the data retrieval stored procedure.

    ```
    Here are the most visited hotels in 2026, displayed in a table:

    | HOTEL_NAME     | VISIT_COUNT |
    |----------------|-------------|
    | Regency        | 5           |
    | Long Island    | 5           |
    | Eighth Avenue  | 5           |
    | Beach          | 5           |
    | Empire State   | 4           |
    | Atlantic       | 4           |
    | Long Beach     | 4           |
    | Indian Horse   | 4           |
    | River Boat     | 4           |
    | Midtown        | 3           |
    | Sunshine       | 3           |
    | Lake Michigan  | 2           |
    | Airport        | 2           |
    | Delta          | 2           |
    | Star           | 1           |
    | Ocean Star     | 1           |

    ---

    **SQL Statement Used:**
    
    SELECT
    "HOTEL"."NAME" AS "HOTEL_NAME",
    COUNT(*) AS "VISIT_COUNT"
    FROM
    "HOTELS"."RESERVATION"
    JOIN
    "HOTELS"."HOTEL"
    ON
    "RESERVATION"."HNO" = "HOTEL"."HNO"
    WHERE
    YEAR("RESERVATION"."ARRIVAL") = 2026
    GROUP BY
    "HOTEL"."NAME"
    ORDER BY
    "VISIT_COUNT" DESC
    
    **Tools Called:**
    - functions.hana_objects_tool (to discover relevant tables)
    - functions.hana_data_tool (to execute the SQL and retrieve results)

    **Tool Execution Plan:**
    1. Discover tables related to hotel visits and bookings.
    2. Construct and execute an SQL query to count visits per hotel for 2026.
    3. Display the results in a table.
    ```

### Understanding the objects created

1. Perform the below query to see the details stored in the RAG table.

    ```SQL
    SELECT * FROM D1_INDEX ORDER BY OBJECT_NAME ASC;
    ```

    ![rag table showing descriptions of each table](rag-table.png)

    Notice that a summary and a vectorized summary of the description is stored for each object that was included as part of the data exploration tool.

2. SAP HANA Cloud provides vector functions such as COSINE_SIMILARITY that can determine semantic similarity and embedding functions that can be used to create a vector representation of an input string.  The below is an example of using some of these methods.  

    ```SQL
    --An example of a similarity search using the SUMMARY_VECTOR COLUMN
    --https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-vector-engine-guide/cosine-similarity-function-vector
    SELECT TOP 10 SCHEMA_NAME, OBJECT_NAME, OBJECT_TYPE, 
            COSINE_SIMILARITY(SUMMARY_VECTOR, TO_REAL_VECTOR(VECTOR_EMBEDDING('work items', 'DOCUMENT', 'SAP_NEB.20240715'))) AS SCORE 
        FROM D1_INDEX 
        ORDER BY SCORE DESC;
    
    --The list of available models
    CALL GET_REMOTE_SOURCE_AI_MODELS('AI_CORE_RS', ?);

    --An example showing how a summary of a table from a well known schema could be created
    --The data exploration tool uses comments on the tables, views, and functions for the summary.
    --https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-sql-reference-guide/ai-text-completion-vector
    SELECT AI_TEXT_COMPLETION(
    'In one sentence, describe the purpose of the table SFLIGHT.STRAVELAG',
    'gpt-5',
    NULL,
    'AI_CORE_RS')
    FROM DUMMY;

    --Examples showing how a summary can be turned into a vector representation
    --https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-vector-engine-guide/vector-embedding-function-vector?locale=en-US
    SELECT VECTOR_EMBEDDING('STRAVELAG is the SAP Flight Model demo table that stores travel agency master data (e.g., agency ID and details) referenced by flight bookings.', 'DOCUMENT', 'SAP_NEB.20240715') FROM DUMMY;

    SELECT VECTOR_EMBEDDING('STRAVELAG is the SAP Flight Model demo table that stores travel agency master data (e.g., agency ID and details) referenced by flight bookings.', 'DOCUMENT', 'text-embedding-ada-002.2', AI_CORE_RS) FROM DUMMY;
    ```

    In the wizard, the summary of the object is obtained either by the object level comment or optionally, you can provide a description for an object.  It should be noted that the [COMMENT ON](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-sql-reference-guide/comment-on-statement-data-definition) statement does not apply to functions.

    ```SQL
    COMMENT ON TABLE HOTELS.CUSTOMER IS 'This table contains customer details who have made hotel reservations';
    SELECT schema_name, table_name, comments
    FROM TABLES
    WHERE table_name = 'CUSTOMER’;
    ```

3. Perform the below query to see the details stored in the knowledge graph used by the data exploration tool.

    ```SQL
    SELECT *
    FROM SPARQL_TABLE('SELECT ?s ?p ?o FROM <DBADMIN.D1_GRAPH>
    WHERE {  ?s ?p ?o .}
    ORDER BY DESC(?p)');
    ```

    ![knowledge graph](knowledge-graph.png)

    This knowledge graph contains details of the database objects that the data exploration tool was provided during its initial set up.  For example it contains details about each object (for example the colums of a table) and how the objects are related (for example by foreign keys).

4. The data exploration tools provide two procedures named AI_OBJECT_RETRIEVAL and AI_DATA_RETRIEVAL.

    ![AI procedures](ai-procedures.png)

    When a data exploration tool instance is created, it creates a set of wrapper stored procedures that have the details such as the name of the remote source, the name of the schema the RAG table is in etc. to make calling the procedures easier.  An example of the SQL for a generated function is shown below.

    ```SQL
    CREATE PROCEDURE "DBADMIN"."MY_OBJECT_RETRIEVAL"( IN QUERY NCLOB, OUT RESULT NCLOB )
        LANGUAGE SQLSCRIPT
        SQL SECURITY INVOKER AS
    BEGIN
        CALL AI_OBJECT_RETRIEVAL( null, 'AI_CORE_RS', 'DBADMIN', 'D1', null, :QUERY, :RESULT );
    END;

    CREATE PROCEDURE "DBADMIN"."MY_DATA_RETRIEVAL"( IN QUERY NCLOB, OUT RESULT NCLOB )
        LANGUAGE SQLSCRIPT
        SQL SECURITY INVOKER AS
    BEGIN
        CALL AI_DATA_RETRIEVAL( null, 'AI_CORE_RS', 'DBADMIN', 'D1', null, :QUERY, :RESULT );
    END;

    CALL "DBADMIN"."MY_OBJECT_RETRIEVAL"( 'Where can I find work orders?', ? );
    CALL "DBADMIN"."MY_DATA_RETRIEVAL"( 'Is there a vendor that we use for pool maintenance?', ? );
    ```

    ![custom stored procedure](customer-stored-procedure.png)

### Using the functions with a different user

The following steps demonstrate creating a read only user that will be granted access to the data exploration procedures.  

1. Execute the below SQL to create a read only user that can all the data exploration tools procedures.

    ```SQL
    CREATE USER DET_USER PASSWORD Password1 SET USERGROUP HOTEL_USER_GROUP;
    CREATE ROLE DET_ROLE;
    GRANT DET_ROLE TO DET_USER;
    
    GRANT SELECT ON DBADMIN.D1_INDEX TO DET_ROLE;
    GRANT EXECUTE ON REMOTE SOURCE AI_CORE_RS TO DET_ROLE;
    
    GRANT EXECUTE ON SYS.AI_OBJECT_RETRIEVAL TO DET_ROLE;
    GRANT EXECUTE ON SYS.AI_DATA_RETRIEVAL TO DET_ROLE;
    GRANT EXECUTE ON DBADMIN.MY_OBJECT_RETRIEVAL TO DET_ROLE;
    GRANT EXECUTE ON DBADMIN.MY_DATA_RETRIEVAL TO DET_ROLE;
    
    GRANT SPARQL QUERY TO DET_ROLE;
    CALL SPARQL_EXECUTE ('GRANT SELECT ON <DBADMIN.D1_GRAPH> TO <DET_ROLE>', '', ?, ?);
    
    CONNECT USER1 PASSWORD Password1;
    GRANT SELECT ON SCHEMA HOTELS TO DET_ROLE;
    CONNECT DET_USER PASSWORD Password1;

    CALL "DBADMIN"."MY_OBJECT_RETRIEVAL"( 'Where can I find work orders?', ? );
    CALL "DBADMIN"."MY_DATA_RETRIEVAL"( 'Is there a vendor that we use for pool maintenance?  Describe the steps taken to arrive at an answer', ? );
    ```

### Knowledge check

Congratulations! You now have an overview of some of the tasks involved with a migration from an on-premise SAP HANA database to an SAP HANA Cloud instance.

---
