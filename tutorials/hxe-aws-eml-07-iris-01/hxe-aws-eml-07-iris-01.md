---
title: Consume your Iris TensorFlow Model from SAP HANA, express edition
description: Provide details on the consumption from SAP HANA, express edition of the model trained using Amazon SageMaker, then deployed in Amazon ECS with the TensorFlow Serving Docker image.
author_name: Josh Bentley
author_profile: https://github.com/jarjarbentley
primary_tag: topic>machine-learning
auto_validation: true
tags: [ tutorial>intermediate, topic>cloud, topic>machine-learning, products>sap-hana\,-express-edition, products>sap-hana ]
time: 15
---

## Details
### You will learn
 - Reload the SAP HANA EML configuration
 - Call the Iris model from SAP HANA SQLScript

[ACCORDION-BEGIN [Step 5: ](Reload SAP HANA EML configuration)]

Now that the model is deployed and the TensorFlow Serving `ModelServer` is up and running, you will need to add the model configuration to your SAP HANA, express edition instance.

Using the **HXE** connection with the **`ML_USER`** user credentials, execute the following SQL statement to register the Iris deployed models with the following SQL statements:

```SQL
INSERT INTO _SYS_AFL.EML_MODEL_CONFIGURATION VALUES('iris' , 'RemoteSource', 'TensorFlow');
```

> ### **Note:**
>
As a reminder, you can execute your SQL statements using the SAP Web IDE (as described in [Prepare for Machine Learning](hxe-aws-eml-04)) :
>
 - `https://hxehost:53075`
>
Or use HDBSQL with the following command (assuming you didn't change the `ML_USER` password):
>
```
hdbsql -n localhost:39015 -u ML_USER -p Welcome19Welcome19
```
>
When using HDBSQL, you need to enable the multi-line mode using the following command in order to successfully run the above commands:
>
```
\mu
```

Now that a new model configuration was added, you will need to reload the EML configuration as this one is only loaded at the SAP HANA, express edition startup time.

```SQL
-- Uncomment the following lines if you want to re-run the script
-- DROP TABLE UPDATE_CONFIGURATION_PARAMS;
-- DROP TABLE UPDATE_CONFIGURATION_RESULT;
-- DROP PROCEDURE UPDATE_CONFIGURATION;

CREATE TABLE UPDATE_CONFIGURATION_PARAMS ("Parameter" VARCHAR(100), "Value" VARCHAR(100));
CREATE TABLE UPDATE_CONFIGURATION_RESULT ("Key" VARCHAR(100), "Value" INTEGER, "Text" VARCHAR(100));
CREATE PROCEDURE UPDATE_CONFIGURATION() AS
BEGIN
    DECLARE CURSOR CUR FOR
        SELECT VOLUME_ID FROM SYS.M_VOLUMES WHERE SERVICE_NAME = 'indexserver';
    FOR CUR_ROW AS CUR DO
        EXEC 'CALL _SYS_AFL.EML_CTL_PROC(''UpdateModelConfiguration'', UPDATE_CONFIGURATION_PARAMS, UPDATE_CONFIGURATION_RESULT)'
            || ' WITH OVERVIEW WITH HINT(ROUTE_TO(' || :CUR_ROW.VOLUME_ID || '))';
    END FOR;
END;
TRUNCATE TABLE UPDATE_CONFIGURATION_RESULT;
CALL UPDATE_CONFIGURATION();
SELECT * FROM UPDATE_CONFIGURATION_RESULT;
```

It should return the following result:

|    Key | Value |  Text |
|--------|-------|-------|
| Status |     0 |    OK |

Now, you can check the registered model:

```SQL
-- Uncomment the following lines if you want to re-run the script
-- DROP TABLE CHECK_PARAMS;
CREATE TABLE CHECK_PARAMS ("Parameter" VARCHAR(100), "Value" VARCHAR(100));
TRUNCATE TABLE CHECK_PARAMS;
INSERT INTO CHECK_PARAMS VALUES ('Model', 'iris');
CALL _SYS_AFL.EML_CHECKDESTINATION_PROC(CHECK_PARAMS, ?);
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test the model from SAP HANA SQLScript)]

The following SQL script will call the deployed Iris model using the test dataset uploaded before.

Using the **HXE** connection with the **`ML_USER`** user credentials, execute the following SQL statement:

```SQL
-- Uncomment the following lines if you want to re-run the script
--DROP TYPE TT_IRIS_FEATURES;
--DROP TYPE TT_IRIS_PARAMS;
--DROP TYPE TT_IRIS_RESULTS;
--DROP TABLE IRIS_PROC_PARAM_TABLE;
--DROP TABLE IRIS_PARAMS;
--DROP TABLE IRIS_RESULTS;

-- Define table types
CREATE TYPE TT_IRIS_FEATURES  AS TABLE (
	SEPALLENGTH FLOAT,
	SEPALWIDTH FLOAT,
	PETALLENGTH FLOAT,
	PETALWIDTH FLOAT
);
CREATE TYPE TT_IRIS_PARAMS    AS TABLE (
	"Parameter" VARCHAR(100),
	"Value" VARCHAR(100)
);
CREATE TYPE TT_IRIS_RESULTS   AS TABLE (
	PREDICTED_CLASS_ID INTEGER,
	CLASSES VARCHAR(100),
	BIAS_0 FLOAT, BIAS_1 FLOAT, BIAS_2 FLOAT,
	PROBABILITY_0 FLOAT, PROBABILITY_1 FLOAT, PROBABILITY_2 FLOAT
);

-- Create description table for procedure wrapper creation
CREATE COLUMN TABLE IRIS_PROC_PARAM_TABLE (
    POSITION        INTEGER,
    SCHEMA_NAME     NVARCHAR(256),
    TYPE_NAME       NVARCHAR(256),
    PARAMETER_TYPE  VARCHAR(7)
);

-- Populate the wrapper procedure parameter table
INSERT INTO IRIS_PROC_PARAM_TABLE VALUES (1, CURRENT_SCHEMA, 'TT_IRIS_PARAMS'    , 'in');
INSERT INTO IRIS_PROC_PARAM_TABLE VALUES (2, CURRENT_SCHEMA, 'TT_IRIS_FEATURES'  , 'in');
INSERT INTO IRIS_PROC_PARAM_TABLE VALUES (3, CURRENT_SCHEMA, 'TT_IRIS_RESULTS'   , 'out');

-- Drop the wrapper procedure
CALL SYS.AFLLANG_WRAPPER_PROCEDURE_DROP(CURRENT_SCHEMA, 'MY_IRIS');

-- Create the wrapper procedure
CALL SYS.AFLLANG_WRAPPER_PROCEDURE_CREATE('EML', 'PREDICT', CURRENT_SCHEMA, 'MY_IRIS', IRIS_PROC_PARAM_TABLE);

-- Create the result table
CREATE TABLE IRIS_RESULTS  LIKE TT_IRIS_RESULTS;

-- Create and populate the parameter table
CREATE TABLE IRIS_PARAMS    LIKE TT_IRIS_PARAMS;

INSERT INTO IRIS_PARAMS   VALUES ('Model', 'iris%predict');
INSERT INTO IRIS_PARAMS   VALUES ('RemoteSource', 'TensorFlow');
INSERT INTO IRIS_PARAMS   VALUES ('Deadline', '10000');

-- Call the TensorFlow model
CALL MY_IRIS (IRIS_PARAMS, IRIS_TEST, IRIS_RESULTS) WITH OVERVIEW;
```

You can note the following:

- The column names for the `TT_IRIS_PARAMS` table type are case sensitive
- The model outputs are all merged into one result table. Therefore it is important that the model signature uses consistent output tensors in term of dimensions.
 - By default the ***`serving_default`*** model signature is used, but the Iris model contains only a ***`predict`*** signature which is reflected in the `IRIS_PARAMS` entry for Model with the `iris%predict` value.
 - The output result stored in `IRIS_RESULTS` is sorted the same way as in the input from `IRIS_TEST`

You can compare the current and predicted value using the following SQL:

```SQL
SELECT
  D.SPECIES AS "CURRENT_SPECIES",
  R.PREDICTED_CLASS_ID AS "PREDICTED_SPECIES",
  CASE WHEN D.SPECIES != R.PREDICTED_CLASS_ID THEN 'INCORRECT' ELSE 'CORRECT' END AS "STATUS",
  ROUND (PROBABILITY_0, 5) AS "PROBABILITY_0",
  ROUND (PROBABILITY_1, 5) AS "PROBABILITY_1",
  ROUND (PROBABILITY_2, 5) AS "PROBABILITY_2"
FROM
  (SELECT *, ROW_NUMBER() OVER() AS RN FROM IRIS_RESULTS) R
JOIN
  (SELECT *, ROW_NUMBER() OVER() AS RN FROM IRIS_TEST) D ON R.RN = D.RN;
```  

[VALIDATE_2]
[ACCORDION-END]
