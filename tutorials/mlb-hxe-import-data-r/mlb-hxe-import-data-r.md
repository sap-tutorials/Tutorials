---
title: Import data from R Datasets package
description: Import data from the R Datasets package in your SAP HANA, express edition instance
auto_validation: true
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
---
## Prerequisites  
- Proficiency: beginner

### You will learn

As part of the [R Datasets package](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/00Index.html), you can get access to over a hundred sample datasets to address many Machine learning scenarios.

In this tutorial, you will learn how to proceed to import the sample dataset into your SAP HANA, express edition instance.

## Details

### Time to Complete
**10 Min**.

[ACCORDION-BEGIN [Prerequisite: ](Prepare your environment)]

The steps detailed in this tutorial and the related links will assume that you have completed the following tutorial:

- [Install a SQL query tool for SAP HANA, express edition](https://www.sap.com/developer/tutorials/mlb-hxe-tools-sql.html)
- [Prepare your SAP HANA, express edition instance for Machine Learning](https://www.sap.com/developer/tutorials/mlb-hxe-setup-basic.html)
- [Configure the SAP HANA R integration with SAP HANA, express edition](https://www.sap.com/developer/tutorials/mlb-hxe-setup-r.html)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Prerequisite : ](Create a dedicated schema)]

In addition, it is a good practice to separate data into different schema based on their origin.

In this tutorial, you will be using the **`R_DATA`** schema to load the SAP Predictive Analytics sample data.

If you have already created the schema, move to the next step.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement:

```SQL
CREATE SCHEMA R_DATA;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info : ](Supported Data Types)]

The data structure supported to exchange data between the SAP HANA database and the R environment is the R data frame, which has a similar data structure to a column table in the SAP HANA database.

The supported data types are listed below:

| R Type                | SAP HANA SQL Type   |
|:----------------------|---------------------|
| numeric (integer)	    | TINYINT <br> SMALLINT <br> INTEGER
| numeric (double)	    | REAL <br> DOUBLE <br> FLOAT <br> FLOAT(p) <br> DECIMAL <br> DECIMAL(p,s) <br> BIGINT
| character / factor    | VARCHAR  <br> NVARCHAR <br> CLOB <br> NCLOB
| Date                  | DATE
| Date/Time (`POSIXct`) | TIMESTAMP <br> SECONDDATE
| Raw                   | VARBINARY <br> BLOB

For example, the `ts` R type is not supported.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Get The Dataset Structure)]

Before being able to import the data inside of your SAP HANA, express edition instance you will need to create the dataset table.

The [R Datasets package documentation](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/00Index.html) doesn't always provide the details to create the corresponding table as data type are not always documented.

However, as most datasets are in fact available as data frame or vectors, and sometime time series, you can easily retrieve the structure and details about the data types.

The following script will give you the information for the Iris dataset.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement:

```SQL
SET SCHEMA R_DATA;

-- Uncomment the drop statement is you want to run it from scratch
-- DROP TABLE 		COLUMN_LIST;
-- DROP PROCEDURE 	GET_COLUMN_NAME;
-- DROP PROCEDURE 	DISPLAY_COLUMN_NAME;

CREATE COLUMN TABLE COLUMN_LIST (
	"name" VARCHAR(5000),
	"type" VARCHAR(5000)
);


CREATE PROCEDURE GET_COLUMN_NAME(OUT col_list "COLUMN_LIST")
LANGUAGE RLANG AS
BEGIN
  library(datasets)

  data(iris)

  n <- c(colnames(as.data.frame(iris)))
  t <-     sapply(as.data.frame(iris), class)

  col_name <- data.frame("name" = n)
  col_type <- data.frame("type" = t)


  col_list <- cbind(as.data.frame(col_name, col_type))
END;


CREATE PROCEDURE DISPLAY_COLUMN_NAME()
AS BEGIN
	CALL GET_COLUMN_NAME(col_list);
    INSERT INTO COLUMN_LIST SELECT * FROM  :col_list;
END;

CALL DISPLAY_COLUMN_NAME();
SELECT * FROM COLUMN_LIST;
```

The expected output should look like this:


| Name           | Type         |
|:---------------|-------------:|
| `Sepal.Length` | numeric
| `Sepal.Width`  | numeric
| `Petal.Length` | numeric
| `Petal.Width`  | numeric
| `Species`      | factor

And you can deduce the following table structure:

```
CREATE COLUMN TABLE IRIS (
	"Sepal.Length" DOUBLE,
	"Sepal.Width"  DOUBLE,
	"Petal.Length" DOUBLE,
	"Petal.Width"  DOUBLE,
	"Species"      VARCHAR(5000)
);
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2 : ](Import the Iris Dataset)]

Now that we have the dataset structure, it is fairly easy to import the data running the following SQL.

```sql
SET SCHEMA R_DATA;

-- Uncomment the drop statement is you want to run it from scratch
-- DROP TABLE 		IRIS;
-- DROP PROCEDURE 	LOAD_IRIS;
-- DROP PROCEDURE 	DISPLAY_IRIS;

CREATE COLUMN TABLE IRIS (
	"Sepal.Length" DOUBLE,
	"Sepal.Width"  DOUBLE,
	"Petal.Length" DOUBLE,
	"Petal.Width"  DOUBLE,
	"Species"      VARCHAR(5000)
);

CREATE PROCEDURE LOAD_IRIS(OUT iris "IRIS")
LANGUAGE RLANG AS
BEGIN
  library(datasets)
  data(iris)
  iris <- cbind(as.data.frame(iris))
END;

CREATE PROCEDURE DISPLAY_IRIS()
AS BEGIN
	CALL LOAD_IRIS(iris);
	INSERT INTO IRIS SELECT * FROM  :iris;
END;

CALL DISPLAY_IRIS();
SELECT COUNT(1) FROM IRIS;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3 : ](Import the Ability Dataset)]

The Dataset package provide data in many format, with the most common one being a data frame.

For example the `ability.cov` is a covariance matrix, and the script used before will still work thanks to the use of the `as.data.frame` method.

Running the step 1 SQL, you can build the following script to load the Air Quality data.

```sql
SET SCHEMA R_DATA;

-- Uncomment the drop statement is you want to run it from scratch
-- DROP TABLE 		ABILITY;
-- DROP PROCEDURE 	LOAD_ABILITY;
-- DROP PROCEDURE 	DISPLAY_ABILITY;

CREATE COLUMN TABLE ABILITY (
	"cov.general" 	DOUBLE,
	"cov.picture" 	DOUBLE,
	"cov.blocks" 	DOUBLE,
	"cov.maze" 		DOUBLE,
	"cov.reading" 	DOUBLE,
	"cov.vocab" 	DOUBLE,
	"center" 		DOUBLE,
	"n.obs" 		DOUBLE
);

CREATE PROCEDURE LOAD_ABILITY(OUT ability "ABILITY")
LANGUAGE RLANG AS
BEGIN
  library(datasets)
  data(ability.cov)
  ability <- cbind(as.data.frame(ability.cov))
END;

CREATE PROCEDURE DISPLAY_ABILITY()
AS BEGIN
	CALL LOAD_ABILITY(ability);
	INSERT INTO ABILITY SELECT * FROM :ability;
END;

CALL DISPLAY_ABILITY();
SELECT COUNT(1) FROM ABILITY;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]
