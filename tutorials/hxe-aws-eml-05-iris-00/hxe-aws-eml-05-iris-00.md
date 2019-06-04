---
title: Import the Iris dataset into SAP HANA, express edition
description: Download and import the Iris dataset into SAP HANA, express edition
primary_tag: topic>machine-learning
auto_validation: true
tags: [ tutorial>intermediate, topic>cloud, topic>machine-learning, products>sap-hana\,-express-edition, products>sap-hana ]
time: 15
---

## Details
### You will learn  
  - Download the Iris dataset locally
  - Connect to SAP HANA, express edition
  - Split the training dataset into a train/evaluate table
  - Import the Iris dataset

In the current scenario, you will build a Deep Neural Network (DNN) mode using the Iris dataset. This dataset is about predicting a class of flower among three possible value (Setosa, Versicolor and Virginica) using only four attributes, their Petal and Sepal length and width.

[ACCORDION-BEGIN [Step 1: ](Access your SAP HANA, express edition instance)]

You can either connect to your SAP HANA, express edition using an SSH client (as described in [Set up SAP HANA, express edition on Amazon Web Services](hxe-xsa-aws-setup)) or use your Jupyter Notebook instance.

If you want to use your Jupyter Notebook instance, open a new Terminal using the menu bar **File > New > Terminal**.

Then run the following command which assumes that you have uploaded your private key file (PEM file) in the SageMaker NoteBook:

```shell
ssh -i <path to pem file>/<pem file name> ec2-user@<instance public IP address>
```

Once you have established an SSH session, the **`ec2-user`** user is logged in and the terminal prompt should be:

```
ec2-user@hxehost:~>
```

Switch now to user **`hxeadm`** to start configuring your instance:

```ssh
sudo su - hxeadm
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Download the dataset files)]

In your SSH session terminal, paste the following code:

```shell
# create folder and remove old files if any
mkdir -p /usr/sap/HXE/HDB90/work/iris
rm -Rf /usr/sap/HXE/HDB90/work/iris/iris_*.csv*

# download the data set locally from http://download.tensorflow.org/
wget -P /usr/sap/HXE/HDB90/work/iris "http://download.tensorflow.org/data/iris_training.csv" -q
wget -P /usr/sap/HXE/HDB90/work/iris "http://download.tensorflow.org/data/iris_test.csv" -q
```
This will create a new directory where the Iris training and test dataset will be downloaded.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the table structure)]

As you may have noticed, the Iris dataset provides a ***training*** and a ***test*** dataset.

In the next tutorial, you will use the ***training*** dataset to run some data analysis and the ***test*** dataset will be used to tryout the model later on using SAP HANA SQL scripts..

Therefore, you will create the following tables:

 - `IRIS_TRAINING`: the full training dataset as downloaded locally
 - `IRIS_TEST`: the full test dataset as downloaded locally

It is assumed that you have completed the [Prepare for Machine Learning](hxe-aws-eml-04) tutorial as you will be reusing the created **`ML_USER`**.

Using the **HXE** connection with the **`ML_USER`** user credentials, execute the following SQL statement:

```SQL
CREATE SCHEMA IRIS;
SET SCHEMA IRIS;

DROP TABLE IRIS_TRAINING;
DROP TABLE IRIS_TEST;

CREATE TABLE IRIS_TRAINING (SEPALLENGTH FLOAT, SEPALWIDTH FLOAT, PETALLENGTH FLOAT, PETALWIDTH FLOAT, SPECIES INT);
CREATE TABLE IRIS_TEST     (SEPALLENGTH FLOAT, SEPALWIDTH FLOAT, PETALLENGTH FLOAT, PETALWIDTH FLOAT, SPECIES INT);
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

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Import the Iris dataset in SAP HANA)]

In order to import the Iris dataset files, you will be using ***IMPORT FROM SQL command***.

For more details, you can check the [Import CSV into SAP HANA, express edition using IMPORT FROM SQL command](mlb-hxe-import-data-sql-import) tutorial.

Using the **HXE** connection with the **`ML_USER`** user credentials, execute the following SQL statement:


```sql
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/iris/iris_training.csv' INTO IRIS.IRIS_TRAINING
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY ','
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/iris/iris_training.csv.err'
;
IMPORT FROM CSV FILE '/usr/sap/HXE/HDB90/work/iris/iris_test.csv' INTO IRIS.IRIS_TEST
WITH
   RECORD DELIMITED BY '\n'
   FIELD DELIMITED BY ','
   OPTIONALLY ENCLOSED BY '"'
   SKIP FIRST 1 ROW
   FAIL ON INVALID DATA
   ERROR LOG '/usr/sap/HXE/HDB90/work/iris/iris_test.csv.err'
;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Validate the imported data)]

You can now validate that you have properly imported the data.

Using the **HXE** connection with the **`ML_USER`** user credentials, execute the following SQL statement:

```sql
SELECT * FROM
(
	SELECT 'TRAINING' as DATASET, SPECIES, COUNT(1) as count
	FROM IRIS.IRIS_TRAINING
	GROUP BY SPECIES
	UNION ALL
	SELECT 'TEST' as DATASET, SPECIES, COUNT(1) as count
	FROM IRIS.IRIS_TEST
	GROUP BY SPECIES
)
ORDER BY 1, 3
;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
