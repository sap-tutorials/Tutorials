---
title: Import the Flowers dataset into SAP HANA, express edition
description: Download and import the Flowers dataset into SAP HANA, express edition
primary_tag: topic>machine-learning
auto_validation: true
tags: [ tutorial>intermediate, topic>cloud, topic>machine-learning ]
time: 15
---

## Details
### You will learn  
  - Download the Flowers dataset locally
  - Connect to SAP HANA, express edition
  - Split the training dataset into a train/evaluate table
  - Import the Flowers dataset

In the current scenario, you will reuse a pre-trained image recognition model with a dataset made of 5 types of flowers (daisy, dandelion, roses, sunflowers & tulips).

As part of the [TensorFlow Hub](https://www.tensorflow.org/hub/), a library to foster the publication, discovery, and consumption of reusable parts of machine learning models, you can now easily import and customize a pre-trained model.

Thanks to the [How to Retrain an Image Classifier for New Categories](https://www.tensorflow.org/tutorials/image_retraining) TensorFlow tutorial, a script can be reused to leverage a pre-trained image recognition model.

In fact the last layer of the model will be retrained with the new image. This model was initially built using millions (if not billions) of images.

[ACCORDION-BEGIN [Step 1: ](Access the SageMaker Notebook instance)]

If you have your Jupyter Notebook instance already open in your browser, then you can move to the next step.

Else, access the <a href="https://console.aws.amazon.com/sagemaker" target="&#95;blank">Amazon SageMaker Console</a> (you also use the search for **SageMaker** in the Amazon Web Services Management Console).

![Amazon Web Services](sagemaker-01.png)

Click on **Open `JupyterLab`**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create a Notebook document)]

Once open, you should have access to your Notebook instance.

![Amazon Web Services](sagemaker-02.png)

On the menu bar, select **File > New > Notebook**.

![Amazon Web Services](sagemaker-03.png)

Select **`conda_tensorflow_p36`** as Kernel then click on **Select**.

![Amazon Web Services](sagemaker-04.png)

Rename your notebook document **`hxe-aws-eml-flowers-00.ipynb`** using the menu bar with **File > Rename Notebook...**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Download the dataset files)]

Now, you can download the flowers pictures library locally.

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
%%sh

rm -rf ~/SageMaker/data
mkdir -p ~/SageMaker/data
cd ~/SageMaker/data

curl -LO http://download.tensorflow.org/example_images/flower_photos.tgz
tar xzf flower_photos.tgz
rm flower_photos.tgz
```

However, before proceeding with the retraining, you need to save some of the input files for testing the model later.

In the next cell, paste the following code then press **SHIFT** + **ENTER** to execute the code:

```Python
%%sh

rm -rf ~/SageMaker/data/flower_photos_test
mkdir -p ~/SageMaker/data/flower_photos_test

cd ~/SageMaker/data/flower_photos_test
mkdir -p ./daisy ./dandelion ./roses ./sunflowers ./tulips

cd  ~/SageMaker/data/flower_photos
find ./daisy \
  | tail -n 5 \
  | xargs -I{} mv {} ~/SageMaker/data/flower_photos_test/daisy
find ./dandelion \
  | tail -n 5 \
  | xargs -I{} mv {} ~/SageMaker/data/flower_photos_test/dandelion
find ./roses \
  | tail -n 5 \
  | xargs -I{} mv {} ~/SageMaker/data/flower_photos_test/roses
find ./sunflowers \
  | tail -n 5 \
  | xargs -I{} mv {} ~/SageMaker/data/flower_photos_test/sunflowers
find ./tulips \
  | tail -n 5 \
  | xargs -I{} mv {} ~/SageMaker/data/flower_photos_test/tulips
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the table structure)]

As you may have noticed, the Flowers dataset doesn't provide a ***training*** and a ***test*** dataset, but simply a series of subfolders representing each type of flowers.

At the end of the tutorial series, you will be testing the model from SAP HANA, which implies that some of the images are available in a table. Technically speaking, you don't need the full dataset available in SAP HANA table as Amazon SageMaker, by default, requires the dataset to be available in a Amazon S3 bucket.

Therefore, you will create the following tables:

 - `FLOWERS_TRAINING`: the full training dataset as downloaded locally
 - `FLOWERS_TEST`: the full test dataset as downloaded locally

It is assumed that you have completed the [Prepare Your SAP HANA, express edition Instance for Machine Learning](mlb-hxe-setup-basic) tutorial as you will be reusing the created **`ML_USER`**.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement:

```SQL
CREATE SCHEMA IRIS;
SET SCHEMA IRIS;

DROP TABLE IRIS_TRAINING;
DROP TABLE IRIS_TEST;

CREATE TABLE IRIS_TRAINING (SEPALLENGTH FLOAT, SEPALWIDTH FLOAT, PETALLENGTH FLOAT, PETALWIDTH FLOAT, SPECIES INT);
CREATE TABLE IRIS_TEST     (SEPALLENGTH FLOAT, SEPALWIDTH FLOAT, PETALLENGTH FLOAT, PETALWIDTH FLOAT, SPECIES INT);
```

As a reminder, you can connect using HDBSQL to your HXE instance with the following command (assuming you didn't change the `ML_USER` password):

```shell
hdbsql -n localhost:39015 -u ML_USER -p Welcome19Welcome19
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Import the Iris dataset in SAP HANA)]

In order to import the Iris dataset files, you will be using ***IMPORT FROM SQL command***.

For more details, you can check the [Import CSV into SAP HANA, express edition using IMPORT FROM SQL command](mlb-hxe-import-data-sql-import) tutorial.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement:

Before running the following command, if you are using HDBSQL, you need to enable the multi-line mode using the following command in order to successfully run the above commands:

```sql
\mu
```

Then you can run the following command:

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

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement:

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
