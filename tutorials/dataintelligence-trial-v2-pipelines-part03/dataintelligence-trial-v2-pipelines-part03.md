---
title: Bundle Sensor Data in SAP Data Intelligence, Trial Edition
description: Bundle sensor data by JavaScript before storing it in Cloud Storage by using SAP Data Intelligence, Trial Edition.
primary_tag: products>SAP-data-intelligence
auto_validation: true
tags: [  tutorial>beginner, topic>big-data, products>sap-data-intelligence ]
time: 15
author_name: Roy Zhong
author_profile: https://github.com/roy-zhong
---

## Details
### You will learn  
- How to bundle the sensor data before storing in AWS S3 or Google Cloud Storage or Azure WASB
- How to use a **JS String Operator**

Please note in this tutorial GCP refers to Google Cloud platform and AWS refers to Amazon Web Services and Azure refers to Microsoft Azure.

---

[ACCORDION-BEGIN [Step 1: ](Add JS String operator)]

Open the pipeline which you have created in the [previous tutorial](dataintelligence-trial-v2-pipelines-part02) `(test.myFirstPipeline)`, in the modeling environment.

1. Remove the connection between the **Data Generator** operator and the **To File** operator.

2. Add a **JS String Operator** to the pipeline by drag & drop.

3. Connect the `output` port of the **Data Generator** operator to the `input` port of the **JS String Operator**.

4. Connect the `output` port of the **JS String Operator** to the `in` port of the **To File** operator.

    !![Add JS String Operator](datahub-trial-v2-pipelines-part03-1.png)

5. Right click **Write File** operator, open the configuration panel and change the following property:

    |  Field Name&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;     | Value
    |  :------------- | :-------------
    | path  | `sensordata/JS_file_<counter>.txt`

The **Write File** operator will write the received data to files in the `/sensordata` directory in the specified GCS or AWS S3 bucket or Azure container. The files follow the scheme `JS_file_<counter>.txt` (where counter is an incremental integer).

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create JavaScript extension)]

1. Right click the JS String Operator.

2. Click on **Open Script** to display the JavaScript snippet which is executed by the **JS String Operator**. The JavaScript snippet opens in a new tab.

    !![Source Code](datahub-trial-v2-pipelines-part03-2.png)

    Currently the JavaScript snippet creates an incremental **counter** every time it receives data via the input port and sends the **counter** to the output port.

3. Replace the code with the following snippet to ensure that `bundles` of 30 sensor records are sent to the output port.

    ```javascript

    var counter = 0;
    var bundle = "";

    $.setPortCallback("input",onInput);

    function onInput(ctx,s) {
        counter++;
        bundle = bundle + s;

        if(counter==30) {
          $.output(bundle);
          counter = 0;
          bundle = "";
        }
    }
    ```
4. Navigate back to the pipeline tab.

5. Click **Save** to save the pipeline including the modified script.

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Execute the data pipeline)]

1. Click **Run** to execute the pipeline.

2. When the **Status** tab indicates that the pipeline is running.

3. Use the context menu **Open UI** of the **Wiretap** operator to see the generated sensor data. You can notice that this time the output is grouped in a chunk of 30 records.

    For GCP open [http://console.cloud.google.com](http://console.cloud.google.com) and navigate to the `/sensordata/` directory.
    For AWS open [https://s3.console.aws.amazon.com](https://s3.console.aws.amazon.com) and navigate to **Search for Buckets** > **Your Bucket name** > `sensordata` folder.
    For Azure open [https://portal.azure.com/](https://portal.azure.com/) and navigate to **Storage accounts** > **filter your Storage account** > **Blob service** > **click Blob** > **Your Container name** > `sensordata folder`.

    You see that the system does not create a file for each single sensor record, but only for each 30 sensor records.

4. Stop the pipeline by clicking **Stop**.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Check the created files in GCS or AWS S3 or Azure WASB)]

Login to Google Cloud Platform - [http://console.cloud.google.com](http://console.cloud.google.com) and navigate to **GCP Left menu** > **Storage** > **Browser** > **Your Bucket name** > `sensordata` folder.

For AWS open [https://s3.console.aws.amazon.com](https://s3.console.aws.amazon.com) and navigate to **Search for Buckets** > **Your Bucket name** > `sensordata` folder.

For Azure open [https://portal.azure.com/](https://portal.azure.com/) and navigate to **Storage accounts** > **filter your Storage account** > **Blob service** > **click Blob** > **Your Container name** > `sensordata folder`.

!![picture4](datahub-trial-v2-pipelines-part03-4.png)

You can open any of the generated file by clicking on the filename which opens in a new tab for GCS. For AWS S3 click on the filename and then click on Open. For Azure WASB click the filename and then click on Edit blob.


[VALIDATE_1]

[ACCORDION-END]

---
