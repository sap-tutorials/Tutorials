---
title: Bundle data (via JavaScript)
description: Bundle sensor data before storing it in HDFS by using SAP Data Hub, developer edition.
primary_tag: products>sap-data-hub
tags: [  tutorial>beginner, topic>big-data, products>sap-data-hub, products>sap-vora ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - You have completed [Store sensor data in HDFS](https://www.sap.com/developer/tutorials/datahub-pipelines-storeinhdfs.html)

## Next Steps
- [Store sensor data in SAP Vora](https://www.sap.com/developer/tutorials/datahub-pipelines-storeinvora.html)

## Details
### You will learn  
During this tutorial, you will learn how you can "bundle" (in the sense that not each single sensor record creates a new file) the sensor data before you store it in HDFS. You will use a **JavaScript Operator** for this.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Add JavaScript Operator)]
Open the pipeline which you have created during the previous tutorials (`test.myFirstPipeline`) in the modelling environment `http://localhost:8090`. For this, select the **Graphs** tab in the tab bar on the left side and search for `test.myFirstPiepline`.

Remove the connection between the **`Kafka Consumer2`** operator and the **HDFS Producer** operator.

Add a **`ToString` Converter** operator to the pipeline by drag & drop. Also add a **JavaScript Operator** to the pipeline by drag & drop.

Connect the `message` port of the **`Kafka Consumer2`** operator to the `ininterface` port of the **`ToString` Converter** operator.
Connect the `outstring` port of the **`ToString` Converter** operator to the `input` port of the **JavaScript Operator**.
Connect the `output` port of the **JavaScript Operator** to the `inFile` port of the **HDFS Producer** operator.

![picture_01](./datahub-pipelines-bundledata_01.png)  

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create JavaScript extension)]
Right click on the **JavaScript Operator** and click on **Open Script** to display the JavaScript snippet which is executed by the **JavaScript Operator**. The JavaScript snippet opens in a new tab.

![picture_02](./datahub-pipelines-bundledata_02.png)  

Currently the JavaScript snippet creates an incremental **counter** every time it receives data via the `input` port and sends the **counter** to the `output` port.

Replace the code with the following snippet to ensure that "bundles" of 30 sensor records are sent to the `output` port.

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

Close the tab for the JavaScript snippet. Afterwards press the **Save** button.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Execute the data pipeline)]
Before you execute the pipeline, delete the contents from the `/tmp/hdfsManager` directory. Thereto log into the running container.

```sh
docker exec -it datahub /bin/bash
```

Then enter the following command.

```sh
hdfs dfs -rm /tmp/hdfsManager/*
```

You can check the deletion via `http://localhost:50070`.

Press the **Run** button to execute the pipeline.

When the **Status** tab indicates that the pipeline is running, use the context menu **Open UI** of the **Terminal** operator to see the generated sensor data.

Open `http://localhost:50070` and display the `/tmp/hdfsManager` directory. You see that the system does not create a file for each single sensor record, but only for each 30 sensor records.

Stop the pipeline by pressing the **Stop** button.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Check the created files in HDFS)]
To check the content of the created files, log into the running container and enter the following (to see, for example, the content of file `test_1.txt` ).

```sh
hdfs dfs -cat /tmp/hdfsManager/test_1.txt
```

![picture_03](./datahub-pipelines-bundledata_03.png)  

**Attention:** You might notice that the first line of file `test_1.txt` does not necessarily have the counter `0` (that is the first column of the file and in the above screenshot it has the counter `123`) as one might expect when looking at the JavaScript snippet which is executed by the **Data Generator**. The reason for this can be that the **Kafka Producer** had sent a message to Kafka, but then you most likely stopped the pipeline before the **Kafka Consumer** consumed the message. When you afterwards restarted the pipeline, the **Kafka Consumer** first of all processed this "previous" message. For the sake of this tutorial you do not have to bother about this behavior.

[ACCORDION-END]

---
