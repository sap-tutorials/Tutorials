---
title: Store sensor data in SAP Vora in SAP Data Hub 2
description: Use SAP Vora to store sensor data by using SAP Data Hub, trial edition.
primary_tag: products>sap-data-hub
tags: [  tutorial>beginner, topic>big-data, products>sap-data-hub, products>sap-vora ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - You have completed [Bundle data (via JavaScript)](https://www.sap.com/developer/tutorials/datahub-trial-pipelines-part04.html)

## Details
### You will learn  
- How to store the data in SAP VORA from Google Cloud Storage
- How to use a **SAP Vora Client**

Please note that this tutorial is similar to the `Store sensor data in SAP Vora` tutorial from [SAP Data Hub, developer edition tutorial group](https://www.sap.com/developer/groups/datahub-pipelines.html).

### Time to Complete
**45 Mins**

---

[ACCORDION-BEGIN [Step 1: ](Create GCS Function Call with Parameters)]

This step requires utmost attention as the configuration is more prone to errors.

Our goal in this tutorial is to attach all the generated files as Data sources to the Vora table. But as the files reside in a Google bucket, Vora needs to use an API in order to access these files. In this section, we would be creating the API call with all the required attributes which can be used in the next step.

Open the **JSON key** that we have downloaded in the tutorial **Store sensor data in GCS** in any editor of your choice. We would be using the key attributes in creating the GCS function call.

The function call should look like :

```sql

GCS('Iss=xxxxxxx@xxxxx.xxx&Key=xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx&Project=xxx-xxxx-xxxx-xxxxx&Path=bucketname/" + s + "')
```

You can now copy the above code to any editor of your choice and replace the following attributes with their values :

- `Iss` (required) - It is shown as `client_email` in the JSON file of your service account
- **Key** (required) - It is shown as `private_key` in the JSON file of your service account. You need to remove the initial `-----BEGIN PRIVATE KEY-----\n` and final `\n-----END PRIVATE KEY-----`. You also need to replace all `\n` occurrences with spaces. You can use the replace function in the text editor to replace all the occurrences at once. So the key should look like `XlgFSm4B9PeEDLB2NzPg1eqm TNqXof9jnOkmL3aun3Yhv6oUQfZAoGAPoEYSy1kSAQ2l9bnMiea 05AsVj8BXxpviDHBHIZV8BRdhip9h ....`
- **Project** (required): It is shown as `project_id` in the JSON file of your service account.
- **Timeout** : The period of time in seconds before an operation fails. The default is 180 seconds.
- **Proxy** : The proxy server address that will be used by the plugin. If you set it to `noproxy`, the plugin will not go through any proxy server.
- **Path** : The file or directory in which all operations will be performed (it must already exist). If set, it must be the last parameter. Default: empty (root directory). **Note that in our case it will be your GCS bucket name** For example - `bucketname/`

>The key will be much longer than the one shown here in the example.

Once you have filled in all the attributes, keep the final code handy as we are going to use it in the next step.

```
Sample code - GCS('Iss=client_email@email.com&Key=XlgFSm4B9PeEDLB2NzPg1eqm TNqXof9jnOkmL3aun3Yhv6oUQfZAoGAPoEYSy1kSAQ2l9bnMiea 05AsVj8BXxpviDHBHIZV8BRdhip9h&Project=my-project-name&Path=bucketname/" + s + "')
```
Above code is just a sample code that demonstrates how your call should look like.

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Add JavaScript and Vora Client Operator)]

As we don't have a `SAP Vora HdfsLoader Operator` equivalent for `GCS-Vora`, we would need to read the files from the GCS bucket and prepare SQL statements based on the file names and paths that can be provided to the **SAP Vora Client Operator**. The Vora Client then reads the files from the Google storage and attaches the file as a data source to the Vora table. From the previous step, we have the API call ready with us which we can use now in the JavaScript code.

Open the pipeline which you have created in the previous tutorial `(test.myFirstPipeline)`, in the modelling environment (`https://sapdatahubtrial/app/pipeline-modeler`)

>As the above URL is a local URL, it will be accessible only if you are doing the tutorials and have already configured the hosts file. If not, please refer to [Getting Started with SAP Data Hub, trial edition](https://caldocs.hana.ondemand.com/caldocs/help/Getting_Started_Data_Hub_23.pdf) guide.

Add a **JavaScript Operator** and a **`ToString Operator`** to the pipeline by drag & drop. Also add a **SAP Vora Client** operator similarly. Then connect the `outFilename` port of the **Read File** (this is an unused port in the Read File operator) to the `ininterface` in port of the **`ToString Operator`**. Then connect the `outstring` out port of the **`ToString Operator`** to the `input` in port of the **JavaScript Operator**. Also connect the `output` out port of the **JavaScript Operator** to the `sql` port of the **SAP Vora Client Operator**

![picture1](datahub-trial-v2-pipelines-part05-1.png)

Click **Script (Highlighted in the above screenshot)** to display the JavaScript snippet which is executed by the **JavaScript Operator**. The JavaScript snippet opens in a new tab.

Replace the code with the following snippet. The below given code creates a table for the sensor data and adds every output file from GCS as a Data source. Therefore, when you see the table contents in Vora, you would not notice any difference between a table row and an attached data source.

```javascript

$.setPortCallback("input",onInput);

var altertablesql = "";
var delflag = true;
//onInput function receives file name as an input through the parameter - s
function onInput (ctx,s)
{
    // Initialize the variable holding the SQL command
    var altertablesql = "";
    // Flag to ensure that table create and drop datasource commands execute only once every execution
    if (delflag)
    {
        //altertablesql += "drop table SENSORDATA;";    
        altertablesql += "CREATE TABLE IF NOT EXISTS SENSORDATA (counter INTEGER, deviceid INTEGER, temperature DOUBLE, humidity DOUBLE, co2 DOUBLE, co DOUBLE, lpg DOUBLE, smoke DOUBLE, presence INTEGER, light DOUBLE, sound DOUBLE) TYPE DATASOURCE STORE ON DISK;";    
        altertablesql += "ALTER TABLE SENSORDATA DROP DATASOURCE ALL;";
        delflag = false;
    }
    // Every file from GCS bucket is attached as a data source
    altertablesql += "ALTER TABLE SENSORDATA ADD DATASOURCE <insert code here> DELIMITED BY ',';";
    // Datasources are attached only after the following command is executed
    altertablesql += "LOAD TABLE SENSORDATA;";
    // SQL command variable is passed on as an output to the VORA client
    $.output(altertablesql);
}


```

In the copied code, replace the text `<insert code here>` with the GCS function call we have prepared in the first step. The sample code should look like follows :

```
$.setPortCallback("input",onInput);

var altertablesql = "";
var delflag = true;

function onInput (ctx,s)
{
    var altertablesql = "";
    if (delflag)
    {
        //altertablesql += "drop table SENSORDATA;";    
        altertablesql += "CREATE TABLE IF NOT EXISTS SENSORDATA (counter INTEGER, deviceid INTEGER, temperature DOUBLE, humidity DOUBLE, co2 DOUBLE, co DOUBLE, lpg DOUBLE, smoke DOUBLE, presence INTEGER, light DOUBLE, sound DOUBLE) TYPE DATASOURCE STORE ON DISK;";
        altertablesql += "ALTER TABLE SENSORDATA DROP DATASOURCE ALL;";    
        delflag = false;
    }
    altertablesql += "ALTER TABLE SENSORDATA ADD DATASOURCE GCS('Iss=client_email@email.com&Key=XlgFSm4B9PeEDLB2NzPg1eqm TNqXof9jnOkmL3aun3Yhv6oUQfZAoGAPoEYSy1kSAQ2l9bnMiea 05AsVj8BXxpviDHBHIZV8BRdhip9h&Project=my-project-name&Path=bucketname/" + s + "') DELIMITED BY ',';";
    altertablesql += "LOAD TABLE SENSORDATA;";
    $.output(altertablesql);
}

```
>The key will be much longer than the one shown here in the example. Also, the drop table command has been commented here but if in case you are not executing it for the first time, you can uncomment it in your code. It merely deletes the table structure along with its contents before creating a new one. You should uncomment the code if you encounter the error "Table already exists" later while executing the pipeline.

Save the code changes by clicking the **Save** button at the top of the page in the graph tab as there is no separate save button for the java script snippet. After this then close the tab for the JavaScript snippet.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Configure the VORA Client Operator)]

Now we would be configuring the second operator that we have added - **SAP Vora Client**. Open the configuration window for the operator and click on the **Connection** attribute as we only have to modify this value. To specify connection details you can either utilize the connections established in the Configuration Manager or provide the details manually.

In case you are willing to re-use the Configuration Manager connections, click on the **Connection** input field and select **Configuration Type** as **Configuration Manager** and select the desired connection from the **Connection ID** dropdown box. In case of Manual connection, select the **Configuration Type** as **Manual** and maintain the following properties:


|  Field Name     | Value
|  :------------- | :-------------
|  `Host`  | `vora-tx-coordinator`
|  `Port`  | `10002`
|  `vSystem Tenant`  | `default`
|  user  | Username for the application
|  password  | Your password for the above username

Firstly, **Save** the connection details and then **Save** the graph.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Execute the data pipeline)]

Before you execute the pipeline, delete the contents from the `/sensordata/` folder in the GCS bucket.

Therefore, login to Google Cloud Platform - [http://console.cloud.google.com](http://console.cloud.google.com) and navigate to **GCP Left menu** > **Storage** > **Browser** > **Your Bucket name** > `sensordata` folder. Please keep this window opened as we would be checking the generated files here again in the following steps.

Here you would be able to see all the files that were created in previous executions of the pipeline. Click on the **Select All Checkbox (1)** and then click on the **Delete (2)** button. Make sure that all the files in the folder are deleted.

![picture2](datahub-trial-v2-pipelines-part05-2.png)

Go back to the SAP Data Hub Modeler and click **Run** to execute the pipeline. When the **Status** tab indicates that the pipeline is running, use the context menu **Open UI** of the **Terminal** operator to see the generated sensor data.

Longer the pipeline runs, more files are generated. Therefore, after a while stop the pipeline execution.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Check the output in GCS and VORA)]

Open [http://console.cloud.google.com](http://console.cloud.google.com) and navigate to the `/sensordata/` directory. You can notice that the output files are generated similar to the earlier tutorial.

Now login to the **SAP Data Hub Launchpad** using the link (`https://sapdatahubtrial/`) and click on the link **VORA Tools** tile

![picture3](datahub-trial-v2-pipelines-part05-3.png)

Now once Vora Tools open up, click on the `default\systemschema`  **(1)** from the left menu and once you see our table `(SENSORDATA)` **(2)**, right click on it and select **Data Preview**.

![picture4](datahub-trial-v2-pipelines-part05-4.png)

This will display all the sensor data that we have attached as a `datasource` to the VORA table.

![picture5](datahub-trial-v2-pipelines-part05-5.png)


>Please note that all the output files in GCS are attached as data sources to this VORA table and you can remove them all with the command `ALTER TABLE sensordata DROP DATASOURCE ALL`.

[DONE]

[ACCORDION-END]

---
