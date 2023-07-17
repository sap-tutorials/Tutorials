---
parser: v2
auto_validation: true
time: 20
tags: [ tutorial>beginner, products>sap-hana-cloud, products>sap-iq, software-product-function>sap-hana-cloud\,-data-lake]
primary_tag: products>sap-hana-cloud
---

# Migrate from SAP IQ On-Premise to SAP HANA Cloud, Data Lake
<!-- description --> Prepare and execute the migration from SAP IQ on-premise to SAP HANA Cloud, data lake using an S3 or a Microsoft Azure bucket.

## Prerequisites
- You need a running SAP IQ system.
- You require a transfer on-premise machine with enough space to hold the data you wish to migrate.
- You need an S3 or a Microsoft Azure bucket to transfer the data.
- You need to have a running instance of SAP HANA Cloud, data lake with enough space to receive the amount of data you wish to migrate.

## You will learn
- How to set up SAP IQ components for migration
- How to configure an S3 or a MS Azure bucket for migration
- How to execute the migration


## Intro
SAP HANA Cloud, data lake brings you all cloud advantages that are part of SAP HANA Cloud, such as ease of provisioning, scaling flexibility, security and more. SAP manages the hardware, virtual machines, and operating systems for you, while you set up and manage your own instances to create the data management landscape you need.

SAP HANA Cloud, data lake was built to be compatible with SAP IQ on-premise . That means that it's easy to move large amounts of data from your on-premise installation to the cloud. Keep in mind that you don't have to move your whole landscape, but can rather move at your own pace.
If you are looking to migrate one of your SAP IQ databases to SAP HANA Cloud, data lake, there is a well-defined process and some tools to help you through it.

The migration process can be summarized in **three major steps**:

1.	Set up the SAP IQ components

2.	Configure an S3 or Microsoft Azure bucket

3.	Execute the migration

Let's look at each of those steps in more detail. You can also watch this demo video to assist you while executing the following steps:

<iframe width="560" height="315" src="https://www.youtube.com/embed/xrI8I4zH_C0" frameborder="0" allowfullscreen></iframe>

---

### Prepare your SAP IQ system


To begin, you need to set up a few things in your on-premise SAP IQ system. That includes the following overall steps:

1.	Install the [SAP IQ client](https://help.sap.com/viewer/4b66bcca37144fa8898eb54a444a2825/LATEST/en-US/0142394c508e437b9746f6baeb7b604b.html). Use the latest version of SAP IQ 16.1 SP04 PL07 or greater. Follow the instructions in [this page to download the file](https://help.sap.com/viewer/4b66bcca37144fa8898eb54a444a2825/LATEST/en-US/b3d9c1d0f8b040228fb2ef54647dd845.html).

2.	Install python, if that is not yet installed on your transfer machine with SAP IQ 16.1 system on it.

    In the example below, these commands were needed for a system running Red Hat 8.1 and will install both python 2 and python 3 while also setting python 3 as the default python.

    ```Shell/Bash
    yum install python38
    yum install rh-python38
    yum install rh-python36
    yum install python-tools
    yum install python3
    yum install python2
    alternatives --set python /usr/bin/python3
    python install python3-devel
    yum install python3-devel
    python3 -m pip install pyodbc
    ```

3.	Install the python utilities made available by SAP together with the latest version of the SAP IQ client software.

    ```Shell/Bash
    tar -xvf python_client-20.00.000.00.3150.tar
    ```

4.	Estimate the space needed from SAP IQ to a transfer on-premise machine or NFS. Use `sp_iqspaceused` or use the output from `sp_iqdbspace`. You can also use the output from `pre_migration.log` / `pre_migration.out`.



### Configure an S3 or Microsoft Azure bucket


Now, it's time to configure your S3 or Microsoft Azure bucket to receive the data you wish to migrate.

> ### Attention
>
> We will use an example with a Microsoft Azure bucket to illustrate the kind of setup you need, but the instructions would be a bit different in an S3 bucket.

1.	[Sign up](https://console.aws.amazon.com/s3/) for an S3 or an Azure account.

2.	Install the [Microsoft Azure utilities (CLI)](https://docs.microsoft.com/en-us/cli/azure/). Basically, you need to run an RPM to import the configuration and then update the Azure CLI dot repository file.

    ```Shell/Bash
    rpm --import https://packages.microsoft.com/keys/microsoft.asc
    vi /etc/yum.repos.d/azure-cli.repo
    ```

3.	Next, do the Yum install of the CLI configuration.

    ```Shell/Bash
    yum install azure-cli
    ```

4.	Then, login to your Azure account. You can either use the browser or the command line:

    ```Shell/Bash
    az login
    ```

5.	To sign in, use a web browser to open the page <https://microsoft.com/devicelogin> and enter your Azure credentials to authenticate. Alternatively, you can use the command:

    ```Shell/Bash
    az login --use-device-code
    ```

6.	Create a storage bucket.

7.	Create blob storage. We recommend using twice the amount of storage from the `sp_iqspaceused` to be safe.

8.	Obtain the Access Key information that contains the key details for you to log in via command line.

You can follow the next steps to complete the procedure for executing the migration.





### Update your extract or transfer config file


Now that the storage is set and the space is allocated, it's time to execute the migration of your data. Here are the steps to migrate:

1.	Configure the file system space on the machine running the Python tools software for export.

2.	Update your extract or transfer `config` file.

    As an example of the `config` file, here is the JSON file used in the video demo:

    ```JSON
    #ITEMS seen in Lines 6-11 and 14-16 are used to EXTRACT the data from SAP IQ.
    #ITEMS seen in Lines 17-29 are used to LOAD data into SAP HANA Cloud, data lake.
    #ITEMS seen in Lines 12 and 13 are optional parameters.
    #Note that all the text fields are "quoted", while numeric fields are not.
    {
    "Host_Name": "iq005",
    "Port_Number": 2638,
    "DBA_User": "DBA",
    "DBA_Pwd": "PASSWORD",
    "Extract_Path": "/home/python_client",
    "Client_Num_Conn": 10,
    "IQ_Host_Login_Id":"<Optional: IQ host login id for ssh connection if IQ installation and Migration Utility Installation (17.1 HDL client installation) are at different host>",
    "IQ_Host_Login_Pwd":"<Optional: IQ host login password for ssh connection>",
    "IQ_Version": "16.1",
    "IQ_Server_Install_Path": "/home/sybaseiq/iq161_sp04PL07",
    "Datalake_Client_Install_Path": "/home/python_client",
    "HDLADMIN_User": "HDLADMIN",
    "HDLADMIN_Pwd": "YOUR PASSWORD",
    "HDL_Coord_Endpoint": " COORDINATOR ENDPOINT TO YOUR DATA LAKE ",
    "HDL_Writer_Endpoint": "SQL ENDPOINT TO YOUR DATA LAKE",
    "HDL_Num_Writer_Conn": 2,
    "HDL_Num_Coord_Conn": 2,
    "Object_Store_Copy_Validation": "No",
    "Hyperscaler_Details": {
        "Name": "HYPERSCALER NAME",
        "Credentials": {
             "Azure_Account_Name": "YOUR STORAGE BLOB NAME",
             "Azure_Account_Key" : "YOUR ACCOUNT KEY",
             "Container_Name"    : "YOUR CONTAINER NAME"
            }
        }
    }
    ```


### Extract the data using the SAP IQ python script




1. Now to verify that the data upload to the Microsoft Azure bucket was successful, you can run the following python command, which changes the location of the directory to migration folder:


    ```Python
cd /home/python_client/HDL_Migration/Migration
```

    This should be the IQ client Directory = $SYBASE

2. Run the following python command with the `config` file.

    ```Python
migration.py --config /home/python_client/HDL_Migration/Common/config_azure.json   -m w
```

    `-m` = mode = `w` = write

    This means that the SAP IQ system is "writeable" during the whole extraction process and it's up to you to do the reconciliation. The `–m w` can be left off if the system will be a "read only" and no changes need to be made. Once the system is in the re-write mode, you need to use `-m w`.

    An example for the output is shown below:

    ```
------------------------------------------------------------------------------------------
Shared directory /home/python_client exists to store Extracted files.
------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------
Starting schema unload
--------------------------------------------------------------------------------
SAP IQ Unload Utility Version 16.1.40.1490
Connecting and initializing
2021-02-18 08:20:53 Unloading user and group definitions
2021-02-18 08:20:53 Unloading spatial units of measure
2021-02-18 08:20:53 Unloading spatial reference systems
2021-02-18 08:20:53 Unloading sequences
2021-02-18 08:20:53 Unloading table definitions
2021-02-18 08:20:54 Unloading text configurations
2021-02-18 08:20:54 Unloading materialized view definitions
2021-02-18 08:20:54 Unloading index definitions
2021-02-18 08:20:54 Unloading functions
2021-02-18 08:20:54 Unloading view definitions
2021-02-18 08:20:54 Unloading procedures
2021-02-18 08:20:54 Unloading triggers
2021-02-18 08:20:54 Unloading SQL Remote definitions
2021-02-18 08:20:54 Unloading MobiLink definitions
Successfully unloaded schema from database "/home/sybaseiq/iq161_sp04PL07/IQ-16_1/demo/iqdemo.db".
--------------------------------------------------------------------------------
Schema unload completed successfully and output logged in /home/python_client/HDL_Migration/Migration/reload.sql
--------------------------------------------------------------------------------
Modifying generated schema file: /home/python_client/Migration_Data/reload.sql
Modification of schema is complete and AutoUpdated_Reload.sql file is saved at: /home/python_client/Migration_Data
--------------------------------------------------------------------------------
Starting data unload
Please check /home/python_client/HDL_Migration/Migration/migration.log file for unload progress of tables
--------------------------------------------------------------------------------
12 tables successfully extracted and 0 tables failed out of total 12 tables.
--------------------------------------------------------------------------------
```

    In the above example, apart from the result at the bottom, you can also view the links to `migration.log` and `reload.sql` files.

3. If you take a look at the migration log file, you will see something similar to the following example:


    ```
Command that is run:
--------------------------------------------------------------------------------
Migration Utility restarted. Deleted existing /home/python_client/Migration_Data folder.
---------------------------------------------------------------------------------
Created migration directory /home/python_client/Migration_Data to store all schema and table extract files.
Input json config file is valid
--------------------------------------------------------------------------------
The current working directory is /home/python_client/HDL_Migration/Migration
--------------------------------------------------------------------------------
Python version: 3.6.8 (default, Oct 11 2019, 15:04:54)
[GCC 8.3.1 20190507 (Red Hat 8.3.1-4)]
--------------------------------------------------------------------------------
Selected Object_store = Azure
Verifying Hyperscaler_Details
--------------------------------------------------------------------------------
host : iq005 , ipaddress : 192.168.1.121 , fullhostname : iq005
--------------------------------------------------------------------------------
IQ Database version:
SAP IQ/16.1.040.1490/14465/P/SP04.07/Linux/Linux64 - x86_64 - 3.10.0-327/64bit/2020-09-10 04:51:43
Extraction of data will be in compressed format with parallelization.
--------------------------------------------------------------------------------
IQ Database Charset: ISO_8859-1:1987
--------------------------------------------------------------------------------
Created extract directory /home/python_client/Migration_Data/Extracted_Data to store table extract files.
--------------------------------------------------------------------------------
Migration Utility is executed from same host as of Coordinator / IQ server
--------------------------------------------------------------------------------
Starting schema unload
Schema unload completed successfully and output logged in /home/python_client/HDL_Migration/Migration/reload.sql
--------------------------------------------------------------------------------
/home/python_client/HDL_Migration/Migration/reload.sql is moved to /home/python_client/Migration_Data/reload.sql
--------------------------------------------------------------------------------
Modifying generated schema file: /home/python_client/Migration_Data/reload.sql
Modification of schema is complete and AutoUpdated_Reload.sql file is saved at: /home/python_client/Migration_Data
--------------------------------------------------------------------------------
Starting data unload
--------------------------------------------------------------------------------
Output:
Total number of active MPX reader nodes: 0
Total number of active MPX writer nodes: 0
--------------------------------------------------------------------------------
Extraction of table GROUPO.Customers was successful
Adding entry in /home/python_client/Migration_Data/ExtractedTables.out file for table: GROUPO.Customers
Time taken to unload table = Customers is : 0 days, 0 hours, 0 minutes and 0 seconds
--------------------------------------------------------------------------------
Extraction of table GROUPO.Employees was successful
Adding entry in /home/python_client/Migration_Data/ExtractedTables.out file for table: GROUPO.Employees
Time taken to unload table = Employees is : 0 days, 0 hours, 0 minutes and 0 seconds
….
Extraction of table DBA.iq_dummy was successful
Adding entry in /home/python_client/Migration_Data/ExtractedTables.out file for table: DBA.iq_dummy
Time taken to unload table = iq_dummy is : 0 days, 0 hours, 0 minutes and 0 seconds
--------------------------------------------------------------------------------
Data unload completed.
Total number of unloaded tables = 12
Total Time taken in unload : 0 days, 0 hours, 0 minutes and 1 seconds
--------------------------------------------------------------------------------
Sample command to copy the data to Azure object store:
az storage blob upload-batch --connection-string "DefaultEndpointsProtocol=https;AccountName=<Azure Account Name>;AccountKey=<Azure Account Key>;EndpointSuffix=core.windows.net"
-d iqdemocontainer -s /home/python_client/Migration_Data
--------------------------------------------------------------------------------
================================================================================
Extraction of all tables is successful
```

    The `migration.log` file gives you more information on the directory being used, the system, the host, the extracted directory, and the extracted data location.

    Note that near the bottom are some log files. If there are no issues, you can just skip them.


### Upload the data to an S3 or Microsoft Azure bucket


Now, to upload the data to your S3 or Microsoft Azure bucket, you will need the following commands.

1.	Log in to Microsoft Azure first:

    ```Shell/Bash
az login
```

2.	After you run the login command, you will see the following message:

    ```
To sign in, use a web browser to open the page https://microsoft.com/devicelogin and enter the code AAWQNFRW7 to authenticate.
or:
az account list
```

3.	If you need help at this point, you can use the following command to get help:

    ```Shell/Bash
az storage blob upload-batch –help
```

4.	Next you can use these basic commands to upload the data:

    ```Shell/Bash
az storage blob upload-batch -d ... (destination container name)
-s ... (source directory) /home/iqexport/Migration_Data
--connection-string "CONNECTION_STRING"
```

5.	This is an example of the output you will see once the upload is completed, which shows all files uploaded:

    ```
Finished[#############################################################]  100.0000%
[
  {
    "Blob": "https://hdlstorageblob.blob.core.windows.net/iqdemocontainer/iq_tables.list",
    "Last Modified": "2021-02-18T13:23:31+00:00",
    "Type": null,
    "eTag": "\"0x8D8D410630C68A3\""
  },
  {
    "Blob": "https://hdlstorageblob.blob.core.windows.net/iqdemocontainer/reload.sql",
    "Last Modified": "2021-02-18T13:23:32+00:00",
    "Type": "application/sql",
    "eTag": "\"0x8D8D4106324FE2E\""
  },
  {
    "Blob": "https://hdlstorageblob.blob.core.windows.net/iqdemocontainer/AutoUpdated_Reload.sql",
    "Last Modified": "2021-02-18T13:23:32+00:00",
    "Type": "application/sql",
    "eTag": "\"0x8D8D4106336B4A3\""
  },
  {
    "Blob": "https://hdlstorageblob.blob.core.windows.net/iqdemocontainer/Extracted_Data/GROUPO.FinancialData/FinancialData1_1.gz",
    "Last Modified": "2021-02-18T13:23:32+00:00",
    "Type": null,
    "eTag": "\"0x8D8D410633E7E44\""
  },
…
```


### Load the data into your target SAP HANA Cloud, data lake


Finally, here is an example of the commands you can run to load the data into your SAP HANA Cloud, data lake. These commands will import the data into the SAP IQ server using a python script using the `config` file that was used previously.

```
[sybaseiq@iq005 ]$ python ./Migration/load_schema_and_data.py --config_file  	/home/python_client/HDL_Migration/Common/config_azure.json
--------------------------------------------------------------------------
Schema load on HDL started.
--------------------------------------------------------------------------
Schema load on HDL complete.
Please check /home/python_client/Migration_Data/HDL_LoadSchema.log file for more details.
--------------------------------------------------------------------------
Data load on HDL started.
------------------------------------------------------------------------------------------
Data Loading on HDL is in progress.
Please check /home/python_client/Migration_Data/HDL_LoadedTables.out file for details of tables loaded on HDL successfully.
Please check /home/python_client/HDL_Migration/load_schema_and_data.log file for Load progress.
------------------------------------------------------------------------------------------
11 tables successfully loaded and 1 tables failed out of total 12 tables.
--------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
Loading of Schema and Data on HDL completed.
Please check /home/python_client/HDL_Migration/load_schema_and_data.log file for details
------------------------------------------------------------------------------------------
```

Note that the output in this example shows failure to upload one table among the 12 tables. The details on the failed table upload is provided in the error file.

And with that, you know all the important steps of migrating from SAP IQ on-premise to SAP HANA Cloud, data lake.

Here are some useful links that can further assist you in the process:

- [Migration Guide](https://help.sap.com/viewer/f70c0f7e072d4f969d3c8b4bc95b4214/LATEST/en-US/84795c5dc3ea41f0982016e57bf2d23f.html)

- [Python Red Hat](https://developers.redhat.com/blog/2019/05/07/what-no-python-in-red-hat-enterprise-linux-8)

- [Python ODBC](https://medium.com/analytics-vidhya/install-pyodbc-on-centos-rhel-ae5062830a2b)

- [Python includes](https://stackoverflow.com/questions/21530577/fatal-error-python-h-no-such-file-or-directory)

- `Pyodbc`([Link](https://pypi.org/project/pyodbc/))

- Azure commands/CLI ([Link 1](https://docs.microsoft.com/en-us/cli/azure/install-azure-cli-linux?pivots=apt) & [Link 2](https://docs.microsoft.com/en-us/cli/azure/storage/blob?view=azure-cli-latest))

- [Storage blob](https://docs.microsoft.com/en-us/cli/azure/storage/blob?view=azure-cli-latest#az_storage_blob_upload_batch)

- [Azure account key/info](https://docs.microsoft.com/en-us/azure/storage/common/storage-configure-connection-string)

> **Well done!**
>
> You have successfully completed this tutorial on how to migrate from SAP IQ to SAP HANA Cloud, data lake.
>
> Follow our tag in the [SAP Community](https://blogs.sap.com/tags/73554900100800002881/) to stay up-to-date on the latest updates and newest content! For more learning materials on SAP HANA Cloud, [click here](https://community.sap.com/topics/hana-cloud).




### Test yourself









---
