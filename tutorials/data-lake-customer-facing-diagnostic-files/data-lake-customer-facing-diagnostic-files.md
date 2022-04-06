---
title: Debug your SAP HANA Cloud, data lake with Diagnostic Files
description: SAP HANA data lake allows you access to diagnostic files that contain logs for server information, transactions, server checkpoints, errors and status messages. Learn how setup and access these files through the HDLFSCLI, REST API, and SQL Console.
auto_validation: true
time: 60
tags: [ tutorial>beginner, software-product>sap-hana-cloud, tutorial>license]
primary_tag: software-product-function>sap-hana-cloud\,-data-lake
---

## Prerequisites
 - Access to the Cloud Foundry space of a SAP HANA Cloud, data lake
 - Installation of the HANA data lake File Store Command Line Interface (HDLFSCLI)
 - [Optional] Completion of [Getting Started with Data Lake Files HDLFSCLI tutorial](group.hana-data-lake-containers)
 - Access to a licensed SAP HANA Cloud, data lake instance

## Details
### You will learn
  - How to set up the file container user to get access to the diagnostic files
  - How to download the diagnostic files to your local machine and read them
  - How to use the REST API to retrieve the diagnostic files
  - How to view the diagnostic files from a SQL Console

---

[ACCORDION-BEGIN [Step 1: ](Get Access to the Diagnostics User)]
The SAP HANA Cloud, data lake (HDL) generates diagnostic files that may be useful for understanding common problems in a HDL instance. There is a specific user that can be used to retrieve these files that needs to be enabled.

Start by heading to the Cloud Foundry space where the HDL instance is provisioned. In the space you will find "Instances" under "Services". This is where the diagnostics user's credentials can be found.

![Selecting the data lake instance in a Cloud Foundry space.](diag-files-1.png)

Upon selecting "create" a modal should appear named "New Service Key". Give the service key a name and paste the following JSON into the parameters box and select create.

```JSON
{
    "scope": "diagnostics",
    "credential-type": "X509_GENERATED",
    "duration": 90
}
```

Use the ellipses on the created service key to view the certificates that will have access to the diagnostic files. Copy the contents of the "certificate" to a local file and call it `tut-diagcert.crt` and copy the contents of the "key" to a local file and call it `tut-diagkey.key`.

![Viewing the certificates created by the service.](diag-files-11.png)

> You will need to manually remove each `\n` in the files and replace them with real like breaks. An alternative to this is to use a code editor like Visual Studio Code and use the find and replace function. Use the regex find option and search for `\\n` and replace it with '\n'.

**[Optional]**: Use Visual Studio Code

![Replacing new line characters with new lines in Visual Studio Code.](diag-files-2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Access and Download the Diagnostic Files using the HDLFSCLI)]

If you've set up an HDLFSCLI configuration before, navigate to the `.hdlfscli.config.json` file in your home directory. Open it with a text editor and add a new configuration by pasting the following JSON.

```JSON
"tut-diagconfig": {
  "timeout": 3000000000,
  "format": "text",
  "output": "",
  "cert": "<path>/tut-diagcert.crt",
  "key": "<path>/tut-diagkey.key",
  "passphrase": "",
  "cacert": "",
  "skipServerCertificateVerification": true,
  "filecontainer": "<file-container-uuid>-diag",
  "endpoint": "<file container endpoint>",
  "pretty": false
}
```

If there are already configurations in the `.hdlfscli.config.json` file you can add the new configuration as another entry. Your `<path>` is the file path to the corresponding certificate or key. The file container endpoint can be found on the instance in SAP HANA Cloud Central. The `<file container uuid>` is the UUID portion of the file container endpoint (the first 36 characters).

![Find the file container endpoint in SAP HANA Cloud Central.](diag-files-3.png)

Use the below JSON skeleton to add the diagnostics user configuration.

If the `.hdlfscli.config.json` file does not exist, create it in your home directory and paste the following JSON. The HDLFSCLI uses this file to identify configurations for easier usage.

```JSON
{
  "configs":{
    "tut-diagconfig": {
      "timeout": 3000000000,
      "format": "text",
      "output": "",
      "cert": "<path>/tut-diagcert.crt",
      "key": "<path>/tut-diagkey.key",
      "passphrase": "",
      "cacert": "",
      "skipServerCertificateVerification": true,
      "filecontainer": "<file-container-uuid>-diag",
      "endpoint": "<file container endpoint>",
      "pretty": false
    }
  }
}
```

Now, open a terminal or a command prompt and look inside the diagnostics file container.

`hdlfscli -config tut-diagconfig lsr`

![Sample output from list recursively command in the diagnostics file container.](diag-files-7.png)

Now that you've seen the diagnostic files, you can use the HDLFSCLI download command to download the diagnostic files and read them. To identify what diagnostic file you wish to download, understanding the format of the file names is helpful.

File names come in two flavors. The date in the file name can be used to identify the most recently updated log file.

1. Server Logs: `iqaas_<YYYYMMDD_HHMMSS.mmm><multiplex_node>.iqmsg`
2. Query Plans: `<iqaas>_<query_name>_<YYYYMMDD_HHMMSS>_<sequence_number>.html`

Choose a file in from the diagnostic files container and paste its path into the below command. The command will create a local directory called `sample-diagnostic-file` and place inside the diagnostic file. Now you can locate the file and open it with a text editor.

`hdlfscli -config tut-diagconfig download <file to download> ./sample-diagnostic-file`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](View the Diagnostic Files Using the REST API)]

The diagnostic files can also be accessed using a REST API. These examples will use Python to write requests to the REST API. The SAP HANA Cloud, data lake Files REST API reference can be found [here](https://help.sap.com/doc/9d084a41830f46d6904fd4c23cd4bbfa/QRC_4_2021/en-US/html/index.html).

Create a file `datalake_diagnostic_files_api.py` and paste the following in the script. The information for the placeholders is the same as in the configuration from step 2.

```Python
import http.client
import json

FILES_REST_API = '<File Container REST API>'
CONTAINER = '<File Container ID>-diag'
CRT_PATH = '<Path to Client Certificate>'
KEY_PATH = '<Path to Client Key>'
```

Now, add the logic for using the `LISTSTATUS_RECURSIVE` endpoint, which will list all the items in your directory. You should also format the response so that it is easier to read.

```Python
# Setup the request
file_path = '/diag/logs/'
request_url = f'/webhdfs/v1/{file_path}?op=LISTSTATUS_RECURSIVE'
request_headers = {
    'x-sap-filecontainer': CONTAINER,
    'Content-Type': 'application/json'
}

# Make the request
connection = http.client.HTTPSConnection(
    FILES_REST_API, port=443, key_file=KEY_PATH, cert_file=CRT_PATH)

connection.request(
    method="GET", url=request_url, body=None, headers=request_headers)

response = connection.getresponse()

# Decode the returned byte string and format it for legibility
files = response.read().decode('utf8')
data = json.loads(files)
formatted_data = json.dumps(data, indent=4)

# Print the response and close the connection
print(formatted_data)
response.close()
```

The output should look something like the following.

![Sample REST API output for LISTSTATUS_RECURSIVE](diag-files-8.png)

Notice where the file name can be identified. Once the file name is known, you can use the OPEN endpoint to read the file and write the contents to a local file. Add the below code to your script and fill in `<diagnostic file name>` with the file you wish to read.

```Python
file_path = '/diag/logs/<diagnostic file name>'
request_url=f'/webhdfs/v1/{file_path}?op=OPEN'
request_headers = {
    'x-sap-filecontainer': CONTAINER,
    'Content-Type': 'application/json'
}

connection = http.client.HTTPSConnection(
    FILES_REST_API, port=443, key_file=KEY_PATH, cert_file=CRT_PATH)

connection.request(
    method="GET", url=request_url, body=None, headers=request_headers)

response = connection.getresponse()

file_contents = response.read().decode('utf8')
print(file_contents)
response.close()
```

Now, if you want to write the contents of the diagnostic file to a local file, replace the print statement in the above code to the following. Make sure that the parent directory `sample-diagnostic-file` exists or remove it from the open statement.

```Python
file_name = 'my-diagnostics-file.txt'
f = open(f"sample-diagnostic-file/{file_name}", "w+")
f.write(file_contents)
f.close()
```

If you look inside the `sample-diagnostic-file` directory, or where ever you specified the file to be written to, you should find a file named `my-diagnostic-file.txt`. Open the file with a text editor to read it.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](View the Diagnostic Files Using Database Explorer)]

Another way to read diagnostic files is through the SQL Console in the Database Explorer. This is made possible through a number of stored procedures and SQL functions. The first procedure you will use is `sp_list_directory` which is similar to the list recursively command in the HDLFSCLI.

```SQL
CALL sp_list_directory('/diag/logs/');
```

Below is a sample of what might be seen with the `sp_list_directory` stored procedure.

![Sample output of sp_list_directory stored procedure](diag-files-9.png)

Using the `file_path` column you can identify which files exist and can be read with the `read_server_file` stored procedure. Choose a file and read it using the SQL below.


```SQL
SELECT * FROM sa_split_list(cast( READ_SERVER_FILE('/diag/logs/<file_path>') as long varchar ), '\n');
```

In the above SQL you cast the `read_server_file` result to a VARCHAR or else it will come back as a long binary and not be legible. You can also use the `sa_split_list` function to format the result so that the result is split by the newline character `\n`. Here is a sample result.

![Sample output of read_server_file stored procedure.](diag-files-10.png)

That is how you can leverage Database Explorer and built in stored procedures to read log files. However, it is important to note that there is a limit on the amount of data that can be returned and displayed in a SQL result. However, the diagnostic files can be retrieved with Python and saved locally using the `sqlanydb` module.

```Python
import sqlanydb

conn = sqlanydb.connect(uid='HDLADMIN', pwd='DBpassword1', host='fdc6d49c-59b1-4fd8-a8a8-415e7e6d84d7.iq.hdl.demo-hc-3-hdl-hc-dev.dev-aws.hanacloud.ondemand.com:443', enc='tls(tls_type=rsa;direct=yes)')
curs = conn.cursor()
curs.execute("select read_server_file('/diag/logs/mpx-coord-0/iqaas_20220225_211225.102_mpx-coord-0.iqmsg')")
s = curs.fetchall()[0][0].decode('utf-8')
print(s)
```

[VALIDATE_1]
[ACCORDION-END]

---
