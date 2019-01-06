---
title: Configure the TensorFlow integration (SAP HANA EML) with SAP HANA, express edition
description: Provide details on the installation and configuration of the SAP HANA External Machine Learning Library with SAP HANA, express edition.
auto_validation: true
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>intermediate, products>sap-hana\, express-edition ]
time: 30
---

## Prerequisites  
- [Prepare your SAP HANA, express edition instance for Machine Learning](https://developers.sap.com/tutorials/mlb-hxe-setup-basic.html)

## Details
### You will learn
During this tutorial, you will learn how to install and configure the TensorFlow integration with SAP HANA, express edition.

First, you will download and install the required SAP HANA components.

Then, as the TensorFlow Serving binaries are only available for a few Linux distribution like `Debian`, you will learn how to use the provided Docker containers to run TensorFlow Serving.

Finally, you will learn how to configure your SAP HANA, express edition instance to consume the exposed TensorFlow models.

[ACCORDION-BEGIN [Info: ](SAP HANA External Machine Learning Library)]

The integration of TensorFlow with SAP HANA is based on the SAP HANA Application Function Library (AFL).

This allows the application developer to elegantly embed TensorFlow function definitions and calls within `SQLScript` and submit the entire code as part of a query to the database.

![SAP HANA EML](00-0.png)

The figure above shows the main components of the integrated solution:

- ***AFL Framework***:
     Allows predefined TensorFlow models to be remotely invoked through `gRPC` calls encapsulated inside AFL procedures
- ***EML AFL***:
     The TensorFlow Serving client implementation for SAP HANA
- ***TensorFlow Serving Server***:
     Makes TensorFlow exported models accessible for execution through `gRPC` remote procedure calls
- ***Active Models***:
    The models currently served and therefore available for execution
- ***`gRPC` Server***:
    The `gRPC` server interface for communication with the TensorFlow Serving `ModelServer` client
- ***Model Persistence***:
    The exported models persisted in a format in a given TensorFlow Serving `ModelServer`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](Google TensorFlow)]

Google TensorFlow is an open source software library for numerical computation using data flow graphs. While it contains a wide range of functionality, TensorFlow is mainly designed for deep neural network models.

Nodes in the graph represent mathematical operations, while the graph edges represent the multidimensional data arrays (tensors) communicated between them.

As a data scientist, you can use TensorFlow to create, train, and evaluate machine learning models.

TensorFlow Serving (`a.k.a.` TensorFlow Serving `ModelServer`) provides out-of-the-box integration with TensorFlow models, and can be easily extended to serve other types of models and data.

TensorFlow Serving makes it easy to deploy new algorithms and run experiments, while keeping the same server architecture and APIs.

TensorFlow Serving is a flexible, high-performance serving system for machine learning models, designed for production environments.

A TensorFlow `ModelServer` (TMS) makes TensorFlow exported models accessible for execution through the `gRPC` (Remote Procedure Call) mechanism which involves a separate process that serves the actual machine learning functionality.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create a Dedicated User)]

As a best practice, it is recommended to create a dedicated user to run your TensorFlow activities.

This will help avoiding side any effect on the `hxeadm` user that is running the SAP HANA, express edition instance.

To create a dedicated TensorFlow Serving administrator user `tmsadm`, you can execute the following commands:

```shell
sudo useradd -m -d /home/tmsadm -c "TensorFlow Administrator" tmsadm  
sudo passwd tmsadm
```

Then, you can execute the following command to add the `tmsadm` user to the `sudoer` list which will be required to proceed will the installation:

```shell
sudo bash -c 'echo "tmsadm ALL=(ALL) NOPASSWD: ALL" >>/etc/sudoers'
```

Now, you can switch to the `tmsadm` user:

```shell
sudo su -l tmsadm
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Download SAP HANA Client)]

In order to connect to your SAP HANA, express edition instances using Python, you will need to download the SAP HANA Client.

to do so, you can use the Download Manager either with the GUI mode or the command line mode as documented in one of the setup tutorials.

The SAP HANA, express edition Download Manager is now provided as part of your SAP HANA, express edition installation in: `/usr/sap/HXE/home/bin/`.

You can download the SAP HANA Client packages for Linux x64 using the following command:

```bash
/usr/sap/HXE/home/bin/HXEDownloadManager_linux.bin linuxx86_64 installer \
    -d ~ \
	clients_linux_x86_64.tgz
```

You can now extract the content into the current home directory using the following command:

```bash
tar -xvzf ~/clients_linux_x86_64.tgz -C ~/.
rm ~/clients_linux_x86_64.tgz
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Install SAP HANA HDB Client)]

The downloaded archive for the SAP HANA Client package contains more than just the SAP HANA HDB Client.

However, you will only install the SAP HANA HDB Client for now.

You need now to decompress the *SAP HANA HDB Client* package executing the following command:

```bash
tar -xvzf ~/hdb_client_linux_x86_64.tgz -C ~
```

And now you can run the installer program executing the following commands:

```bash
~/HDB_CLIENT_LINUX_X86_64/hdbinst
```

Accept the prompts default values to configure your installation:

 - Installation Path : `/home/tmsadm/sap/hdbclient`


Once the installation is completed, you should get the following elements in your console:

```
Installation done
```

In order to permanently add the SAP HANA Client executable to your user path, you will add the binary directory path in your profile file:

```shell
cd ~/
echo "export PATH=\$PATH:/home/tmsadm/sap/hdbclient" >> ~/.profile
source .profile
```

You can then run the following cleanup commands:

```bash
rm -r ~/HDB_CLIENT_LINUX_X86_64
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Install SAP HANA External Machine Learning AFL)]

Before you can proceed with the next steps, you will need to complete the [Install the SAP HANA External Machine Learning Library Package for SAP HANA, express edition](https://developers.sap.com/tutorials/hxe-ua-eml-vm.html).

To confirm that the SAP HANA EML functions were installed successfully, you can check the following public views:

- `sys.afl_areas`
- `sys.afl_packages`
- `sys.afl_functions`

Connect to the **HXE** tenant using the **SYSTEM** user credentials and execute the following SQL statement:

```shell
cd ~
echo $'SELECT * FROM "SYS"."AFL_AREAS" WHERE AREA_NAME = \'EML\';' > eml.sql
echo $'SELECT * FROM "SYS"."AFL_PACKAGES" WHERE AREA_NAME = \'EML\';' >> eml.sql
echo $'SELECT * FROM "SYS"."AFL_FUNCTIONS" WHERE AREA_NAME = \'EML\';' >> eml.sql

hdbsql -n localhost:39015 -d HXE -u system -f -m
```

You will be prompted to provide the password for the SYSTEM database user (the master password).

Once logged in, you run the following commands (the prompt should be `hdbsql HXE=>`):

```SQL
\o eml-result.txt
\i eml.sql
\q
```

You can now validate the queries output using the following commands:

```shell
more eml-result.txt
rm eml-result.txt
```

The `AFL_AREAS` & `AFL_PACKAGES` should return 1 row each, and the `AFL_FUNCTIONS` should return 10 rows.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Run the Memory Management Script)]

After the installation is completed, it is recommended to run the ***Memory Management Script*** as described in the ***Best Practice*** to release all unused resources and free up some memory.

```bash
sudo su - hxeadm -c '/usr/sap/HXE/home/bin/hxe_gc.sh'
```

Provide the ***System database user (SYSTEM)***.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Install Required Utility Packages)]

To complete the  setup, you will need some utilities packages to be installed.

### **For SUSE Linux Enterprise Server (including the SAP HANA, express edition VM):**

First, you will need to check your current status using the following command:

```shell
sudo SUSEConnect --status-text
```

It should return the following information in the console:

```
Installed Products:
------------------------------------------
  SUSE Linux Enterprise Server for SAP Applications 12 SP3
  (SLES_SAP/12.3/x86_64)

  Registered
------------------------------------------
```

If your system is marked as *Not Registered*, then you will need to register  with `SUSEConnect` using your registration code and email:

```shell
sudo SUSEConnect -r <registration code> -e <registration email>
```

Once registered, you will be able to list the available extensions using the following command:

```shell
sudo SUSEConnect --list-extension
```

You can then activate these extensions/repositories using the following commands:

The following extensions/repositories are required to install the Python packages dependencies:

- SUSE Linux Enterprise Software Development Kit 12 SP2

	```shell
	sudo SUSEConnect -p sle-sdk/12.2/x86_64
	```

Make sure to adjust the version/extension name based on the result from the ***`--list-extension`*** result.

These commands will be successful only if you have registered your system with `SUSEConnect`:

Then, you can clean and refresh the repository cache:

```shell
sudo zypper refresh
```

Then, install the Python `devel` and additional tools packages using the following command:

```shell
sudo zypper install \
    wget  \
    git
```

### **For Red Hat Enterprise Linux:**

The following extensions/repositories are required to install the additional packages in a later step:

You can add these extensions/repositories using the following commands:

```shell
sudo subscription-manager repos --enable="rhel-7-server-extras-rpms"
sudo subscription-manager repos --enable="rhel-7-server-optional-rpms"
```

Then, you can clean and refresh the repository cache:

```shell
sudo yum clean all
sudo yum repolist
```

Then, install the additional tools packages using the following command:

```shell
sudo yum install \
    wget \
    git
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Install Python Pip)]

As Python Pip is not part of all the default Linux distributions or repositories, you will get it from the [Python Packaging Authority](https://www.pypa.io/) site.

First, you will need to download the install script then run it using the following commands:

```shell
cd ~
curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
python get-pip.py --user
rm get-pip.py
```

In order to permanently add the Pip executable to your user path, you will add the binary directory path in your profile file:

```shell
cd ~/
echo "export PATH=\$PATH:/home/tmsadm/.local/bin" >> ~/.profile
source .profile
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Install Python Virtual Environment)]

Now, you can install Python Virtual Environment):

```shell
pip install --user virtualenv
```

You can now create and activate a Python Virtual Environment (named `tms`) using the following commands:

```shell
cd ~/
virtualenv --python=python2.7 --system-site-packages ~/tms
```

Once created, you activate the Python Virtual Environment:

```shell
source ~/tms/bin/activate
```

Your terminal prompt should now look like the following:

```
(tms) tmsadm@hxehost:~>
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Install Additional Python Packages)]

In order to complete this tutorial series, you will need to install a set of Python packages.

Before proceeding, make sure that you have activated your Python Virtual Environment (named `tms`).

Your terminal prompt should now look like the following:

```
(tms) tmsadm@hxehost:~>
```

#### SAP HANA HDBCLI Python driver package:

You can install SAP HANA HDBCLI Python driver using the following command (the `tar.gz` file name may differ):

```bash
pip install --user /home/tmsadm/sap/hdbclient/hdbcli-2.3.119.tar.gz
```

#### TensorFlow package:

Then you can install TensorFlow using the following command:

```shell
pip install --user 'tensorflow==1.8'
```

#### TensorFlow Serving API package:

the TensorFlow Serving API:

```shell
pip install --user 'tensorflow-serving-api==1.12.0'
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 1: ](Install Docker CE)]

As stated before, TensorFlow Serving binaries are not available for all Linux distributions.

However, a Docker container has been recently made available to help when you target operating system, like SUSE Linux Enterprise or Red Hat are being use.

To run the provided Docker container, you can use the Docker Community Edition. To find out more details, you can check the following [Docker CE](https://docs.docker.com/install/) documentation page.

Again, Docker CE executables are available for multiple platform but not for SUSE Linux Enterprise nor Red Hat.

Therefore, you will be using the generic binaries.

This tutorial series was tested successfully with the following binaries:

 - `https://download.docker.com/linux/static/stable/x86_64/docker-18.09.0.tgz`  

You can following the instructions provided in the [Install Docker CE from binaries](https://docs.docker.com/install/linux/docker-ce/binaries/) documentation.

Make sure you also complete the [Post-installation steps for Linux](https://docs.docker.com/install/linux/linux-postinstall/).

At some point, you will need to close all your terminal sessions with the `tmsadm` user for the group change to take place.

If you have trouble running the `hello-world` example without `sudo`, then you can try to restart the `dockerd` process using the following command:

```shell
ps -edf | grep "docker" | grep -v grep | awk '{print $1}' | xargs sudo kill -9
sudo dockerd &
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 1: ](Start TensorFlow Serving)]

Create a model export directory where you will store your TensorFlow Serving configuration and exported models:

```shell
mkdir -p ~/export
```

First, create the following empty model configuration file **`~/export/models.config`** and add the following content:

```js
model_config_list: {
}
```

Now that your installation and configuration is completed, you can check that there is no running container for TensorFlow Serving using the following command:

```shell
docker ps -a | grep "tensorflow/serving"
```

If there is any entry , you can kill and remove it using the following command:

```shell
docker ps -a | grep "tensorflow/serving" | grep -v grep | awk '{print $1}' | xargs docker kill
docker ps -a | grep "tensorflow/serving" | grep -v grep | awk '{print $1}' | xargs docker rm
```

You can now start the TensorFlow Serving container using the following command:

```shell
docker run \
  -p 8500:8500 \
  --mount type=bind,source=/home/tmsadm/export/models.config,target=/tf_models/config/models.config \
  --mount type=bind,source=/home/tmsadm/export,target=/tf_models \
  --entrypoint "/bin/sh" tensorflow/serving:1.6.1 -c "tensorflow_model_server --port=8500 --model_config_file=/tf_models/config/models.config" &
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Configure SAP HANA External Machine Learning)]

Now, that the TensorFlow Serving `ModelServer` is up and running, you will need to add its configuration to your SAP HANA, express edition instance.

For operational systems, it is highly recommended to use two separate type of  users with distinctive roles:

- Administering the TensorFlow `ModelServer` and model deployments
- Calling the deployed models in your code

Each type of user will require dedicated roles to be granted.

In your case, you will be reusing the `ML_USER` created during the [Prepare your SAP HANA, express edition instance for Machine Learning](https://developers.sap.com/tutorials/mlb-hxe-setup-basic.html) tutorial for both roles.

Make also sure that the Script Server has been enabled for your instance.

Connect to the **HXE** tenant using the **SYSTEM** user credentials and execute the following SQL statement:

```SQL
GRANT AFL__SYS_AFL_EML_EXECUTE TO ML_USER;
GRANT SELECT, UPDATE, DELETE, INSERT ON  _SYS_AFL.EML_MODEL_CONFIGURATION TO ML_USER;
```

You can now proceed with the rest of the configuration as `ML_USER`.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement:

```SQL
CREATE REMOTE SOURCE "TensorFlow" ADAPTER "grpc" CONFIGURATION 'server=localhost;port=8500';
```

Now that the remote source was added, you will need to reload the EML configuration as this one is loaded once at the SAP HANA, express edition startup time.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement:

```SQL
CREATE SCHEMA EML_DATA;
SET SCHEMA EML_DATA;
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

Now, you can check the registered models:

```SQL
SET SCHEMA EML_DATA;
-- Uncomment the following lines if you want to re-run the script
-- DROP TABLE CHECK_PARAMS;
CREATE TABLE CHECK_PARAMS ("Parameter" VARCHAR(100), "Value" VARCHAR(100));
INSERT INTO CHECK_PARAMS VALUES ('Model', '*');
CALL _SYS_AFL.EML_CHECKDESTINATION_PROC(CHECK_PARAMS, ?);
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
