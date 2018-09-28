---
title: Use JupyterLab with SAP HANA, express edition
description: Provide details on the installation and configuration of JupyterLab to use with  SAP HANA, express edition.
auto_validation: true
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>intermediate, products>sap-hana\, express-edition ]
time: 20
---

## Prerequisites  
- [Select and Install a SQL query tool for SAP HANA, express edition](https://www.sap.com/developer/tutorials/mlb-hxe-tools-sql.html).
- [Optional - Configure the SAP HANA R integration with SAP HANA, express edition](https://www.sap.com/developer/tutorials/mlb-hxe-setup-r.html).

## Details
### You will learn
During this tutorial, you will learn how to install the `JupyterLab` application on your system and connect it to your SAP HANA, express edition.

[ACCORDION-BEGIN [Step 1: ](Create a Dedicated User)]

As a best practice, I recommended you to create a dedicated user to run Jupyter. It will help you both restrict access to your system but also help avoiding side any effect on the `hxeadm` user that is running the SAP HANA, express edition instances.

For the rest of this tutorial, `jupyteradm` will be referred as the Jupyter administrator user.

To create `jupyteradm` user you can execute the following commands:

```shell
sudo useradd -m -d /home/jupyteradm -c "Jupyter Administrator" jupyteradm     
sudo passwd jupyteradm
```

Then, you can execute the following command to configure properly the `jupyteradm` user to proceed will the installation:

```shell
sudo bash -c 'echo "jupyteradm ALL=(ALL) NOPASSWD: ALL" >>/etc/sudoers'
sudo bash -c 'echo "umask 022" >>/home/jupyteradm/.bashrc'
sudo bash -c 'echo "PATH=/home/jupyteradm/.local/bin:$PATH" >>/home/jupyteradm/.bashrc'
```

Now, you can switch to the `jupyteradm` user if not done yet:

```shell
sudo su -l jupyteradm
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Install C Compiler & Required Packages)]

To complete the Jupyter setup, you will need some additional Python utilities packages.

By default, SAP HANA, express edition setup a Python 2.7 version, but this one doesn't include the `pip` or `virtualenv` package.

Therefore, you will now add the missing packages.

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

- SUSE Linux Package for SAP Applications 12 SP2

	```shell
	sudo SUSEConnect -p SLES_SAP/12.2/x86_64
	```

- SUSE Linux Enterprise Software Development Kit 12 SP2

	```shell
	sudo SUSEConnect -p sle-sdk/12.2/x86_64
	```

- `Toolchain` Module

	```shell
	sudo SUSEConnect -p sle-module-toolchain/12/x86_64
	```

Make sure to adjust the version/extension name based on the result from the ***`--list-extension`*** result.

These commands will be successful only if you have registered your system with `SUSEConnect`:

Then, you can clean and refresh the repository cache:

```shell
sudo zypper refresh
```

Then, you can execute the following command to install the compiler:

```shell
sudo zypper install --type pattern Basis-Devel
```

Then, install the Python `devel` package using the following command:

```shell
sudo zypper install python-devel
```

### **For Red Hat Enterprise Linux:**

The following extensions/repositories are required to install the `Development Tools` package group and some additional packages in a later step:

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

Then, you can execute the following command to install the compiler:

```shell
sudo yum groupinstall "Development Tools"
```

As Python Pip is not part of the default Red Hat repositories, you will use the ***Extra Packages for Enterprise Linux*** (EPEL) as described in the Red Hat solution note [3358](https://access.redhat.com/solutions/3358).

EPEL has an `epel-release` package that includes the `gpg` keys for package signing and repository information.

Installing this package should allow you to use normal tools such as yum to install packages and their dependencies.

Execute the following command to install the `epel-release` package:

```shell
sudo rpm -ivh https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
```

Then, you can install Python tools using the following command:

```shell
sudo yum install python-devel
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Install Python Pip and Virtual Environment)]

As Python Pip and Virtual Environments are not part of the default SUSE repositories, you will be building from the source.

First, you will need to download the install script, run it then install `virtualenv` using the following command:

```shell
curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
python get-pip.py --user
pip install --user virtualenv
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Install SAP HANA Client)]

To download SAP HANA, express edition  components, you can use the Download Manager either with the GUI mode or the command line mode as documented in one of the setup tutorials.

The SAP HANA, express edition Download Manager is now provided as part of your SAP HANA, express edition installation in: `/usr/sap/HXE/home/bin/`.

You can then download the packages using the following command:

```bash
/usr/sap/HXE/home/bin/HXEDownloadManager_linux.bin linuxx86_64 installer \
    -d . \
	clients_linux_x86_64.tgz
```

Once completed, you can proceed with the installation.

#### SAP HANA Client

The downloaded archive for the client package contains both the ***SAP HANA HDB Client*** and the ***SAP HANA XS CLI***.

Here you will only install the ***SAP HANA HDB Client***.

The ***SAP HANA HDB Client*** software package includes the following connectivity/drivers:

 - SQLDBC
 - ODBC
 - JDBC
 - Python (`PyDBAPI`)
 - Node.js
 - Ruby

Then, you need to extract the contents of `clients_linux_x86_64.tgz` into the ***`/opt/hxe`*** directory using the following command:

```bash
tar -xvzf ./clients_linux_x86_64.tgz -C .
```

The following files will be extracted:

 - ***`hdb_client_linux_x86_64.tgz`*** : the *SAP HANA HDB Client* software package
 - ***`xs.onpremise.runtime.client_linuxx86_64.zip`*** : the *SAP HANA XS CLI* software package

You need now to decompress the *SAP HANA HDB Client* package executing the following command:

```bash
tar -xvzf ./hdb_client_linux_x86_64.tgz -C .
```

And now you can run the installer program executing the following commands:

```bash
cd ./HDB_CLIENT_LINUX_X86_64
./hdbinst
```

Accept the prompts default values to configure your installation:

 - Installation Path : `/home/jupyteradm/sap/hdbclient`


Once the installation is completed, you should get the following elements in your console:

```
Installation done
```

[DONE]
[ACCORDION-END]



[ACCORDION-BEGIN [Step 4: ](Install JupyterLab)]

Now that the `pip` or `virtualenv` package are installed, you have to create and activate a Python Virtual Environment (named `jupyter`) using the following commands:

```shell
cd ~/
virtualenv ~/jupyter
source ~/jupyter/bin/activate
```

And finally you can install Jupyter using the following commands:

```shell
pip install jupyterlab
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Configure and Start Jupyter)]

Once the installation is completed, you can run the following command to generate the default configuration file.

```shell
jupyter-lab --generate-config
```

The command should output the path to the configuration file:

```
Writing default config to: /home/jupyteradm/.jupyter/jupyter_notebook_config.py
```

Let's now create a local *dummy* certificate using `openssl`:

```shell
openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout /home/jupyteradm/.jupyter/mykey.key -out /home/jupyteradm/.jupyter/mycert.pem
```

Once the certificate is created, you can edit the generated configuration file:

```
vi /home/jupyteradm/.jupyter/jupyter_notebook_config.py
```

And append the following configuration elements

```text
# Set options for certfile
c.NotebookApp.certfile = u'/home/jupyteradm/.jupyter/mycert.pem'
c.NotebookApp.keyfile = u'/home/jupyteradm/.jupyter/mykey.key'
# Set ip to '*' to bind on all interfaces (ips) for the public server
c.NotebookApp.ip = '*'
c.NotebookApp.open_browser = False

# It is a good idea to set a known, fixed port for server access
c.NotebookApp.port = 8888
```

> ### **Note**: You can paste content in **`vi`** using the following keyboard combination:
> - Enable the insert mode : **ESC** then **I**
> - Paste the clipboard content : **CTRL+SHIFT+V**
> - Exit `vi`: **ESC** then **`:wq!`**

Then, you should set a password to secure the environment using the following command:

```shell
jupyter notebook password
```

You can now start `JupyterLab` using the following command where `xxx.xxx.xxx.xxx` represent the host IP address where you have just installed `JupyterLab`:

```shell
jupyter lab --ip=xxx.xxx.xxx.xxx  &
```

The following output will be displayed:

```
[I 17:44:46.071 LabApp] The port 8889 is already in use, trying another port.
[I 17:44:46.087 LabApp] JupyterLab extension loaded from /home/jupyteradm/jupyter/lib/python2.7/site-packages/jupyterlab
[I 17:44:46.087 LabApp] JupyterLab application directory is /home/jupyteradm/jupyter/share/jupyter/lab
[W 17:44:46.092 LabApp] JupyterLab server extension not enabled, manually loading...
[I 17:44:46.098 LabApp] JupyterLab extension loaded from /home/jupyteradm/jupyter/lib/python2.7/site-packages/jupyterlab
[I 17:44:46.098 LabApp] JupyterLab application directory is /home/jupyteradm/jupyter/share/jupyter/lab
[I 17:44:46.108 LabApp] Serving notebooks from local directory: /home/jupyteradm
[I 17:44:46.109 LabApp] The Jupyter Notebook is running at:
[I 17:44:46.109 LabApp] https://xxx.xxx.xxx.xxx:8890/
[I 17:44:46.109 LabApp] Use Control-C to stop this server and shut down all kernels (twice to skip confirmation).
```

You can now open the following URL after replacing `hxehost` with the current hostname.

 - `https://xxx.xxx.xxx.xxx:8888/tree`

As your certificate has been signed locally, you will receive a **Your connection is not private** message from your browser, but you can proceed.

![Jupyter](05-1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Use Python Kernel as a SQL query tool)]

If you are planning to use a Python Kernel in Jupyter Notebooks to simply execute SQL, then you will need to install:

 - [`SQLAlchemy`](https://www.sqlalchemy.org/)
 - [`SQLAlchemy` for HANA](https://github.com/SAP/sqlalchemy-hana)
 - [`IPython SQL`](https://github.com/catherinedevlin/ipython-sql)
 - SAP HANA Python driver (as installed previously)

First activate the Jupyter Virtual Environment if not done yet:

```shell
source ~/jupyter/bin/activate
```

Once activated, you can proceed with the Python modules installation:

```shell
pip install \
  sqlalchemy \
  sqlalchemy-hana \
  ipython-sql \
  /home/jupyteradm/sap/hdbclient/hdbcli-x.y.Z.tar.gz
```

> **Note:** the `hdbcli` tar file may be with a different version than the one displayed above

Once completed, you can create your first Python notebook using the **File > New > Notebook** menu:

![Jupyter](06-1.png)

Select Python 2 as **Kernel** then click on **Select**:

![Jupyter](06-2.png)

You can then paste the following code in the first cell after updating the `hxe_coonection` variable with your system details.

Here, it is assumed that you have completed the [Prepare your SAP HANA, express edition instance for Machine Learning](https://developers.sap.com/tutorials/mlb-hxe-setup-basic.html) tutorial:

```python
import sqlalchemy
%reload_ext sql

%config SqlMagic.displaylimit = 5

hxe_connection = 'hana://ML_USER:Welcome18Welcome18@localhost:39015';

%sql $hxe_connection
```

Then add a second cell with the following content:

```python
%sql select count(1) FROM TABLES;
```

Then, hit the ![Run](00-run.png) button. You should get the following output on the screen:

![Jupyter](06-3.png)

You can then manipulate the output using the following

```python
result = _
print(result)
print(type(result))
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Use Python Kernel for code)]

As stated before Jupyter is very powerful in term of supported programming languages.

When considering Python as programming language, you can use directly the SAP HANA Python driver which follows the [Python Database API Specification](https://www.python.org/dev/peps/pep-0249/) to consume data.

Here is an code sample you can run:

```python
from hdbcli import dbapi
conn = dbapi.connect(
    address="localhost",
    port=39015,
    user="ML_USER",
    password="Welcome18Welcome18"
)
with conn.cursor() as cursor:
    cursor.execute("SELECT CURRENT_USER FROM DUMMY")
    result = cursor.fetchone()
print(result[0])
```

Also, `SQLAlchemy` goes far beyond making your life easy running SQL in Jupyter with `IPython-sql`.

`SQLAlchemy` is most famous for its object-relational mapper (ORM), an optional component that provides the data mapper pattern, where classes can be mapped to the database in open ended, multiple ways - allowing the object model and database schema to develop in a cleanly decoupled way from the beginning.

Here is an code sample you can use to instantiate a `SQLAlchemy` engine:

```python
from sqlalchemy import create_engine
engine = create_engine('hana://ML_USER:Welcome18Welcome18@localhost:39015')

print(type(engine))
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Use R Kernel for code)]

Jupyter allows you to run R, but before you will need to enable it.

To so, you will need to install `ÌKernel` as described in the following [installation guide](https://irkernel.github.io/installation/).

For more details about the R kernel in Jupyter you can check the site : [https://irkernel.github.io/](https://irkernel.github.io/).

All the steps from the [installation guide](https://irkernel.github.io/installation/) are to be executed in a R console as the `jupyteradm` user.

It assumes that you have a local R installation. If not done yet, you can use the instructions from the following tutorial: [Configure the SAP HANA R integration with SAP HANA, express edition](https://www.sap.com/developer/tutorials/mlb-hxe-setup-r.html)

Once the installation is completed successfully, you will first need to restart `JupyterLab`.

Then you will have now the ability to create R kernels:

![Jupyter](08-1.png)

You can then consume HANA data either via JDBC or ODBC.

When using JDBC, you will first need to install the `RJDBC` package in a R console:

```R
install.packages("RJDBC")
```

Then you can use the following sample a R kernel notebook from `JupyterLab`:

```R
library("RJDBC")
jdbcDriver <- JDBC(driverClass="com.sap.db.jdbc.Driver", "/home/jupyteradm/sap/hdbclient/ngdbc.jar")
jdbcConnection <- dbConnect(
    jdbcDriver,
    "jdbc:sap://localhost:39015/?autocommit=false",
    "ML_USER",
    "Welcome18Welcome18"
)
result <- dbGetQuery(jdbcConnection, "select CURRENT_USER from DUMMY")
print(result)
dbDisconnect(jdbcConnection)
```

When using ODBC, you will need to install the `RJDBC` package in a R console:

```R
install.packages("RODBC")
```

Then you will need to configure an ODBC DSN for you target database.

If not done yet, you can use the instructions from the following tutorial: [Use an ODBC based querying tools for SAP HANA, express edition](https://www.sap.com/developer/tutorials/mlb-hxe-tools-sql-odbc.html)

Then you can use the following sample a R kernel notebook from `JupyterLab`:

```R
library("RODBC")
odbcConnection <- odbcConnect(
    "DSN_HXE",
    uid="ML_USER",
    pwd="Welcome18Welcome18"
)
result <- sqlQuery(odbcConnection,"select CURRENT_USER from DUMMY")
print(result)
odbcClose(odbcConnection)
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Troubleshooting: Use R Kernel for code)]

##### **Error: "Unable to start device PNG"**

If you receive the following message in the logs when starting Jupyter:

```
Error in .External2(C_X11, paste0("png::", filename), g$width, g$height,  :
   unable to start device PNG
```

You will need to add the following configuration to the `~/.Rprofile` file and then restart Jupyter:

```text
options(bitmapType='cairo')
```

##### **Error:  "configure: error: ODBC headers `sql.h` and `sqlext.h` not found"**

When installing the `RODBC` package, you may receive the following error message and a non-zero status at the installation:

```
configure: error: "ODBC headers sql.h and sqlext.h not found"
```

You will need to install the `unixODBC-devel` to solve this issue.

[DONE]
[ACCORDION-END]
