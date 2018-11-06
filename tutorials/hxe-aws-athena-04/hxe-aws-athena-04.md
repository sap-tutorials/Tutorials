---
title: Set Up the ODBC Driver for Amazon Athena
description: Set up the Amazon Athena ODBC to use it with SAP HANA, express edition.
primary_tag: products>sap-hana\,-express-edition
auto_validation: true
tags: [  tutorial>beginner, topic>cloud, topic>sql, products>sap-hana\,-express-edition ]
time: 10
---


## Details
### You will learn  
  - How to install and configure the Simba ODBC driver for Amazon Athena

[ACCORDION-BEGIN [Step 1: ](Switch to the ec2-user user)]

Connect to your SAP HANA, express edition instance using an SSH client as the **`ec2-user`** user.

The prompt should be:

```
ec2-user@hxehost:~>
```

If you are using an existing session and the prompt is **```hxeadm@hxehost:~>```** , then run the **exit** command to return to **`ec2-user`**.

Make sure the prompt is **```ec2-user@hxehost:~>```** before moving to the next step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Install unixODBC)]

Before installing the Simba ODBC Driver for Amazon Athena, you will need to install **`unixODBC`**, an ODBC driver manager for Linux platforms.

Execute the following command:

```shell
sudo zypper install -y unixODBC
```

This will install the **`unixODBC`** driver manager.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Install Simba ODBC driver for Amazon Athena RPM)]

Then, you can install the Simba ODBC Driver for Amazon Athena available <a href="https://docs.aws.amazon.com/athena/latest/ug/connect-with-odbc.html" target="&#95;blank">online</a>.

Form your SSH session, execute the following commands:

```shell
cd ~
wget https://s3.amazonaws.com/athena-downloads/drivers/ODBC/Linux/simbaathena-1.0.2.1003-1.x86_64.rpm

sudo rpm -ivh simbaathena-1.0.2.1003-1.x86_64.rpm
```

To check that the RPM package was properly installed, execute the following command:

```shell
rpm -qa | grep SimbaAthenaODBC
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Configure the ODBC driver definition)]

Now that the software package is properly installed, you need to configure the ODBC driver definition.

Form your SSH session, execute the following command:

```shell
sudo odbcinst -i -d -f /opt/simba/athenaodbc/Setup/odbcinst.ini
```

The output should be:

```
odbcinst: Driver installed. Usage count increased to 1.
    Target directory is /etc/unixODBC
odbcinst: Driver installed. Usage count increased to 1.
    Target directory is /etc/unixODBC
odbcinst: Driver installed. Usage count increased to 1.
    Target directory is /etc/unixODBC
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Switch to the hxeadm user)]

As the ODBC connection will be initialized by the SAP HANA, express edition process, it is important to configure the ODBC DSN as the ***`hxeadm`*** user.

Form your SSH session, execute the following command:

```shell
sudo su - hxeadm
```
The prompt should be:

```
hxeadm@hxehost:~>
```

Make sure the prompt is **```hxeadm@hxehost:~>```** before moving to the next step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Check the driver installation)]

Execute the following command to validate that ***`unixODBC`*** is properly configured for the ***`hxeadm`*** user:

```shell
odbcinst -j
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Set up the credentials)]

In order to connect using the Simba ODBC Driver for Amazon Athena, you will need to provide credentials from a user that have access to both Amazon Athena and Amazon S3.

This is the reason you have created a ***programmatic user*** (named ***`athena`*** in the first tutorial).

The proposed user name was ***`athena`***.

You will also need the **Access key ID** and **Secret access key** downloaded during the creation.

First, you will need to create the ***credentials*** file in the hidden directory ***`.aws`***.

Form your SSH session, execute the following commands:

```shell
cd ~
mkdir ~/.aws
vi ~/.aws/credentials
```
> ### **Note**: Below are a few useful **`vi`** keyboard combinations:
> - Enable the insert mode : **ESC** then **I**
> - Paste the clipboard content : **CTRL+SHIFT+V**
> - Exit and save `vi`: **ESC** then **`:wq!`**

Adjust the **Access key ID** and **Secret access key** in a local notepad before inserting then save and exit in the ***`vi`*** session:

```property
[default]
aws_access_key_id=<< replace with the athena user access key >>
aws_secret_access_key=<< replace with the athena user secret access key >>
```

Make sure to update the **Access key ID** and **Secret access key**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Set up the ODBC DSN)]

Now, you can create the ODBNC DSN that will be used in SAP HANA, express edition to initiate the remote source connection.

For this, you will need to edit the ODBC user data source file named ***`.odbc.ini`***.

Form your SSH session, execute the following command:

```shell
vi ~/.odbc.ini
```
> ### **Note**: Below are a few useful **`vi`** keyboard combinations:
> - Enable the insert mode : **ESC** then **I**
> - Paste the clipboard content : **CTRL+SHIFT+V**
> - Exit and save `vi`: **ESC** then **`:wq!`**

Adjust the **`AwsRegion`** and the **`S3OutputLocation`** properties based on your current environment in a local notepad before inserting then save and exit in the ***`vi`*** session:

```property
[ODBC]
Trace=no

[ODBC Data Sources]
AWSAthena=Simba Athena ODBC Driver 64-bit]

[AWSAthena]
Description=Simba Athena ODBC Driver (64-bit) DSN
Driver=/opt/simba/athenaodbc/lib/64/libathenaodbc_sb64.so

AwsRegion=us-east-1
Schema=default
S3OutputLocation=s3://sap-hana-athena-<my unique id>/

AuthenticationType=Default Credentials
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Test the connection)]

Form your previous SSH session, execute the following command:

```shell
isql AWSAthena
```

You can now run the following SQL statement:

```sql
select count(distinct year) from gdelt_athena.events
```
Provide an answer to the question below then click on **Validate**.

[VALIDATE_3]
[ACCORDION-END]
