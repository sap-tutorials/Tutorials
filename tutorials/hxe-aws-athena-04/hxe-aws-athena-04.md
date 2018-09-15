---
title: Setup the ODBC Driver for Amazon Athena
description: Learn how to setup the Amazon Athena ODBC to use it with SAP HANA, express edition
primary_tag: products>sap-hana\,-express-edition
auto_validation: true
tags: [  tutorial>beginner, topic>cloud, topic>sql, products>sap-hana\,-express-edition ]
time: 10
---


## Details
### You will learn  
  - How to do install and configure the Simba ODBC Driver for Amazon Athena

[ACCORDION-BEGIN [Step 1: ](Start a SSH session)]

Connect to your SAP HANA, express edition using an SSH client as **`ec2-user`**.

For reference, you can check the ***Set up SAP HANA, express edition, on Amazon Web Services*** **<a href="https://www.sap.com/developer/tutorials/hxe-aws-setup.html" target="&#95;blank">server only</a>** or **<a href="https://www.sap.com/developer/tutorials/hxe-xsa-aws-setup.html" target="&#95;blank">server + applications</a>** tutorial for more details.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Install unixODBC)]

Before installing the Simba ODBC Driver for Amazon Athena, you will need to install **`unixODBC`**, an ODBC driver manager for Linux platforms.

Form your SSH session, execute the following command:

```shell
sudo zypper install -y unixODBC
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Install Simba ODBC Driver for Amazon Athena RPM)]

Then you can install the Simba ODBC Driver for Amazon Athena available <a href="https://docs.aws.amazon.com/athena/latest/ug/connect-with-odbc.html" target="&#95;blank">online</a>.

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

[ACCORDION-BEGIN [Step 1: ](Configure the ODBC Driver definition)]

Now that the software package is properly installed, you need to configure the ODBC Driver definition.

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

[ACCORDION-BEGIN [Step 1: ](Switch user)]

As the ODBC connection will be initialized by the SAP HANA, express edition process, it is important to configure the ODBC DSN as the ***`hxeadm`*** user.

Form your SSH session, execute the following command:

```shell
sudo su - hxeadm
```

Then execute the following command to validate that ***`unixODBC`*** is properly configured for the ***`hxeadm`*** user:

```shell
odbcinst -j
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Setup the credentials)]

In order to connect using the Simba ODBC Driver for Amazon Athena, you will need to provide credentials from a user that have access to both Amazon Athena and Amazon S3.

This is the reason you have created a ***programmatic user*** (named ***`athena`*** in the first tutorial).

The proposed user name was ***`athena`***.

You will also need the **Access key ID** and **Secret access key** downloaded during the creation.

First, you will need to create the ***credentials*** file in the hidden directory ***`.aws`***.

Form your SSH session, execute the following commands:

```shell
cd ~

mkdir .aws

vi .aws/credentials
```

Insert the following content then save and exit ***`vi`***:

```property
[default]
aws_access_key_id=<< replace with the athena user access key >>
aws_secret_access_key=<< replace with the athena user secret access key >>
```

Make sure to update the **Access key ID** and **Secret access key**.

> ### **Note**: You can paste content in **`vi`** using the following keyboard combination:
> - Enable the insert mode : **ESC** then **I**
> - Paste the clipboard content : **CTRL+SHIFT+V**

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Setup the ODBC DSN)]

Now, you can create the ODBNC DSN that will be used in SAP HANA, express edition to initiate the remote source connection.

For this, you will need to edit the ODBC user data source file named ***`.odbc.ini`***.

Form your SSH session, execute the following command:

```shell
vi ~/.odbc.ini
```
Insert the following content then save and exit ***`vi`***:

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
S3OutputLocation=s3://sap-hana-athena/

AuthenticationType=Default Credentials
```

Make sure to update the **`AwsRegion`** and the **`S3OutputLocation`** (if you used a different S3 bucket name).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Test the connection)]

Form your SSH session, execute the following command:

```shell
isql AWSAthena
```

You can now run the following SQL statement:

```sql
select distinct year from gdelt_athena.events
```
Provide an answer to the question below then click on **Validate**.

[VALIDATE_3]
[ACCORDION-END]
