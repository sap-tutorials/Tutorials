---
parser: v2
auto_validation: true
time: 60
tags: [ tutorial>beginner, software-product>sap-hana-cloud, tutorial>license]
primary_tag: software-product-function>sap-hana-cloud\,-data-lake
---

# Getting Started with Data Lake Files HDLFSCLI
<!-- description --> Learn how to setup the SAP HANA, data lake Files storage command line interface and use it to manage your data files.

## Prerequisites
 - A licensed SAP HANA, data lake instance (non-trial / non-free tier)
 - Access to SAP Software Center
 - Basic understanding of the public key infrastructure (PKI)
 - Sample TPCH Data, which can be downloaded [here](https://help.sap.com/docs/hana-cloud-data-lake/quick-start-tutorial-for-standalone-data-lake/download-sample-tpch-data?version=2023_3_QRC)

## You will learn
  - How to install and use the SAP HANA, data lake Files storage Command Line Interface (HDLFSCLI)
  - How to use the HDLFSCLI to put, manage, and remove files from an [SAP HANA, data lake Files container](https://help.sap.com/docs/hana-cloud-data-lake/user-guide-for-data-lake-files/sap-hana-cloud-data-lake-administration-for-data-lake-files?version=2023_3_QRC)

---

### Download the SAP HANA Data Lake Client


The HDLFSCLI is included in the HANA Data Lake Client download from the [SAP software center](https://launchpad.support.sap.com/#/softwarecenter/template/products/_APP=00200682500000001943&_EVENT=NEXT&HEADER=Y&FUNCTIONBAR=Y&EVENT=TREE&NE=NAVIGATE&ENR=73555000100800003274&V=MAINT&TA=ACTUAL/HANA%20CLOUD%20CLIENTS). The first step is to download and install the latest version of the HANA Data Lake client.

![HANA Data Lake client search](image-1.png)

The latest HANA Data Lake Client package can by identified by the most recent release date.

![HANA Data Lake software center client installer](image-2.png)

Once you've identified the correct package for your operating system, download it.


### Install HDLFSCLI

[OPTION BEGIN [Linux]]

Once the package is downloaded, extract the contents of the file using a terminal.

```Terminal
tar -xvf <file_name>
```

Change directories to the extracted files.

```Terminal
cd <extracted_directory_name>
```

Then, run the setup file.

```Terminal
./setup.bin
```

#### GUI Installation
If you are using a Linux distribution with a GUI, an installer GUI should appear.

![HANA Data Lake client installer](image-3.png)

Proceed with the installer to complete the installation. Take note of the installation directory that is chosen during the installation setup. This is where necessary environment setup scripts are placed.

#### No GUI Command Line Installation [Optional].
Alternatively, the installation can be completed using the -i console command in the terminal. This will require you to specify all the setup parameters on the command line.

```Terminal
./setup.bin -i console
```

This will guide you through the installation process via the terminal that you are using. Follow the prompts until the installation is completed.

#### Source the IQ and SYBASE Scripts.
After completing either of the installation processes outlined above, navigate to the installation directory that was specified in the installation process.

```Terminal
cd <installation directory>
```

In the installation directory there should be two script files: IQ.sh and SYBASE.sh. Source both of these files to start using the HDLFSCLI.

```Terminal
source IQ.sh
```

```Terminal
source SYBASE.sh
```

These commands can also be added to a `.bash_profile` or `.bashrc` file to persist these changes in new terminal sessions. Once theses commands have been run, run the `hdlfscli` command to ensure it is accessible.

```Terminal
hdlfscli -help
```

Some HDLFSCLI help documentation should appear if it is successfully installed and accessible.

[OPTION END]

[OPTION BEGIN [Windows]]

The HDLFSCLI is included in the HANA Data Lake Client download in the [SAP software center](https://launchpad.support.sap.com/#/softwarecenter/template/products/_APP=00200682500000001943&_EVENT=NEXT&HEADER=Y&FUNCTIONBAR=Y&EVENT=TREE&NE=NAVIGATE&ENR=73555000100800003274&V=MAINT&TA=ACTUAL/HANA%20CLOUD%20CLIENTS). The first step is to download and install the latest version of the HANA Data Lake Client. Once the compressed client is downloaded, extract the components, and run the installer with Admin Privileges.

![Windows installer executable](image-4.png)

Proceed with the installer to complete the installation. Then open a command window and verify a successful installation with the command below.

```cmd
hdlfscli -help
```

![Sample HDLFSCLI help output](image-5.png)

Some HDLFSCLI help documentation should appear if it is successfully installed and accessible.

[OPTION END]


### Generate Certificates

To connect the HDLFSCLI to an SAP HANA, data lake Files container, a certificate will need to be generated to make a secure connection. Below are the steps required to create a self-signed certificate to get started using the HDLFSCLI. You will require an installation of OpenSSL. Use your preferred Linux package installer to install OpenSSL if it is not already installed. If you're using a Windows machine, then Windows Subsystem for Linux (WSL) will have OpenSSL installed. Alternatively, OpenSSL can be installed for Windows from [here](https://slproweb.com/products/Win32OpenSSL.html).

Then, follow these steps to creating your self-signed certificate.

Make sure the certificate fields are not all exactly the same between the Certificate Authority (CA) and client certificates. Otherwise, it is assumed to be a self-signed cert and the cert validation below will fail.

Create a private key for the CA (2048 bits).

```Shell
openssl genrsa -out ca.key 2048
```

Create the CA's public certificate (valid for 200 days). Provide at least a common name and fill other fields as desired. Fields can be skipped; it is not necessary to fill out every field.

```Shell
openssl req -x509 -new -key ca.key -days 200 -out ca.crt
```

Create a signing request for the client certificate.

Provide at least a common name and fill other fields as desired. Also, leave the email-Id field blank.

```Shell
openssl req -new -nodes -newkey rsa:2048 -out client.csr -keyout client.key
```

Create the client certificate (valid for 100 days)

```Shell
openssl x509 -days 100 -req -in client.csr -CA ca.crt -CAkey ca.key -CAcreateserial -out client.crt
```

Verify the certificate was signed by the CA.

```Shell
openssl verify -CAfile ca.crt client.crt
```

To obtain the subject string of a certificate in the RFC2253 format used in HDL Files authorizations (omit the "subject=" prefix).

**Note:** You will need this later when you configure authentication for HDL Files.

```Shell
openssl x509 -in client.crt -nameopt RFC2253 -subject -noout
```


### Update the SAP HANA, data lake Trust Configuration


Navigate to SAP HANA Cloud Central and select "Manage Configuration" on the HDL instance.

![SAP HANA Cloud Central Manage Configuration button](image-6.png)

Select the "Data Lake Files" button at the top of the page.

![Manage Configuration Data Lake Files button.](image-7.png)

Here is a full view of the screen showing the location of "Trusts" and "Authorizations".
You may need to scroll down to find these.

![Mange configuration page displaying the Trusts and Authorization locations.](image-8.png)

Click on **"Add"** under Trusts configuration and hit on **"Upload"** file button and browse to the location where your `ca.crt` is located and upload that file and click on apply.

The **alias** can be anything, but the certificate should be exactly what is in the generated `ca.crt`.

![Add Trust modal.](image-9.png)

Click on **"Add"** under Authorizations and select the roles as **"Admin"** or **"User"** and then click on **"Generate pattern"** from the output of the following command. (exclude the "subject=" prefix):

```Shell
openssl x509 -in client.crt -nameopt RFC2253 -subject -noout
```

Alternatively, you can use the **"Generate Pattern"** option and similarly upload the `client.crt` file after clicking on the "Upload" file option. It will automatically generate a pattern like above.

![Authorizations Generate Pattern modal.](image-10.png)

Now click "Review and Save" at the bottom of the page.

![Manage configuration review and save button.](image-11.png)

Click "Save Changes" on the modal to confirm changes.

![Manage configuration save changes modal.](image-12.png)

### Check that a Connection can be Established from the HDLFSCLI


Next, we will verify that the configuration we did in Steps 5 & 6 work.

The `<Instance ID>` and `<REST API Endpoint>` can be copied by clicking on the ellipses in SAP HANA Cloud Central.

![Copy instance ID and REST API Endpoint in Cloud Central.](image-13.png)

`<PATH>` is the path to the corresponding certificate. The following command lists out files in the root folder of the HDL files instance. Thus if the instance has no files, the command will not return anything.

```Shell
hdlfscli -cert <PATH>\client.crt -key <PATH>\client.key -cacert <PATH>\ca.crt -k -s https://<REST API Endpoint> -filecontainer <Instance ID> ls
```

**[Optional]:** Configure a configuration file to make using the CLI simpler.

**Note:** The configuration will be placed in the **user's root directory**. It is saved as a JSON file that can be modified in any text editor.

```Shell
hdlfscli -cert <PATH>\client.crt -key <PATH>\client.key -k -s <REST API Endpoint> -config myconfig -dump-config ls
```

Test the configuration that was just created.

```Shell
hdlfscli -config myconfig ls
```

Upload a file to the SAP HANA, data lake Files storage instance. Ensure you know the path to the TPCH data files that were downloaded in the prerequisites.

```Shell
hdlfscli -config myconfig upload <Your Local Path>\TPCH <Target File Path>\TPCH
```

Verify that the files has been uploaded.

```Shell
hdlfscli -config myconfig lsr
```

Now that the TPCH data files are in the data lake Files container, we can use SQL on Files to query the data. Learn how to do this in the tutorial [Explore SAP HANA Cloud, SAP HANA Database SQL on Files](hana-dbx-sof).


**[Troubleshoot]:** If you get this error while verifying the configuration, follow the steps below:

![Troubleshoot the configuration error.](image-14.png)

Copy the content of the **Client** field inside [ ] brackets. Then go to your Data Lake instance, click on Manage Configuration, and scroll down to Authorizations. **Delete** the entire value from the existing "Pattern" field and paste in the Client field value.

Now, re-verify the configuration and it should work.

For more information on HDLFSCLI, check the [SAP Help Portal documentation](https://help.sap.com/docs/hana-cloud-data-lake/utility-reference-for-data-lake-relational-engine/hdlfscli-data-lake-files-utility?version=2021_4_QRC).

### Knowledge check

---
