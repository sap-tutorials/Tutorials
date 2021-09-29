---
title: Installing XS CLI Client
description: Install the client package if you intend to develop XS applications on a machine that will not have a local SAP HANA 2.0, express edition installation. The clients let you access SAP HANA 2.0, express edition from your client machine.
author_name: Adrian Plata
author_profile: https://github.com/aplata-sap
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
time: 10
---

<!-- loioa0fb36b2aebf4b6fbda0564d5e8e58c5 -->

## Prerequisites

## Details
### You will learn
How to install the XS advanced client-tools bundle.

---

The `server machine` in these instructions refers to the machine on which SAP HANA 2.0, express edition is installed, while `client machine` refers to your local machine. You do not need to install the two on the same machine or VM.

[ACCORDION-BEGIN [Step 1: ](XS CLI client info.)]

The XS advanced `client-tools` bundle (`xs.onpremise.runtime.client_<platform>-<version>.zip`) also includes the `Javascript` bundle (`xs_javascript-1.3.0-bundle.tar.gz`), which includes a selection of mandatory `Node.js` packages developed by SAP for use with the `Node.js` applications running XS Advanced runtime.

You can use the XS command line client to perform a wide variety of developer- and administrator-related tasks. For example, in the role of a developer, you can use the XS CLI to connect to the XS Advanced runtime installed on the server machine, log on as a specific user, and deploy and manage your applications.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download the client package.)]

Install the Download Manager to your client machine and download the client package.

1.  Save the Download Manager installation files to your client machine and open it. For instructions on downloading and running the Download Manager, see either the [Installing SAP HANA 2.0, express edition (Binary Installer Method)](https://developers.sap.com/tutorials/hxe-ua-installing-binary.html) or [Installing SAP HANA 2.0, express edition (Virtual Machine Method)](https://developers.sap.com/tutorials/hxe-ua-installing-vm-image.html) tutorials, or go straight to the SAP HANA, express edition [registration page](https://www.sap.com/cmp/ft/crm-xu16-dat-hddedft/index.html).

2.  In Download Manager, in the `Image` pull-down, select either `Virtual Machine` or `Binary Installer`.

3.  Click `Browse` and select a directory where your client package will be saved.

4.  Select the `Clients` package that matches the machine you will be installing the clients on. Clear the Select boxes of all other packages.

5.  Click `Download`. The `hdb_client_<OS>.tgz` file, or `clients_windows.zip` for Windows, downloads to your save directory.

6.  Extract the compressed clients file:

    -   For Windows and Mac machines, use a compression utility.

    -   For Linux machines, navigate to the directory in which you wish to extract the client files and use the `tar` command:

```cd <preferred_filepath>
sudo tar -xzf <download_filepath>/clients_<OS>.tgz
```            

These files are extracted:

`clients_linux_x86_64.tgz`

-   `hdb_client_linux_x86_64.tgz`

-   `xs.onpremise.runtime.client_linuxx86_64.zip`


`clients_linux_ppc64.le.tgz`

-   `hdb_client_linux_ppc64le.tgz`

-   `xs.onpremise.runtime.client_linuxx86_64.zip`


`clients_windows.zip`

-   `hdb_client_windows_x86_32.tgz`

-   `hdb_client_windows_x86_64.tgz`

-   `xs.onpremise.runtime.client_ntamd64.zip`


`clients_mac.tgz`

-   `hdb_client_mac.tgz`

-   `xs.onpremise.runtime.client_darwinintel64.zip`


[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Install the XS CLI client.)]

Use a compression utility to extract the file you downloaded for your platform:

-   (Windows) `xs.onpremise.runtime.client_ntamd64.zip`

-   (Mac) `xs.onpremise.runtime.client_darwinintel64.zip`

-   (Linux) `xs.onpremise.runtime.client_linuxx86_64.zip`

-   (PowerPC) `xs.onpremise.runtime.client_linuxppc64le.zip`


The system creates this folder:

```bash
xs.onpremise.runtime.client_<version>
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add the bin folder to the PATH environment variable.)]

-   (Windows) In the Environment Variables dialog:

    -   Edit *System variables > Path*

    -   Add `<extracted_filepath>\bin`

    -   Restart the command line application for your new environment variable settings to take effect.
-   (Mac) Run `export PATH=$PATH:/<extracted_filepath>/bin`

-   (Linux) Run `export PATH=$PATH:/<extracted_filepath>/bin`

-   (Power PC) Run `export PATH=$PATH:/<extracted_filepath>/bin`


[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Verify XS Advanced runtime is installed.)]

Enter the following URL into your Web browser:

```bash
https://<hana_hostname>:3<instance_number>30/v2/info
```

For example:

```bash
https://my.hana.server:39030/v2/info
```

The response displayed in the Web browser is a JSON string with details that indicate whether there was a successful connection to the XSA controller. The connection must exists before you can connect from within the API command.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Confirm XS Advanced is Available)]

On your client machine, open a command window and run the following.

```bash
xs help
xs -v
```

On Linux, run these as `<sid>adm`.

You see the `Client Version` in the output. If not, you cannot connect to XS Advanced runtime on SAP HANA to deploy your XS Advanced applications.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Connect to XS Advanced controller.)]

Specify the URL of the API end point on the SAP HANA server you want to connect to:

```bash
xs api https://<hostname>:3<instance_number>30
```

> Note:
> If this step fails, it may be due to a missing SSL certificate. Continue on to the next step to add the SSL certificate, otherwise skip the next step.
>
>

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Add SSL certificate to connect to the server.)]

Open a command session on the server machine or open a PuTTY session to the server machine. From the command prompt, log in as `sudo` and go to the certificate `default.root.crt.pem`, which is typically located here:

```bash
<installation_path>/<SID>/xs/controller_data/controller/ssl-pub/router
```

For example, where `<installation_path>` is `/hana/shared` and `<SID>` is `HXE` the certificate location would be:

```bash
/hana/shared/HXE/xs/controller_data/controller/ssl-pub/router/default.root.crt.pem
```

Copy the certificate to a folder on the server where you can easily access it. Using an FTP client or the `scp` command, send a copy of the certificate from your server machine to a safe location on your client machine.

FTP:

```bash
/<path>/default.root.crt.pem
```

`scp`:

```bash
scp <server_machine_user>@<ip_address_server>:<file_destination>/default.root.crt.pem <client_machine_user>@<ip_address_client>:<your_desired_filepath>\
```

Exit your FTP and PuTTY sessions and return to your client machine. Try the previous command again, but use the `-cacert` option and specify the local certificate you just copied:

```bash
xs api https://<hostname>:3<instance_number>30 - cacert "<copied_filepath>/default.root.crt.pem"
```

Log on to the SAP HANA instance specified in the API end point that you set in a previous step. SAP HANA provides the default `XSA_ADMIN` user with administrator permissions. Although you can use this user ID to test the connection, you should create a new user with more limited permissions to use for developer tasks.

To log on, run the following:

```bash
xs login -u XSA_ADMIN -p "<password>"
```

> Note:
> A password is assigned to the `XSA_ADMIN` user during SAP HANA 2.0, express edition installation.
>
>

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Test the XS Advanced connection.)]

To test your connection to XS Advanced by running the following command on the SAP HANA 2.0, express edition server:

```bash
xs apps
```

[DONE]

[ACCORDION-END]
