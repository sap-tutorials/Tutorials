---
title: Set Up the SAP Business Connector for On-Premise WebSocket RFC to Cloud
description: Learn how to set up the SAP Business Connector to to connect an on-premise system prior to 1909 to cloud using WebSocket RFC.
auto_validation: true
time: 30
tags: [ tutorial>intermediate, topic>abap-development]
primary_tag: topic>abap-connectivity
---


## Details
### You will learn
- to set up the SAP BC for WebSocket RFC from on-premise to cloud systems.


To download and install the business connector, see the following information.

The installer can be downloaded free of charge from the [SAP BC homepage](https://support.sap.com/sbc-download).

(A one-time registration of the SAP customer ID is required.)

An installation guide and other documentation can be downloaded from the [SAP BC details page](https://support.sap.com/en/product/connectors/bc/details.html).


Installation and some basic setup like sizing can be done even by someone without prior SAP BC knowhow in less than an hour.
In case of problems, SAP support tickets can be opened under component BC-MID-BUS.
In this tutorial, you will learn how to set up the SAP Business Connector for WebSocket RFC calls from on-premise to Cloud systems.

**If the on-premise ABAP System is an S/4HANA version 1909 or newer, the underlying ABAP platform can perform WebSocket RFC calls without any additional component. The business connector is not required in this case.**

---

[ACCORDION-BEGIN [Step 1: ](Download SAP Business Connector)]

1. Go to the [SAP BC homepage](https://support.sap.com/sbc-download) and download the SAP BC server and developer version.
2. Install the SAP BC by running the setup file in administrator mode. We recommend to access the setup file from the console.
**When installing the developer version, make sure to also install the certificate toolkit.**
3. Install the latest core fix and service release. You can find them on the download page of the SAP BC. Follow the instructions of the installation guides. You can find them in the ReadMe text file in the download folder.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set up the certificate directory)]

1. Go to `config` folder in the directory where SAP BC Server is installed. For example `C:\sapbc481\Server\config`.
2. Create a new folder with the name `trusted`.
3. Start SAP BC Server.
4. Go to **Security > Certificates**.
5. Choose **Edit Certificates Settings**.
  ![System URL](setupbc1.png)
6. Under **Trusted Certificates**, enter `config/trusted` in field **CA Certificate Directory**.
7. Choose **Save Changes**.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Generate certificates)]

1. Start the Certificate Toolkit. You can find it in the SAP BC Developer directory. Here, go to `certkit/bin/ssltoolkit.bat`.
2. Select **Generate a Private Key** and choose **Next**.
    - Key size: `2048 (Bit)`
    - File name: `privkey.der`
    - Choose **Next**.
    - Optional: Enter a directory where the private key should be stored.
    - Under **Enter CSR file name**, enter `privkey.pem`.
    - Choose **Next** to generate the Certificate Signing Request.
    - Have the request signed by a certificate authority.
    - In the Certificate Toolkit, choose **Convert and Save Certificates for use with SAP Business Connector**.
    - Upload the CA's response and choose **Next**. Change the name of the CA's response to `cacert.der`
    - Certificate filename: `cert.der`
    - Choose **Next** to generate the certificate.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Update certificate directory)]
1. Copy the private key, the CA's response, and the certificate to the `config` folder of the SAP BC server.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]




---
