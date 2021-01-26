---
author_name: Supriya Cherukunnathu
author_profile: https://github.com/SupriyaCherukunnathu
title: Install OpenSSL
description: Install OpenSSL to send data to the SAP Cloud Platform Internet of Things Service Cloud using MQTT and REST.
auto_validation: true
primary_tag: topic>internet-of-things
tags: [ tutorial>beginner, tutorial>license, topic>internet-of-things, topic>cloud, products>sap-leonardo-iot, products>sap-edge-services, products>sap-cloud-platform-internet-of-things, products>sap-cloud-platform ]
time: 10
---


## Details
### You will learn
- How to install OpenSSL on Windows or macOS


---

[ACCORDION-BEGIN [Step 1: ](Install OpenSSL)]

1.  For Windows download [OpenSSL](https://sourceforge.net/projects/openssl/files/latest/download).

2.  Extract the downloaded ZIP archive.

3.  For ease of use, we recommend that you add the directories from the OpenSSL binaries to your PATH environment variable. For example, you could enter the following in your terminal (only valid for one session):

    `set PATH=%PATH%;c:\<PATH_TO_OPENSSL.EXE>`

    `set PATH=%PATH%;c:\OpenSSL-Win32\bin\`

    >On macOS, OpenSSL is usually installed by default (`/user/bin/openssl`).



[DONE]
[ACCORDION-END]



[ACCORDION-BEGIN [Step 2: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
