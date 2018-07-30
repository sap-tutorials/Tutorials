---
title: Install OpenSSL
description: Install OpenSSL to send data to the SAP Cloud Platform Internet of Things Service Cloud using MQTT and REST.
primary_tag: products>sap-cloud-platform-internet-of-things
tags: [ tutorial>beginner, products>sap-cloud-platform-internet-of-things, topic>internet-of-things ]
---

<!-- loio62aa257a98114802977b98495bdb4e6d -->

## Prerequisites
 - **Proficiency:** Beginner


## Details
### You will learn
- How to install OpenSSL on Windows or macOS.

### Time to Complete
10 min

---

[ACCORDION-BEGIN [Step 1: ](Install OpenSSL on Windows 10)]

1.  Download [OpenSSL (Windows)](https://sourceforge.net/projects/openssl/files/latest/download).

2.  Extract the downloaded ZIP archive. It should include a bin folder with the `openssl.exe` file.

3.  For ease of use we recommend that you add the directories from the OpenSSL binaries to your PATH environment variable. For example, you could enter the following in your terminal (only valid for one session):

    ```bash
    set PATH=%PATH%;c:\<PATH_TO_OPENSSL.EXE>
    ```

    ```bash
    set PATH=%PATH%;c:\OpenSSL-Win32\bin\
    ```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](OpenSSL on macOS)]

1.  OpenSSL is usually installed by default on macOS (`/user/bin/openssl`).


[ACCORDION-END]
