---
author_name: Supriya Cherukunnathu
author_profile: https://github.com/SupriyaCherukunnathu
title: Install the Paho Client
description: Install the Paho client to send data to the SAP Cloud Platform Internet of Things Service Cloud using MQTT and REST.
auto_validation: true
primary_tag: topic>internet-of-things
tags: [ tutorial>beginner, tutorial>license, topic>internet-of-things, topic>cloud, products>sap-leonardo-iot, products>sap-edge-services, products>sap-cloud-platform-internet-of-things, products>sap-cloud-platform ]
---


## Prerequisites
 - **Proficiency:** Beginner


## Details
### You will learn
- How to install the Paho client for MQTT on Windows or macOS

### Time to Complete
5 min

## Next Steps
- **Tutorials:** [Install OpenSSL](iot-cf-install-openssl)

---

[ACCORDION-BEGIN [Step 1: ](Install the Paho Client on Windows 10)]

1.  Download the [Paho Client (Windows)](http://www.eclipse.org/paho/components/tool//).

    A ZIP archive `org.eclipse.paho.ui.app-1.0.0-win32.win32.x86_64.zip` should be downloaded.

2.  Extract the downloaded ZIP archive.

3.  Launch the `paho.exe` in the extracted folder.

    > **Known Issues:** Paho does not start.
    >
    > **Solution:** Unpack the Paho ZIP archive into a folder without white spaces in the absolute path.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Install the Paho Client on macOS)]

1.  Download the [Paho Client (macOS)](http://www.eclipse.org/paho/components/tool//).

    A GZ archive `org.eclipse.paho.ui.app-1.0.0-macosx.cocoa.x86_64.tar.gz` should be downloaded.

2.  Extract the downloaded GZ archive.

3.  Launch the `paho.app` in the extracted folder.

    > **Known Issues:** Paho quits unexpectedly.
    >
    > **Solution:** Choose `Paho.app` and show package content. Launch Paho in folder `/Contents/MacOS`.
    >

[DONE]

[ACCORDION-END]
