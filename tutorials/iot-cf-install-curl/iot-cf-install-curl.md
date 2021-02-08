---
author_name: Supriya Cherukunnathu
author_profile: https://github.com/SupriyaCherukunnathu
title: Install cURL
description: Install cURL to send data to the SAP Cloud Platform Internet of Things Service Cloud using REST.
auto_validation: true
primary_tag: topic>internet-of-things
tags: [ tutorial>beginner, tutorial>license, topic>internet-of-things, topic>cloud, products>sap-leonardo-iot, products>sap-edge-services, products>sap-cloud-platform-internet-of-things, products>sap-cloud-platform ]
---


## Prerequisites
 - **Proficiency:** Beginner


## Details
### You will learn
- How to install cURL on Windows or macOS

### Time to Complete
10 min

---

[ACCORDION-BEGIN [Step 1: ](Install cURL on Windows 10)]

1.  Download [cURL (Windows)](https://curl.haxx.se/windows/).

2.  Extract the downloaded ZIP archive.

3.  For ease of use we recommend that you add the directories from the cURL binaries to your PATH environment variable. For example, you could enter the following in your terminal (only valid for one session):

    `set PATH=c:\<PATH_TO_CURL.EXE>;%PATH%`

4.  Check the version of cURL. It must be enabled with OpenSSL. Enter the following command in the terminal:

    `curl --version`

    The version should include OpenSSL, for example:

    `curl 7.64.0 (x86_64-pc-win32) libcurl/7.64.0 OpenSSL/1.1.1a (Schannel) zlib/1.2.11 brotli/1.0.7 WinIDN libssh2/1.8.0 nghttp2/1.36.0
    Release-Date: 2019-02-06
    Protocols: dict file ftp ftps gopher http https imap imaps ldap ldaps pop3 pop3s rtsp scp sftp smb smbs smtp smtps telnet tftp
    Features: AsynchDNS IDN IPv6 Largefile SSPI Kerberos SPNEGO NTLM SSL libz brotli TLS-SRP HTTP2 HTTPS-proxy MultiSSL`

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Install cURL on macOS)]

1.  cURL is usually installed by default on macOS (`/user/bin/curl`).

2.  Check the version of cURL. Enter the following command in the terminal:

    `curl --version`

    Version **with SecureTransport**, for example:

    `curl 7.61.0 (x86_64-apple-darwin17.6.0) libcurl/7.61.0 SecureTransport zlib/1.2.11
    Release-Date: 2018-07-11
    Protocols: dict file ftp ftps gopher http https imap imaps ldap ldaps pop3 pop3s rtsp smb smbs smtp smtps telnet tftp
    Features: AsynchDNS IPv6 Largefile NTLM NTLM_WB SSL libz UnixSockets`

    Version **without SecureTransport using LibreSSL**, for example:

    `curl 7.54.0 (x86_64-apple-darwin17.0) libcurl/7.54.0 LibreSSL/2.0.20 zlib/1.2.11 nghttp2/1.24.0
    Protocols: dict file ftp ftps gopher http https imap imaps ldap ldaps pop3 pop3s rtsp smb smbs smtp smtps telnet tftp
    Features: AsynchDNS IPv6 Largefile GSS-API Kerberos SPNEGO NTLM NTLM_WB SSL libz HTTP2 UnixSockets HTTPS-proxyt`

[DONE]

[ACCORDION-END]
