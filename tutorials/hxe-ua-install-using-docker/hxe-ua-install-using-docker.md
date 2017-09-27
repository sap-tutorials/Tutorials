---
title: Installing SAP HANA, express edition with Docker
description: Install SAP HANA, express edition with Docker.
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, products>sap-hana\,-express-edition ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Software:**
 - SUSE Linux Enterprise Server 12.0, 12.1, 12.2
 - Docker Enterprise Edition for SUSE Enterprise Linux Server


## Next Steps
 - [Start Using SAP HANA, express edition](http://www.sap.com/developer/tutorials/hxe-ua-getting-started-vm.html). See Step 2.

## Details
### You will learn  
How to install SAP HANA, express edition on your preferred Docker setup.

This tutorial will show you how to install a server-only installation of SAP HANA, express edition on your Docker installation.

If you wish to install SAP HANA, express edition on a different virtual machine, or you want a custom setup on your Linux machine, see the [Virtual Machine](https://www.sap.com/developer/tutorials/hxe-ua-installing-vm-image.html) or [Binary Method](https://www.sap.com/developer/tutorials/hxe-ua-installing-binary.html) installation guides.

Before you begin, ensure your proxy settings have been properly set up. See [**HTTP/HTTPS proxy**](https://docs.docker.com/engine/admin/systemd/#httphttps-proxy) in the Docker documentation.

### Time to Complete
**10 Min**

---

SAP HANA, express edition is a streamlined version of the SAP HANA platform which enables developers to dive into application development in the cloud or personal computer to build and deploy modern applications that use up to 32GB memory. SAP HANA, express edition includes the in-memory data engine with advanced analytical data processing engines for business, text, spatial, and graph data - supporting multiple data models on a single copy of the data. The software license allows for both non-production and production use cases, enabling you to quickly prototype, demo, and deploy next-generation applications using SAP HANA, express edition without incurring any license fees.

**This installation does not support Docker for Windows or Docker for Mac.**

[ACCORDION-BEGIN [Step 1: ](Install Docker)]

Download and install Docker Enterprise Edition for SUSE Enterprise Linux Server. Visit the [Docker Enterprise Edition SUSE Enterprise Linux Server](https://store.docker.com/editions/enterprise/docker-ee-server-sles?tab=description) page for information on how to install Docker on your SLES system.

> Note:
> The remaining steps in the tutorial assume you are running as `sudo`.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download the SAP HANA, express edition Image from the Docker Library)]

Go to the [Docker Store](https://store.docker.com/).

Click on the search bar and search for "SAP HANA, express edition".

Click on the **Setup Instructions** button.

Copy the Docker pull address. Here is an example:

```
docker pull store/saplabs/hanaexpress:2.00.020.01.20170829.3
```

Open your Docker-enabled command line and use the Docker pull address to download the image.

This loads the SAP HANA, express edition image. To ensure that the image was loaded successfully, run:

```bash
docker images
```

The SAP HANA, express edition image will be listed as `hanaexpress`.

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Edit the host sysctl.conf file)]

Before you can start the container, ensure that the following parameters are set in your host's `/etc/sysctl.conf` file. The host can be a virtual machine, physical machine, or a cloud instance.

```
fs.file-max=20000000
fs.aio-max-nr=262144
vm.memory_failure_early_kill=1
vm.max_map_count=135217728
net.ipv4.ip_local_port_range=40000 60999
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a Directory for the SAP HANA, express edition Container)]

Create a directory for the SAP HANA, express edition container and grant it the proper permissions.  

```
mkdir -p /data/<system_name>
chown 12000:79 /data/<system_name>
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create a Password JSON File)]

Create a `json` file with one of the following formats:  

```
{
"master_password" : "<password>"
}  
```

or:  

```
{
"system_user_password" : "<password",
"default_tenant_system_user_password" : "<second_password>"
}
```

Here is an example:

```
{
  "master_password" : "HXEHana1"
}
```

This file serves as the master password for your SAP HANA, express edition users. The password must comply with these rules:

* At least 8 characters
* At least 1 uppercase letter
* At least 1 lowercase letter
* At least 1 number
* Cannot contain dictionary words
* Cannot contain simplistic or systemic values, like strings in ascending or descending numerical or alphabetical order

Make a note of the path to the `JSON` file. You will need this to load the SAP HANA, express edition container.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Load the SAP HANA, express edition Container)]

Use the SAP HANA, express edition image to create a container.

```
docker run -p 39013:39013 -p 39017:39017 -p 39041-39045:39041-39045 -p 1128-1129:1128-1129 -p 59013-59014:59013-59014 -v /data/<system_name>:/hana/mounts \
-v <path_to_json_file_folder>:<path_to_json_file_folder> \
--ulimit nofile=1048576:1048576 \
--sysctl kernel.shmmax=1073741824 \
--sysctl kernel.shmmni=524288 \
--sysctl kernel.shmall=8388608 \
--name <system_name> \
store/saplabs/hanaexpress:2.00.020.01.20170829.3 \
--passwords-url <file://<path_to_json_file> OR http/https://<url_to_json_file>> \
--agree-to-sap-license
```

Example:

```
docker run -p 39013:39013 -p 39017:39017 -p 39041-39045:39041-39045 -p 1128-1129:1128-1129 -p 59013-59014:59013-59014 -v /data/express_edition:/hana/mounts \
-v /hana:/hana \
--ulimit nofile=1048576:1048576 \
--sysctl kernel.shmmax=1073741824 \
--sysctl net.ipv4.ip_local_port_range='40000 60999' \
--sysctl kernel.shmmni=524288 \
--sysctl kernel.shmall=8388608 \
--name express_edition \
store/saplabs/hanaexpress:2.00.020.01.20170829.3 \
--passwords-url file:///hana/password.json \
--agree-to-sap-license
```

This example creates the SAP HANA, express edition container with the name `express_edition`.  

> Note:
> If the `JSON` file you are using is an *http* or *https* URL, you can leave out the `-v` option.

> Note:
> For Linux kernel versions earlier than 4, omit the `net.ipv4.ip_local_port_range` option.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Start SAP HANA, express edition)]

To start your SAP HANA, express edition container, run the following command:

```bash
docker exec -it -u <container_name> bash
```

Example:

```
docker exec -it -u express_edition bash
```


[ACCORDION-END]

[ACCORDION-BEGIN [Optional: ](Create additional SAP HANA, express edition Containers)]

You will need to repeat the previous steps of creating a directory and `JSON` password for each additional SAP HANA, express edition container you wish to create.  

```
docker run -p 10013:39013 -p 10017:39017 -p 10041-10045:39041-39045 -p 10028-10029:1128-1129 -p 19013-19014:59013-59014 -v /data/<additional_system_name>:/hana/mounts \
-v <path_to_json_file_folder>:<path_to_json_file_folder> \
--ulimit nofile=1048576:1048576 \
--sysctl kernel.shmmax=1073741824 \
--sysctl kernel.shmmni=524288 \
--sysctl kernel.shmall=8388608 \
--name <additional_system_name> \
store/saplabs/hanaexpress:2.00.020.01.20170829.3 \
--passwords-url <file://<path_to_json_file> OR http/https://<url_to_json_file>>
--agree-to-sap-license
```

This process will take several minutes. The prompt will read `Startup finished` once the container has been successfully running. This container starts in detached mode so you will need to open another command prompt to continue.  

[ACCORDION-END]


[ACCORDION-BEGIN [Docker Run Usage: ](-Help Command)]

The following is a list of options available for the `docker run saplabs/hanaexpress` command.

```
docker run store/saplabs/hanaexpress:2.00.020.01.20170829.3 -h
usage: [options]
--dont-check-consistency Skip consistency check between mount points
--dont-check-mount-points Skip check for allowed mount points
--dont-check-version Skip compatibility check of current and last HANA version
--dont-check-system Skip check for incompatible /proc/sys values
--dont-exit-on-error Halt script on error to allow root cause analysis
(MUST NOT be used in production)
--license-url <url> URL for a license file (json)
Format: {"landscape-id":"<8-4-4-4-12 GUID>", "license":"<license>"}
--passwords-url <url> URL for a password file (json)
Format:
{"master_password":"<pwd>"}
or
{"system_user_password":"<pwd>","default_tenant_system_user_password":"<pwd>"}
--print <print_option> Print and exit, options are:
README
hdb_version
--agree-to-sap-license Indicates you agree to the SAP Developer Center Software Developer License Agreement.
```

[ACCORDION-END]

## Next Steps
- [Start Using SAP HANA, express edition](http://www.sap.com/developer/tutorials/hxe-ua-getting-started-vm.html). Begin at Step 2.
