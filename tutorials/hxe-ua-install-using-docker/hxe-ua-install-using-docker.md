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
- [How to Install SAP HANA 2.0, express edition Clients](https://www.sap.com/developer/tutorials/hxe-ua-howto-installing-clients.html)
 - [How to download and install the HANA Eclipse plugin](https://www.sap.com/developer/tutorials/hxe-howto-eclipse.html)

## Details
### You will learn  
How to install SAP HANA, express edition on your preferred Docker setup.

This tutorial will show you how to install a **server-only** installation of SAP HANA, express edition on your Docker installation. This version of SAP HANA, express edition does not contain XSC.

If you wish to install SAP HANA, express edition on a different virtual machine, or you want a custom setup on your Linux machine, see the [Virtual Machine](https://www.sap.com/developer/tutorials/hxe-ua-installing-vm-image.html) or [Binary Method](https://www.sap.com/developer/tutorials/hxe-ua-installing-binary.html) installation guides.

Before you begin, ensure your proxy settings have been properly set up. See [**HTTP/HTTPS proxy**](https://docs.docker.com/engine/admin/systemd/#httphttps-proxy) in the Docker documentation.

### Time to Complete
**10 Min**

---

SAP HANA, express edition is a streamlined version of the SAP HANA platform which enables developers to jump-start application development in the cloud or personal computer to build and deploy modern applications that use up to 32GB memory. SAP HANA, express edition includes the in-memory data engine with advanced analytical data processing engines for business, text, spatial, and graph data - supporting multiple data models on a single copy of the data.

The software license allows for both non-production and production use cases, enabling you to quickly prototype, demo, and deploy next-generation applications using SAP HANA, express edition without incurring any license fees. Memory capacity increases beyond 32GB are available for purchase at the [SAP Store](https://www.sapstore.com/solutions/99055/SAP-HANA%2C-express-edition).

In addition to SUSE Enterprise, SAP HANA, express edition for Docker has been tested on the following Linux operating system versions:

| Linux OS | OS Version |
| --- | --- |
| `Ubuntu`  | `17.04 (Zesty Zapus)` |
| `openSUSE` | `openSUSE Leap` |
| `CentOS` | `7 (Core)` |
| `Debian` | `9 (Stretch)` |
| `Fedora` | `25 (Server Edition)` |

**This installation does not support Docker for Windows or Docker for Mac.**

[ACCORDION-BEGIN [Step 1: ](Install Docker)]

Download and install Docker Enterprise Edition for SUSE Enterprise Linux Server. Visit the [Docker Enterprise Edition SUSE Enterprise Linux Server](https://store.docker.com/editions/enterprise/docker-ee-server-sles?tab=description) page for information on how to install Docker on your SLES system.

> **Note:**
> Ensure your proxy settings have been properly set up. See [**HTTP/HTTPS proxy**](https://docs.docker.com/engine/admin/systemd/#httphttps-proxy) in the Docker documentation.

[ACCORDION-END]

[ACCORDION-BEGIN [Optional: ](Test Your Docker Installation)]

Test your Docker installation by running the "Hello World" container application. Run the following command from your Docker-enabled command prompt:

```
sudo docker run --name helloWorld alpine echo hello
```

If successful, the following should display:

```
Unable to find image 'alpine:latest' locally
latest: Pulling from library/alpine
88286f41530e: Pull complete
Digest: sha256:<log_number>
Status: Downloaded newer image for alpine:latest
hello
```

If `hello` is printed, you have successfully pulled the container image **alpine** (a demo Linux distribution), and run the instance of the container `helloWorld`, and ran the command `echo` with an input parameter of `hello`.

If you **did not** get this output, the Docker installation has not been completed or the Docker daemon can not connect to the internet. Review the process and check the [Docker Documentation](https://docs.docker.com/get-started/) for more information in troubleshooting your Docker installation.

Remove the alpine image with the following command:

```
sudo docker image rm alpine -f
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download the SAP HANA, express edition Image from the Docker Library)]

Go to the [Docker Store](https://store.docker.com/).

Click on the search bar and search for "SAP HANA, express edition".

Click on the **Setup Instructions** button.

Copy the Docker pull address. Here is an example:

```
sudo docker pull store/saplabs/hanaexpress:2.00.022.00.20171211.1
```

Open your Docker-enabled command line and use the Docker pull address to download the image.

This loads the SAP HANA, express edition image. To ensure that the image was loaded successfully, run:

```bash
docker images
```

The SAP HANA, express edition image will be listed as `hanaexpress`.

> Note:
> You may have to log into your Docker account to pull the image. From your Docker-enabled command line, run `docker login` and follow the prompts to enter your Docker ID and password. Once you have logged in, try the pull command again.

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

To edit the `sysctl.conf` file, use the `vi` command to open the file and press `i` to switch to interactive mode. Edit the file as necessary, hit the `esc` key, and type and enter `:wq!` to write and save the changes.

[ACCORDION-END]

[ACCORDION-BEGIN [Optional: ](Create a Directory to Persist SAP HANA, express edition Data Outside of the Container)]

Create a directory for the SAP HANA, express edition container and grant it the proper permissions.  

```
mkdir -p /data/<directory_name>
chown 12000:79 /data/<directory_name>
```

The name of this directory does not need to match the name you give to your SAP HANA, express edition container.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Set Up Password for SAP HANA, express edition)]

To make your system more secure, you specify your own password before you create your container. This is done by creating a `json` file as opposed to having a default password. The file can be stored locally or on another system accessible by URL. If the file is to be stored locally, store it in the */data/<directory_name>* directory you created earlier.

Create the `json` file:

```
vi <file_name>.json
```

Press `i` to start editing and use one of the following formats to create the file:  

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

Press `esc` and then enter `wq!` to write and save the file.

This file serves as the master password for your SAP HANA, express edition users. The password must comply with these rules:

* At least 8 characters
* At least 1 uppercase letter
* At least 1 lowercase letter
* At least 1 number
* Can contain special characters, but not _&grave;_ (backtick), _&#36;_ (dollar sign),  _&#92;_ (backslash), _&#39;_ (single quote), or _&quot;_ (double quotation marks).
* Cannot contain dictionary words
* Cannot contain simplistic or systemic values, like strings in ascending or descending numerical or alphabetical order

You must then add permissions for this file to be readable by the `hxeadm` user in the container. Change permissions with:

```
sudo chmod 600 /data/<directory_name>/<file_name>.json
sudo chown 12000:79 /data/<directory_name>/<file_name>.json
```

Be sure to do this with each `json` file you use for your Docker containers.

Make a note of the path to the `json` file. You will need this to load the SAP HANA, express edition container.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Start SAP HANA, express edition Container)]

Use the SAP HANA, express edition image to create a container.

```
sudo docker run -p 39013:39013 -p 39017:39017 -p 39041-39045:39041-39045 -p 1128-1129:1128-1129 -p 59013-59014:59013-59014 -v /data/<directory_name>:/hana/mounts \
--ulimit nofile=1048576:1048576 \
--sysctl kernel.shmmax=1073741824 \
--sysctl net.ipv4.ip_local_port_range='40000 60999' \
--sysctl kernel.shmmni=524288 \
--sysctl kernel.shmall=8388608 \
--name <container_name> \
store/saplabs/hanaexpress:2.00.022.00.20171211.1 \
--passwords-url <file://<path_to_json_file> OR http/https://<url_to_json_file>> \
--agree-to-sap-license
```

Example:

```
sudo docker run -p 39013:39013 -p 39017:39017 -p 39041-39045:39041-39045 -p 1128-1129:1128-1129 -p 59013-59014:59013-59014 -v /data/express_edition:/hana/mounts \
--ulimit nofile=1048576:1048576 \
--sysctl kernel.shmmax=1073741824 \
--sysctl net.ipv4.ip_local_port_range='40000 60999' \
--sysctl kernel.shmmni=524288 \
--sysctl kernel.shmall=8388608 \
--name express_edition \
store/saplabs/hanaexpress:2.00.022.00.20171211.1 \
--passwords-url file:///hana/password.json \
--agree-to-sap-license
```

This example creates the SAP HANA, express edition container with the name `express_edition`. This process will take several minutes. The prompt will read `Startup finished` once the container has been successfully running. This container starts in detached mode so you will need to open another command prompt to continue.  

> **Note:**
> If you placed the password file in `/data/<directory_name>/<file_name>.json`, substitute  `file://<path_to_json_file>` with `file:///hana/mounts/<file_name>.json`.

> **Note:**
> Check if the password file `/hana/mounts/<file_name>.json` was deleted after the SAP HANA, express edition container starts.  If not, you can manually delete it. If the `JSON` file you are using is an *http* or *https* URL, you can leave out the `-v` option.

> **Note:**
> For Linux kernel versions earlier than 4, omit the `net.ipv4.ip_local_port_range` option.

> **Note:**
> If the Docker container fails to create an SAP HANA instance, you will need to remove the Docker container to try again. Run `docker rm <container_name>` to remove the container. Additionally, you will need to delete the files created in the `/data/directory_name/` directory.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Log into SAP HANA, express edition Container)]

To start your SAP HANA, express edition container, run the following command:

```bash
sudo docker exec -it -u <container_name> bash
```

Example:

```
sudo docker exec -it -u express_edition bash
```


[ACCORDION-END]

[ACCORDION-BEGIN [Optional: ](Test the Container)]

When you are logged into the SAP HANA, express edition container, you can test your installation by entering the following:

```
whoami
```

You should be logged in as `hxeadm`, the default SAP HANA, express edition user.

You can also enter the following:

```
HDB info
```

And you should see the following services running:

* `hdbnameserver`
* `hdbcompileserver`
* `hdbdiserver`
* `hdbwebdispatcher`

[ACCORDION-END]

[ACCORDION-BEGIN [Optional: ](Log Into System or Tenant Database)]

You can log into the system database with the following command:

```
hdbsql -i 90 -d <system_database> -u SYSTEM -p <password>
```

You can log into your tenant database with the following command:

```
hdbsql -i 90 -d <tenant_database> -u SYSTEM -p <password>
```

__JDBC__

---

To log into your system database via JDBC, use the following command:

```
jdbc:sap://<ip_address>:39017/databaseName=<database_name>
```

To log into your tenant database via JDBC, use the following command:

```
jdbc:sap://<ip_address>:39041/databaseName=<tenant_name>
```

[ACCORDION-END]

[ACCORDION-BEGIN [Optional: ](Create Additional SAP HANA, express edition Containers)]

You will need to repeat the previous steps of creating a directory and `JSON` password for each additional SAP HANA, express edition container you wish to create.  

```
sudo docker run -p 10013:39013 -p 10017:39017 -p 10041-10045:39041-39045 -p 10028-10029:1128-1129 -p 19013-19014:59013-59014 -v /data/<additional_directory_name>:/hana/mounts \
--ulimit nofile=1048576:1048576 \
--sysctl kernel.shmmax=1073741824 \
--sysctl net.ipv4.ip_local_port_range='40000 60999' \
--sysctl kernel.shmmni=524288 \
--sysctl kernel.shmall=8388608 \
--name <additional_container_name> \
store/saplabs/hanaexpress:2.00.022.00.20171211.1 \
--passwords-url <file://<path_to_json_file> OR http/https://<url_to_json_file>>
--agree-to-sap-license
```

This process will take several minutes. The prompt will read `Startup finished` once the container has been successfully running. This container starts in detached mode so you will need to open another command prompt to continue.  

[ACCORDION-END]

[ACCORDION-BEGIN [Optional: ](Update Your Docker Image)]

Update your SAP HANA, express edition Docker image when new versions are released.

Stop your old SAP HANA, express edition Docker image:

```
sudo docker stop <old_container_name>
```

Remove the old Docker image:

```
sudo docker rm <old_container_name>
```

Pull the new Docker image:

```
sudo docker pull store/saplabs/hanaexpress:2.00.022.00.20171211.1
```

Run the new Docker image using the old mounts:

```
sudo docker run -p 39013:39013 -p 39017:39017 -p 39041-39045:39041-39045 -p 1128-1129:1128-1129 -p 59013-59014:59013-59014 -v /data/<old_directory_name>:/hana/mounts \
--ulimit nofile=1048576:1048576 \
--sysctl kernel.shmmax=1073741824 \
--sysctl net.ipv4.ip_local_port_range='40000 60999' \
--sysctl kernel.shmmni=524288 \
--sysctl kernel.shmall=8388608 \
--name <new_container_name> \
store/saplabs/hanaexpress:2.00.022.00.20171211.1 \
--agree-to-sap-license
```

[ACCORDION-END]

[ACCORDION-BEGIN [Docker Run Usage: ](-Help Command)]

The following is a list of options available for the `sudo docker run saplabs/hanaexpress` command.

```
sudo docker run store/saplabs/hanaexpress:2.00.022.00.20171211.1 -h
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
- [How to Install SAP HANA 2.0, express edition Clients](https://www.sap.com/developer/tutorials/hxe-ua-howto-installing-clients.html)
- [Download and Install the HANA Eclipse plugin](https://www.sap.com/developer/tutorials/hxe-howto-eclipse.html)
