---
title: Installing SAP HANA, express edition Server + Apps, with Docker
description: How to install SAP HANA, express edition with XSA on your preferred Docker setup.
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, products>sap-hana\,-express-edition ]
---

## Prerequisites  
 - **Proficiency:** Beginner


## Next Steps
- [Install the SAP HANA, express edition clients](https://www.sap.com/developer/groups/hxe-install-clients.html)
- [How to download and install the HANA Eclipse plugin](https://www.sap.com/developer/tutorials/hxe-howto-eclipse.html)

## Details
### You will learn  
How to install SAP HANA, express edition on your preferred Docker setup.

This tutorial will show you how to install an installation of SAP HANA, express edition with XSA on your Docker installation. This version of SAP HANA, express edition does not contain XSC.

If you wish to install SAP HANA, express edition on a different virtual machine, or you want a custom setup on your Linux machine, see the [Virtual Machine](https://www.sap.com/developer/tutorials/hxe-ua-installing-vm-image.html) or [Binary Method](https://www.sap.com/developer/tutorials/hxe-ua-installing-binary.html) installation guides.

Before you begin, ensure your proxy settings have been properly set up. See [**HTTP/HTTPS proxy**](https://docs.docker.com/engine/admin/systemd/#httphttps-proxy) in the Docker documentation.

### Time to Complete
**10 Min**

---

SAP HANA, express edition is a streamlined version of the SAP HANA platform which enables developers to jump-start application development in the cloud or personal computer to build and deploy modern applications that use up to 32GB memory. SAP HANA, express edition includes the in-memory data engine with advanced analytical data processing engines for business, text, spatial, and graph data - supporting multiple data models on a single copy of the data.

The software license allows for both non-production and production use cases, enabling you to quickly prototype, demo, and deploy next-generation applications using SAP HANA, express edition without incurring any license fees. Memory capacity increases beyond 32GB are available for purchase at the [SAP Store](https://www.sapstore.com/solutions/99055/SAP-HANA%2C-express-edition).

SAP HANA, express edition for Docker has been tested on the following Linux operating system versions:

| Linux OS | OS Version | Docker Editions
| --- | --- | --- |
| `Ubuntu`  | `17.04 (Zesty Zapus)` | [Community](https://store.docker.com/editions/community/docker-ce-server-ubuntu),  [Enterprise](https://store.docker.com/editions/enterprise/docker-ee-server-ubuntu) |
| `openSUSE` | `openSUSE Leap` | [Enterprise](https://store.docker.com/editions/enterprise/docker-ee-server-sles) |
| `CentOS` | `7 (Core)` | [Community](https://store.docker.com/editions/community/docker-ce-server-centos),  [Enterprise](https://store.docker.com/editions/enterprise/docker-ee-server-centos) |
| `Debian` | `9 (Stretch)` | [Community](https://store.docker.com/editions/community/docker-ce-server-debian) |
| `Fedora` | `28 (Server Edition)` | [Community](https://store.docker.com/editions/community/docker-ce-server-fedora) |

**This installation does not support Docker for Windows or Docker for Mac.**

> **Note:**
> These instructions use `openSUSE` and Docker Enterprise Edition as an example. However, any of the supported operating systems listed above are compatible. Use the operating system and Docker installation that fits your needs best.

[ACCORDION-BEGIN [Step 1: ](Install Docker)]

Download and install the appropriate Docker Edition for your system. Visit the [Docker Community Edition](https://store.docker.com/search?offering=community&type=edition) or [Docker Enterprise Edition](https://store.docker.com/search?offering=enterprise&type=edition) lists for more information and to download Docker for your machine.

> **Note:**
> Ensure your proxy settings have been properly set up. See [**HTTP/HTTPS proxy**](https://docs.docker.com/engine/admin/systemd/#httphttps-proxy) in the Docker documentation.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Manage Storage System)]

If your host file system is `xfs`, you can recommend the storage driver through the `devicemapper` in Docker. Add `-s devicemapper` to the `DOCKER_OPTS` property in the `docker` file.

The minimum recommended storage size is 50G. For example:

```
DOCKER_OPTS="-s devicemapper --storage-opt dm.basesize=50G"
```

For `Ubuntu` and `Debian`, the `DOCKER_OPTS` property can be found at `/etc/default/docker`.

For `Fedora`, `SuSE`, and `Centos`, the `DOCKER_OPTS` property can be found at `/etc/sysconfig/docker`.

Restart the Docker service.

For example, on `SuSE`:

```
sudo systemctl restart docker.service
```

For more information on the storage driver, visit the [Docker storage drivers](https://docs.docker.com/storage/storagedriver/select-storage-driver/) documentation page.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Log Into Docker)]

To log into your Docker account, run:

```bash
sudo docker login
```

Follow the prompts and provide your Docker ID and password.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ]((Optional) Test Your Docker Installation)]

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

If `hello` is printed, you have successfully pulled the container image **alpine** (a demo Linux distribution), and ran the instance of the container `helloWorld`, and ran the command `echo` with an input parameter of `hello`.

If you **did not** get this output, the Docker installation has not been completed or the Docker daemon can not connect to the internet. Review the process and check the [Docker Documentation](https://docs.docker.com/get-started/) for more information in troubleshooting your Docker installation.

Remove the alpine image with the following command:

```bash
sudo docker image rm alpine -f
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Download the SAP HANA, express edition Image from the Docker Library)]

Go to the [Docker Store](https://store.docker.com/).

Click on the search bar and search for "SAP HANA, express edition".

Choose **SAP HANA, express edition (database and application services)**.

![Docker Store](choose_dockerxsa.png)

Click on the **Setup Instructions** button.

Copy the Docker pull address. Here is an example:

```bash
sudo docker pull store/saplabs/hanaexpressxsa:2.00.033.00.20180925.2
```

Open your Docker-enabled command line and use the Docker pull address to download the image.

This loads the SAP HANA, express edition image. To ensure that the image was loaded successfully, run:

```bash
sudo docker images
```

The SAP HANA, express edition image will be listed as `hanaexpressxsa`.

> **Note:**
> You may have to log into your Docker account to pull the image. From your Docker-enabled command line, run `docker login` and follow the prompts to enter your Docker ID and password. Once you have logged in, try the pull command again.

[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Edit the host sysctl.conf file)]

Before you can start the container, ensure that the following parameters are set in your host's `/etc/sysctl.conf` file. The host can be a virtual machine, physical machine, or a cloud instance.

```
fs.file-max=20000000
fs.aio-max-nr=262144
vm.memory_failure_early_kill=1
vm.max_map_count=135217728
net.ipv4.ip_local_port_range=60000 65535
```

To edit the `sysctl.conf` file, use the `vi` command to open the file and press `i` to switch to interactive mode. Edit the file as necessary, hit the `esc` key, and type and enter `:wq!` to write and save the changes.

Load the changes by running:

```bash
sudo /sbin/sysctl -p
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Edit the /etc/hosts file)]

The `hxehost` IP address is private to the installation. In order for applications to access `hxehost`, add the `hxehost` IP address to your machine's hostname map. The hostname map is your machine's **`/etc/hosts`** file. You must edit **`/etc/hosts`** if you want to access any XS Advanced applications or use HANA Cockpit from your machine.

Use the following command:

```bash
sudo sh -c 'echo <hxehost_IP_address>    hxehost >> /etc/hosts'
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Set Up Password for SAP HANA, express edition)]

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
  "master_password" : "SAPhxe123"
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

[ACCORDION-BEGIN [Step 9: ](Start SAP HANA, express edition Container)]

Use the SAP HANA, express edition image to create a container.

```bash
sudo docker run -p 39013:39013 -p 39015:39015 -p 39041-39045:39041-39045 -p 1128-1129:1128-1129 -p 59013-59014:59013-59014  -p 39030-39033:39030-39033 -p 51000-51060:51000-51060  -p 53075:53075  \
-h hxehost \
-v /data/<directory_name>:/hana/mounts \
--ulimit nofile=1048576:1048576 \
--sysctl kernel.shmmax=1073741824 \
--sysctl net.ipv4.ip_local_port_range='60000 65535' \
--sysctl kernel.shmmni=524288 \
--sysctl kernel.shmall=8388608 \
--name <container_name> \
store/saplabs/hanaexpressxsa:2.00.033.00.20180925.2 \
--agree-to-sap-license \
--passwords-url <file://<path_to_json_file> OR http/https://<url_to_json_file>> \
--proxy-host <proxy_hostname> \
--proxy-port <proxy_port> \
--no-proxy <no_proxy>
```

Example:

```
sudo docker run -p 39013:39013 -p 39015:39015 -p 39041-39045:39041-39045 -p 1128-1129:1128-1129 -p 59013-59014:59013-59014  -p 39030-39033:39030-39033 -p 51000-51060:51000-51060  -p 53075:53075  \
-h hxehost \
-v /data/express_edition:/hana/mounts \
--ulimit nofile=1048576:1048576 \
--sysctl kernel.shmmax=1073741824 \
--sysctl net.ipv4.ip_local_port_range='60000 65535' \
--sysctl kernel.shmmni=524288 \
--sysctl kernel.shmall=8388608 \
--name express_edition \
store/saplabs/hanaexpressxsa:2.00.033.00.20180925.2 \
--agree-to-sap-license \
--passwords-url file:///hana/password.json \
--proxy-host <proxy_hostname> \
--proxy-port <proxy_port> \
--no-proxy <no_proxy>
```

This example creates the SAP HANA, express edition container with the name `express_edition`. This process will take several minutes. The prompt will read `Startup finished` once the container has been successfully running. This container starts in detached mode so you will need to open another command prompt to continue.  

> **Note:**
> If you placed the password file in `/data/<directory_name>/<file_name>.json`, substitute  `file://<path_to_json_file>` with `file:///hana/mounts/<file_name>.json`.

> **Note:**
> Check if the password file `/hana/mounts/<file_name>.json` was deleted after the SAP HANA, express edition container starts.  If not, you can manually delete it. If the `JSON` file you are using is an *http* or *https* URL, you can leave out the `-v` option.

> **Note:**
> Ignore `--proxy-host, --proxy-port, --no-proxy` if you do not have a proxy server.

> **Note:**
> For Linux kernel versions earlier than 4, omit the `net.ipv4.ip_local_port_range` option.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Log into SAP HANA, express edition Container)]

To start your SAP HANA, express edition container, run the following command:

```bash
sudo docker exec -it <container_name> bash
```

Example:

```
sudo docker exec -it express_edition bash
```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ]((Optional) Test the Container)]

When you are logged into the SAP HANA, express edition container, you can test your installation by entering the following:

```bash
whoami
```

You should be logged in as `hxeadm`, the default SAP HANA, express edition user.

You can also enter the following:

```bash
HDB info
```

And you should see the following services running:

* `hdbnameserver`
* `hdbcompileserver`
* `hdbdiserver`
* `hdbwebdispatcher`

[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ]((Optional) Log Into System or Tenant Database)]

You can log into the system database with the following command:

```bash
hdbsql -i 90 -d <system_database> -u SYSTEM -p <password>
```

You can log into your tenant database with the following command:

```bash
hdbsql -i 90 -d <tenant_database> -u SYSTEM -p <password>
```

__JDBC__

---

To log into your system database via JDBC, use the following command:

```bash
jdbc:sap://<ip_address>:39013/databaseName=<database_name>
```

To log into your tenant database via JDBC, use the following command:

```bash
jdbc:sap://<ip_address>:39015/databaseName=<tenant_name>
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ]((Optional) Test SAP Web IDE)]

After you have logged in, view the list of XSA applications. Enter:

```bash
xs apps
```

> **Note:**
> When you run the `xs apps` command for the first time, it may take 1-2 minutes for the system to return the list of XSA applications.

Check that the application `webide` shows `STARTED` in the list of XSA applications, and has 1/1 instances. (If the list shows 0/1 in the instance column, the application is not started.)

> **Note:**
> Normally it only takes a few minutes for XSA services to start. However. depending on your machine, it can take over 30 minutes for XSA services to begin. If the service doesn't show `STARTED` and doesn't show `1/1` instances, keep waiting until the service is enabled.

Make a note of the URL for `webide`.

![loio5edd67a000a745cdb47d3a00973d0632_HiRes](loio5edd67a000a745cdb47d3a00973d0632_HiRes.png)

> **Note:**
> The command `xs apps | grep webide` returns the `webide` row only.

Test your Web IDE connection. Enter the URL for Web IDE in a web browser.

```
Example: https://hxehost:53075
```

Log on to Web IDE using the `XSA_DEV` user and the password you made during installation.

If you are prompted to change your password, follow the instructions.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ]((Optional) Update Your Docker Image)]

To update your Docker image, refer to the [SAP HANA, express edition Getting Started Guide](https://help.sap.com/viewer/product/SAP_HANA,_EXPRESS_EDITION) updating section.

[ACCORDION-END]

[ACCORDION-BEGIN [Docker Run Usage: ](-Help Command)]

The following is a list of options available for the `sudo docker run store/saplabs/hanaexpressxsa` command.

```
docker run store/saplabs/hanaexpressxsa:2.00.033.00.20180925.2 -h
usage: [options]

--dont-check-consistency           Skip consistency check between mount points
--dont-check-mount-points          Skip check for allowed mount points
--dont-check-version               Skip compatibility check of current and last HANA version
--dont-check-system                Skip check for incompatible /proc/sys values
--dont-exit-on-error               Halt script on error to allow root cause analysis
                                   (MUST NOT be used in production)
--license-url <url>                URL for a license file (json)
                                   Format: {"landscape-id":"<8-4-4-4-12 GUID>", "license":"<license>"}
--passwords-url <url>              URL for a password file (json). Format:
                                       {"master_password":"<pwd>"}
                                   or
                                       {"system_user_password":"<pwd>","default_tenant_system_user_password":"<pwd>"}
--proxy-host                       Proxy host name
--proxy-host                       Proxy port number
--no-proxy                         Comma separated list of hosts/domains that do not need proxy

--agree-to-sap-license             Indicates you agree to the SAP Developer Center Software Developer License Agreement.
```

[ACCORDION-END]

## Next Steps
- [Install the SAP HANA, express edition clients](https://www.sap.com/developer/groups/hxe-install-clients.html)
- [Download and Install the HANA Eclipse plugin](https://www.sap.com/developer/tutorials/hxe-howto-eclipse.html)
