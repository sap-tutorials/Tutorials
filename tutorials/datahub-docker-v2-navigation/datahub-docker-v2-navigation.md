---
title: Navigate around SAP Data Hub, developer edition
description: Find your way around SAP Data Hub, developer edition.
auto_validation: true
primary_tag: products>sap-data-hub
tags: [  tutorial>beginner, topic>big-data, products>sap-data-hub, products>sap-vora ]
---

## Prerequisites  

## Details
### You will learn  
During this tutorial, you will learn how to find your way around SAP Data Hub, developer edition. You will also learn how to troubleshoot problems.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Access UIs via a web browser)]
To access the different user interfaces running inside the Docker container use the following URLs (as already explained during the previous tutorial):

* `http://localhost:8090` (SAP Data Hub Modeler)
* `http://localhost:9225` (SAP Vora Tools)
* `http://localhost:50070` (Apache Hadoop User Interface)

Where necessary enter **Username** and **Password** which you have set while running the Docker image (`VORA_USERNAME` and `VORA_PASSWORD` environment variables)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Log into the running container)]
You are able to log into the container running SAP Data Hub, developer edition by opening a terminal window and entering the following

```sh
docker exec -it datahub /bin/bash
```

Afterwards you are logged into the container as `root` user. To leave the container, you simply use the `exit` command.

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Copy files to and from the container)]
If you like to copy files from your local computer to the Docker container (or vice versa), you can use the `docker cp` command. Let's illustrate this with an example.

Create a text file `test.txt` on your local computer. You can use any text editor for this. Then open a terminal window and navigate to the directory where you have saved `test.txt`.  Enter the following command to copy `test.txt` from your local computer to the `/tmp` directory inside the container.

```sh
docker cp test.txt hdfs:/tmp/test.txt
```

Now delete `test.txt` from your local computer. Afterwards copy the file from the `/tmp` directory inside the container back to your local computer by entering the following command.

```sh
docker cp hdfs:/tmp/test.txt test.txt
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Copy files to and from HDFS)]
If you like to copy files from your local computer to the Hadoop Distributed File System (HDFS) running inside the Docker container, you first copy them to a directory inside the container as described in the previous step. Then you can use the `hdfs` command to copy them to HDFS (and vice versa). Let's illustrate this with an example. The example assume that a file `test.txt` is stored in the `/tmp` directory inside the container.

To find out the container for **HDFS**, run the the following command:

```sh
docker ps
```
This will provide you a list of all the running containers. From the list, copy the **`Container ID`** for the container with the name **`hdfs`**. For this example, we are assuming that our **`Container ID`** is `8560ee632f69`

Now log into the running container.

```sh
docker exec -it 8560ee632f69 /bin/bash
```
>Replace `8560ee632f69` with your **`Container ID`** while executing navigate to the `/tmp` directory. Then copy `test.txt` to HDFS.

```sh
hdfs dfs -copyFromLocal test.txt /tmp/test.txt
```

Open `http://localhost:50070` and navigate to **Utilities -> Browse the file system** to check that the file `test.txt` exists in HDFS.

Now delete `test.txt` from the `/tmp` directly inside the container (and **not**  inside HDFS). Then copy `test.txt` from HDFS back to the container directory.

```sh
hdfs dfs -copyToLocal /tmp/test.txt test.txt
```

Finally delete `test.txt` from HDFS again (you can verify the deletion via `http://localhost:50070`).

```sh
hdfs dfs -rm /tmp/test.txt
```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Troubleshoot problems and show logs)]
To troubleshoot problems, take a look at the logs inside the Docker container. You find the logs related to SAP Data Hub (and SAP Vora) inside the directory `/var/log/vora`.


[VALIDATE_1]

[ACCORDION-END]

---
