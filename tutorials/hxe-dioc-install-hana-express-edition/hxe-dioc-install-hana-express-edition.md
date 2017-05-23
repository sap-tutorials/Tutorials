---
title: HANA Express Edition - Execute the Installation script and finish setup
description: This tutorial will guide you through the execution of the installation scripts to set your HANA Express Edition platform up.
primary_tag: products>sap-hana\,-express-edition  
tags: [  tutorial>beginner, topic>cloud, products>sap-hana\,-express-edition   ]
---

## Prerequisites  
 - [HANA Express Edition - Extract installation Files and Configure Firewall Rules](http://www.sap.com/developer/tutorials/hxe-dioc-extract-files-configure-firewall.html)


## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)

## Disclaimer
SAP HANA, express edition (HXE) is officially supported on SLES and RHEL. SAP Community members have been successful in running HXE on other Linux operating systems that are not formally supported by SAP, such as Ubuntu, openSUSE and Fedora. SAP is not committing to resolving any issues that may arise from running HXE on these platforms.

## Details
### You will learn  
This tutorial will guide you through the final process to have your HANA Express Edition instance up and running on the Digital Ocean platform.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Running the install)]
You have finally got to the most important and exciting part! Go into your home directory or wherever you have extracted the installation files into.

>IMPORTANT: As this process should not be interrupted, use a separate SSH client that handles connection intermittence, instead of the web client.

Log into the SSH client. Become `root` and go to the directory where you extracted the installation packages. You will now execute the installation script:

```
sudo su –
cd /tmp
./setup_hxe.sh
```

The script will prompt for different parameters:

- Installer root directory

This is the directory where the files have been extracted, plus `HANA_EXPRESS_20`, for example `/tmp/HANA_EXPRESS_20`.

Paste this value and press **Enter**.

- Components you want to install
Choose `all` to include XS Advanced tools (provided you have uploaded them) or `server` for the server-only installation:

- In this example, `ubuntu-hxe-server`:

- System ID:
The system ID you want to use. As you will probably use the tutorials on the many things you can do in HANA Express Edition, you may want to use `HXE` as proposed:
![Hostname](5.png)

- Enter Instance Number:
 The firewall rules have been created for instance `00`, so please update them accordingly if you choose a different instance number. Also, bear this in mind as the usual instance number used in tutorials is also `00`.

 - Master Password:
 This will be the password for administrator users such as SYSTEM or <SID>ADM.  You will definitely need to remember this password.

 ![Master Password](6.png)

 > If you are installing the Applications package, this password is also used for the `XSA_ADMIN`, `XSA_DEV`, and `XSA_SHINE` users.

 Hit **Enter**. 

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Let the installation begin)]

Review the Installation summary and hit *Y*. 

After you hit **Enter** the script will verbosely inform what it is doing. **Avoid disconnecting the terminal or interrupting the execution.**

Wait until the command prompt in the console is available for input again.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Check the installation)]

Once the installation finishes, you can scroll up and see if there are any visible errors in the console log.

![HXE last piece of installation](8.png)

You can also check the log by running command `ls -ltr` in the directory
`/var/tmp` and open the latest installation log file using commands such as `more <name of the file>` or `cat <name of the file>`. 

Finally, the command-line Lifecycle Management program can check the installation for you: `/hana/shared/HXE/hdblcm/hdblcm`.

>Hint: You can start the database again with command `./HDB start` and check if the services are running with `./HDB info`

> Remember: SAP HANA Express Edition license is free up to 32 GB of RAM.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Set the host name)]

Remember to set you hosts file in your local computer with the pair <<external IP>>  <<host name>>. You will generally fins these files in folders `/etc/hosts` or in `C:\Windows\System32\drivers\etc`, and the configuration would look like the following example:

```
207.154.xxx.xxx	ubuntu-hxe-server
```

Where the  `207.154.XXX.XXX` stands for the external IP and the `ubuntu-hxe-server` value stands for the name of the host that you have set up in the server.


[DONE]
[ACCORDION-END]


---

## Next Steps
- Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
