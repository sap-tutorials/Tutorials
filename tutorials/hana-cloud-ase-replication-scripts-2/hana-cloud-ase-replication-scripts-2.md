---
title: Adjust Example Scripts to Your SAP HANA Cloud Landscape
description: Adjust the example scripts to set up an SAP HANA Cloud, SAP Adaptive Server Enterprise replication, as well as the connections from it to the SAP ASE databases in SAP HANA Cloud.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-hana-cloud, products>sap-adaptive-server-enterprise, software-product-function>sap-hana-cloud\,-sap-adaptive-server-enterprise-replication]
primary_tag: products>sap-hana-cloud
---

## Prerequisites
- You have completed the [tutorial](hana-cloud-ase-replication-scripts-1) on how to set up your SAP HANA Cloud environment for replication.

## Details
### You will learn
- How to automate the creation of an interfaces file using a script
- How to create connections to SAP HANA Cloud, SAP ASE replication
- How to set up a replication agent
- How to create a database replication definition and subscription


In this tutorial, you will learn how to adapt an example shell script to include the right information about your specific landscape to build a replication system and replicate data between two SAP HANA Cloud, SAP ASE instances. If you are a Windows user, you will need to use the proper comment statements and reference environment variables using `%VAR%` instead of `$VAR`.

Let's get started.

> ### Disclaimer
>
> Within this group of tutorials, the phrase **"replication server"** is used to refer to **"SAP HANA Cloud, SAP Adaptive Server Enterprise replication"** unless mentioned otherwise.


---

[ACCORDION-BEGIN [Step 1: ](Enter credentials of database instances and replication server)]

Use the example script below as a starting point. In your version of the script, you need to include the credentials from your SAP ASE databases and the replication server in SAP HANA Cloud. You must also include the name, `aseadmin` or `repadmin` passwords for each of the instance.

In this example, you can see the variables used to refer to the SAP ASE database instance names, passwords, and host addresses.


|  Variables      | Placeholder for     | Example
|  :------------- | :-------------  | :----------
|  `ASE_1`        | the name of the source SAP ASE server as listed in your interfaces file.| `#_ASE_1=DEMO_ASE_1`
|  `ASE_2`        | the name of the target SAP ASE server as listed in your interfaces file.| `#_ASE_2=DEMO_ASE_2`
|  `ASE_R`        | the name of the replication server as listed in your interfaces file.| `#_ASE_R=DEMO_ASE_R`


If you use the same password for each system, you just need to set it once here. For example: `PASSWORD=MyWeakPassword`

```Shell/Bash
# ------------------------------------------------------------------------------
# Set variables
# ------------------------------------------------------------------------------

ASE_1=
ASE_2=
ASE_R=
ASE_1_PASSWORD=$PASSWORD
ASE_2_PASSWORD=$PASSWORD
ASE_R_PASSWORD=$PASSWORD
# Take the host addresses from the SQL endpoints for the ASE_1 and ASE_2 server, dropping the port. For example, this may look like this:
# ASE_1_HOST=f376c6c8-cbec-427c-9278-2ab7c941f6c8.ase.hxtp.beta-us21.hanacloud.ondemand.com
ASE_1_HOST=
ASE_2_HOST=
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Automate creation of interfaces file using script)]

With the script below, you can automate the creation of an interfaces file that includes the names, host addresses, and passwords of your instances. Please be aware of the placeholders for your `repadmin` user password, as well as host information and instance name.

```Shell/Bash
# echo
# echo
# echo At $ASE_R, create interface entries for $ASE_1 and $ASE_2...
# echo ==========================================================================================
# echo
# echo
isql -U repadmin -P $ASE_R_PASSWORD -J utf8 -S $ASE_R -e -w1000 <<EOF
sysadmin interface, insert, $ASE_1, "$ASE_1_HOST", 443, 'ssl="CN=hanacloud.ondemand.com"'
go
EOF

isql -U repadmin -P $ASE_R_PASSWORD -J utf8 -S $ASE_R -e -w1000 <<EOF
sysadmin interface, insert, $ASE_2, "$ASE_2_HOST", 443, 'ssl="CN=hanacloud.ondemand.com"'
go
EOF

# isql -U repadmin -P $ASE_R_PASSWORD -J utf8 -S $ASE_R -e -w1000 <<EOF
# sysadmin interface, show
# go
# EOF
```


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Create connections to SAP HANA Cloud, SAP ASE database instances)]

Now it's time to adjust the script to create connections to each SAP ASE database in SAP HANA Cloud, from the replication server in SAP HANA Cloud. In the example code below, we used a database placeholder name called `DBNAME`.

```Shell/Bash
# echo
# echo
# echo At $ASE_R, create connections to $ASE_1 and $ASE_2...
# echo ==========================================================================================
# echo
# echo
isql -U repadmin -P $ASE_R_PASSWORD -J utf8 -S $ASE_R -e -w1000 <<EOF
create connection to $ASE_1.DBNAME
set error class to rs_sqlserver_error_class
set function string class to rs_sqlserver_function_class
set username to DBNAME_maint
set password to $ASE_R_PASSWORD
set stream_replication to 'true'
with log transfer on
use login aseadmin password '$ASE_1_PASSWORD'
set ra_user rauser
set password $ASE_R_PASSWORD
go
EOF
echo
echo
sleep 2

isql -U repadmin -P $ASE_R_PASSWORD -J utf8 -S $ASE_R -e -w1000 <<EOF
create connection to $ASE_2.DBNAME
set error class to rs_sqlserver_error_class
set function string class to rs_sqlserver_function_class
set username to DBNAME_maint
set password to $ASE_R_PASSWORD
set stream_replication to 'true'
with log transfer on
use login aseadmin password '$ASE_2_PASSWORD'
set ra_user rauser
set password $ASE_R_PASSWORD
go
EOF

# isql -U repadmin -P $ASE_R_PASSWORD -J utf8 -S $ASE_R -e -w1000 <<EOF
# admin show_connections
# go
# EOF
```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Set up replication agent)]

Now, you can set up the replication agent in the primary SAP ASE database in SAP HANA Cloud.

```Shell/Bash
# echo
# echo
# echo At $ASE_1, set up the replication agent...
# echo ==========================================================================================
# echo
# echo
isql -U aseadmin -P $ASE_R_PASSWORD -J utf8 -S $ASE_1 -e -w1000 <<EOF
use DBNAME
go
exec sp_reptostandby DBNAME, 'all'
go
exec sp_config_rep_agent DBNAME, 'send warm standby xacts', 'true'
go
exec sp_stop_rep_agent DBNAME
go
waitfor delay "00:00:03"
go
exec sp_start_rep_agent DBNAME
go
EOF
```



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create database replication definition and subscription)]

Finally, you can create a database replication definition and subscription.

```Shell/Bash
# echo
# echo
# echo At $ASE_R, create database replication definition and subscription...
# echo ==========================================================================================
# echo
# echo
isql -U repadmin -P $ASE_R_PASSWORD -J utf8 -S $ASE_R -e -w1000 <<EOF
create database replication definition DBNAMErep
with primary at $ASE_1.DBNAME
replicate DDL
go
create subscription DBNAMEsub
for database replication definition DBNAMErep
with primary at $ASE_1.DBNAME
with replicate at $ASE_2.DBNAME
without materialization
go
# check subscription DBNAMEsub
# for database replication definition DBNAMErep
# with primary at $ASE_1.DBNAME
# with replicate at $ASE_2.DBNAME
# go
EOF
```

Now that you have successfully adjusted the example script to include the information about your instances in SAP HANA Cloud, learn how to test the script in the next tutorial.




[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test yourself)]



[VALIDATE_7]
[ACCORDION-END]

---
