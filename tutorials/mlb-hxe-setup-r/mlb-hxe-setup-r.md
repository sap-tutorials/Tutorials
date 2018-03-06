---
title: Configure the SAP HANA R integration with SAP HANA, express edition
description: Provide details on the installation and configuration of the SAP HANA R integration with SAP HANA, express edition.
primary_tag: products>sap-hana\, express-edition
tags: [ tutorial>how-to, tutorial>intermediate, products>sap-hana\, express-edition ]
---

## Prerequisites  
- Proficiency: Intermediate

### You will learn

The pre-built version of R are not compiled with dynamic/shared libraries enable which is required for the SAP HANA integration.

Therefore, you must compile the R package from its source code with the dynamic/shared libraries (`--enable-R-shlib`).

At the end, you will also test the configuration by uploading one of the R built-in dataset (Iris).

Some elements of configuration, such as authentication or SSL, will not be covered in this tutorial.

For further details, you can consult the [SAP HANA R Integration Guide](https://help.sap.com/viewer/a78d7f701c3341339fafe4031b64f015/2.0.02/en-US/dbad714484d242789688a551fbdf5573.html).

It also includes a section dedicated to debugging and tracing.

## Details

### Time to Complete
**10 Min**.


[ACCORDION-BEGIN [Info: ](SAP HANA R integration)]

To process R code in the context of the SAP HANA database, the R code is embedded in SAP HANA SQL code in the form of a RLANG procedure.

The SAP HANA database uses an external R environment to execute this R code, similarly to native database operations like joins or aggregations.

This allows the application developer to elegantly embed R function definitions and calls within `SQLScript` and submit the entire code as part of a query to the database.

![SAP HANA R integration](00-0.png)

The figure above shows three main components of the integrated solution:

 - the SAP HANA based application
 - the SAP HANA database
 - the R environment.

When the calculation model plan execution reaches an R-operator, the calculation engine's R-client issues a request through the `Rserve` mechanism to create a dedicated R process on the R host.

Then, the R-Client efficiently transfers the R function code and its input tables to this R process, and triggers R execution.

Once the R process completes the function execution, the resulting R data frame is returned to the calculation engine, which converts it.

Since the internal column-oriented data structure used within the SAP HANA database for intermediate results is very similar to the vector-oriented R data frame, this conversion is very efficient.

A key benefit of having the overall control flow situated on the database side is that the database execution plans are inherently parallel and, therefore, multiple R processes can be triggered to run in parallel without having to worry about parallel execution within a single R process.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Verify Your Java Installation)]

One the requirement to install and enable the SAP HANA R integration with SAP HANA, express edition is a 64-bit Java Runtime Environment (JRE) 8 or Higher.

To check if Java is installed, you can run the following command from your terminal console:

```shell
java -version
```

which should return:

```
java version "1.8.0_xx"
Java(TM) SE Runtime Environment (build 1.8.0_xx-yyy)
```

If you don't have it yet installed, you can check the following link for download link and installation instructions : <a href="https://tools.hana.ondemand.com/#cloud" target="new">https://tools.hana.ondemand.com/#cloud</a>

Using the RPM option is most likely the easiest, as you will have to simply run the following command from your terminal console (where **<version>** needs to be adjusted based on the downloaded version):

```
sudo rpm -ivh <rpm directory>/sapjvm-<version>-linux-x64.rpm
```

Then you will need to update the "alternatives" and enable your flavor of java using the following commands:

```bash
sudo update-alternatives --install "/usr/bin/java" "java" "/usr/java/sapjvm_8_latest/bin/java" 1
sudo update-alternatives --set java /usr/java/sapjvm_8_latest/bin/java
```
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Install package dependencies)]

In order to successfully complete the setup, you will need to add the following packages:

|-----------------------|-------------------|-------------------|
|`xorg-x11-devel` 		|`gcc-fortran` 		|`texinfo` 			|
|`readline-devel` 		|`gcc-c++` 			|`cairo-devel` 		|
|`libcurl-devel` 		|`xz-devel` 		|`pcre-devel` 		|

From your terminal console, execute the following command:

- For SUSE Linux Enterprise Server (including the SAP HANA, express edition VM):
```shell
sudo zypper install xorg-x11-devel \
	readline-devel \
	libcurl-devel \
	gcc-fortran \
	gcc-c++ \
	xz-devel \
	pcre-devel \
	texinfo \
	cairo-devel
```
To install these dependencies, you will need to either have your system properly registered or download the SUSE Linux Enterprise Software Development Kit from [SUSE Downloads](https://download.suse.com), and add it as a new repository:
```shell
sudo zypper addrepo iso:/?iso=/<path to SDK ISO>/SLE-12-SP2-SDK-DVD-x86_64-GM-DVD1.iso SLE-12-SP2-SDK-DVD-x86_64-GM-DVD1
```

- For Red Hat Enterprise Linux:
```shell
sudo yum -y install readline-devel \
	libcurl-devel \
	gcc-gfortran \
	gcc-c++ \
	xz-devel \
	pcre-devel \
	texinfo \
	texlive \
	cairo-devel \
	libX* \
	bzip2-devel
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Install TexInfo)]

[`Texinfo`](https://www.gnu.org/software/texinfo/) is the official documentation format of the GNU project and is used by multiple project including R to build the manuals.

However, the `texinfo` package available in most repository does not provide all the tools required to compile R from the ground.

For mode details about `texinfo`, you can visit: [`https://www.gnu.org/software/texinfo/`](https://www.gnu.org/software/texinfo/).

The `texinfo` required to compile is 5.1, but in this example we will be using a newer version.

In the below script, `curl` is used to download the package, but if your machine is not connected to the Internet, you can download manually the `texinfo` package from [http://ftp.gnu.org/gnu/texinfo/](http://ftp.gnu.org/gnu/texinfo/) and transfer it.

From your terminal console, execute the following command:

```shell
cd ~
curl http://ftp.gnu.org/gnu/texinfo/texinfo-6.5.tar.gz -o texinfo-6.5.tar.gz
tar -xf texinfo-6.5.tar.gz

cd texinfo-6.5

./configure --prefix=/usr --disable-static

make clean
make
make info

sudo make install
sudo chmod -R 755 /usr/lib/texinfo
sudo chmod -R 755 /usr/share/texinfo

```

To verify that your setups is correct you can run the following command:

```shell
texi2any --help
```

No error message should be displayed.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Download, Compile and Install R)]

As explained previously, we need to recompile R with `shlib` enabled in order to use it with SAP HANA, express edition.

In this example we will be using a newer version than the one listed in the PAM.

In the below script, `curl` is used to download the package, but if your machine is not connected to the Internet, you can download manually the `R` package from [https://cran.r-project.org/](https://cran.r-project.org/) and transfer it.


```shell
cd ~
curl https://cloud.r-project.org/src/base/R-3/R-3.4.3.tar.gz -o R-3.4.3.tar.gz
tar -xf R-3.4.3.tar.gz

cd R-3.4.3

./configure --prefix=/usr --enable-R-shlib

make clean
make
make info

sudo make install
sudo chmod -R 755 /usr/lib64/R
```

To verify that your setups is correct you can run the following command:

```shell
echo "R.version.string" | R --save -q
```

The output should look like the following:

```
> R.version.string
[1] "R version 3.4.3 (2017-11-30)"
>
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Download, Compile and Install Rserve)]

`Rserve` acts as a socket server (TCP/IP or local sockets) which allows binary requests to be sent to R.

Every connection has a separate workspace and working directory.

Client-side implementations are available for popular languages such as C/C++ and Java, allowing any application to use facilities of R without the need of linking to R code.

`Rserve` supports remote connection, user authentication and file transfer.

If your host is connected to the Internet, you can leverage the CRAN mirror to install `Rserve` else you can download it manually and transfer it.

To install the `Rserver` package and make available to every user you should start R as a supper user running the following command:

```shell
cd ~
curl https://cloud.r-project.org/src/contrib/Rserve_1.7-3.tar.gz -o Rserve_1.7-3.tar.gz

sudo R
```
Then you can use the following command if your server is connected to the internet:

```shell
install.packages("Rserve")
```

You will be prompted to select one of the CRAN mirror from which the package will be downloaded.

If your server is not connected to the internet you can use instead:

```shell
install.packages("/<path to Rserve archive>/Rserve_1.7-3.tar.gz", repos = NULL)
```
You can find the archive on the cloud mirror: [https://cloud.r-project.org/src/contrib](https://cloud.r-project.org/src/contrib)

You can pick version 1.7-3.

Type `q()` to quit your R session as super user.

To verify that the `Rserve` package is properly installed, open a new R session and execute the following command:

```shell
library("Rserve")
```

You should not receive any message after executing the command.

Now, as we installed the `Rserver` as super user, we need to add proper rights to any users executing the following command:

```shell
sudo chmod 755 /usr/lib64/R/bin/Rserve
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Start Rserve)]

You can start `Rserve` using the following command:

```
R CMD Rserve --RS-port <PORT> --no-save --RS-encoding utf8
```

The port for starting `Rserve` has to be chosen wisely as it will be configured in SAP HANA over the next step.

You can use 9999 as this port is not used often:

```shell
R CMD Rserve --RS-port <PORT> --no-save --RS-encoding utf8
```

The `--no-save` option makes sure that the invoked R runtime do not store the R environment onto the file system after the R execution has been stopped.

This is important to avoid the file system to be filled over time due to multiple R runs.

There is currently no support for automatically starting the `Rserve` server after rebooting the Linux host.

To accomplish this, you can use `crontab` with a shell script like the following, which starts a new `Rserve` process if none is running:

```
pgrep -u <OS user> -f "Rserve --RS-port <PORT> --no-save --RS-encoding utf8" || R CMD Rserve --RS-port <PORT> --no-save --RS-encoding utf8
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Configure SAP HANA)]

To enable the calling of R procedures from SAP HANA, the index server configuration parameters from the `calcEngine` section must be configured.

Connect to the **HXE** tenant using the **SYSTEM** user credentials and execute the following SQL statement:

```sql
ALTER SYSTEM ALTER CONFIGURATION ('indexserver.ini', 'SYSTEM') SET ('calcEngine', 'cer_rserve_addresses'  ) = 'localhost:9999' 	WITH RECONFIGURE;
ALTER SYSTEM ALTER CONFIGURATION ('indexserver.ini', 'SYSTEM') SET ('calcEngine', 'cer_timeout'           ) = '300' 			WITH RECONFIGURE;
ALTER SYSTEM ALTER CONFIGURATION ('indexserver.ini', 'SYSTEM') SET ('calcEngine', 'cer_rserve_maxsendsize') = '0' 				WITH RECONFIGURE;
```

You will notice that the port number must correspond to the one used to start `Rserve`.

Now, you need to create the `Rsever` source by executing the following SQL statement:

```sql
CREATE REMOTE SOURCE "RSERVE"
    ADAPTER "rserve"
    CONFIGURATION 'server=localhost;port=9999';
```

Now, you need to grant the `ML_USER` by executing the following SQL statement:

```sql
GRANT CREATE R SCRIPT TO ML_USER;
ALTER USER ML_USER SET PARAMETER RSERVE REMOTE SOURCES = 'RSERVE';
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Test the configuration)]

In order to test the configuration, you will execute a simple procedure that will read the Iris dataset and store it into a table.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement:

```sql
-- Uncomment the drop statement is you want to run it from scratch
-- DROP TABLE 		IRIS;
-- DROP PROCEDURE 	LOAD_IRIS;
-- DROP PROCEDURE 	DISPLAY_IRIS;

CREATE COLUMN TABLE IRIS (
	"Sepal.Length" DOUBLE,
	"Sepal.Width" DOUBLE,
	"Petal.Length" DOUBLE,
	"Petal.Width" DOUBLE,
	"Species" VARCHAR(5000)
);

CREATE PROCEDURE LOAD_IRIS(OUT iris "IRIS")
LANGUAGE RLANG AS
BEGIN
  library(datasets)
  data(iris)
  iris <- cbind(iris)
END;

CREATE PROCEDURE DISPLAY_IRIS()
AS BEGIN
	CALL LOAD_IRIS(iris);
	INSERT INTO IRIS SELECT * FROM  :iris;
END;

CALL DISPLAY_IRIS();
SELECT * FROM IRIS;
```

The Iris dataset will display the measurements in centimeters of the sepal length and width and petal length and width  for about 50 flowers from each of 3 species of iris. Therefore, the result should display 150 rows.

[ACCORDION-END]
