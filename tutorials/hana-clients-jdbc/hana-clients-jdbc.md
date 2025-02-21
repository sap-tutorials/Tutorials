---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product-function>sap-hana-cloud\,-sap-hana-database, software-product>sap-hana, software-product>sap-hana\,-express-edition, programming-tool>java]
primary_tag: software-product>sap-hana-cloud
---

# Connect Using the SAP HANA JDBC Driver
<!-- description --> Create and debug a Java application that connects to SAP HANA using the SAP HANA client.

## Prerequisites
 - You have completed the first 3 tutorials in this mission.

## You will learn
  - How to install Java
  - How to create and debug a Java application that queries an SAP HANA database
  - How to connect to SAP HANA in `DBeaver` using the SAP HANA JDBC driver

## Intro
[Java Database Connectivity](https://en.wikipedia.org/wiki/Java_Database_Connectivity) (JDBC) provides an [API](https://docs.oracle.com/javase/8/docs/technotes/guides/jdbc/) for accessing databases from Java.  An application written to the JDBC standard can be ported to other databases.  Database vendors provide JDBC drivers for their database products.

---

### Install a JDK
Ensure that you have a Java Development Kit (JDK) installed and ensure that it is accessible from your path.  Details on supported versions can be found at SAP Note [3165810 - SAP HANA Client Supported Platforms](https://launchpad.support.sap.com/#/notes/3165810).

An OpenJDK from SAP is available at [SapMachine](https://sap.github.io/SapMachine/#download). If you don't already have a JDK installed, please install it from SapMachine. 

To verify that the JDK is correctly set up, run the following:

```Shell
java -version
javac -version
```

If these commands fail, ensure that the folder they are located in, is included in your path.  

The following command will install Java on openSUSE Leap 15.5.

```Shell (Linux)
sudo zypper install java-11-openjdk-devel
```


### Examine the SAP HANA JDBC driver
The SAP HANA driver for JDBC is a [Multi-Release JAR file](https://openjdk.java.net/jeps/238) and as such supports multiple versions of Java.  It is available in the client installation folder at `C:\SAP\hdbclient\ngdbc.jar` and in the maven repository at [MVN Repository - ngdbc](https://mvnrepository.com/artifact/com.sap.cloud.db.jdbc/ngdbc).

![maven](maven.png)

1. Run the following command for version information.  If needed, adjust the path to match the installation location on your machine.

    ```Shell (Microsoft Windows)
    java -jar C:\SAP\hdbclient\ngdbc.jar -v
    ```  

    ```Shell (Linux or Mac)
    java -jar ~/sap/hdbclient/ngdbc.jar -v
    ```

2. Run the command again without the `-v` to open the configuration window, which provides driver information and the ability to set trace information:

    ![JDBC-Driver-Trace-Config](JDBC-Driver-Trace-Config.png)

    >The JDBC driver has a different version number than the rest of the SAP HANA client interfaces.

    The trace options are further described at [JDBC Tracing and Trace Options](https://help.sap.com/docs/SAP_HANA_CLIENT/f1b440ded6144a54ada97ff95dac7adf/4033f8e603504c0faf305ab77627af03.html).


3. Run the following and use either connection details stored in the user store or specify the connection details.

    ```Shell (Windows)
    java -jar C:\SAP\hdbclient\ngdbc.jar -k USER1UserKey -o encrypt=True -o validatecertificate=false -c "SELECT  * FROM HOTELS.CUSTOMER"
    ```
    
    Alternatively, you may run

    ```Shell (Windows)
    java -jar C:\SAP\hdbclient\ngdbc.jar -u USER1,Password1 -n your_host:your_port -o encrypt=True -o validatecertificate=false -c "SELECT  * FROM HOTELS.CUSTOMER"
    ```  


    ```Shell (Linux or Mac)
    java -jar ~/sap/hdbclient/ngdbc.jar -u USER1,Password1 -n your_host:your_port -o encrypt=True -o validatecertificate=false -c "SELECT  * FROM HOTELS.CUSTOMER"
    ```

    ![Link text e.g., Destination screen](java-driver-result.png)

See [JDBC Command-Line Connection Options](https://help.sap.com/docs/SAP_HANA_CLIENT/f1b440ded6144a54ada97ff95dac7adf/9ac4e1eedbbc4961bce0db6ad64b3612.html) for additional details on parameters of `ngdbc.jar`.


### Create a Java application that queries SAP HANA
1. The following commands create a folder named `java`, enter the newly created directory, create a file named `JavaQuery.java`, and open the file in the Notepad text editor.

    ```Shell (Microsoft Windows)
    mkdir %HOMEPATH%\HANAClientsTutorial\java
    cd %HOMEPATH%\HANAClientsTutorial\java
    notepad JavaQuery.java
    ```

    Substitute `pico` below for your preferred text editor.

    ```Shell (Linux or Mac)
    mkdir -p $HOME/HANAClientsTutorial/java
    cd $HOME/HANAClientsTutorial/java
    pico JavaQuery.java
    ```

2. Copy the following code into `JavaQuery.java`. Then save and exit the file.

    ```Java
    import java.sql.*;
    import com.sap.db.jdbc.Driver;
    public class JavaQuery {
        public static void main(String[] argv) {
            System.out.println("Java version: " + com.sap.db.jdbc.Driver.getJavaVersion());
            System.out.println("SAP driver details: " + com.sap.db.jdbc.Driver.getVersionInfo() + "\n");
            Connection connection = null;
            try {  
                connection = DriverManager.getConnection(  
                    //Option 1, retrieve the connection parameters from the hdbuserstore
                    //The below URL gets the host, port and credentials from the hdbuserstore.
                    "jdbc:sap://dummy_host:0/?KEY=USER1UserKey&encrypt=true&validateCertificate=false");

                    //Option2, specify the connection parameters
                    //"jdbc:sap://10.11.123.134:39015/?encrypt=true&validateCertificate=false", "User1", "Password1");

                    //As of SAP HANA Client 2.6, connections on port 443 enable encryption by default
                    //validateCertificate should be set to false when connecting
                    //to an SAP HANA, express edition instance that uses a self-signed certificate.

                    //As of SAP HANA Client 2.7, it is possible to direct trace info to stdout or stderr
                    //"jdbc:sap://10.11.123.134:39015/?encrypt=true&validateCertificate=false&traceFile=stdout&traceOptions=CONNECTIONS", "User1", "Password1");

            }
            catch (SQLException e) {
                System.err.println("Connection Failed:");
                System.err.println(e);
                return;
            }
            if (connection != null) {
                try {
                    System.out.println("Connection to HANA successful!");
                    Statement stmt = connection.createStatement();
                    ResultSet resultSet = stmt.executeQuery("SELECT TITLE, FIRSTNAME, NAME FROM HOTELS.CUSTOMER;");
                    while (resultSet.next()) {
                        String title = resultSet.getString(1);
                        String firstName = resultSet.getString(2);
                        String lastName = resultSet.getString(3);
                        System.out.println(title + " " + firstName + " " + lastName);
                    }
                }
                catch (SQLException e) {
                    System.err.println("Query failed!");
                }
            }
        }
    }
    ```

3. Compile the `.java` file into a `.class` file using the following command:

    ```Command Prompt (Microsoft Windows)
    javac -cp C:\SAP\hdbclient\ngdbc.jar;. JavaQuery.java
    ```  
    ```Powershell (Microsoft Windows)
    javac -cp "C:\SAP\hdbclient\ngdbc.jar;." JavaQuery.java
    ```  

    ```Shell (Linux or Mac)
    javac -cp ~/sap/hdbclient/ngdbc.jar:. JavaQuery.java
    ```  

4. Run `JavaQuery.class` and indicate where the SAP HANA JDBC driver is located.  Note that the host, port, UID and PWD will be retrieved from the `hdbuserstore`.

    ```Command Prompt (Microsoft Windows)
    java -classpath C:\SAP\hdbclient\ngdbc.jar;. JavaQuery
    ``` 
    ```Powershell (Microsoft Windows)
    java -classpath "C:\SAP\hdbclient\ngdbc.jar;." JavaQuery
    ```  

    ```Shell (Linux or Mac)
    java -classpath ~/sap/hdbclient/ngdbc.jar:. JavaQuery
    ```  

    ![Java Query](java-query.png)

See [JDBC Connection Options in Java Code](https://help.sap.com/docs/SAP_HANA_CLIENT/f1b440ded6144a54ada97ff95dac7adf/1c86038c05464d31a7dcae14f2d8a7dd.html) for additional details on the `getConnection` method of the `DriverManager`.  

See [Connect to SAP HANA Cloud via JDBC](https://help.sap.com/docs/SAP_HANA_CLIENT/f1b440ded6144a54ada97ff95dac7adf/ff15928cf5594d78b841fbbe649f04b4.html) for additional details on the certificate used during the connection.


### Debug the application
Visual Studio Code provides plugins for Java and can be used to debug an application.  

1. If you have not already done so, download [Visual Studio Code](https://code.visualstudio.com/Download).

2. If you have not already done so, in Visual Studio Code, choose **File | Open Folder**, and then select the `HANAClientsTutorial` folder.

    ![Workspace](workspace.png)

3. Open the file `JavaQuery.java`, and if asked, install the recommended extensions.

    ![Java extensions](extensions.png)

4. Once the Java Extension Pack has been installed, expand the Java Project Explorer, and click on the **+** icon to add the JDBC driver as a referenced library.

    ![referenced libraries](ref-libraries.png)

    The JDBC driver is located at `C:\SAP\hdbclient\ngdbc.jar`.

5. Place a breakpoint and then select **Run | Start Debugging**.  

    Notice that the debug view becomes active.  

    Notice that the program stops running at the breakpoint that was set. Step through the code by pressing F10 and observe the variable values in the variables pane.

    ![VS Code Debugging](debugging.png)

### Browse SAP HANA using DBeaver
`DBeaver` is a free and open-source database tool and can be used with the SAP HANA JDBC driver.

The following steps demonstrate how to configure `DBeaver` to connect to SAP HANA Cloud or SAP HANA, express edition using the JDBC driver.

1. [Download](https://dbeaver.io/download/), install or unzip, and run the community edition of `DBeaver`.

    ![Install DBeaver](dbeaver-install1.png)

2. Create a new SAP HANA database connection.

    ![New Connection](dbeaver-connect1.png)

    Specify the connection type and fill in the **Host** and **Port**.  Then press **Test Connection**.  You may be asked to download the required SAP HANA client JDBC driver.  The driver can also be updated under the **Driver Settings**.

    ![Connection Settings](dbeaver-connect2.png)

    Click on **Connection details** to specify the connection name.

    ![Connection Details](dbeaver-connect4.png)

3. After finishing the wizard, the catalog of the database can be viewed, and SQL statements can be executed.

    ![Query](dbeaver-query1.png)

    `DBeaver` can also be used to create an entity relationship (ER) diagram, perform a comparison of two selected objects, execute import and export operations, view spatial data on a map, and perform data analysis with its grouping and `calc` panels.

### Knowledge check
Congratulations! You have now created and debugged a Java application that connects to and queries an SAP HANA database and used the JDBC driver in `DBeaver`.








---
