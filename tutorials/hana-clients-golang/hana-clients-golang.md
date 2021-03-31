---
title: Connect Using the SAP HANA Go Interface
description: Create and debug a Go application that connects to SAP HANA using the SAP HANA client.
auto_validation: true
time: 15
tags: [ tutorial>beginner, products>sap-hana\,-express-edition, products>sap-hana-cloud]
primary_tag: products>sap-hana
---

## Prerequisites
 - You have completed the first 3 tutorials in this mission.

## Details
### You will learn
  - How to install Go
  - How to create a Go application that queries a SAP HANA Database

Go is an open source programming language developed by Google to increase productivity among programmers. For more information, see the [Go Documentation](https://golang.org/doc/).

---

[ACCORDION-BEGIN [Step 1: ](Install Go)]
The first step is to check if Go is installed, and if so, which version. To do so, enter the following command:

```Shell
go version
```

![go version](version.png)


If Go is installed, then it will return the currently installed version, such as 1.15.2. It is required to have at least version 1.8 or later installed.

>For further details on supported versions, see SAP Note [3006307 - SAP HANA Client Supported Platforms for 2.7](https://launchpad.support.sap.com/#/notes/3006307).


If it is not installed, download it from [Download Go](https://golang.org/dl/), run the installer, follow the provided instructions, and ensure that Go is in your path.

![Go Install](GoInstall.png)

In order for the shell to recognize that Go has been installed and for any go commands in future steps to be recognized, a new shell window needs to be opened.

Before proceeding, ensure you can create and run an example Go program as described in [Test your installation](https://golang.org/doc/install#testing).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Build the SAP HANA Client Go driver)]
The SAP HANA Client interface for Go, like the other SAP HANA client interfaces, except for JDBC, makes use of a C library named SQLDBC.  The Go driver loads the SQLDBC library  named `libdbcapiHDB` using [`cgo`](https://golang.org/cmd/cgo/).  For further information on the following steps, consult [Build the Go Driver](https://help.sap.com/viewer/f1b440ded6144a54ada97ff95dac7adf/latest/en-US/fba20e31f75c4f7ca5629083869069e5.html) in the SAP HANA Client Interface Programming Reference Guide.  A 64-bit `gcc` compiler is required.

1. To check if a 64-bit `gcc` compiler is installed, run the following command:

    ```Shell
    gcc --version
    ```

    ![gcc 64-bit](gccWindows.png)

    If it is not installed, on Linux install the System GNU C compiler.  

    On Windows download it from [Download MinGW](http://mingw-w64.org/doku.php/download) and run the installer.

    During the install, set the architecture option to `x86_64`.

    ![MinGW Installation](MingwInstallation.png)

    Add the bin folder to your path.

2. Examine the Go environment by running the below command:

    ```Shell
    go env
    ```

    Notice that GOROOT is set to a location such as `C:\go` or `/usr/local/go`.  This is the location that the Go SDK is installed to.  

    GOPATH is set to a location such as `C:\Users\user\go` or `$HOME/go` and defines the root of your workspace which stores your codebase.

3. Copy the SAP HANA client source code to the Go workspace.  
    ```Shell (Windows)
    mkdir %GOPATH%\src
    xcopy C:\SAP\hdbclient\golang\src %GOPATH%\src\ /s
    ```

    ```Shell (Linux or Mac)
    mkdir -p $HOME/go/src
    cp -r ~/sap/hdbclient/golang/src/SAP $HOME/go/src/
    ```

    The results are shown in the screenshot below:  

    ![Result of copying source](hana-client-in-go-src.png)

4. Set the `CGO_LDFLAGS` environment variable to point to the location of `libdbcapiHDB` library as shown below.  Preferably, set these values in the Microsoft Windows environment variables dialog or on Linux/Mac in the `.profile` or `.bash_profile` file so these values persist and can be seen by other applications such as Visual Studio Code.

    ```Shell (Windows)
    set CGO_LDFLAGS=c:\sap\hdbclient\libdbcapiHDB.dll
    ```
    ```Shell (Linux)
    export CGO_LDFLAGS=$HOME/sap/hdbclient/libdbcapiHDB.so
    ```
    ```Shell (Mac)
    export CGO_LDFLAGS=$HOME/sap/hdbclient/libdbcapiHDB.dylib
    ```

5. Run the install for the driver:
    ```Shell (Windows)
    cd c:\sap\hdbclient\golang\src
    go install SAP/go-hdb/driver
    ```

    ```Shell (Linux or Mac)
    cd ~/sap/hdbclient/golang/src
    go install SAP/go-hdb/driver
    ```

    The result of this should be that a new go-hdb package is installed into `$HOME/go`

    ![result of installing the driver](driver-built.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a Go application that queries an SAP HANA database)]
1. In a shell, create a folder named `go`, enter the newly created directory, and open a file named `goQuery.go` in an editor.
    ```Shell (Microsoft Windows)
    mkdir %HOMEPATH%\HANAClientsTutorial\go
    cd %HOMEPATH%\HANAClientsTutorial\go
    notepad goQuery.go
    ```

    ```Shell (Linux or Mac)
    mkdir -p $HOME/HANAClientsTutorial/go
    cd $HOME/HANAClientsTutorial/go
    pico goQuery.go
    ```
2. Add the code below to `goQuery.go`:

    ```Go Code
    package main

    import (
      "fmt"
    	"database/sql"
    	"log"

      _ "SAP/go-hdb/driver"
      )

    func main() {
      //Option 1, retrieve the connection parameters from the hdbuserstore
      //host, port, user name and password come from the hdbuserstore key USER1UserKey
      connectString := "hdb://?key=USER1UserKey&encrypt=true&sslValidateCertificate=false"

      //Option 2, specify the connection parameters
      //connectString := "hdb://User1:Password1@999deec0-ccb7-4a5e-b317-d419e19be648.hana.prod-us10.hanacloud.ondemand.com:443?encrypt=true&sslValidateCertificate=false"

      //encrypt and sslValidateCertificate should be true for HANA Cloud connections
      //As of SAP HANA Client 2.6, connections on port 443 enable encryption by default

      fmt.Println("Connect String is " + connectString)

      db, err := sql.Open("hdb", connectString)
      if err != nil {
        log.Fatal(err)
        return
      }
      defer db.Close()

      rows, err := db.Query("SELECT NAME, ADDRESS from HOTEL.CUSTOMER")
      if err != nil {
        log.Fatal(err)
      }
      defer rows.Close()

      var lastName string
      var address string
      for rows.Next() {
        err = rows.Scan(&lastName, &address)
        if err != nil {
          log.Fatal(err)
        }
        fmt.Println(lastName + ": " + address)
      }

      err = rows.Err()
      if err != nil {
      	log.Fatal(err)
      }
    }
    ```

    Once the `goQuery.go` file has been updated, save and close the file.

3. On Linux or Mac, add the `libdbcapiHDB` library to the path so that the Go driver can find it.  You may do this by adding the following lines to the `.profile` or `.bash_profile` files:

    ```Shell (Linux)
    export LD_LIBRARY_PATH=~/sap/hdbclient
    ```
    ```Shell (Mac)
    export DYLD_LIBRARY_PATH=~/sap/hdbclient
    ```

    >If the next step to run the application fails with an error message of "Library not loaded", a workaround to place the libdbcapiHDB library in the same directory as the application executable is available.
    >```Shell (Mac)
    >cp ~/sap/hdbclient/libdbcapiHDB.dylib .
    >```


4. Run the application:

    ```Shell
    go run goQuery.go
    ```

    ![Result](results.png)

5. For more information on the API's used, consult the SAP HANA connection specific properties at [Connect to SAP HANA from Go](https://help.sap.com/viewer/f1b440ded6144a54ada97ff95dac7adf/latest/en-US/8d61ae225ae44b0bab2fb2285009f68d.html), [Go Database/SQL Tutorial](http://go-database-sql.org/index.html), and [Package SQL](https://golang.org/pkg/database/sql/)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Debug the application)]

Visual Studio Code provides plugins for Go and can be used to debug an application.

1. If you have not already done so, download [Visual Studio Code](https://code.visualstudio.com/Download).

2. If you have not already done so, in Visual Studio Code, choose **File | Add Folder to Workspace**, and then add the `HANAClientsTutorial` folder.

    ![Open Workspace](workspace.png)

3. Open the file `goQuery.go`.

    ![Go Extension](GoExtension.png)

    Visual Studio Code will recognize the `go` file extension and will suggest installing the Go for Visual Studio Code extension.  Click **Install**.

4. Place a breakpoint.

    ![SetBreakpoint](SetGoBreakpoint.png)

5. Select **Run | Start Debugging**.

    Notice that the program stops running at the breakpoint that was set.

    Observe the variable values in the leftmost pane.  Step through code.

    ![Breakpoint](GoBreakpoint.png)  

    >Note that debugging can also be performed from the command line using [Delve](https://github.com/go-delve/delve ).

Congratulations! You have now created and debugged a Go application that connects to and queries an SAP HANA database.


[VALIDATE_1]
[ACCORDION-END]

---
