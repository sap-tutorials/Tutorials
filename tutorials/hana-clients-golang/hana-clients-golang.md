---
parser: v2
author_name: Dan van Leeuwen
author_profile: https://github.com/danielva
auto_validation: true
time: 15
tags: [tutorial>beginner, software-product>sap-hana-cloud, software-product-function>sap-hana-cloud--sap-hana-database, software-product>sap-hana]
primary_tag: software-product>sap-hana-cloud
---

# Connect Using the SAP HANA Go Interface

<!-- description --> Create and debug a Go application that connects to SAP HANA using the SAP HANA client.

## Prerequisites

- You have completed the first 3 tutorials in this mission.

## You will learn

- How to install Go
- How to create a Go application that queries a SAP HANA Database

## Intro

Go is an open-source programming language developed by Google to increase productivity among programmers. For more information, see the [Go Documentation](https://golang.org/doc/).

---

### Install Go

The first step is to check if Go is installed, and if so, which version. To do so, enter the following command:

```Shell
go version
```

![go version](version.png)

If Go is installed, then it will return the currently installed version, such as 1.22.1.

> [!NOTE]
> For further details on supported versions, see SAP Note [3165810 - SAP HANA Client Supported Platforms](https://launchpad.support.sap.com/#/notes/3165810).

[OPTION BEGIN [Windows and Mac]]

If Go is not installed, download it from [Download Go](https://golang.org/dl/), run the installer, follow the provided instructions, and ensure that Go is in your path.

[OPTION END]

[OPTION BEGIN [Linux]]

If Go is not installed, follow the instructions for the appropriate Linux version such as the [Installing Go for openSUSE](https://en.opensuse.org/SDB:Go).

[OPTION END]

> [!TIP]
> In order for the shell to recognize that Go has been installed and for any go commands in future steps to be recognized, a new shell window needs to be opened.

### Configure the environment

The SAP HANA Client interface for Go, like the other SAP HANA client interfaces, except for JDBC, makes use of a C library named SQLDBC.  The Go driver loads the SQLDBC library  named `libdbcapiHDB` using [`cgo`](https://golang.org/cmd/cgo/).  For further information on the following steps, consult [Set Up Your Application to Use the Go Driver Package](https://help.sap.com/docs/SAP_HANA_CLIENT/f1b440ded6144a54ada97ff95dac7adf/fba20e31f75c4f7ca5629083869069e5.html) in the SAP HANA Client Interface Programming Reference Guide.  A 64-bit `gcc` compiler is required.

1. To check if a 64-bit `gcc` compiler is installed, run the following command:

    ```Shell
    gcc --version
    ```

    ![gcc 64-bit](gccLinux.png)

    [OPTION BEGIN [Windows]]

    If `gcc` is not installed, it can be downloaded from [Download MinGW](https://www.mingw-w64.org/downloads/). Under **WinLibs.com**, you can install from [winlibs.com](https://winlibs.com/), by scrolling to the **Download** section and downloading the latest release version (UCRT runtime) of the ZIP archive for Win64 to install for the x86_64 architecture, and then extracting the folder.

    ![Download minGW from WinLibs](winLibsMinGW.png)

    If command prompt isn't showing you the installed version after running the version check command, manually add the bin folder to your path by setting it in your environment variables.

    Search **Edit the System Environment Variables** in the Microsoft Windows Start menu and click on **Environment Variables...**

    ![Edit environment variables](editEnvironmentVariables.png)

    Look for the `Path` environment variable and double click to edit. Select **Browse** and manually browse through your File Explorer to find the bin folder.

    ![Add bin to path](add-bin-to-path.png)

    [OPTION END]

    [OPTION BEGIN [Linux]]

    If `gcc` is not installed, install the system GNU C compiler for your version of Linux. The GNU C compiler includes the `gcc` command required by `cgo`. Note that if you are using openSUSE, MinGW is included in the installation for Go through YaST.

    [OPTION END]

    [OPTION BEGIN [Mac]]

    If `gcc` is not installed, install the Xcode Command Line Tools if they are not already installed. These tools include the compiler and related development tools required by `cgo`.

    ```Shell
    xcode-select --install    
    ```

    [OPTION END]

2. Examine the Go environment by running the below command:

    ```Shell
    go env
    ```

    Notice that GOROOT is set to a location such as `C:\go` or `/usr/lib64/go/1.22`.  This is the location that the Go SDK is installed to.  

    GOPATH is set to a location such as `C:\Users\user\go` or `$HOME/go` and defines the root of your workspace which stores your codebase.

3. Set the `CGO_LDFLAGS` environment variable to point to the location of the `libdbcapiHDB` library as shown below and set the appropriate library path variable if needed.

    [OPTION BEGIN [Windows]]

    Add a **NEW** variable. Set the variable name to **`CGO_LDFLAGS`** and the variable value to the location of `dbcapi` library: `C:\SAP\hdbclient\libdbcapiHDB.dll`

    ![Set environment variables](setEnvVar.png)

    Ensure that the `hdbclient` folder is set in your **Path** environment variable. You can set this path by clicking on **New** and browsing for your `hdbclient` folder.

    ![hdbclient path environment variable](hdbclient-path.png)

    [OPTION END]

    [OPTION BEGIN [Linux]]

    Add the following lines to the appropriate shell profile for your environment.

    ```Shell
    export CGO_LDFLAGS=$HOME/sap/hdbclient/libdbcapiHDB.so
    export LD_LIBRARY_PATH=$HOME/sap/hdbclient
    ```

    [OPTION END]

    [OPTION BEGIN [Mac]]

    Add the following lines to the appropriate shell profile for your environment.

    ```Shell
    export CGO_LDFLAGS="$HOME/sap/hdbclient/libdbcapiHDB.dylib"
    export DYLD_LIBRARY_PATH="$HOME/sap/hdbclient"
    ```

    [OPTION END]

4. Go to the driver folder and create a go module.

    [OPTION BEGIN [Windows]]

    ```Shell
    cd C:\SAP\hdbclient\golang\src\SAP\go-hdb\driver
    go mod init "SAP/go-hdb/driver"
    go mod tidy
    ```

    [OPTION END]

    [OPTION BEGIN [Mac and Linux]]

    ```Shell
    cd ~/sap/hdbclient/golang/src/SAP/go-hdb/driver
    go mod init "SAP/go-hdb/driver"
    go mod tidy
    ```

    [OPTION END]

    ![Create module](createModule.png)

    The contents of the HANA Client folder is not writeable so you may need to change the permissions on the driver folder or copy files to a new location.

### Create a Go application that queries an SAP HANA database

1. In a shell, create a folder named `go`, enter the newly created directory, and open a file named `goQuery.go` in an editor.

    [OPTION BEGIN [Windows]]

    ```Shell
    mkdir %HOMEPATH%\HANAClientsTutorial\go
    cd %HOMEPATH%\HANAClientsTutorial\go
    notepad goQuery.go
    ```

    [OPTION END]

    [OPTION BEGIN [Mac and Linux]]

    ```Shell
    mkdir -p $HOME/HANAClientsTutorial/go
    cd $HOME/HANAClientsTutorial/go
    pico goQuery.go
    ```

    [OPTION END]

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
        connectString := "hdb://?key=USER1UserKey&encrypt=true&sslValidateCertificate=false&sessionVariable:APPLICATION=goQuery.go"

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

        rows, err := db.Query("SELECT NAME, ADDRESS from HOTELS.CUSTOMER")
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

    > [!NOTE]
    > The `sessionVariable:APPLICATION` parameter sets a custom application name for the connection. This value can be viewed in the `CLIENT_APPLICATION` column of the `M_CONNECTIONS` system view and may also appear in SAP HANA telemetry data. 

3. Create another go module and modify its contents:

    [OPTION BEGIN [Windows]]

    ```Shell
    go mod init "go/goQuery"
    go mod tidy
    notepad go.mod
    ```

    [OPTION END]

    [OPTION BEGIN [Mac and Linux]]

    ```Shell
    go mod init "go/goQuery"
    go mod tidy
    pico go.mod
    ```

    [OPTION END]

4. Add the code below to `go.mod` under the go version line:

    > [!TIP]
    > Make sure the path to the driver folder is correct and make any necessary changes.

    [OPTION BEGIN [Windows]]

    ```Code
    replace SAP/go-hdb/driver v0.1.0 => C:\SAP\hdbclient\golang\src\SAP\go-hdb\driver
    require SAP/go-hdb/driver v0.1.0
    ```

    [OPTION END]

    [OPTION BEGIN [Mac and Linux]]

    ```Code
    replace SAP/go-hdb/driver v0.1.0 => $HOME/sap/hdbclient/golang/src/SAP/go-hdb/driver
    require SAP/go-hdb/driver v0.1.0
    ```

    [OPTION END]

    ![go.mod contents](goModContents.png)

5. Place the dbcapi shared library files in /HANAClientsTutorial/go directory by copying or dragging them into that folder. Then, run the application:

   ```Shell
   go run goQuery.go
   ```

    ![Result](results.png)

For more information on the API's used, consult the SAP HANA connection specific properties at [Go Connection Properties](https://help.sap.com/docs/SAP_HANA_CLIENT/f1b440ded6144a54ada97ff95dac7adf/8d61ae225ae44b0bab2fb2285009f68d.html), [Go Database/SQL Tutorial](http://go-database-sql.org/index.html), and [Package SQL](https://golang.org/pkg/database/sql/).

### Debug the application

Visual Studio Code provides plugins for Go and can be used to debug an application.

1. If you have not already done so, download [Visual Studio Code](https://code.visualstudio.com/Download).

2. If you have not already done so, in Visual Studio Code, choose **File | Add Folder to Workspace**, and then add the `HANAClientsTutorial` folder.

    ![Open the workspace](workspace.png)

3. Open the file `goQuery.go`.

    ![Go extension install](GoExtension.png)

    Visual Studio Code will recognize the `go` file extension and will suggest installing the Go for Visual Studio Code extension.  Click **Install**.

    Check whether a `launch.json` file has been created. If not, go to the Run and Debug tab and select “Create a launch.json file.”

    ![Create a launch](GoCreateJson.png)

4. Open the file `launch.json` and add the following code

    ```Go Code
    "env": {
        "CGO_LDFLAGS": "./libdbcapiHDB.dylib"
    }
    ```

    ![Go JSON file](GoJsonFile.png)

    Change `./libdbcapiHDB.dylib` to the correct file for your operating system: use `libdbcapiHDB.dylib` on macOS, `libdbcapiHDB.dll` on Windows, or `libdbcapiHDB.so` on Linux. Also ensure the dbcapi file has been copied or moved into this folder.

5. Place a breakpoint.

    ![Set a breakpoint](SetGoBreakpoint.png)

6. Select **Run | Start Debugging**.

    Notice that the program stops running at the breakpoint that was set.

    Observe the variable values in the leftmost pane.  Step through code.

    ![Breakpoint](GoBreakpoint.png)  

    > [!NOTE]
    > Debugging can also be performed from the command line using [Delve](https://github.com/go-delve/delve).

### Knowledge check

Congratulations! You have now created and debugged a Go application that connects to and queries an SAP HANA database.

---
