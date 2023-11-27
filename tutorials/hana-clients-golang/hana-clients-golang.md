---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product-function>sap-hana-cloud--sap-hana-database, software-product>sap-hana, software-product>sap-hana--express-edition]
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

If Go is installed, then it will return the currently installed version, such as 1.19.

>For further details on supported versions, see SAP Note [3165810 - SAP HANA Client Supported Platforms](https://launchpad.support.sap.com/#/notes/3165810).

If it is not installed, download it from [Download Go](https://golang.org/dl/), run the installer, follow the provided instructions, and ensure that Go is in your path.

On Linux, follow the instructions for the appropriate Linux version such as the [Installing Go for openSUSE](https://en.opensuse.org/SDB:Go).

>In order for the shell to recognize that Go has been installed and for any go commands in future steps to be recognized, a new shell window needs to be opened.

### Configure the environment

The SAP HANA Client interface for Go, like the other SAP HANA client interfaces, except for JDBC, makes use of a C library named SQLDBC.  The Go driver loads the SQLDBC library  named `libdbcapiHDB` using [`cgo`](https://golang.org/cmd/cgo/).  For further information on the following steps, consult [Set Up Your Application to Use the Go Driver Package](https://help.sap.com/docs/SAP_HANA_CLIENT/f1b440ded6144a54ada97ff95dac7adf/fba20e31f75c4f7ca5629083869069e5.html) in the SAP HANA Client Interface Programming Reference Guide.  A 64-bit `gcc` compiler is required.

1. To check if a 64-bit `gcc` compiler is installed, run the following command:

    ```Shell
    gcc --version
    ```

    For Windows (if it is not installed), it can be downloaded from [Download MinGW](https://www.mingw-w64.org/downloads/). Under **WinLibs.com**, you can install from [winlibs.com](https://winlibs.com/), by scrolling to the **Download** section and downloading the latest release version (UCRT runtime) of the ZIP archive for Win64 to install for the x86_64 architecture, and then extracting the folder.

    ![download minGW from WinLibs](winLibsMinGW.png)

    If command prompt isn't showing you the installed version after running the version check command, manually add the bin folder to your path by setting it in your environment variables.

    On Linux, install the System GNU C compiler for your version of Linux. Note that if you are using openSUSE, minGW is included in the installation for Go through YaST.

    ![gcc 64-bit](gccLinux.png)

2. Examine the Go environment by running the below command:

    ```Shell
    go env
    ```

    Notice that GOROOT is set to a location such as `C:\go` or `/usr/lib64/go/1.19`.  This is the location that the Go SDK is installed to.  

    GOPATH is set to a location such as `C:\Users\user\go` or `$HOME/go` and defines the root of your workspace which stores your codebase.

3. Set the required environment variables.

    On Windows, search **Edit the System Environment Variables** and click on **Environment Variables...**.

    >Optionally, you can also use the **SETX** command in command prompt to set a Windows environment variable. For example, `SETX CGO_LDFLAGS C:\SAP\hdbclient\libdbcapiHDB.dll`. Note that this will set a user variable, not a system variable.

    ![Edit Environment Variables](editEnvironmentVariables.png)

    On Linux, open the `.bash_profile`.

    ```Shell (Linux or Mac)
    pico ~/.bash_profile
    ```

4. Set the `CGO_LDFLAGS` environment variable to point to the location of the `libdbcapiHDB` library as shown below and set the `LD_LIBRARY_PATH` if needed.

    On Windows, add the **NEW** System Variable. Set the variable name to **`CGO_LDFLAGS`** and the variable value to the location of `dbcapi` library: `C:\SAP\hdbclient\libdbcapiHDB.dll`

    ![Set Environment Variables](setEnvVar.png)

    Ensure that the `hdbclient` folder is set in your **Path** environment variable. You can set this path by clicking on **New** and browsing for your `hdbclient` folder.

    ![hdbclient path env variable](hdbclient-path.png)

    On Linux, add the following lines to the `bash_profile`.

    ```Shell (Linux or Mac)
    export CGO_LDFLAGS=$HOME/sap/hdbclient/libdbcapiHDB.so
    export LD_LIBRARY_PATH=$HOME/sap/hdbclient
    ```

5. Go to the driver folder and create a go module.

    ```Shell (Windows)
    cd C:\SAP\hdbclient\golang\src\SAP\go-hdb\driver
    go mod init "SAP/go-hdb/driver"
    go mod tidy
    ```

    ```Shell (Linux or Mac)
    cd ~/sap/hdbclient/golang/src/SAP/go-hdb/driver
    go mod init "SAP/go-hdb/driver"
    go mod tidy
    ```
    ![createModule](createModule.png)

### Create a Go application that queries an SAP HANA database

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

3. Create another go module and modify its contents:

    ```Shell (Windows)
    go mod init "go/goQuery"
    go mod tidy
    notepad go.mod
    ```   


    ```Shell (Linux or Mac)
    go mod init "go/goQuery"
    go mod tidy
    pico go.mod
    ```


4. Add the code below to `go.mod` under the go version line:

    >Make sure the path to the driver folder is correct and make any necessary changes.

    ```Code (Windows)
    replace SAP/go-hdb/driver v0.1.0 => C:\SAP\hdbclient\golang\src\SAP\go-hdb\driver   
    require SAP/go-hdb/driver v0.1.0
    ```

    ```Code (Linux or Mac)
    replace SAP/go-hdb/driver v0.1.0 => /home/name/sap/hdbclient/golang/src/SAP/go-hdb/driver
    require SAP/go-hdb/driver v0.1.0
    ```

    ![go.mod contents](goModContents.png)

5. Run the application:

    ```Shell
    go run goQuery.go
    ```

    ![Result](results.png)

For more information on the API's used, consult the SAP HANA connection specific properties at [Go Connection Properties](https://help.sap.com/docs/SAP_HANA_CLIENT/f1b440ded6144a54ada97ff95dac7adf/8d61ae225ae44b0bab2fb2285009f68d.html), [Go Database/SQL Tutorial](http://go-database-sql.org/index.html), and [Package SQL](https://golang.org/pkg/database/sql/).

### Debug the application

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

    >Debugging can also be performed from the command line using [Delve](https://github.com/go-delve/delve ).

### Knowledge check

Congratulations! You have now created and debugged a Go application that connects to and queries an SAP HANA database.



---
