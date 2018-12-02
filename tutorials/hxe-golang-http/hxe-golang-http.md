---
title: How to write a simple go application that queries HANA, express edition.
description: A How-To that shows how to write a simple go application that queries HANA, express edition.
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, products>sap-hana, products>sap-hana\,-express-edition, tutorial>how-to ]
---
## Prerequisites  
 - Proficiency: beginner
 - Setup: `HANA, express edition` must be running and accessible from your client platform. For instructions on how to setup a `HANA, express edition` see the [HANA Express database deploy tutorial](https://www.sap.com/developer/tutorials/hxe-db-deploy.html).
 - This tutorial assumes that you have a database login that can access the `M_DATABASE` view in the `HANA, express edition` `SystemDB`.

## Next Steps
 - Complete other tutorials.

## How-To Details
This How-To shows how to create a simple go language application that queries a HANA, express edition database and displays the results in a web page.

### Time to Complete
**20 Min**.

[ACCORDION-BEGIN [Step 1: ](Setup Your Go Environment)]

1. Install Go.

a. You will need Go installed on your client operating system. This tutorial was tested on Linux, Mac and Windows client platforms.

b. If you do not have Go installed, you can download the Go installer for your operating system from [the Go Download Page](https://golang.org/dl/). This tutorial has been tested with Go 1.7 and 1.8.3. Follow the Go setup installation and setup instructions to set up your development environment.

2. Download and install the go database driver for HANA

 - Enter the following command in a command shell on your client `go get github.com/SAP/go-hdb/driver`. This will install the `go-hdb` driver in the following directory `$GOPATH/pkg/<operating_system>/github.com/SAP/go-hdb` (or `%GOPATH%\pkg\(your operating system)\github.com\SAP\go-hdb` on Windows).


[ACCORDION-BEGIN [Step 2: ](Write the Source Code)]

Your application will include the following four source files in the source code directory:

    `application.go` - The main go source file. The file starts an http server, queries the HANA Express database, places the results in a data structure, then merges that data with `template.html` and served up the results via `http`.

    `template.html` - An HTML formatted template that uses the annoations from the `html/template` go package to merge data generated in `application.go` to create the html page returned by `application.go`.

    `config.go` - This go source file parses the config.json file into go objects.

    `config.json` - This configuration file contains configuration parameters that you will need to be customize.

1. Navigate to your $GOPATH directory. If you have not yet created a GOPATH, you can find out more about setting your GOPATH [in the `Go` documentation](https://golang.org/doc/code.html#GOPATH).

2. Make your project source directory, then navigate to it:
```
mkdir -p $GOPATH/src/hxe_golang_http`
cd $GOPATH/src/hxe_golang_http
```

3. Create a file called `application.go` and paste or type the following code into it.

```
package main
/*
  This is the main source file for the application. It does the following:
    - starts an http server
    - connects to a HANA database
    - queries the M_Database_View view
    - merges the data returned into an html template
    - returns the merged html page

*/
import (
        "database/sql"
        "fmt"
        _ "github.com/SAP/go-hdb/driver"
        "html/template"
        "log"
        "net/http"
        "os"
)

// M_Database_View holds the columns of the M_Database view
type M_Database_View struct {
      System_id string
      Database_name string
      Host string
      Start_time string
      Version string
      Usage string
}

// Return a Database URL with the following format using the Config.Db from config:
//     <driverName>://<Database user>:<Password>@<Database Host IP>:<Database Port>
//
// Example: hdb://System:MyPassword@10.173.171.35:39013
//
func getDbUrl()(string) {
  return Config.Db.Driver + "://" +
         Config.Db.User + ":" + Config.Db.Password + "@" +
         Config.Db.Ip_addr + ":" + Config.Db.Db_port
}

// Returns the port either using an operating system environment variable
// or, if that isn't found, using the default port value from the config.
func getHttpPort() (string){
    var port string
    if len(Config.HttpPort.OsVar) > 0 {
      port = os.Getenv(Config.HttpPort.OsVar)
    }
    if len(port) == 0 {
       return Config.HttpPort.Default
    } else {
       return port
    }
}

// Initialize the configuration
// Then start listening on the correct port for the current platform
func main() {
        err := InitializeConfig()  
        if err != nil {
          log.Fatal(err)
        }
        httpPort := getHttpPort()        
        http.HandleFunc("/", myHandler)
        log.Printf("Listening on port %s\n\n", httpPort)
        http.ListenAndServe(":"+httpPort, nil)
}

// Query the database using the pyhdb driver and display the results in an html formatted page
func myHandler(w http.ResponseWriter, r *http.Request) {

        log.Printf("Serving on %s...\n", r.RemoteAddr)

        // Query the HANA Database
        db, err := sql.Open(Config.Db.Driver,getDbUrl())
        if err != nil {
          fmt.Fprintf(w, "Failed to connect to the database. %s\n", err.Error() )
          log.Printf("Failed to connect to the database: %s...\n", err.Error())
          return
        }

        defer db.Close()

        mdbs := make([]*M_Database_View, 0)

        // Query the M_DATABASE view
        rows, err := db.Query("Select * from M_DATABASE")
        if err != nil {
          fmt.Fprintf(w, "Failed to select data from M_DATABASE view. %s\n", err.Error() )
          log.Printf("Failed to select data from M_DATABASE view: %s...\n", err.Error())
          return
        }
        defer rows.Close()

        for rows.Next() {
          mdb := new(M_Database_View)
          err := rows.Scan(&mdb.System_id,
                           &mdb.Database_name,
                           &mdb.Host,
                           &mdb.Start_time,
                           &mdb.Version,
                           &mdb.Usage)
          if err != nil {
            fmt.Fprintf(w, "Failed to extract data from the M_DATABASE view. %s\n", err.Error() )
            log.Printf("Failed to extract data from the M_DATABASE view: %s...\n", err.Error())
            return
          }
          mdbs = append(mdbs, mdb)
        }

        // Merge the M_DATABASE_VIEW values with the template html file
        // and write the results to the ResponseWriter
        t, err := template.ParseFiles("template.html")
        if err != nil {
            fmt.Fprintf(w, "Failed to parse template.html. %s\n", err.Error() )
            log.Printf("Failed to parse template.html.  %s...\n", err.Error())
            return
        }

        err = t.Execute(w, mdbs)
        if err != nil {
            fmt.Fprintf(w, "Failed to merge template.html. %s\n", err.Error() )
            log.Printf("Failed to merge template.html.  %s...\n", err.Error())
            return
        }        
}
```

4. Create a file called `config.go` and paste or type the following code into it:

```
package main
/*
  This utility file gets the Database and HTTP port variables used in application.go
  from the a file called config.json. config.json is expected to be in the same directory.
*/

import (
        "encoding/json"
        "io/ioutil"
        "errors"
)

// Config will hold the entire config file contents after initialization.
var Config Configuration


// Configuration contains the top level collection of config.json
type Configuration struct {
  Db Database `json:"database"`
  HttpPort Port `json:"http_port"`
}


// Database contains the collection for the database
type Database struct {
  User string `json:"user"`
  Password string `json:"password"`
  Ip_addr string `json:"ip_addr"`
  Db_port string `json:"db_port"`
  Driver string `json:"driver"`
}


// Port contains the collection for a port in config.json
type Port struct {
  //OsVar is the name of the OS environment variable for the port
  OsVar          string `json:"os_var"`
  //DefaultPort is the port to use if there is no os environment variable set
  Default       string `json:"default"`
}


//Reads and parses config.json and populates the Config variable
func InitializeConfig()(error) {
	file, err := ioutil.ReadFile("config.json")
	if err != nil {
		return errors.New("Unable to read the configuration file, config.json. " + err.Error())
	}

	err = json.Unmarshal(file, &Config)
	if err != nil {
		return errors.New("Unable to parse the configuration file, config.json " + err.Error())
	}

    return nil
}
```

5. Create a file called `template.html` and paste or type the following code into it:

```
<html>
<body>
{{range .}}
  <p>System ID: {{.System_id}}</p>
  <p>Database Name: {{.Database_name}}</p>
  <p>Host: {{.Host}}</p>
  <p>Start Time: {{.Start_time}}</p>
  <p>Version: {{.Version}}</p>
  <p>Usage: {{.Usage}}</p>
{{end}}
</body>
</html>
```

*Note: This template uses the [go html/template package] (https://golang.org/pkg/html/template/)*

5. Create a file called `config.json` and paste or type the following text into it.  

```
{
  "database": {
    "driver": "hdb",
    "user": "system",
    "password": "MyPassword",
    "ip_addr": "10.173.171.241",
    "db_port": "39013"   
  },
  "http_port": {
    "os_var": "",    
    "default": "8080"
  }
}
```
- Replace the password and `ip_addr` fields with the correct values for your `HANA, express edition` database. (A host name can be used in place of an `ip` address for the `ip_addr` value).
- The `http_port` represents the port on which the http server will be listening. `os_var` is the name of an operating system variable that contains the port number being used. "default" is the default port to use if no operating system variable is set. The values of the `http_port` fields can vary based on the environment in which you are deploying. Here, for example, are some values for common deployment platforms. If you will be deploying to those environments, make sure to make these changes before deploying:

```
| Platform               |  os_var              | default |
| ---------------------- |:--------------------:| -------:|
| Local                  | ""                   | "8080"  |        
| Google App Engine      | ""                   | "8080"  |        
| Azure App Service      | "HTTP_PLATFORM_PORT" | "8080"  |
| AWS Elastic Beanstalk  | "PORT"               | "5000"  |
```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Build and Deploy the Application)]

1. Build your application.

- Navigate to the the `$GOPATH/src/hxe_golang_http` (`%GOPATH%\src\hxe_golang_http` on Windows) directory. Execute the following command: `go build`.
  This will create an executable file called `hxe_golang_http` in the `hxe_golang_http` directory.

  *Note: Running `go install` will create the executable in the `$GOPATH/src` directory instead.*

4. Execute `hxe_golang_http` to start the application. (`hxe_golang_http` will be built in the `$GOPATH/bin` directory.)

5. In a browser, enter the `url` where `hxe_golang_http` is running `http://localhost:8080`. When you do so, you should see something like the following screenshot:

![Results in Browser](1.png).


[ACCORDION-END]

### Additional Information
 - If you intend to deploy your application to Google App Engine, go to the [Google App Engine deployment tutorial](https://www.sap.com/developer/tutorials/hxe-app-deploy-gcp.html)
 - If you intend to deploy your application to Azure App Service, go to the [Azure App Service deployment tutorial](https://www.sap.com/developer/tutorials/hxe-app-deploy-azure.html)
 - If you intend to deploy your application to AWS Elastic Beanstalk, go to the [AWS Elastic Beanstalk deployment tutorial](https://www.sap.com/developer/tutorials/hxe-app-deploy-aws.html)
 - [View similar How-Tos](http://www.sap.com/developer/tutorials.html) or [View all How-Tos](http://www.sap.com/developer/tutorials.html)
