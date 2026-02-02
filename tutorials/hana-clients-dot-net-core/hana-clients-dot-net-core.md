---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product-function>sap-hana-cloud--sap-hana-database, software-product>sap-hana, software-product>sap-hana--express-edition]
primary_tag: software-product>sap-hana-cloud
---

# Connect Using the SAP HANA .NET Interface

<!-- description --> Create and debug a .NET application that connects to SAP HANA using the SAP HANA client.

## Prerequisites

- You have completed the first 3 tutorials in this mission.

## You will learn

- How to install the .NET SDK
- How to create and debug a .NET application that queries an SAP HANA database

## Intro

[.NET](https://en.wikipedia.org/wiki/.NET_Core) is a free and open-source software framework for Microsoft Windows, Linux and macOS operating systems and is the successor to the .NET Framework.  .NET was previously known as .NET Core.

---

### Install the .NET SDK

The first step is to check if you have the .NET SDK  installed and what version it is.  Enter the following command:

```Shell
dotnet --version  
```

If the `dotnet` command is not recognized, it means that the .NET SDK has not been installed. If the SDK is installed, the command returns the currently installed version, such as 8.0.203.  

If the .NET SDK is not installed, download it from [Download .NET](https://dotnet.microsoft.com/download) and run the installer on Microsoft Windows or Mac.

![.NET Core SDK Install](dotnet-install.png)

On Linux, follow the instructions for the appropriate Linux version such as [Install the .NET SDK or the .NET Runtime on openSUSE](https://docs.microsoft.com/en-us/dotnet/core/install/linux-opensuse).

In order for the shell to recognize that the .NET SDK is installed and for any `dotnet` commands in future steps to be recognized, a new shell window needs to be opened.

>For further details on supported versions, see SAP Note [3165810 - SAP HANA Client Supported Platforms](https://launchpad.support.sap.com/#/notes/3165810).

### Create a .NET application that queries an SAP HANA database

1. Create a new console app with the below commands:

    ```Shell (Microsoft Windows)
    cd %HOMEPATH%/HANAClientsTutorial
    dotnet new console -o dotNET
    ```  

    ```Shell (Linux or Mac)
    cd $HOME/HANAClientsTutorial
    dotnet new console -o dotNET
    ```

2. Add the SAP HANA .NET data provider which is available on [nuget](https://www.nuget.org/packages/Sap.Data.Hana.Net.v8.0/).

    ```Shell
    dotnet add package Sap.Data.Hana.Net.v8.0
    ```

    ![HANAClientDriverDownload](HANAClientDriver.png)
  
3. Run the app to validate that SAP HANA driver can be loaded:

    ```Shell
    dotnet run
    ```

4. Open an editor to edit the file `Program.cs`.

    ```Shell (Windows)
    notepad Program.cs
    ```

    ```Shell (Linux or Mac)
    pico Program.cs
    ```

5. Replace all content of `Program.cs` with the code below. Be sure to update values where necessary and save the file when finished.

    ```C#
    using System;
    using Sap.Data.Hana;
    namespace dotNETQuery
    {
        class Program
        {
            static void Main(string[] args)
            {
                try
                {
                    using (var conn = new HanaConnection("Server=999deec0-ccb7-4a5e-b317-d419e19be648.hana.prod-us10.hanacloud.ondemand.com:443;UID=User1;PWD=Password1;encrypt=true;sslValidateCertificate=false"))

                    // encrypt and sslValidateCertificate should be true for HANA Cloud connections
                    // As of SAP HANA Client 2.6, connections on port 443 enable encryption by default
                    // sslValidateCertificate should be set to false when connecting
                    // to an SAP HANA, express edition instance that uses a self-signed certificate.

                    {
                        conn.Open();
                        Console.WriteLine("Connected");
                        var query = "SELECT TITLE, FIRSTNAME, NAME FROM HOTELS.CUSTOMER";
                        using (var cmd = new HanaCommand(query, conn))
                        using (var reader = cmd.ExecuteReader())
                        {
                            Console.WriteLine("Query result:");
                            // Print column names
                            var sbCol = new System.Text.StringBuilder();
                            for (var i = 0; i < reader.FieldCount; i++)
                            {
                                sbCol.Append(reader.GetName(i).PadRight(20));
                            }
                            Console.WriteLine(sbCol.ToString());
                            // Print rows
                            while (reader.Read())
                            {
                                var sbRow = new System.Text.StringBuilder();
                                for (var i = 0; i < reader.FieldCount; i++)
                                {
                                    var result = reader[i].ToString();
                                    if (result is not null) {
                                        sbRow.Append(result.PadRight(20));
                                    }
                                }
                                Console.WriteLine(sbRow.ToString());
                            }
                            conn.Close();
                        }
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine("Error - " + ex.Message);
                    Console.WriteLine(ex.ToString());
                }
            }
        }
    }
    ```

    Save and close the `Program.cs` file after replacing the code and updating the host and port.

    The above app makes use of some of the SAP HANA client .NET driver  methods, such as [HanaConnection](https://help.sap.com/docs/SAP_HANA_CLIENT/f1b440ded6144a54ada97ff95dac7adf/d19390d16d6110149af29776dce510bc.html).  Connection details for this class can be found at [Microsoft ADO.NET Connection Properties](https://help.sap.com/docs/SAP_HANA_CLIENT/f1b440ded6144a54ada97ff95dac7adf/469e137b6d611014ac27bffe40be2f18.html).  Further .NET API details can be found in the [.NET API browser](https://docs.microsoft.com/en-us/dotnet/api/?view=net-6.0).

6. Run the app:

    ```Shell
    dotnet run
    ```

    >Before running the program make sure to be in the directory where Program.cs is saved

    ![Result of running the app](result.png)

7. Optionally remove any unused platform files from the runtimes folder at HANAClientsTutorial\dotNET\bin\Debug\net9.0\runtimes.

### Debug the application

1. Open Visual Studio Code. If needed, download Visual Studio Code [here](https://code.visualstudio.com/Download).

2. If you have not already done so, choose **File | Add Folder to Workspace**, and then add the `HANAClientsTutorial` folder.

    ![Workspace](workspace.png)

3. Open the file `Program.cs`.

    ![C# Extension](extension.png)

    Visual Studio Code will recognize the `cs` file extension and will suggest installing the C# for Visual Studio Code extension.  Click **Install**.

4. Place a breakpoint on the line sbRow.Append line.  Select **Run | Start Debugging | .NET Core**.  A configuration will be added.  Choose **Run | Start Debugging**.

    Notice that the debug view becomes active and that the RUN option is .NET Launch.

    Notice that the program stops running at the breakpoint that was set.

    Observe the variable values in the leftmost pane.  Step through code.

    ![VS Code Debugging](debugging.png)  

    For further information on debugging .NET apps consult [Tutorial: Debug a .NET Core console application using Visual Studio Code](https://docs.microsoft.com/en-us/dotnet/core/tutorials/debugging-with-visual-studio-code) and [Instructions for setting up the .NET Core debugger](https://github.com/OmniSharp/omnisharp-vscode/blob/master/debugger.md).

### Knowledge check

Congratulations! You have now created and debugged a .NET application that connects to and queries an SAP HANA database.  

---
