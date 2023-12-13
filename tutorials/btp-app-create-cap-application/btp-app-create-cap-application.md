---
author_name: Mahati Shankar
author_profile: https://github.com/smahati
title: Create a CAP-Based Application
description: This tutorial shows you how to create a new CAP-based application, which exposes the OData V4 protocol.
keywords: cap
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Prepare Your Development Environment for CAP](btp-app-prepare-dev-environment-cap)

## Details
### You will learn
 - How to use the CAP's tooling `cds init` to create your project
 - How to use the CAP's tooling `cds watch` to launch your project
 - How to add files to your project

---

[ACCORDION-BEGIN [Step 1: ](Create and initialize the project)]
1. Open a command line window.

2. Navigate to your tutorial root directory.

    ```Shell/Bash
    cd <tutorial root directory>
    ```

4. Execute the following command:

    ```Shell/Bash
    cds init cpapp
    ```
    This creates a `cpapp` directory and an initial CAP project within the `cpapp` directory. 

3. Switch to your `cpapp` directory.

    ```Shell/Bash
    cd cpapp
    ```

5. Open the project in VS Code.

    ```Shell/Bash
    code .
    ```

    The project looks like this in VS Code:

    ![VS Code](vscode.png)

    > You might see some hidden files in your project in case you have previously customized your `Files: Exclude` settings in VS Code. More info in [Default settings](https://code.visualstudio.com/docs/getstarted/settings#_default-settings).

6. In VS Code choose **Terminal** &rarr; **New Terminal** from its menu.

    A new terminal opens in the lower right part of the VS Code screen.

7. In the VS Code terminal, run the following command.

    ```Shell/Bash
    npm install
    ```

8. In the VS Code terminal, start a CAP server.

    ```Shell/Bash
    cds watch
    ```

    > In case you get the error: `cds : File <npmIstallDirectory>\cds.ps1 cannot be loaded because running scripts is disabled on this system.` after you run `cds watch`

    > You can run the command:

    > ```bash
    > Set-ExecutionPolicy -ExecutionPolicy Bypass -Scope CurrentUser
    > ```

    > This will change the script execution policy for your user to `Bypass` directly from the VS Code terminal. To learn more about execution policies, see [About Execution Policies](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_execution_policies?view=powershell-7.1).

    The CAP server serves all the CAP sources from your project. It also "watches" all the files in your projects and conveniently restarts whenever you save a file. Changes you have made will immediately be served without you having to do anything.

    The screen now looks like this:

    ![CDS Watch](cdswatch.png)

    The CAP server tells you that there is no model and no service definitions yet that it can serve. You add some in the next step.

[VALIDATE_1]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Add files to the project)]
1. Open the Finder on Mac or the Explorer on Windows and navigate to the `tutorial` directory created in [Prepare Your Development Environment for CAP](btp-app-prepare-dev-environment-cap).

2. Open the directory `templates` and keep it open as you copy a number of files from there. For this part of the tutorial and others, it's probably best if you place it next to your VS Code instance.

    !![Windows](codeandfinder.png)

    Alternatively, you can open it as a second folder in your VS Code project: **File** &rarr; **Add Folder to Workspace...**.

3. Copy the file `schema.cds` from `templates/create-cap-application/db` to the `db` folder of your app.

    This is the code:

    <!-- cpes-file db/schema.cds -->
    ```JavaScript
    namespace sap.ui.riskmanagement;
    using { managed } from '@sap/cds/common';
      entity Risks : managed {
        key ID      : UUID  @(Core.Computed : true);
        title       : String(100);
        prio        : String(5);
        descr       : String;
        miti        : Association to Mitigations;
        impact      : Integer;
        criticality : Integer;
      }
      entity Mitigations : managed {
        key ID       : UUID  @(Core.Computed : true);
        description  : String;
        owner        : String;
        timeline     : String;
        risks        : Association to many Risks on risks.miti = $self;
      }
    ```


    It creates two entities in the namespace `sap.ui.riskmanagement`: `Risks` and `Mitigations`. Each of them has a key called `ID` and several other properties. A Risk has a Mitigation and, therefore, the property `miti` has an association to exactly one Mitigation. A Mitigation in turn can be used for many Risks, so it has a "to many" association. The key is automatically filled by the CAP server, which is exposed to the user of the service with the annotation `@(Core.Computed : true)`.

    Notice how the CAP server reacted to dropping the file. It now tells you that it has a model but there are no service definitions yet and, thus, it still can't serve anything. Next, you add a service definition.

4. Copy the file `risk-service.cds` from `templates/create-cap-application/srv` to the `srv` folder of your app.

    The content of the file looks like this:

    <!-- cpes-file srv/risk-service.cds -->
    ```JavaScript
    using { sap.ui.riskmanagement as my } from '../db/schema';
    @path: 'service/risk'
    service RiskService {
      entity Risks as projection on my.Risks;
        annotate Risks with @odata.draft.enabled;
      entity Mitigations as projection on my.Mitigations;
        annotate Mitigations with @odata.draft.enabled;
    }
    ```

    It creates a new service `RiskService` in the namespace `sap.ui.riskmanagement`. This service exposes two entities: `Risks` and `Mitigations`, which are exposing the entities of the database schema you've created in the step before.

    If you again look at the terminal, you see that the CAP server has noticed the new file and now tells us that it serves something under <http://localhost:4004>.

5. In your browser open the link <http://localhost:4004>.

    !![Service](service.png)

    > You may have to stop the CAP server with <kbd>Ctrl</kbd> + <kbd>C</kbd> and restart it with the `cds watch` command.

6. Choose the `$metadata` link.

    You see the OData metadata document of your new service. So, with just the two files for the database schema and the service exposure you added to your project, you have already got a running OData service! You might wonder why the service itself is called `risk` even though in the file it's called `RiskService`. This is a convention in CAP, the service suffix is subtracted from the name.

    If you now choose the `Risks` link, you only get this:

    ```JavaScript
    {
        @odata.context: "$metadata#Risks",
        value: [ ]
    }
    ```

    So, there's no data yet. This is because so far, your model doesn't contain any data. You add some now.

7. Copy the folder `data` from `templates/create-cap-application/db` to the `db` folder of your app. If VS Code asks you whether to copy the folder, confirm.

    You have now added two comma-separated value (CSV) files that contain local data for both the `Risks` and the `Mitigations` entities. A quick look into the `sap.ui.riskmanagement-Risks.csv` (the name consists of your namespace and the name of your database entity from the `schema.cds` file) file shows data like this:

    ```csv
    ID;createdAt;createdBy;title;prio;descr;miti_ID;impact
    20466922-7d57-4e76-b14c-e53fd97dcb11;2021-04-27T00:00:00.000Z;max.mustermann@muster.com;CFR non-compliance;Fred Fish;3;Recent restructuring might violate CFR code 71;20466921-7d57-4e76-b14c-e53fd97dcb11;10000
    ...
    ```

    The first line contains all the properties from your `Risks` entity. While the other ones are straight forward, consider the `miti_ID` property. In your entity, you only have a `miti` property, so where does it come from? `miti` is an association to `Mitigations`, as `Mitigations` could have several key properties, the association on the database needs to point to all of these, therefore the CAP server creates a property `<AssociationProperty>_<AssociatedEntityKey>` for each key.

    As always, the CAP server has noticed the change.

    > You may have to stop the CAP server with <kbd>Ctrl</kbd> + <kbd>C</kbd> and restart it with the `cds watch` command.

8. Revisit the `Risks` entity <http://localhost:4004/service/risk/Risks> in your browser. You now see the data exposed.

    !![Service Data](servicedata.png)


    > The Risks entity looks different?

    > When you revisit the **Risks** entity, you might see something like this instead of the nicely-formatted output above.
    >     !![No JSON Viewer](no-json-viewer.png)
    > However, this doesn't mean you have made a mistake in the tutorial. Rather, this is the correct output without any formatting. If you'd like to see a formatted output in your browser, you can add a plugin to your browser. Here are a few exemplary JSON formatters for different browsers:

    > - [Chrome](https://chrome.google.com/webstore/detail/jsonvue/chklaanhfefbnpoihckbnefhakgolnmc)
    > - [Microsoft Edge](https://microsoftedge.microsoft.com/addons/detail/jsonview/kmpfgkgaimakokfhgdahhiaaiidiphco)
    > - [Safari](https://apps.apple.com/us/app/json-peep-for-safari/id1458969831?mt=12)

And that's it. You now have a full blown OData service, which complies with the OData standard and supports the respective queries without having to code anything but the data model and exposing the service itself.

> The service is completely exposed without any authentication or authorization check. You extend the service later in the tutorial [Implement Roles and Authorization Checks In CAP](btp-app-cap-roles) with such checks.



[DONE]
The result of this tutorial can be found in the [`create-cap-application`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/create-cap-application) branch.


[ACCORDION-END]
---