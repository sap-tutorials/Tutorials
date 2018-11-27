---

title: Continuous Integration (CI) Best Practices with SAP – Generic Project
description: Part 4.1 – Configuring the CI system for Maven-based generic Java project.
primary_tag: products>sap-cloud-platform
tags: [  tutorial>intermediate, tutorial:type/project ]
time: 15

---

## Prerequisites

  - **Proficiency:** Intermediate
  - [Landscape Configuration](https://www.sap.com/developer/tutorials/ci-best-practices-landscape.html)

## Next Steps

  - [Back to the Navigator](https://www.sap.com/developer/tutorials/ci-best-practices-intro.html)

---


This generic project outlines a CI process for a pure Java project without any deployment. The build tool is Maven,
which must be installed on a local developer host.
The general procedure is as follows: first, create a project in Gerrit together with the first branch, the main integration branch `master`. Clone the (initially empty)
repository on a local developer workplace, commit the code and push it as a first commit to Gerrit. Next use Jenkins to configure a voter build, which builds
the commit before it is merged into the `master` branch. Finally implement the CI build.

#### Prerequisites

- A developer workplace (PC) with the following installations:
    - Java JDK
    - Git
    - Maven
- A running Git/Gerrit instance
- A running Jenkins master and a Jenkins build slave
- A running Nexus with at least one snapshot and one release repository

![Component Setup](generic-project-1.png)

Eclipse as development environment is commonly used but not required, and considered a prerequisite.


### Basic Project Setup

#### Procedure

1. Use a browser to access the Gerrit front end on port 8080. Log in as your admin user.

2. Go to **Projects > Create New Project**.

3. Enter the project name `HelloWorld` and select **Create initial empty commit**. This automatically creates the `master` branch.

4. Enter `Rights Inherit From:` `CI-Projects`.

    ![Component Setup](generic-project-2.png)  

    Alternatively, you may follow the steps for command line-based creation as described in the Gerrit documentation:  

    > [Gerrit: create-project](https://gerrit-review.googlesource.com/Documentation/cmd-create-project.html)

5. Create an initial clone of the project on your PC and check out the `master` branch.

6. On your local PC, to push commits to Gerrit, add a commit hook into the
    `<Git repository root>/.git/hooks` directory. You can obtain this from `http://<hostname of Gerrit>:8080/tools/hooks/commit-msg`.
    Every developer who plans to do commits against Gerrit needs this hook.

    > [Git: Commit Message Hook](https://git.eclipse.org/r/Documentation/cmd-hook-commit-msg.html)

7. Add the initial project files into the Git workspace.
    For example, add the following files:

    ```
    <Git repository root>/pom.xml
    <Git repository root>/src/main/java/company/org/App.java
    <Git repository root>/src/test/java/company/org/AppTest.java
    ```

    The content of the files is available in the appendix.

8. Create a commit and push:

    ```
    git add .
    git commit -m "Initial version of HelloWorld"
    git push origin master:refs/for/master
    ```

9. In Gerrit, the commit should be listed under **My > Changes**.

10. Assign the commit a `Code-Review+2` and select **Submit**.
    Now the commit is merged into the master branch.


### Jenkins Voter Build

Configure a Jenkins voter build for this branch. The global configurations are already done, so now we only need to define the job.

#### Procedure

1. Open the Jenkins front end. Select **New Item**, then **Maven project**.

2. Enter a project name, for example `VO_HelloWorld_master_build`.
    The name only identifies the job, that is, it plays a technical role. But keep in mind that over time you might have to differentiate many jobs by Gerrit project name,
    branch, or other purposes (such as using the prefix `VO` for "voter build"). You may want to establish your own naming convention.

3. In the general section of the project configuration, select **Restrict where this project can be run** and
    enter the label that you have assigned to the slave, in this case `builds`.

4. In the **Source Code Management** section, select **Git** and enter the following data:

    Field                                    | Value
    :--------------------------------------- | :------------------------------------------------------------------------
    Repository URL                           | `<the ssh based URL of your repository>`
    Credentials                              | `jenkins`

    Click on **Advanced...**:

    Field                                    | Value
    :--------------------------------------- | :------------------------------------------------------------------------
    Ref spec                                 | `refs/changes/*:refs/changes/*`
    Branches to build; Branch Specifier      | `$GERRIT_REFSPEC` (this seems unintuitive, but does work according to the Gerrit documentation)

5. In the **Build Triggers** section, select **Gerrit Event**.

6. In the **Gerrit Trigger** section, choose the server which you already have defined
    in Jenkins.  
    Add a new trigger by clicking on **Add > Patch set Created**. In the **Gerrit Project** pane, enter `HelloWorld` as project name and `master` as
    branch name, both with type `plain`.

7. In the **Build Environment** section, select **Delete workspace before build starts**. This ensures that you always have a clean build without the risk that it is polluted
    by the build before.

8. In the **Build** section, add `clean verify` as **Goals and Options**.

    The Jenkins project is now ready to listen for Gerrit events.

    > [Jenkins Gerrit Trigger Plugin](https://wiki.jenkins.io/display/JENKINS/Gerrit+Trigger)

9. From the local sources on your PC, perform a small change, commit, and push it. Once the change reaches Gerrit, a Jenkins build is
    triggered. Depending on the build status, the code review is rated -1 or +1.

    ![Gerrit configuration](generic-project-3.png)

10. You may want to review the change with +2 and press **Submit** to merge the change into the `master` branch.


### Jenkins CI Build

The CI build reacts on new commits pushed into the branch in Git, that is, an integration with Gerrit is unnecessary.
Instead, you can define a time schedule or manually trigger the build at any time.

#### Procedure

1. Open the Jenkins front end and go to **New Item**.  
    The CI build differs from the voter build only in the details how it is triggered. Hence,
    enter `CI_HelloWorld_master_build` as name, select **Copy existing item** and enter `VO_HelloWorld_master_build`.

2. In the **Source Code Management** section, click on **Advanced...** and clean the field **Ref spec**.  
    Enter `master` as **Branch Specifier**.

3. In the **Build Triggers** section, select **Poll SCM** and enter a pull frequency. In order to have immediate build results after a merge,
    every two minutes could be an appropriate value.

4. When you submit a change into the `master` branch in Gerrit, you see this job start running after some time according
    to the pull frequency.


### Artifact Upload to Nexus

In the current setup, the build results are stored only in the build node's workspace. However, you may need to reuse them:
you might add test steps or you might have other Maven projects that depend on this project.
In most examples of our guide, we will use the
Jenkins archiving feature to temporarily store artifacts instead of a snapshot repository in Nexus. The upload to Nexus
 is usually used by for released versions only.
Nevertheless, we demonstrate here the Nexus upload on a snapshot version to demonstrate the principle.

#### Procedure

1. In your development project, add the following section into the project's `pom.xml` file directly under the `<project>` node:

    ```
    <distributionManagement>
      <snapshotRepository>
        <id>nexus1</id>
        <url><snapshot repository url on Nexus></url>
      </snapshotRepository>
    </distributionManagement>
    ```

    The id `nexus1` refers to the credential entry in the maven installation's `settings.xml` on the Jenkins node machine.
    You can fetch the URL of the snapshot repository from the Repository overview page of the Nexus front end.

2. In Jenkins, go to the build job of the project and open the **Configure** page.

3. In the **Build** section, add `clean deploy` as goals.

4. When you do an additional commit on your project and push it, you should be able to see the artifacts in Nexus uploaded from the build.


### Appendix

#### `App.java`

```
package company.org.halloworld;

public class App
{
    public static void main( String[] args )
    {
        System.out.println( "Hello World!" );
    }
}
```


#### `AppTest.java`

```
package company.org.halloworld;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class AppTest
    extends TestCase
{
    public AppTest( String testName )
    {
        super( testName );
    }
    public static Test suite()
    {
        return new TestSuite( AppTest.class );
    }
    public void testApp()
    {
        assertTrue( true );
    }
}
```

#### `pom.xml`

```
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
  <modelVersion>4.0.0</modelVersion>
  <groupId>company.org</groupId>
  <artifactId>halloworld</artifactId>
  <packaging>jar</packaging>
  <version>1.0-SNAPSHOT</version>
  <name>halloworld</name>
  <url>http://maven.apache.org</url>
  <dependencies>
    <dependency>
      <groupId>junit</groupId>
      <artifactId>junit</artifactId>
      <version>3.8.1</version>
      <scope>test</scope>
    </dependency>
  </dependencies>
<!--  <distributionManagement>
    <snapshotRepository>
      <id>nexusCIProcess</id>
      <url>http://<your nexus host>:8081/nexus/content/repositories/snapshots</url>
    </snapshotRepository>
  </distributionManagement> -->  
</project>
```


