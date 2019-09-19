---
title: Set up and start the Jenkins Cx Server
description: Set up and start the Jenkins Cx Server running in a Docker container on an AWS EC2 instance.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>cloud]
primary_tag: products>sap-s-4hana-cloud-sdk
---

## Prerequisites
 - An account on Amazon AWS
 - Created an SAP Cloud SDK app

## Details
### You will learn
  - How to securely copy files from your local environment to your AWS EC2 instance
  - How to edit your AWS EC2 instance's Inbound Rules
  - How to start and run the Jenkins CI/CD Server
  - How to secure your Jenkins CI/CD Server

As mentioned in the introduction tutorial of this series, the **`Cx Server`** is the software infrastructure required to run Cloud SDK pipeline. It makes use of Jenkins, various plugins for Jenkins, Docker, various command line tools (maven, npm, cf-cli) and Nexus Open Source Edition.


---

[ACCORDION-BEGIN [Step 1: ](Copy cx-server folder from app to AWS)]

Open a new terminal and navigate to the root of your SAP Cloud SDK app.

You now copy the `cx-server` folder and its content to the home directory of the `ec2-user` on your AWS EC2 instance with the following command:

```
scp -i /path/my-key-pair.pem -r ./cx-server ec2-user@<public hostname>:~
```

For instance:

```
scp -i ~/.ssh/AWS_CI_CF.pem -r ./cx-server ec2-user@ec2-3-123-30-162.eu-central-1.compute.amazonaws.com:~
```

![Copy cx-server directory](ci-aws-3-jenkins-01.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Make cx-server script executable)]

After you have copied the `cx-server` directory to AWS, you need to make the containing script executable in order to run it.

Log in to your AWS EC2 instance with the command:

```
ssh -i /path/my-key-pair.pem ec2-user@<public hostname>
```

For example:

```
ssh -i ~/.ssh/AWS_CI_CF.pem ec2-user@ec2-1-123-35-162.eu-central-1.compute.amazonaws.com
```

Change directory to `cx-server`:

```
cd cx-server
```

If you now list the contents of that directory, you'll notice the containing `cx-server` script is not yet executable.

You can fix that with the following command:

```
chmod +x cx-server
```

If you now list the directory contents, you see the script is now executable:

![Copy cx-server directory](ci-aws-3-jenkins-02.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Start the SAP Cloud SDK Cx server)]

You can now start the SAP Cloud SDK CI/CD server. While in the `cx-server` directory on your AWS EC2 instance, run the following command:

```
sudo ./cx-server start
```

If you run this script for the first time,  it will download the latest version of the SAP Cloud SDK CI/CD Server from Docker Hub and instantiates a container. This may take a couple of seconds:

![Start cx-server](ci-aws-3-jenkins-03.png)

Any subsequent starts, the script will simply reuse the already existing container.

When the Docker download and container instantiation has finished, you can try and open the Jenkins application in the next step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Try to run Cx Server in a browser)]

Open a new browser window, and navigate to the public DNS of your AWS EC2 instance. Since the CI/CD Server by default runs on port 80, you can just point to the public hostname, for instance:

```
http://ec2-3-123-30-162.eu-central-1.compute.amazonaws.com
```

However, if you followed the tutorial from the letter and created a new EC2 instance from scratch, nothing will happen and eventually your browser will time out... This is because by default the EC2 instance only accepts inbound communication via SSH port 22.

In order to be able to run the CI/CD Server on port 80, you need to create a new Inbound Rule for the AWS EC2 instance

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add Inbound Rule)]

Open the AWS Console, navigate to the **EC2 Dashboard** and select your EC2 instance:

![Start cx-server](ci-aws-3-jenkins-04.png)

Scroll to the right until you see the column **Security Groups** and click the **launch-wizard-1** link. You are now displaying the details of your EC2 security group:

![Start cx-server](ci-aws-3-jenkins-05.png)

Click the **Actions** button and from the context-menu, select **Edit Inbound Rules**.

In the popup, you'll see an existing rule for SSH connections on port 22.

Click the **Add Rule** button, and a new rule is added. Change the value of the **Type** to **HTTP**, and leave the other defaults.

![Start cx-server](ci-aws-3-jenkins-06.png)

Click the **Save** button to dismiss the popup and save the changes to the EC2 instance's security group.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Run the Cx Server in a browser)]

If you now try to run the CI/CD Server in a browser by pointing it to the public DNS of your EC2 instance, Jenkins should display just fine:

![Start cx-server](ci-aws-3-jenkins-07.png)

> As you have noticed, you can access Jenkins *without logging in*. And since it is exposed on a publicly accessible URL, *potentially anyone could access it*. In the next and final step, you will enable security so only a logged in administrator has access to your Jenkins environment.

[VALIDATE_6]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Secure your Cx Server)]

On Jenkins landing page, click **Manage Jenkins** and then click the **Configure Global Security** button.

Make sure you set the following settings:

| Field | Value | Enabled / Disabled |
|----|----|----|
| Enable security | | **`enabled`** |
| Access Control -> Security Realm | Jenkins' own user database | **`enabled`** |
| Access Control -> Authorization | Logged-in users can do anything | **`enabled`** |

![Start cx-server](ci-aws-3-jenkins-08.png)

> You may set up additional security, but for now this is sufficient.

Click **Save** when done.

After you have saved the security settings, you are now required to create a Jenkins administrator account.

Provide an account name with strong password, a descriptive full name and email address:

![Start cx-server](ci-aws-3-jenkins-10.png)

Click the **Create First Admin User** button when done. Jenkins security is now set up, and you are now logged in as a Jenkins administrator. The next you access the Jenkins URL, you are asked to provide your login credentials.

Now security is enabled, you are ready to set up the Jenkins pipeline in the next tutorial,

[DONE]
[ACCORDION-END]


---
