---
author_name: Josh Bentley
author_profile: https://github.com/jarjarbentley
title: Setup AWS - Customize your SageMaker Notebook for SAP HANA
description: In your SageMaker Notebook instance, download and install the SAP HANA client and the Python driver to connect to your SAP HANA, express edition instance.
primary_tag: topic>machine-learning
auto_validation: true
tags: [ tutorial>intermediate, topic>cloud, topic>machine-learning, products>sap-hana\,-express-edition, products>sap-hana ]
time: 20
---

## Details
### You will learn  
  - Access Amazon SageMaker Notebook instance
  - Download & install the SAP HANA client and the Python driver (***`hdbcli`***)

Once downloaded and installed, you will be able to connect to your SAP HANA, express edition and start running commands to create tables, or simply run select statements directly from your SageMaker Notebook instance.

Additionally, you could also leverage your SAP HANA instance when running SageMaker training jobs, but this would require to build and deploy a new container image for SageMaker. Therefore, this tutorial won't address this scenario.

> ### **Note:**
>
Be aware that SageMaker Notebooks are ephemeral, therefore any package installed will be lost after the notebook instance is restarted.
>
To avoid that you can create a lifecycle configuration that runs each time you start your notebook instance.
But in this series, you simply install the Python packages whenever needed in your notebooks and assume you will run the series without restarting the SageMaker Notebook instance.

[ACCORDION-BEGIN [Step 1: ](Access the SageMaker Notebook instance)]

If you have your Jupyter Notebook instance already open in your browser, then you can move to the next step.

Else, access the <a href="https://console.aws.amazon.com/sagemaker" target="&#95;blank">Amazon SageMaker Console</a> (you also use the search for **SageMaker** in the Amazon Web Services Management Console).

![Amazon Web Services](sagemaker-01.png)

Click on **Open `JupyterLab`**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Open a new terminal)]

Once open, you should have access to your Notebook instance.

![Amazon Web Services](sagemaker-02.png)

On the menu bar, select **File > New > Terminal**.

![Amazon Web Services](sagemaker-03.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Download SAP HANA client)]

The following sets of commands which will download and extract the SAP HANA client package for Linux locally.

> ### **Note:** If the URL is not working you can get the latest one from <https://tools.hana.ondemand.com>.

In the terminal console, paste the following code:

```shell
mkdir -p ~/SageMaker/downloads/hanaclient
cd ~/SageMaker/downloads

wget --no-cookies \
  --header "Cookie: eula_3_1_agreed=tools.hana.ondemand.com/developer-license-3_1.txt" \
  "https://tools.hana.ondemand.com/additional/hanaclient-2.4.151-linux-x64.tar.gz" \
  -P ~/SageMaker/downloads

tar -xvf ~/SageMaker/downloads/hanaclient-*-linux-x64.tar.gz
rm -f ~/SageMaker/downloads/hanaclient-*-linux-x64.tar.gz
```

> ### **Note:** To paste the content of your current clipboard, press **SHIFT** + **INSERT** instead of CTRL + V and use **CTRL** + **INSERT** instead of CTRL + C.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Setup SAP HANA client)]

The SAP HANA client package provides the client binary installer which needs to be installed in order to access the HDBCLI Python library.

In the terminal console, you can now paste the following sets of commands which will install the SAP HANA client package:

```shell
mkdir -p ~/SageMaker/sap/hdbclient

cd ~/SageMaker/downloads/client
./hdbinst --path=/home/ec2-user/SageMaker/sap/hdbclient/

cd ~
rm -Rf ~/SageMaker/downloads/client
```

The default installation path will be: ***`/home/ec2-user/SageMaker/sap/hdbclient/`***

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Install HDBCLI Python library)]

You can now run the following commands to install the HDBCLI Python library:

```shell
source activate tensorflow_p36
pip install /home/ec2-user/SageMaker/sap/hdbclient/hdbcli-*.tar.gz
```

As reminder, the installed package will be removed if the SageMaker Notebook instance is restarted.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Install HANA ML Python library)]

You can now run the following commands to install the HDBCLI Python library:

```shell
source activate tensorflow_p36
pip install /home/ec2-user/SageMaker/sap/hdbclient/hana_ml-*.tar.gz
```

As reminder, the installed package will be removed if the SageMaker Notebook instance is restarted.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_3]
[ACCORDION-END]
