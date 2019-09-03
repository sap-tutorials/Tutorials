---
author_name: Josh Bentley
author_profile: https://github.com/jarjarbentley
title: Setup AWS - Create a SageMaker Notebook
description: Create a SageMaker Notebook instance that will be used to complete this tutorial series.
primary_tag: topic>machine-learning
auto_validation: true
tags: [ tutorial>intermediate, topic>cloud, topic>machine-learning ]
time: 15
---

## Details
### You will learn  
  - Access Amazon SageMaker Management Console
  - Create a SageMaker Notebook instance

During this tutorial series, you will be using a SageMaker Notebook as your primary tool to not only leverage the SageMaker platform to train your TensorFlow model.

Notebooks are powerful tools that can be used for multiple purposes.

For example in the current scenario, you will use Notebooks to analyze your dataset using Python code, but also to interact with S3 buckets, trigger a SageMaker training job or deploy your trained model as part of a using a TensorFlow Serving Docker image.

To find out more about Amazon SageMaker, please check the following URL: <https://aws.amazon.com/sagemaker/>

[ACCORDION-BEGIN [Step 1: ](Access the SageMaker Console)]

Access the <a href="https://console.aws.amazon.com/sagemaker" target="&#95;blank">Amazon SageMaker Console</a> (you also use the search for **SageMaker** in the Amazon Web Services Management Console).

![Amazon Web Services](sagemaker-01.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create a Notebook instance)]

First, you will need to create a SageMaker Notebook instance.

On the left side, select **Notebook instances**, then click on **Create notebook instance**.

![Amazon Web Services](sagemaker-02.png)

Enter the following **Notebook instance settings** details:

 - Notebook instance name: **`sap-hxe-eml-demo`**

![Amazon Web Services](sagemaker-03-0.png)

Enter the following **Permissions and encryption** details:

 - IAM role: **Enter a custom IAM role ARN**
 - Custom IAM role ARN: enter the ARN for the **`sap-hxe-eml-sagemaker-role`** role create previously

![Amazon Web Services](sagemaker-03-1.png)

Scroll down, then click on **Create notebook instance**.

The process will take up to a few minutes to complete.

![Amazon Web Services](sagemaker-04.png)

Once **In Service**, click on **Open `JupyterLab`**.

![Amazon Web Services](sagemaker-05.png)

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Check the Python and TensorFlow in a Console)]

Now that you have accessed your `JupyterLab` notebook, let's check the Python and TensorFlow version in a `IPython` console.

![Amazon Web Services](sagemaker-06.png)

On the menu bar, select **File > New > Console**.

![Amazon Web Services](sagemaker-07.png)

Select **`conda_tensorflow_p36`** as Kernel then click on **Select**.

![Amazon Web Services](sagemaker-08.png)

This will open an interaction Python environment.

Once open, paste the following code in the input field:

```Python
import sys, tensorflow as tf
print("Python version: {}".format(sys.version))
print("TensorFlow version: {}".format(tf.__version__))
```

![Amazon Web Services](sagemaker-09.png)

Then press **SHIFT** + **ENTER** to execute the command.

![Amazon Web Services](sagemaker-10.png)

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Check the Python and TensorFlow in a Terminal)]

Let's now do the same in a Terminal console.

On the menu bar, select **File > New > Terminal**.

![Amazon Web Services](sagemaker-07.png)

This will open a terminal console with bash.

Once open, paste the following series of commands:

```shell
source activate tensorflow_p36
python -c 'import tensorflow as tf; print(tf.__version__)'
python --version
```

> ### **Note:** To paste the content of your current clipboard, press **SHIFT** + **INSERT** instead of CTRL + V and use **CTRL** + **INSERT** instead of CTRL + C.

![Amazon Web Services](sagemaker-11.png)

[VALIDATE_3]
[ACCORDION-END]
