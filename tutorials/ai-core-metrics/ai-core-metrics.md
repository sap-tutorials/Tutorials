---
parser: v2
auto_validation: true
time: 45
tags: [ tutorial>beginner, topic>artificial-intelligence, topic>machine-learning, software-product>sap-ai-launchpad, software-product>sap-ai-core ]
primary_tag: software-product>sap-ai-core
author_name: Dhrubajyoti Paul
author_profile: https://github.com/dhrubpaul
---

# Generate Metrics and Compare Models in SAP AI Core
<!-- description --> Explore different ways of logging metrics during training and compare generated models.

## Prerequisites
- You have an understanding of using data and generating models in SAP AI Core, from [this tutorial](https://developers.sap.com/tutorials/ai-core-data.html/#)

## You will learn
- How to log simple metrics on validation data and training data
- How to log step information along with metrics
- How to log custom metrics structure

## Intro
In this tutorial, you'll use SAP AI Launchpad to compare two models that have been generated using SAP AI Core. This tutorial builds on the previous tutorials on house price prediction and ingesting data.

> Important: Comparing models is only available using SAP AI Launchpad, and not the API endpoints. The comparison step is optional.

---

### Starter code


Create a folder named `hello-aicore-metrics`. Within it, create a file called `main.py`, and paste the following starter code to this file.

```PYTHON
import os
from sklearn.tree import DecisionTreeRegressor
from sklearn.model_selection import KFold, train_test_split
from sklearn.inspection import permutation_importance
from datetime import datetime
import pandas as pd
from ai_core_sdk.models import Metric, MetricTag, MetricCustomInfo, MetricLabel
#
# Logging Metrics: SAP AI Core connection (Step 2)
# <PASTE CODE HERE>
#
# Variables
DATA_PATH = '/app/data/train.csv'
DT_MAX_DEPTH= int(os.getenv('DT_MAX_DEPTH'))
MODEL_PATH = '/app/model/model.pkl'
#
# Load Datasets
df = pd.read_csv(DATA_PATH)
X = df.drop('target', axis=1)
y = df['target']
#
# Metric Logging: Basic (Step 3)
# <PASTE CODE HERE>
#
# Partition into Train and test dataset
train_x, test_x, train_y, test_y = train_test_split(X, y, test_size=0.3)
#
# K-fold
kf = KFold(n_splits=5, random_state=31, shuffle=True)
i = 0 # storing step count
for train_index, val_index in kf.split(train_x):
    i += 1
    # Train model on subset
    clf = DecisionTreeRegressor(max_depth=DT_MAX_DEPTH, random_state=31)
    clf.fit(train_x.iloc[train_index], train_y.iloc[train_index])
    # Score on validation data (hold-out dataset)
    val_step_r2 = clf.score(train_x.iloc[val_index], train_y.iloc[val_index])
    # Metric Logging: Step Information (Step 4)
    # <PASTE CODE HERE>
    # Delete step model
    del(clf)
#
# Final Model
clf = DecisionTreeRegressor(max_depth=DT_MAX_DEPTH, random_state=31)
clf.fit(train_x, train_y)
# Scoring over test data
test_r2_score = clf.score(test_x, test_y)
# Metric Logging: Attaching to metrics to generated model (Step 5)
# <PASTE CODE HERE>
#
# Model Explaination
r = permutation_importance(
    clf, test_x, test_y,
    n_repeats=30,
    random_state=0
)
# Feature importances
feature_importances = str('')
for i in r.importances_mean.argsort()[::-1]:
    feature_importances += f"{df.columns[i]}: {r.importances_mean[i]:.3f} +/- {r.importances_std[i]:.3f} \n"
# Metric Logging: Custom Structure (Step 6)
# <PASTE CODE HERE>
#
# Save model
import pickle
pickle.dump(clf, open(MODEL_PATH, 'wb'))
#
# Metric Logging: Tagging the execution (Step 7)
# <PASTE CODE HERE>
```

> The snippet includes some placeholders that state `# <PASTE CODE HERE>`. You'll complete these entries throughout the tutorial. For clarity, the comments in the code also include the relevant step number.

This Python script contains all of the modifications needed for logging metrics, meaning that you can leave your previous workflows as they are.


### Add connection


Add the following code snippet.

```PYTHON
from ai_core_sdk.tracking import Tracking
aic_connection = Tracking()
...
```

> **CAUTION**: This code snippet is very similar to the code used to connect the SAP AI Core SDK to your local system. However, in this case, it is required to establish a connection within the SAP AI Core execution environment.


### Add basic metric logging


Add the following snippet to log the number of observations in your dataset.

```PYTHON
# from ai_core_sdk.models import Metric
aic_connection.log_metrics(
    metrics = [
        Metric(
            name= "N_observations", value= float(df.shape[0]), timestamp=datetime.utcnow()),
    ]
)
```

After execution, this is shown in SAP AI Launchpad. You can zoom in for details.

<!-- border -->![image](img/ail/basic.png)


### Step information


Add the following snippet to store metrics for step information. This snippet is also useful for tracking the metrics on epochs of the training process.

```PYTHON
aic_connection.log_metrics(
    metrics = [
        Metric(name= "(Val) Fold R2", value= float(val_step_r2), timestamp=datetime.utcnow(), step=i),
    ]
)
```

The variable `i` is already present in your code to pass to the parameter `step=i`.  This is also shown in SAP AI Launchpad. You can zoom in to view.

<!-- border -->![image](img/ail/step.png)


### Attach metrics to generated model


Add the following snippet to store metrics for artifact information.

```PYTHON
aic_connection.log_metrics(
    metrics = [
        Metric(
            name= "Test data R2",
            value= float(test_r2_score),
            timestamp=datetime.utcnow(),
            labels= [
                MetricLabel(name="metrics.ai.sap.com/Artifact.name", value="housepricemodel")
            ]
        )
    ]
)
```

The parameter `value="housepricemodel"` refers to the artifact name, which references the model that will be stored in AWS S3. The name of this parameter must match the name that you defined in your YAML workflow.

Your code should resemble:

<!-- border -->![image](img/ail/ModelA.png)

After execution, this is shown in SAP AI Launchpad.

<!-- border -->![image](img/ail/ModelB.png)


### Custom metrics for model inspection


Add the following snippet to store metrics based on a customized structure.

```PYTHON
aic_connection.set_custom_info(
    custom_info= [
        MetricCustomInfo(name= "Feature Importance (verbose)", value=  str(r)),
        MetricCustomInfo(name= "Feature Importance (brief)", value= feature_importances )
    ]
)
```

The structure must be type-cast to `str` (a string). Here, the structure used is [**permutation feature importance**](https://scikit-learn.org/stable/modules/permutation_importance.html#permutation-importance).

The variables `r` and `feature_importances` are already created in the starter code.

After execution, you can see this in SAP AI Launchpad.

<!-- border -->![image](img/ail/custom.png)

> ### Permutation Feature Importance
>
> Sources: [`Scikit Learn Python Package`](https://scikit-learn.org/stable/modules/permutation_importance.html#permutation-importance)
>
> What it is?
>
> - Shows how much a model depends on a given feature for a given target, model, dataset and task.
> - Gives an empirical estimate of how much loss is attributed to the removal of a given feature.
>
> What it is not:
>
> - A model, dataset or task-agnostic indication of the importance of a given feature. While the method is agnostic, the results are applicable only to the specific input combination.
> - An accurate indication of the importance of a given feature for a specific prediction. Although this is the goal of the method, it does not account for weaknesses in the model.
>
> Advantages:
>
> - Model agnostic.
> - Provides global `explainability` - meaning that it estimates each feature's importance to the prediction task.
> - Contributes to model transparency,
> - The method or function used to measure "error" can be customized, with reference to `scikit` package implementation.



### Add tags for execution meta after training


Add the following snippet to tag your execution. The `tags` are customizable key-values.

```PYTHON
aic_connection.set_tags(
    tags= [
        MetricTag(name="Validation Method Used", value= "K-Fold"), # your custom name and value
        MetricTag(name="Metrics", value= "R2"),
    ]
)
```

After execution, you can see this in SAP AI Launchpad.

<!-- border -->![image](img/ail/tag.png)


### Complete files


Check your modified `main.py` by comparing it with the following expected `main.py`.

```PYTHON
import os
from sklearn.tree import DecisionTreeRegressor
from sklearn.model_selection import KFold, train_test_split
from sklearn.inspection import permutation_importance
from datetime import datetime
import pandas as pd
from ai_core_sdk.models import Metric, MetricTag, MetricCustomInfo, MetricLabel
#
from ai_core_sdk.tracking import Tracking
aic_connection = Tracking()
#
# Variables
DATA_PATH = '/app/data/train.csv'
DT_MAX_DEPTH= int(os.getenv('DT_MAX_DEPTH'))
MODEL_PATH = '/app/model/model.pkl'
#
# Load Datasets
df = pd.read_csv(DATA_PATH)
X = df.drop('target', axis=1)
y = df['target']
#
# Metric Logging: Basic
aic_connection.log_metrics(
    metrics = [
        Metric(
            name= "N_observations", value= float(df.shape[0]), timestamp=datetime.utcnow()),
    ]
)
#
# Partition into Train and test dataset
train_x, test_x, train_y, test_y = train_test_split(X, y, test_size=0.3)
#
# K-fold
kf = KFold(n_splits=5, random_state=31, shuffle=True)
i = 0 # storing step count
for train_index, val_index in kf.split(train_x):
    i += 1
    # Train model on subset
    clf = DecisionTreeRegressor(max_depth=DT_MAX_DEPTH, random_state=31)
    clf.fit(train_x.iloc[train_index], train_y.iloc[train_index])
    # Score on validation data (hold-out dataset)
    val_step_r2 = clf.score(train_x.iloc[val_index], train_y.iloc[val_index])
    # Metric Logging: Step Information
    aic_connection.log_metrics(
        metrics = [
            Metric(name= "(Val) Fold R2", value= float(val_step_r2), timestamp=datetime.utcnow(), step=i),
        ]
    )
    # Delete step model
    del(clf)
#
# Final Model
clf = DecisionTreeRegressor(max_depth=DT_MAX_DEPTH, random_state=31)
clf.fit(train_x, train_y)
# Scoring over test data
test_r2_score = clf.score(test_x, test_y)
# Metric Logging: Attaching to metrics to generated model
aic_connection.log_metrics(
    metrics = [
        Metric(
            name= "Test data R2",
            value= float(test_r2_score),
            timestamp=datetime.utcnow(),
            labels= [
                MetricLabel(name="metrics.ai.sap.com/Artifact.name", value="housepricemodel")
            ]
        )
    ]
)
#
# Model Explaination
r = permutation_importance(
    clf, test_x, test_y,
    n_repeats=30,
    random_state=0
)
# Feature importances
feature_importances = str('')
for i in r.importances_mean.argsort()[::-1]:
    feature_importances += f"{df.columns[i]}: {r.importances_mean[i]:.3f} +/- {r.importances_std[i]:.3f} \n"
# Metric Logging: Custom Structure
aic_connection.set_custom_info(
    custom_info= [
        MetricCustomInfo(name= "Feature Importance (verbose)", value=  str(r)),
        MetricCustomInfo(name= "Feature Importance (brief)", value= feature_importances )
    ]
)
#
# Save model
import pickle
pickle.dump(clf, open(MODEL_PATH, 'wb'))
#
# Metric Logging: Tagging the execution
aic_connection.set_tags(
    tags= [
        MetricTag(name="Validation Method Used", value= "K-Fold"), # your custom name and value
        MetricTag(name="Metrics", value= "R2"),
    ]
)
```

Check your modified `requirements.txt` by comparing it with the following expected `requirements.txt`.

```TEXT
sklearn==0.0
pandas
ai-core-sdk>=1.15.1
```

Create a file called `Dockerfile` with the following snippet. This file must not have a file extension or alternative name.

```TEXT
# Specify which base layers (default dependencies) to use
# You may find more base layers at https://hub.docker.com/
FROM python:3.7
#
# Creates directory within your Docker image
RUN mkdir -p /app/src/
# Don't place anything in below folders yet, just create them
RUN mkdir -p /app/data/
RUN mkdir -p /app/model/
#
# Copies file from your Local system TO path in Docker image
COPY main.py /app/src/
COPY requirements.txt /app/src/  
#
# Installs dependencies within you Docker image
RUN pip3 install -r /app/src/requirements.txt
#
# Enable permission to execute anything inside the folder app
RUN chgrp -R 65534 /app && \
    chmod -R 777 /app
```

Use the following commands to build your Docker image and push the contents to the cloud.

```BASH
docker build -t <YOUR_DOCKER_REGISTRY>/<YOUR_DOCKER_USERNAME>/house-price:04 .
docker push <YOUR_DOCKER_REGISTRY>/<YOUR_DOCKER_USERNAME>/house-price:04
```

Paste the following snippet to a file named `hello-metrics.yaml` in your GitHub repository. Edit it with you own Docker registry secret and username. This file is your AI workflow file.

```YAML
apiVersion: argoproj.io/v1alpha1
kind: WorkflowTemplate
metadata:
  name: house-metrics-train # executable id, must be unique across all your workflows (YAML files)
  annotations:
    scenarios.ai.sap.com/description: "Learning how to ingest data to workflows"
    scenarios.ai.sap.com/name: "House Price (Tutorial)" # Scenario name should be the use case
    executables.ai.sap.com/description: "Generate metrics"
    executables.ai.sap.com/name: "training-metrics" # Executable name should describe the workflow in the use case
    artifacts.ai.sap.com/housedataset.kind: "dataset" # Helps in suggesting the kind of artifact that can be attached.
    artifacts.ai.sap.com/housemodel.kind: "model" # Helps in suggesting the kind of artifact that can be generated.
  labels:
    scenarios.ai.sap.com/id: "learning-datalines"
    ai.sap.com/version: "2.0"
spec:
  imagePullSecrets:
    - name: credstutorialrepo # your docker registry secret
  entrypoint: mypipeline
  arguments:
    parameters: # placeholder for string like inputs
        - name: DT_MAX_DEPTH # identifier local to this workflow
  templates:
  - name: mypipeline
    steps:
    - - name: mypredictor
        template: mycodeblock1
  - name: mycodeblock1
    inputs:
      artifacts:  # placeholder for cloud storage attachements
        - name: housedataset # a name for the placeholder
          path: /app/data/ # where to copy in the Dataset in the Docker image
    outputs:
      artifacts:
        - name: housepricemodel # name of the artifact generated, and folder name when placed in S3, complete directory will be `../<executaion_id>/housepricemodel`
          globalName: housemodel # local identifier name to the workflow, also used above in annotation
          path: /app/model/ # from which folder in docker image (after running workflow step) copy contents to cloud storage
          archive:
            none:   # specify not to compress while uploading to cloud
              {}
    container:
      image: docker.io/<YOUR_DOCKER_USERNAME>/house-price:04 # Your docker image name
      command: ["/bin/sh", "-c"]
      env:
        - name: DT_MAX_DEPTH # name of the environment variable inside Docker container
          value: "{{workflow.parameters.DT_MAX_DEPTH}}" # value to set from local (to workflow) variable DT_MAX_DEPTH
      args:
        - "python /app/src/main.py"
```



### Create configuration and execution


Create a configuration using the following values. The values are taken from the workflow from previous steps. For help creating a configuration, see step 11 of [this tutorial](https://developers.sap.com/tutorials/ai-core-data.html/#).

|  | Value |
| --- | --- |
| Configuration Name | House Price (Jan) metrics |
| Scenario Name | `House Price (Tutorial)`
| Version | `2.0`
| Executable Name | `training-metrics`
| Scenario ID | `learning-datalines`
| Executable ID | `house-metrics-train`

The value for `Input Parameters` `DT_MAX_DEPTH` is your choice. Until now, this has been set using an environment variable. If a variable is not specified, this parameter continues to be defined by the environment variables.

> Information: This parameter can be defined using an integer as a string, to set a maximum depth or as `None`, which means that nodes are expanded until all leaves are single nodes, or contain all contain fewer data points than specified in the `min_samples_split samples`, if specified. For more information, see the [Scikit learn documentation](https://scikit-learn.org/stable/modules/generated/sklearn.tree.DecisionTreeClassifier.html).

Attach your registered artifact to `Input Artifact`, by specifying `housedataset` for this value.

Create an execution from this configuration.


### Retrieve metrics



[OPTION BEGIN [SAP AI Launchpad]]

In the `ML Operations` app, choose `Executions`. Navigate to your execution and choose the `Metric Resources` tab.

<!-- border -->![image](img/ail/locate.png)

For metrics tagged with the artifact name, you can also locate the metrics in the **Models** details for the artifact.

<!-- border -->![image](img/ail/artifact.png)

[OPTION END]


[OPTION BEGIN [Postman]]

Click `AI Core` > `lm` > `metrics` > `Get metrics` and double check the `executionId`.

<!-- border -->![image](img/postman/metric.png)

[OPTION END]

[OPTION BEGIN [SAP AI Core SDK]]

Paste and edit, then execute the following snippet:

```PYTHON
tracking_client = Tracking(base_url = aic_service_key["serviceurls"]["AI_API_URL"] + "/v2",
    auth_url=  aic_service_key["url"] + "/oauth/token",
    client_id = aic_service_key['clientid'],
    client_secret = aic_service_key['clientsecret'])

response = tracking_client.query(
    execution_ids = [
        'e1f2169db8760b5d' # list of execution IDs for which to query metrics
    ],
    select=["metrics","tags", "customInfo"], # Optional, if not supplied then all the metrics resources are retrieved.
    resource_group = 'default'
)


for execution in response.resources:
  print("Execution ID:", execution.execution_id)
  print("Metrics")
  for metric in execution.metrics:
      print(metric.__dict__)
  for tag in execution.tags:
      print(tag.__dict__)
  for custom_metric in execution.custom_info:
      print(" Name: ", custom_metric.name)
      print("  Value: ", custom_metric.value)
  print('')
```


**OUTPUT**

```
Execution ID: e1f2169db8760b5d
Metrics
{'name': '(Val) Fold R2', 'value': 0.5182814362664407, 'timestamp': datetime.datetime(2022, 4, 25, 7, 52, 59), 'step': 1, 'labels': []}
{'name': '(Val) Fold R2', 'value': 0.5038220342940908, 'timestamp': datetime.datetime(2022, 4, 25, 7, 52, 59), 'step': 2, 'labels': []}
{'name': '(Val) Fold R2', 'value': 0.5137276831150328, 'timestamp': datetime.datetime(2022, 4, 25, 7, 52, 59), 'step': 3, 'labels': []}
{'name': '(Val) Fold R2', 'value': 0.5397039116247253, 'timestamp': datetime.datetime(2022, 4, 25, 7, 53), 'step': 4, 'labels': []}
{'name': '(Val) Fold R2', 'value': 0.5325857010663992, 'timestamp': datetime.datetime(2022, 4, 25, 7, 53), 'step': 5, 'labels': []}
{'name': 'N_observations', 'value': 20640.0, 'timestamp': datetime.datetime(2022, 4, 25, 7, 52, 59), 'step': 0, 'labels': []}
{'name': 'Test data R2', 'value': 0.5403981219634746, 'timestamp': datetime.datetime(2022, 4, 25, 7, 53), 'step': 0, 'labels': [<ai_api_client_sdk.models.metric_label.MetricLabel object at 0x00000215B8EFA850>]}
{'name': 'Validation Method Used', 'value': 'K-Fold'}
{'name': 'Metrics', 'value': 'R2'}
...
 Name:  Feature Importance (brief)
  Value:  MedInc: 1.069 +/- 0.017
AveOccup: 0.126 +/- 0.005
AveRooms: 0.025 +/- 0.002
Longitude: 0.000 +/- 0.000
Latitude: 0.000 +/- 0.000
Population: 0.000 +/- 0.000
AveBedrms: 0.000 +/- 0.000
HouseAge: 0.000 +/- 0.000
Unnamed: 0: 0.000 +/- 0.000  
```

[OPTION END]


### Compare metrics (optional)


Create two configurations: one with `DT_MAX_DEPTH = 3` and the other with `DT_MAX_DEPTH = 6`, then create executions for both configurations.

<!-- border -->![image](img/ail/compare-1.png)

You can then compare metrics for the executions using the two different configurations.

<!-- border -->![image](img/ail/compare-2.png)


