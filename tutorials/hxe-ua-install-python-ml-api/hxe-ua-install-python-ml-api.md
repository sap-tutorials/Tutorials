---
title: Install the SAP HANA Python Client API for Machine Learning Algorithms
description: Install the SAP HANA Python Client API for machine learning algorithms.
author_name: Adrian Plata
author_profile: https://github.com/aplata-sap
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>intermediate, products>sap-hana\,-express-edition ]
time: 5
---

<!-- loiof3365096bb2440fcafdb30e9f51877f1 -->

## Prerequisites
Ensure that you have Python 2.7 or 3.6 installed.

You have downloaded, extracted, and installed one of the SAP HANA HDB clients. See [Install the SAP HANA, express edition clients](https://developers.sap.com/group.hxe-install-clients.html).

You have also installed the Python driver that comes with the clients. See [Use Clients to Query an SAP HANA Database](https://developers.sap.com/mission.hana-cloud-clients.html).


## Details
### You will learn
You will learn how to install the SAP HANA Python Client API for Machine Learning Algorithms.

---

SAP HANA provides a python client API for working with machine learning algorithms. The Python Client API for ML files come packages with the Windows and Linux SAP HANA HDB client downloads.

> Note:
> This API is released as an experimental preview and subject to changes now and in the future.
>
>

To install the Python Client API for ML on a client machine, do the following:

[ACCORDION-BEGIN [Step 1: ](Install the Python Client API for ML.)]

For the location you extracted the SAP HANA HDB client files, run:

```bash
pip install hana_ml-<version>.tar.gz
```

This command installs the `hana_ml` package containing the following modules and sub-modules:

-   `hana_ml.dataframe`

-   `hana_ml.algorithms` package

    -   `hana_ml.algorithms.classification`

    -   `hana_ml.algorithms.clustering`

    -   `hana_ml.algorithms.decomposition`

    -   `hana_ml.algorithms.metrics`

    -   `hana_ml.algorithms.mixture`

    -   `hana_ml.algorithms.neighbors`

    -   `hana_ml.algorithms.neural_network`

    -   `hana_ml.algorithms.preprocessing`

    -   `hana_ml.algorithms.regression`

    -   `hana_ml.algorithms.stats`

    -   `hana_ml.algorithms.svm`

    -   `hana_ml.algorithms.trees`

-   `hana_ml.ml_exceptions`


[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Test your installation.)]

To test that Python Client API for ML was installed successfully, run:

```bash
cd /usr/lib/python<version>/site-packages/
ls
```

You will see a list of installed Python packages. You will find `hana_ml` among them if it was installed successfully. Alternatively, you can run:

```bash
pip list
```

You will receive a list of various Python packages that are installed on your system. If you see `hana-ml` and the appropriate version, the Python Client API for ML was installed.

[DONE]

[ACCORDION-END]
