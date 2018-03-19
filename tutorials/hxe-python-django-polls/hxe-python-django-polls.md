---
title: How to use Django with HANA, express edition
description: A How-To that shows how to integrate Django with the SAP HANA, express edition by identifying HANA specific changes needed to complete the Django `polls` tutorial.
primary_tag: products>sap-hana
tags: [  tutorial>intermediate, products>sap-hana, products>sap-hana\,-express-edition, tutorial>how-to ]
---
## Prerequisites  
 - Proficiency: beginner
 - Setup: `HANA, express edition` must be running.

## Next Steps
 - This is a standalone How-To on basic integration of the Django web framework with HANA, express edition. [View similar How-Tos](http://www.sap.com/developer/tutorials.html) or [View all How-Tos](http://www.sap.com/developer/tutorials.html)

## Details
Most Django based web applications use database back-ends for data persistence. This how-to tutorial will show you how to configure Django and your application to use HANA, express-edition as the backend database. You will learn how to:

  - configure Django settings for HANA, express edition
  - run [the Django Project tutorial](https://docs.djangoproject.com/en/1.10/intro/tutorial01/) using HANA, express edition as the database.

>Note: The `PyHDB` connector is an open source pure Python client for HANA. `PyHDB` does not support the full `Python DBAPI Specification v2.0`, nor does it support the full `SAP HANA SQL Command Network Protocol`.

 The available HANA, express edition versions (1.0 SP12 and 2.0 SP00) have different default instance numbers. The published Tutorials and How-Tos refer to the default HANA 2.0 SP00 instance numbers. When using the SP12 version please use the old default instance number and port (3`<instance number>`15):

 HANA Express Version  | Default Instance ID | Port
 :-------------------  | :------------------ | :---------------
 1.0 SP12              |  00                 | 30013
 2.0 SP00              |  90                 | 39013


### Time to Complete
**20 Min**.

---

## Installing Django and Django HANA Modules

1. Install Django by following the [Django installation instructions]( https://docs.djangoproject.com/en/1.10/topics/install/#). Verify the installation by running `django-admin --version` in a shell. The command should return a version string such as `1.10.5`

2. The Django HANA implementation relies on the `pyhdb` driver, a pure python HANA database driver. Run the pip installer to install `pyhdb`: `pip install pyhdb`. If successful, this pip install command should indicate a successful installation and the version number (e.g. `Successfully installed pyhdb-0.3.3`).

3. Verify that you can connect to the HXE database by creating a small test script.

    a. Create a file called `test_hxe_conn.py` and add the following python code to it. Modify the value based on your needs, using the hints in the comments. At a minimum, you will need to change the host `ip` address and the password.

    ```
    import pyhdb

    # Define the connection to the HXE database
    connection = pyhdb.connect(
        # replace with the ip address of your HXE Host (This may be a virtual machine)
        host="10.172.91.122",
        # 39013 is the systemDB port for HXE on the default instance of 90.
        # Replace 90 with your instance number as needed (e.g. 30013 for instance 00)
        port=39013,
        #Replace user and password with your user and password.
        user="system",
        password="mypassword"
        )

    #Connect to the database and issue a dummy query to verify connectivity
    cursor = connection.cursor()
    cursor.execute("SELECT 'Hello Django HANA World' FROM DUMMY")
    #This should return "Hello Django HANA World"
    myString = cursor.fetchone()[0]
    print myString
    #Close the cursor
    connection.close()
    ```

      b. Run your test script in the python shell: `python test_hxe_conn.py`. If you are able to connect, the following string should be returned: `Hello Django HANA World`. If you are unable to connect, make sure that the connectivity setting--host, user, port and password--are correct for your database.

      c. At this point, you have verified that you can use the `pyhdb` database driver to connect to HANA, express edition. Now you need to verify that Django can communicate with the `pyhdb` driver. To do this, you need to download the `django_hana` module from https://github.com/mathebox/django_hana_pyhdb.

      - Navigate to https://github.com/mathebox/django_hana_pyhdb in a browser.
      - Select `Clone or download`.
      - Select `Download ZIP` and download the zip to a downloads or temporary directory of your choice. This will create the file `django_hana_pyhdb-master.zip`.
      - Unzip the contents of `django_hana_pyhdb-master.zip` under `djhxe`. This will create the folder `djhxe/django_hana_pyhdb-master`.
      - Navigate to `djhxe/django_hana_pyhdb-master`.
      - Run  `python setup.py install`. This should write out messages to the console indicating that it is `running install` and should conclude with the line `Installed <path>/lib/site-packages/django_hana-1.1-py2.7.egg`. There should not be any errors reported by the pip installer.
      - You will also need to install the `six` package, a library for cross version transparency of Python code. To install `six`, run `pip install six`. There should not be any errors reported by the pip installer.

## Following the Django Tutorial

Now that you have verified the installation of Django and verified connectivity from Python to HXE, you can follow [the Django tutorial](https://docs.djangoproject.com/en/1.10/intro/tutorial01/) with the following HANA specific modifications.

1. Note: Be sure to complete this step (Step 1) at the beginning of the [Database Setup section of the Django tutorial](https://docs.djangoproject.com/en/1.10/intro/tutorial02/). You will need to make the following modification to `djhxe/settings.py` that is not included in the tutorial.

    a. Modify the database settings in `djhxe/settings.py` to point to your HANA, express edition database.
    ```
    DATABASES = {
        'default': {
            'ENGINE': 'django_hana',
            'NAME': 'djhxe',  # This is db schema name. It will be added if it does not exist.
            'USER': 'System',
            'PASSWORD': 'myPassword', # Replace with your password.
            'HOST': '10.172.91.122',  # Replace with your HXE server IP address
            'PORT': '39013',
        }
    }
    ```
    b. Because HANA does not support Timezone, you will also need to change the value of `USE_TZ` in `djhxe/settings.py` from True to False: `USE_TZ=False`.
    >Note: As a result of this change, there will be time zone values shown in the online tutorial that do not show in your results.*


## Next Steps
 - [View similar How-Tos](http://www.sap.com/developer/tutorials.html) or [View all How-Tos](http://www.sap.com/developer/tutorials.html)
 - [Complete the Django tutorials](https://docs.djangoproject.com/en/1.10/intro/tutorial01/)
