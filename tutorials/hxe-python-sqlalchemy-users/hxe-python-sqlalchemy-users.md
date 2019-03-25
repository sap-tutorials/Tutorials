---
title: How to use SQLAlchemy with HANA, express edition
description: A How-To that shows how to integrate SQLAlchemy with the SAP HANA, express edition by identifying HANA specific changes needed to complete the SQLAlchemy `users` tutorial.
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>intermediate, products>sap-hana, products>sap-hana\,-express-edition, tutorial>how-to ]
---
## Prerequisites  
 - Proficiency: beginner
 - Setup: `HANA, express edition` must be running.

## Next Steps
 - This is a standalone How-To on basic integration of the SQLAlchemy Object Relational Mapper (ORM) with HANA, express edition. [View similar How-Tos](http://developers.sap.com/tutorials.html) or [View all How-Tos](http://developers.sap.com/tutorials.html)


 __ NOTE: SAP HANA, express edition version 2.0 implications on 'How-Tos' and 'Tutorials' __

 The available HANA, express edition versions (1.0 SP12 and 2.0 SP00) have different default instance numbers. The published Tutorials and How-Tos refer to the default HANA 2.0 SP00 instance numbers. When using the SP12 version please use the old default instance number and port (3`<instance number>`15):

 HANA Express Version  | Default Instance ID | Port
 :-------------------  | :------------------ | :---------------
 1.0 SP12              |  00                 | 30013
 2.0 SP00              |  90                 | 39013

## Experimental Packages
This tutorial relies on the experimental packages [`pyhdb`](https://github.com/SAP/PyHDB/blob/master/README.rst) and [`sqlalchemy-hana`](https://github.com/SAP/sqlalchemy-hana/blob/master/README.rst).
`pyhdb` is a pure python database client for HANA.
`sqlalchemy-hana` is a `SQLAlchemy` dialect for HANA. A `SQLAlchemy` dialect is the system `SQLAlchemy` uses to communicate with a particular database.

## How-To Details
`SQLAlchemy` is an Object Relational Mapper (ORM) that maps python objects to SQL database management system entities. This how-to tutorial will show you how to:
- configure `SQLAlchemy` settings for HANA, express edition
- complete [the SQLAlchemy Project tutorial](http://docs.sqlalchemy.org/en/latest/orm/tutorial.html) using HANA, express edition as the database.
This tutorial assumes that you are installing `SQLAlchemy` and `SQLAlchemy` HANA packages on your client operating system and not on the HANA, express edition virtual machine itself. If you intend to install `SQLAlchemy` on your HANA, express edition virtual machine, you should review the tutorial ["Installing Python Modules on HANA, express edition"](http://developers.sap.com/topics/sap-hana-express.tutorials.html) to prepare the virtual machine for installing Python packages from `pypi`.


### Time to Complete
**20 Min**.

---

## Installing `SQLAlchemy` and `SQLALchemy` HANA Modules

1. Install `sqlalchemy-hana` by following the Install `SQLAlchemy` by following the [SQLAlchemy installation instructions]( https://docs.djangoproject.com/en/1.10/topics/install/#). The `sqlalchemy-hana` installation will also install `SQLAlchemy` if it is not already installed in your environment.

2. The `sql-alchemy-hana` implementation relies on the `pyhdb` driver, a pure python HANA database driver. Run the pip installer to install `pyhdb`: `pip install pyhdb`. If successful, this pip install command should indicate a successful installation and the version number (e.g. `Successfully installed pyhdb-0.3.3`).

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
  cursor.execute("SELECT 'Hello HANA SQLAlchemists' FROM DUMMY")
  #This should return "Hello HANA SQLAlchemists"
  myString = cursor.fetchone()[0]
  print myString
  #Close the cursor
  connection.close()
  ```
  b. Run your test script in the python shell: `python test_hxe_conn.py`. If you are able to connect, the following string should be returned: `Hello HANA SQLAlchemists`. If you are unable to connect, make sure that the connectivity setting--host, user, port and password--are correct for your database.


## Running the Tutorial

After making the changes below, you will be able to follow the [SQLAlchemy tutorial](http://docs.sqlalchemy.org/en/latest/orm/tutorial.html) using HANA Express as the backend database.

1. In the "Connecting" section of the [tutorial](http://docs.sqlalchemy.org/en/latest/orm/tutorial.html) you will need to replace the following code for connecting to `sqlite`:

```
from sqlalchemy import create_engine
engine = create_engine('sqlite:///:memory:', echo=True)
```

with the following to connect to HANA, express edition:

```
from sqlalchemy import create_engine
engine = create_engine('hana+pyhdb://{}:{}@{}:{}'.format('system', 'myPassword', '10.125.20.57', '39013'))
```

The order of the format arguments is user , password, IP address or host name, port.


2. In the "Declare a Mapping" section of the [tutorial](http://docs.sqlalchemy.org/en/latest/orm/tutorial.html) you will need to add a line defining the `__table_args__` to the User class.

```
from sqlalchemy import Column, Integer, String
class User(Base):
    __tablename__ = 'users'
    #This line was added for HANA express
    __table_args__ = {'hana_table_type': 'COLUMN'}        

    id = Column(Integer, primary_key=True)
    name = Column(String)
    fullname = Column(String)
    password = Column(String)

    def __repr__(self):
    return "<User(name='%s', fullname='%s', password='%s')>" % (self.name, self.fullname, self.password)

```

