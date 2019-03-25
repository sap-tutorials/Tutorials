from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import sys, os

sys.path.append('/home/tmsadm/models/samples/core/get_started')
import iris_data

import tensorflow as tf
from hdbcli import dbapi

tf.app.flags.DEFINE_string ('hxehost', 'localhost' , 'HXE host')
tf.app.flags.DEFINE_integer('hxeport', 39015       , 'HXE port')
tf.app.flags.DEFINE_string ('hxeusr' , 'ML_USER'   , 'HXE user name')
tf.app.flags.DEFINE_string ('hxepwd' , '<< password >>'  , 'HXE password')

args = tf.app.flags.FLAGS

def main(_):
    connection = dbapi.connect(address=args.hxehost, port=args.hxeport, user=args.hxeusr, password=args.hxepwd)
    cursor = connection.cursor()

    # Create a TF_DATA schema
    try:
        cursor.execute('CREATE SCHEMA TF_DATA;')
    except (RuntimeError, TypeError, NameError, dbapi.Error):
        pass

    # Set the current schema to TF_DATA schema
    cursor.execute('SET SCHEMA TF_DATA;')

    # Drop the table before creating them
    try:
        cursor.execute('DROP TABLE TF_DATA.IRIS_DATA;')
    except (RuntimeError, TypeError, NameError, dbapi.Error):
        pass

    # Create the tables
    cursor.execute('CREATE TABLE TF_DATA.IRIS_DATA  (ID INTEGER, SEPALLENGTH FLOAT, SEPALWIDTH FLOAT, PETALLENGTH FLOAT, PETALWIDTH FLOAT, SPECIES INT);')

    query_iris  = 'INSERT INTO TF_DATA.IRIS_DATA  (ID, SEPALLENGTH, SEPALWIDTH, PETALLENGTH, PETALWIDTH, SPECIES) VALUES (?,?,?,?,?,?)'

    # Load the IRIS data in the table
    imported_row = 0
    (train_x, train_y), (test_x, test_y) = iris_data.load_data()
    for index, row in test_x.iterrows():
        cursor.execute(query_iris, (index, row["SepalLength"], row["SepalWidth"], row["PetalLength"], row["PetalWidth"], test_y[index]))
        imported_row +=1
    print("Import row(s) " + str(imported_row))

    cursor.close()
    connection.close()
if __name__ == '__main__':
    tf.app.run()
