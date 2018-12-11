from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import sys, os, collections, re

from PIL import Image

import tensorflow as tf
from hdbcli import dbapi

tf.app.flags.DEFINE_string ('host', 'localhost' , 'HXE host')
tf.app.flags.DEFINE_integer('port', 39015       , 'HXE port')
tf.app.flags.DEFINE_string ('usr' , 'ML_USER'   , 'HXE user name')
tf.app.flags.DEFINE_string ('pwd' , '<< password >>'  , 'HXE password')

tf.app.flags.DEFINE_string ('image_dir' , os.path.expanduser("~") + '/models/tutorials/image_retraining/test/flower_photos' , 'image directory')

args = tf.app.flags.FLAGS

def create_image_lists(image_dir):
    if not tf.gfile.Exists(image_dir):
        tf.logging.error("Image directory '" + image_dir + "' not found.")
        return None
    result = collections.OrderedDict()

    sub_dir_paths = sorted(x[0] for x in tf.gfile.Walk(image_dir))

    # The root directory comes first, so skip it.
    is_root_dir = True
    for sub_dir_path in sub_dir_paths:
        if is_root_dir:
            is_root_dir = False
            continue
        extensions = ['jpg', 'jpeg', 'JPG', 'JPEG']
        file_list = []
        dir_name = os.path.basename(sub_dir_path)
        if dir_name == args.image_dir:
            continue
        tf.logging.info("Looking for images in '" + dir_name + "'")
        for extension in extensions:
            file_glob = os.path.join(image_dir, dir_name, '*.' + extension)
            file_list.extend(tf.gfile.Glob(file_glob))
        if not file_list:
            tf.logging.warning('No files found for label '+ dir_name)
            continue

        label_name = re.sub(r'[^a-z0-9]+', ' ', dir_name.lower())
        files = []
        for file_name in file_list:
            base_name = os.path.basename(file_name)
            files.append(base_name)
        result[label_name] = {
            'dir': sub_dir_path,
            'label': label_name,
            'files': files,
        }
    return result

def main(_):
    connection = dbapi.connect(address=args.host, port=args.port, user=args.usr, password=args.pwd)
    connection.setautocommit(False)
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
        cursor.execute('DROP TABLE TF_DATA.FLOWERS_DATA;')
    except (RuntimeError, TypeError, NameError, dbapi.Error):
        pass

    # Create the tables
    cursor.execute('CREATE TABLE TF_DATA.FLOWERS_DATA  (IMAGE_ID INTEGER, IMAGE_LABEL VARCHAR(50), IMAGE_FILE_PATH VARCHAR(255), IMAGE_FILE_NAME VARCHAR(255), IMAGE_RAW_DATA BLOB);')
    query_flowers = 'INSERT INTO TF_DATA.FLOWERS_DATA  (IMAGE_ID, IMAGE_LABEL, IMAGE_FILE_PATH, IMAGE_FILE_NAME, IMAGE_RAW_DATA) VALUES (:id, :label,:path,:file,:raw_data)'

    image_lists = create_image_lists(args.image_dir)

    imported_row = 0
    for image_label in image_lists.keys():
        image_file_list = image_lists[image_label]
        image_file_dir  = image_file_list["dir"]
        for image_file in image_file_list["files"]:
            with open(image_file_dir + "/" + image_file, 'rb') as f:
                imported_row +=1
                jpeg_data = f.read()
                img_blob = dbapi.LOB()
                cursor.execute(query_flowers, {"id": imported_row,"label": image_label, "path": image_file_dir, "file": image_file, "raw_data": img_blob })
                img_blob.write(data=jpeg_data)
                img_blob.close()
                connection.commit()

    print("Import row(s) " + str(imported_row))
    cursor.close()
    connection.close()

if __name__ == '__main__':
    tf.app.run()
