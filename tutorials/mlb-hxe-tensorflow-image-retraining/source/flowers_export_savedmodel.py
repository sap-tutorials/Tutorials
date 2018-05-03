from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import numpy as np
import os, shutil, time, urllib

import tensorflow as tf

tf.app.flags.DEFINE_string ('export_path' , os.path.expanduser("~") + '/export/flowers'                                , 'export path')
tf.app.flags.DEFINE_string ('labels_path' , os.path.expanduser("~") + '/models/tutorials/image_retraining/labels.txt'  , 'labels path')
tf.app.flags.DEFINE_string ('graph_path'  , os.path.expanduser("~") + '/models/tutorials/image_retraining/output_graph.pb'  , 'graph path')

args = tf.app.flags.FLAGS

def build_raw_image_node (raw_image_data):

    input_height   = 299
    input_width    = 299
    input_mean     = 0
    input_std      = 255
    input_channels = 3

    raw_image_data_squeezed = tf.squeeze(raw_image_data)
    image_reader = tf.image.decode_jpeg(raw_image_data_squeezed, channels=input_channels, name="jpeg_reader")

    float_caster = tf.cast(image_reader, tf.float32)
    dims_expander = tf.expand_dims(float_caster, 0)

    resized = tf.image.resize_bilinear(dims_expander, [input_height, input_width])
    normalized = tf.divide(tf.subtract(resized, [input_mean]), [input_std])
    return normalized

def main(unused_argv):
    session = tf.InteractiveSession()

    # create a new tf.string input placeholder that will be processed to produce the proper format
    raw_image_data = tf.placeholder(tf.string, shape = [None])
    new_input_placeholder = build_raw_image_node(raw_image_data)

    # reload the graph definition, and update the Placeholder input with the new definition
    with tf.gfile.FastGFile(args.graph_path, 'rb') as f:
        graph_def = tf.GraphDef()
        graph_def.ParseFromString(f.read())
        _, final_result = tf.import_graph_def(
            graph_def,
            name='',
            return_elements=['Placeholder:0', 'final_result:0'],
            input_map={'Placeholder:0':new_input_placeholder}
        )

    # extract the list of labels
    label_lines = [line.rstrip() for line in tf.gfile.GFile(args.labels_path)]
    # get a tensor with the top result
    values, indices = tf.nn.top_k(final_result, 1)

    # build the table lookup tensor with labels from the indices
    prediction_classes_descriptions = []
    for idx in range(len(label_lines)):
        prediction_classes_descriptions.append(label_lines[idx])
    prediction_classes_tensor = tf.constant(prediction_classes_descriptions)
    table = tf.contrib.lookup.index_to_string_table_from_tensor(prediction_classes_tensor, default_value="UNKNOWN")

    prediction_classes = table.lookup(tf.to_int64(indices))
    tf.tables_initializer().run(session=session)

    # build the signature def
    signature_def = (
        tf.saved_model.signature_def_utils.build_signature_def(
            inputs = {
                'image_blob'  : tf.saved_model.utils.build_tensor_info(raw_image_data),
            },
            outputs = {
                'class'       : tf.saved_model.utils.build_tensor_info(prediction_classes),
                'probability' : tf.saved_model.utils.build_tensor_info(values),
            },
            method_name = tf.saved_model.signature_constants.PREDICT_METHOD_NAME
        )
    )
    # re-create the model sub-directory
    shutil.rmtree(args.export_path)
    os.makedirs(args.export_path)

    # save the model with proper format
    legacy_init_op = tf.group(tf.tables_initializer(), name='legancy_init_op')
    builder = tf.saved_model.builder.SavedModelBuilder(args.export_path + "/" + str(int(round(time.time()))))
    builder.add_meta_graph_and_variables (
        session,
        [tf.saved_model.tag_constants.SERVING],
        signature_def_map = {
            tf.saved_model.signature_constants.DEFAULT_SERVING_SIGNATURE_DEF_KEY : signature_def
        },
        legacy_init_op = legacy_init_op,
    )
    builder.save(as_text=False)
    session.close()

if __name__ == '__main__':
    tf.app.run()
