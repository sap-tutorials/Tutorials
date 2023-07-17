FROM tensorflow/tensorflow:latest-gpu

ENV LANG C.UTF-8

COPY requirements.txt ./requirements.txt
RUN pip3 install -r requirements.txt

ENV SERVE_FILES_PATH=/mnt/models

RUN mkdir -p /app/src
RUN chgrp -R nogroup /app && \
    chmod -R 770 /app

COPY serve.py /app/src/
COPY tf_template.py /app/src/

RUN mkdir -p /mnt/models

CMD ["gunicorn", "--chdir", "/app/src", "serve:app", "-b", "0.0.0.0:9001"]
