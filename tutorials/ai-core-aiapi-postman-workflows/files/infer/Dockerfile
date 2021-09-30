FROM python:3.7.11
ENV LANG C.UTF-8

COPY requirements.txt ./requirements.txt
RUN pip3 install -r requirements.txt

RUN mkdir -p /app/src

RUN chgrp -R nogroup /app && \
    chmod -R 770 /app

COPY infer_scikit.py /app/src/

CMD ["gunicorn", "--chdir", "/app/src", "infer_scikit:app", "-b", "0.0.0.0:9001"]