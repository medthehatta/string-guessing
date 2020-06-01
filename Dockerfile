FROM ubuntu:20.04

# Based on https://github.com/blebo/docker-bottlebase
MAINTAINER med

# Install nginx, uwsgi and pip.
RUN echo 'deb http://archive.ubuntu.com/ubuntu trusty main universe' >> /etc/apt/sources.list
RUN apt-get update -y
ARG DEBIAN_FRONTEND=noninteractive
ARG DEBCONF_NONINTERACTIVE_SEEN=true
RUN echo "tzdata tzdata/Areas select Europe" >> /tmp/preseed && \
    echo "tzdata tzdata/Zones/Europe select Berlin" >> /tmp/preseed && \
    debconf-set-selections /tmp/preseed && \
    apt-get update -y
RUN apt-get install -y \
    gcc \
    make \
    python3 \
    python3-venv \
    python3-pip \
    python3-dev \
    python3-wheel \
    nginx-full

RUN ln -sf /usr/bin/python3 /usr/bin/python

# Set-up app folder.
RUN mkdir -p /var/www/bottledockit

# Remove default nginx site config.
RUN rm /etc/nginx/sites-enabled/default

# Symlink nginx config files for the app.
RUN ln -s /var/www/bottledockit/nginx_bottledockit /etc/nginx/sites-enabled/

# Set-up virtualenv with system Python
RUN mkdir /opt/venv
RUN python -m venv /opt/venv/bottledockit

# Expose port 80.
EXPOSE 80

# Add uwsgi and bottle to the virtualenv.
RUN /opt/venv/bottledockit/bin/pip install wheel
RUN /opt/venv/bottledockit/bin/pip install bottle uwsgi

# Add other requirements to the virtualenv.
ADD requirements.txt /var/www/bottledockit/requirements.txt
RUN /opt/venv/bottledockit/bin/pip install -r /var/www/bottledockit/requirements.txt

# Need a /games directory for this app
RUN mkdir /games
RUN chown root:www-data /games
RUN chmod g+w /games

# Copy in the app
ADD . /var/www/bottledockit

# Set permissions so that uwsgi can access app and virtualenv.
RUN chown -R www-data:www-data /opt/venv/bottledockit
RUN chown -R www-data:www-data /var/www/bottledockit
RUN chmod 755 /var/www

CMD service nginx restart; /opt/venv/bottledockit/bin/uwsgi --ini /var/www/bottledockit/uwsgi_bottledockit.ini
