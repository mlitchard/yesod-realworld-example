FROM ubuntu:16.04

RUN    touch /etc/apt/sources.list.d/pgdg.list \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ xenial-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
    && apt-get update \
    && apt-get -y -q install wget \
    && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | \
       apt-key add - \
    && apt-get update \
    && apt-get -y -q install postgresql-9.6 postgresql-client-9.6 


USER postgres

RUN    /etc/init.d/postgresql start \
    && psql --command "CREATE USER yrw WITH SUPERUSER PASSWORD 'konstrUCT&';" \
    && createdb -O yrw mediumdb

USER root

RUN echo "host all  all    0.0.0.0/0  md5" >> /etc/postgresql/9.6/main/pg_hba.conf

RUN echo "listen_addresses='*'" >> /etc/postgresql/9.6/main/postgresql.conf

# Expose the PostgreSQL port
EXPOSE 5432

RUN mkdir -p /var/run/postgresql && chown -R postgres /var/run/postgresql

# Add VOLUMEs to allow backup of config, logs and databases
VOLUME  ["/etc/postgresql", "/var/log/postgresql", "/var/lib/postgresql"]

# ENTRYPOINT ["/bin/bash"]

USER postgres

# Set the default command to run when starting the container
CMD [ "/usr/lib/postgresql/9.6/bin/postgres" \
    , "-D"                                   \
    , "/var/lib/postgresql/9.6/main"         \
    , "-c"                                   \
    , "config_file=/etc/postgresql/9.6/main/postgresql.conf"]
