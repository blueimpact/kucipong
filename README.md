# Kucipong

## What is Kucipong?

TODO

## Steps to install postgres

Use the following steps to install postgres and set it up to be used with kucipong.

```sh
# install postgres

# become postgres user
$ sudo -i -u postgres

# as postgres user, initialize the database
# (maybe only needed on arch linux?)
$ initdb --locale en_US.UTF-8 -E UTF8 -D '/var/lib/postgres/data'

# as postgres user, comment out all lines in the following file and add the
# following line.  This disables local trust-based authentication (any local
# user can login as any database user without a password), and enables
# password-based authentication (but only from localhost).
$ echo 'host all all 127.0.0.1/32 md5' >> /var/lib/postgres/data/pg_hba.conf

# create the kucipong user for developement and testing
$ sudo -u postgres -- psql --command "CREATE ROLE kucipong NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN ENCRYPTED PASSWORD 'nuy07078akyy1y7anvya7072'"

# create the kucipong database for developement
$ sudo -u postgres -- createdb kucipong

# create the kucipong database for testing
$ sudo -u postgres -- createdb kucipong_test

# grant access to both dev db and testing db for kucipong user
$ sudo -u postgres -- psql --command "GRANT ALL PRIVILEGES ON DATABASE kucipong TO kucipong"
$ sudo -u postgres -- psql --command "GRANT ALL PRIVILEGES ON DATABASE kucipong_test TO kucipong"

# restart postgres service
$ sudo systemctl restart postgresql

# as normal user, try accessing database
$ psql -U kucipong -d kucipong -h 127.0.0.1
```

## Step to install

### Install `stack`

Follow instructions [here](https://github.com/commercialhaskell/stack#how-to-install).

### Build

```sh
$ make build
```

### Run on command line

```sh
$ make run
```

### Other make target

Look in the `Makefile` for other targets to run.
