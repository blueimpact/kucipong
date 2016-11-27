# Kucipong

[![Build Status](https://secure.travis-ci.org/blueimpact/kucipong.svg)](http://travis-ci.org/blueimpact/kucipong)

## What is Kucipong?

Only [Japanese version](doc/api.md) now.

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

## Run

### Set environment variables

In order to run the web server or the command-line-interface, the user must set
three environment variables, `KUCIPONG_MAILGUN_DOMAIN`,
`KUCIPONG_MAILGUN_APIKEY`, `KUCIPONG_AWS_ACCESS_KEY`, and
`KUCIPONG_AWS_SECRET_KEY`.

`KUCIPONG_MAILGUN_DOMAIN` and `KUCIPONG_MAILGUN_APIKEY` can be found by logging
on to the [Mailgun Dashboard](https://mailgun.com/app/dashboard).  If you are
running the web server or cli while doing development, you can probably use the
default `KUCIPONG_MAILGUN_DOMAIN` (which is
`sandboxfaf3e17ba66f42a5ac5cd36e8a71ad97.mailgun.org`).  You need to find the
api-key for this domain on the Mailgun Dashboard.

`KUCIPONG_AWS_ACCESS_KEY` and `KUCIPONG_AWS_SECRET_KEY` can be found by
creating an AWS account to access S3.

```sh
$ export \
  KUCIPONG_MAILGUN_DOMAIN="sandboxfaf3e17ba66f42a5ac5cd36e8a71ad97.mailgun.org" \
  KUCIPONG_MAILGUN_APIKEY="key-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" \
  KUCIPONG_AWS_ACCESS_KEY="someaccesskey" \
  KUCIPONG_AWS_SECRET_KEY="somesecretkey"
```

Other environment variables can be found in `src/Kucipong/Config.hs`.

### Run web server

```sh
$ make run
```

### Add admin user (CLI)

This sends an email to the user so they can login as an admin.

```bash
$ make build
$ export KUCIPONG_MAILGUN_APIKEY="key-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
$ stack exec -- kucipong-add-admin kucipong.dev@gmail.com "Some User Name"
```

The email will include a url like this: http://localhost:8101/admin/login/4on%2FXBbDmKxFE%2F9pA3gw0m5T2BNhJNDGiZfcgpsKS7rEHEursYi8%2Bm3Fn3l%2FTZRQ%2FPc%3D

This url can be accessed with `curl` in order to get an admin cookie:

```bash
$ curl -v 'http://localhost:8101/admin/login/4on%2FXBbDmKxFE%2F9pA3gw0m5T2BNhJNDGiZfcgpsKS7rEHEursYi8%2Bm3Fn3l%2FTZRQ%2FPc%3D'
...
HTTP/1.1 302 Found
Transfer-Encoding: chunked
Date: Sun, 18 Sep 2016 02:00:04 GMT
Server: Warp/3.2.8
Location: /
Set-Cookie: adminEmail=wv6kyV14ZoF5tMlVLrBP%2FgPLkvEFKog2Lvu%2F8PY4zt4tWbQwbki7hG3Mn83Zilr6LTGWdphD4mnDL96RvMbcxWj1rZbhfQ%3D%3D; path=/; max-age=31536000; expires=Mon, 18-Sep-2017 02:00:04 UTC; HttpOnly
...
```

This admin cookie can be used to access login-protected urls:

```bash
$ curl --cookie "adminEmail=wv6kyV14ZoF5tMlVLrBP%2FgPLkvEFKog2Lvu%2F8PY4zt4tWbQwbki7hG3Mn83Zilr6LTGWdphD4mnDL96RvMbcxWj1rZbhfQ%3D%3D" 'http://localhost:8101/admin/store/create'
```

## Other make targets

Look in the `Makefile` for other targets to run.
