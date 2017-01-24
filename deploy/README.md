# Deployment

This directory contains files for staging/production server setup/deployment.

## Deploy

Run following command on the parent directory (root of kucipong repository).

```bash
GOOGLE_MAP_API_KEY="xxxxx" ./deploy/deploy.sh SERVER_HOST_NAME
```

where `SERVER_HOST_NAME` is the host name for the server you want to deploy to.
(i.e., The `ssh SERVER_HOST_NAME` successfully connected to the server.)

Make sure to replace `xxxxx` with proper API key listed in [API manager](https://console.developers.google.com/apis/credentials?project=kucipong-dev)

The deploy command make it possible to realize no-down-time deployment with the help of [`keter`](https://hackage.haskell.org/package/keter).

## Set up server

In this section, I'll mention about set up steps for Ubuntu 16.04 server.

### Install dependent files

```bash
sudo apt-get -y install ufw postgresql
```

### Fire wall settings

If you planning to run kucipong server on port `PORT`, open the port with [`ufw`](https://help.ubuntu.com/community/UFW).
Also, make sure to open port for ssh.

### PostgreSQL settings

Make sure to replace `YOUR_PASSWORD_HERE` with complicated password.

```bash
sudo -u postgres -- psql --command "CREATE ROLE kucipong NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN ENCRYPTED PASSWORD 'YOUR_PASSWORD_HERE'"
sudo -u postgres -- createdb kucipong
sudo -u postgres -- psql --command "GRANT ALL PRIVILEGES ON DATABASE kucipong TO kucipong"
```

### Add user for deployment

For security reason, add a non-sudoers user for deployment.

```bash
sudo adduser kucipong
sudo -i -u kucipong
mkdir -p .ssh/
# Paste your id_rsa.pub for ssh as this user
cat >> .ssh/authorized_keys
```

### Set up keter

First, install `keter` on your local machine and copy it to the server.

```bash
# on your local machine.
stack install keter
scp ~/.local/bin/keter ${SUDOERS_USER}@${SERVER_HOST}:
```

Second, after ssh to the server as a sudoers user, set up keter.

```bash
# on remote machine
sudo mkdir -p /opt/keter/bin
sudo cp ~/keter /opt/keter/bin
sudo mkdir -p /opt/keter/etc
cat > /tmp/keter-config.yaml <<EOF
# Directory containing incoming folder, where to store logs, etc. Relative to
# the config file directory.
root: ..

# Keter can listen on multiple ports for incoming connections. These ports can
# have HTTPS either enabled or disabled.
listeners:
    # HTTP
    - host: "*4" # Listen on all IPv4 hosts
      #port: 80 # Could be used to modify port
    # HTTPS
    # - host: "*4"
    #   #port: 443
    #   key: key.pem
    #   certificate: certificate.pem

# User to run applications as

# setuid: ubuntu

# Get the user's IP address from x-forwarded-for. Useful when sitting behind a
# load balancer like Amazon ELB.

# ip-from-header: true
EOF
sudo chown root:root /tmp/keter-config.yaml
sudo mv /tmp/keter-config.yaml /opt/keter/etc
sudo mkdir -p /opt/keter/incoming
sudo chown $USER_NAME_FOR_DEPLOYMENT /opt/keter/incoming
```

### Start `keter` daemon

```bash
# on remote machine
# Make sure to replace `xxxx` with real password and api key.
cat > /tmp/keter.service <<EOF
[Unit]
Description=Keter
After=network.service

[Service]
Type=simple
ExecStart = /opt/keter/bin/keter /opt/keter/etc/keter-config.yaml
Environment=KUCIPONG_DB_PASSWORD=xxxx
Environment=KUCIPONG_MAILGUN_APIKEY=xxxx
Restart = always

[Install]
WantedBy=multi-user.target
EOF

sudo chown root:root /tmp/keter.service
sudo mv /tmp/keter.service /etc/systemd/system/
sudo systemctl enable keter
sudo systemctl start keter
```
