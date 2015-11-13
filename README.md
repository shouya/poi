# poi

**(IN DEV)** Personal VPS automatic deployment system desu poi.

## What you need to have

* A fresh new Linux VPS (Ubuntu 14.04 LTS recommended)
* A public IP with A/CNAME record points to that VPS
* Full access to a github repository


## Aim

* [x] Lightweight daemon
* [ ] Service monitoring and auto restarting (with supervisord)
* [ ] Instant service deployment (with Github hook)
* [ ] Email feedback with full log (with Mailgun, optional)
* [ ] Plugin support
* [ ] Fully dockerized apps for portability
* [ ] Re-deploy all services on a second server within 10 mins

## Prepare the environment

Get a brand new VPS with Ubuntu 14.04 LTS installed.

Run the follow command and follow the instruction prompted to set up the basic environment:

```
wget xxxx/ubuntu-setup.sh
sudo sh ubuntu-setup.sh
```

Or,

```
curl xxxx/ubuntu-setup.sh | sh -
```

If you already have curl installed.

## Roadmap

* Set up script
  - setup hostname/user/login/security fix/software
  - git clone git@github.com:xxx/poi.git
  - poi --init .
  - poi --test

* Supervisord
  - start poi monitor daemon
  - start other generated confs

* Poi
  - init working directory
  - test if the env works and send an email report
  - github web hook for monitoring

* Poi monitoring
  - git fetch (LOG)
  - git reset --hard origin/master (LOG)
  - compute updated/new services (LOG)
  - rebuild updated services (LOG)
  - regenerate supervisord confs (LOG)
  - pkill -HUP supervisord
  - collect LOGs and send to the dearest user via email
