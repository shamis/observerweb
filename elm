#!/bin/bash

docker-compose run --rm --service-ports -u $UID:$GID observerweb_elm elm $@
