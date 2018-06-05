#!/bin/sh
mix phx.server 2>&1 | tee -a server.log 
