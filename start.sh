#!/usr/bin/env bash
stack exec tll 2>&1 | tee -a server.log 
