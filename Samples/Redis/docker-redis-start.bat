@echo off
title Redis Server
color 0c

SETLOCAL ENABLEEXTENSIONS
SETLOCAL ENABLEDELAYEDEXPANSION

cd /d "%~dp0"

docker network rm redis
docker rm -f redis

docker network create --driver bridge redis
docker run --rm -it  --network redis --name redis --hostname redis -p 6379:6379 -m 2048m -v redis:/data redis

echo.
pause