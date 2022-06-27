@echo off
title RabbitMQ Server
color 0e

SETLOCAL ENABLEEXTENSIONS
SETLOCAL ENABLEDELAYEDEXPANSION

cd /d "%~dp0"

start http://localhost:8080

docker network rm rabbitmq
docker rm -f rabbitmq

docker network create --driver bridge rabbitmq
docker run --rm -it --network rabbitmq --name rabbitmq --hostname rabbitmq -p 8080:15672 -p 5672:5672 -p 61613:61613 -m 2048m rabbitmq:3-management

echo.
pause