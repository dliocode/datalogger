@echo off
title RabbitMQ Command
color 0e

SETLOCAL ENABLEEXTENSIONS
SETLOCAL ENABLEDELAYEDEXPANSION

cd /d "%~dp0"

docker exec -it rabbitmq rabbitmq-plugins enable rabbitmq_stomp

echo.
pause