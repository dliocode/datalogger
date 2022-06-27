@echo off
title Redis Client
color 0f

SETLOCAL ENABLEEXTENSIONS
SETLOCAL ENABLEDELAYEDEXPANSION

cd /d "%~dp0"

docker exec -it redis redis-cli

echo.
pause