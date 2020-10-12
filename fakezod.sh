#!/usr/bin/env bash
set -ex
if [[ -d ./zod ]]
then
    urbit $@ zod
else
    urbit $@ -F zod
fi
