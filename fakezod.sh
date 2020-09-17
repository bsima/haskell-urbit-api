#!/usr/bin/env bash
if [[ -d ./zod ]]
then
    urbit zod
else
    urbit -F zod
fi
