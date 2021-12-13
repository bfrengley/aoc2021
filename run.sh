#!/bin/bash

if ! raco pkg show | grep threading >/dev/null 2>&1; then
    echo "Installing deps..."
    raco pkg install --auto threading
fi

for (( day=1; day <= 25; day++ )); do
    if ! [[ -f "./src/day$day.rkt" ]]; then
        break
    fi

    echo "--> Day $day"
    racket ./src/day"$day".rkt
done
