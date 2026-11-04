#!/bin/bash
# Doc 243 S4.3: inner lambdas with a free (captured) variable; see check-net.sh.
exec bash "$(dirname "${BASH_SOURCE[0]}")/check-net.sh" free S4.3
