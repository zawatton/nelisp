#!/bin/bash
# Doc 243 S4.1: closed inner lambdas (no free variable); see check-net.sh.
exec bash "$(dirname "${BASH_SOURCE[0]}")/check-net.sh" closed S4.1
