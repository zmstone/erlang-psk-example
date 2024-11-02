#!/bin/bash

set -euo pipefail

rebar3 compile
erl -pa _build/default/lib/psker/ebin -eval 'psker_server:start_link()'
