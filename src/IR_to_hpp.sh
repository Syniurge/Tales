#!/bin/sh

cat >"$2" <<EOF
#pragma once

namespace Tales {
	const char* RuntimeIR = R"TalesIR(
EOF

cat "$1" >>"$2"

cat >>"$2" <<EOF
	)TalesIR";
}
EOF