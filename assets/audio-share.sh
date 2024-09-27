as-cmd -b 0.0.0.0 -e $(as-cmd -l | awk '/JamesDSP Sink/ {print $2}')
