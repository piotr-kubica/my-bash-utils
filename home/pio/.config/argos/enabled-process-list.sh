#!/usr/bin/env bash
echo "<span color='#AAA' font-family='Monospace' weight='normal'><small>Services</small></span> | length=50"
echo "---"
echo "enabled | bash='systemctl list-unit-files --type=service | grep enabled'"
echo "disabled | bash='systemctl list-unit-files --type=service | grep disabled'"
