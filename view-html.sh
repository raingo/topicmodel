#!/bin/sh
# vim ft=sh
#
# Watches the folder or files passed as arguments to the script and when it
# detects a change it automatically refreshes the current selected Chrome tab or
# window.
#
# http://razius.com/articles/auto-refreshing-google-chrome-on-file-changes/
#
# Usage:
# ./chrome-refresh.sh /folder/to/watch /some/folder/file_to_watch.html

TIME_FORMAT='%F %H:%M'
OUTPUT_FORMAT='%T Event(s): %e fired for file: %w. Refreshing.'

while inotifywait -q --timefmt "${TIME_FORMAT}" --format "${OUTPUT_FORMAT}" "$@"; do
    CHROME_WINDOW_ID=$(xdotool search --onlyvisible --class firefox | sort | tail -1)
    xdotool key --window $CHROME_WINDOW_ID 'CTRL+r'
done
