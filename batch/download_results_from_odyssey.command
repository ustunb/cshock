#!/bin/bash
TITLE="Odyssey Download Results"
local_dir="/Users/berk/Dropbox (Harvard University)/repos/fwh"
login_name="berk@login.rc.fas.harvard.edu"

#set title
window_title="\033]0;${TITLE}\007"
echo -n -e ${window_title}

#sync data
rsync -avze ssh --include '*_results.pickle' --exclude '/*/*' --prune-empty-dirs ${login_name}:'~/repos/fwh/results/' "${local_dir}/results/"

#close window
window_close_cmd='tell application "Terminal" to close (every window whose name contains "${window_title}")'
osascript -e "${window_close_cmd}"