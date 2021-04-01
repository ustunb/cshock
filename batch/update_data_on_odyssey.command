#!/bin/bash
TITLE="Odyssey Data Sync"
login_name="berk@login.rc.fas.harvard.edu"
local_dir="/Users/berk/Dropbox (Harvard University)/repos/fwh/"
cluster_dir="~/repos/fwh/"

#set title
window_title="\033]0;${TITLE}\007"
echo -n -e ${window_title}

#data files
rsync -avze ssh --include '*_processed.pickle' --exclude '*' "${local_dir}data/" --progress "${login_name}:${cluster_dir}data/"

#close window
window_close_cmd='tell application "Terminal" to close (every window whose name contains "${window_title}")'
osascript -e "${window_close_cmd}"