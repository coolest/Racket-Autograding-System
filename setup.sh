#!/bin/bash
sudo apt-get update
sudo apt-get install libssl-dev

wget https://download.racket-lang.org/releases/8.5/installers/racket-8.5-x86_64-linux.sh
chmod +x racket-8.5-x86_64-linux.sh

./racket-8.5-x86_64-linux.sh --in-place --dest ~/racket --create-dir
rm racket-8.5-x86_64-linux.sh

export PATH="~/racket/bin:$PATH"

yes | apt-get install python3.6