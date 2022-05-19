#! /bin/bash

# Connect to uni vpn without entering your credentials everytime again...

echo <YOUR_PASSWORD> | sudo openconnect -u <YOUR_USERNAME> --passwd-on-stdin asa2.uni-saarland.de
