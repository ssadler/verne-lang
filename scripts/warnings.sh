#!/bin/bash

rm -rf output/Verne.*
pulp build --force --modules Verne.Program 2>&1
