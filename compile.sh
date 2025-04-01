#!/bin/bash

rm -rf target
mkdir target

mvn clean compile assembly:single