#!/bin/bash

if [ $# -eq 1 ]; then
	cp pre_post_ops.glsl $1.glsl
	cp pre_post_ops.dat $1.dat
	cp pre_post_ops.out $1.out
fi
