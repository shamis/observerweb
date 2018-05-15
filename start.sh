#!/bin/bash

# Parameters:  NodeID ClusterID IMEMPort DDERLPort

host=127.0.0.1 # temporary
name=observerweb@$host # temporary
ck=observerweb
exename=erl

# Node name
node_name="-name $name"

# Cookie
cookie="-setcookie $ck"

# PATHS
paths="-pa"
paths=$paths" $PWD/_checkouts/*/ebin"
paths=$paths" $PWD/_build/default/lib/*/ebin"

start_opts="$paths $cookie $node_name $dist_opts $kernel_opts $imem_opts $dderl_opts $sasl_opts"

# Starting observerweb
echo $exename $start_opts -s observerweb
$exename $start_opts -s observerweb
