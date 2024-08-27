#!/bin/sh -x

nim --path:. --profiler:on --stackTrace:on c main.nim
