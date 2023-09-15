#!/bin/zsh
cd ..
zip merge-parallel.zip \
    merge-parallel/lib/**/* \
    merge-parallel/*.sig \
    merge-parallel/*.sml \
    merge-parallel/*.md \
    merge-parallel/project.cm
