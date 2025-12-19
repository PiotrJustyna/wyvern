#!/bin/bash

TAG="0.3.0.2"
TAG_DESCRIPTION="first wyvern tag"

git tag -a "$TAG" -m "$TAG_DESCRIPTION"
git push origin "$TAG"
