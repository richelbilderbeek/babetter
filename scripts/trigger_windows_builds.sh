#!/bin/bash
#
# Trigger all Windows builds
#
repos="beautier_on_windows beastier_on_windows tracerer_on_windows mauricer_on_windows babette_on_windows"

# Clone
for repo in $repos
do
  git clone https://github.com/richelbilderbeek/$repo
done

# Checkout develop
for repo in $repos
do
  cd $repo
  git checkout develop
  git pull
  cd ..
done

# Trigger build
for repo in $repos
do
  cd $repo
  echo " " >> README.md
  git add .
  git commit -m "Trigger rebuild"
  git push
  cd ..
done
