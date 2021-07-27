# CLESSNVERSE V2

Welcome to our work in progress. This branch is dedicated to the creation of an improved version of CLESSNVERSE which will cover a wider array of applications. Two main categories of functions are to be explored. First, specific functions which are useful for the research chair's projects, but not for the wider public. Second, universal functions that are generalized and useful for domestication, analysis and visualization of data. 

The package is divided into 6 `R` scripts each containing multiple functions :

  1. [agoraplus.R](https://github.com/clessn/clessnverse/blob/v2/R/agoraplus.R)
  2. [civimetre.R](https://github.com/clessn/clessnverse/blob/v2/R/civimetre.R)
  3. [radarplus.R](https://github.com/clessn/clessnverse/blob/v2/R/radarplus.R)
  4. [domestication.R](https://github.com/clessn/clessnverse/blob/v2/R/domestication.R)
  5. [analysis.R](https://github.com/clessn/clessnverse/blob/v2/R/analysis.R)
  6. [visualization.R](https://github.com/clessn/clessnverse/blob/v2/R/visualization.R)

It might not be the best way to organize our repo long term, but for the scale we are in, it's the best option. 

## How to use the branch system

### Creating an empty new branch

  - git branch
  - git checkout *name-of-master*
  - git checkout -- orphan *name-new-branch*
  - git rm -rf
  - git push --set-upstream origin *name-new-branch*

### Creating a new branch

  - git branch
  - git checkout *name-of-master*
  - git checkout -b *name-new-branch*
  - git push --set-upstream origin *name-new-branch*

### Push-Pull recipe

  - git branch
  - git checkout *name-of-right-branch*
  - git status
  - git add -A
  - git commit -m "*usefull message*"
  - git pull
  - git status
  - git push
