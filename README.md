# clessnverse
Welcome to the CLESSN's package where all our functions are "supposedly" stored. The package is still under construction. You are now on the master branch, which is the last working version of the package.

## Install package

To install the package, run the code `devtools::install_github("clessn/clessnverse")`.

## Beta functions

If you want to access newer functions currently being developed, you may go look at **branch v1**. Here's how:

### On GitHub

1. See the `master` button a bit up left, right under the `<> Code` button? Click on it.
2. You are now able to switch from branch to branch on **GitHub**.
3. This allows you to **look** at the code and copy paste what you want.

### On your terminal

1. After cloning the repository, you'll see the master branch on your computer. 
2. To change branches, it easy, just do:
    - `git branch` to see where you are
    - `git checkout v1` to switch to the right branch
3. You CANNOT contribute to V1. Otherwise I'll cut you.
4. Seriously, check out number 3.  

<!--Welcome to our work in progress. This branch is dedicated to the creation of an improved version of CLESSNVERSE which will cover a wider array of applications. Two main categories of functions are to be explored. First, specific functions which are useful for the research chair's projects, but not for the wider public. Second, universal functions that are generalized and useful for domestication, analysis and visualization of data. 

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
  - git commit -m "*useful message*"
  - git pull
  - git status
  - git push-->
