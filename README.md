# clessnverse

clessnverse regroups general functions for data domestication, analysis and visualization along with functions specific to the research chair's projects.

*Note: This package is under construction.*

## Prerequisites

```R
devtools::install_github("clessn/clessn-hub-r")
devtools::install_github("clessn/hublotr")
```

## Installation

To install the latest stable version of this package, run the following line in your R console:

```R
devtools::install_github("clessn/clessnverse")
```

To access functions under developement, you may go look at the branch `v1`. Here's how:

### On GitHub

1. See the `master` button a bit up left, right under the `<> Code` button? Click on it.
2. You are now able to switch from branch to branch on **GitHub**.
3. This allows you to **look** at the code and copy the desired content.

### On your terminal

1. After cloning the repository, you'll see the master branch on your computer. 
2. To change branches in your terminal, enter:
    - `git branch` to see where you are
    - `git checkout V1` to switch to the desired branch
3. Branch `V1`.

## "I found a bug. What do I do?"

You can submit bugs or suggestions in the Issues tab of this repo. To facilitate problem solving, please include a [minimal reproducible example](https://reprex.tidyverse.org/articles/reprex-dos-and-donts.html).

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
