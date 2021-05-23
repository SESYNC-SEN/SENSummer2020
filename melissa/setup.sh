## check out another git repository and add as sub-directory
## subtree merge
git remote add pg_parser https://github.com/melkimble/pq_parser.git
git fetch pg_parser --no-tags

git checkout -b pg_parser_branch pg_parser/master
git checkout master

git read-tree --prefix=pg_parser/ -u pg_parser_branch
git checkout pg_parser_branch
git pull

git checkout master
git merge --squash -s recursive -Xsubtree=pg_parser pg_parser_branch
##
##

## diff used for multiple pg_parser branches within same subdirectory 
## we won't need to use this
git diff-tree -p pg_parser_branch
# compare pg_parser subdirectory with pg_parser origin/master repo
git diff-tree -p pg_parser/master

## git reset --soft HEAD~1 # undo last commit; pre-push

## Data setup (RStudio)
#Download https://drive.google.com/file/d/11hP2ZEayYl9Ci3RoVpwvlOkdE3604OV-/view?usp=sharing
#Upload to ~

## Commands (Terminal)
cd ~/SENData_subtest
ln -s "$(pwd)" ~/SENSummer2020/Data/01_Original



## fix subproject