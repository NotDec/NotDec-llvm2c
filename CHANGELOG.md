# Changelog & TODO

## TODO

- Add tests.
- Translate the document to English, maintain in both language.
- Write the book and add the link to readme.
- Add CI and docker to release.

## Changelog

**2024-06-19**

Supporting SwitchStmt. While debuggin `switch_loop.c`, it is trying to remove an edge that is must viable. When folding SwitchStmt, it is not a good idea to fold entry block into it, or any block that dominates the switch.

**2024-04-29**

Initial Commit: Split from the NotDec main repo.

**2024-04-30**

`74_kmp.c` testcase: the reg2mem demote SSA pass create some assignments before the loop condition. it makes a while loop become while true. 
